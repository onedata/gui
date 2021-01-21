%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This is the main module for gui library application, used primarily to
%%% start / stop GUI server.
%%% @end
%%%-------------------------------------------------------------------
-module(gui).
-author("Lukasz Opiola").

-include("gui.hrl").
-include("gui_session.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-type method() :: binary(). % <<"GET">> | <<"POST">> etc.
-type gui_config() :: #gui_config{}.
% GUI package for distribution to Onezone, given by path or binary content
-type package() :: file:name_all() | {binary, binary()}.
-export_type([method/0, gui_config/0, package/0]).

% Returns the value converted to bytes.
-define(MAX_GUI_PACKAGE_SIZE, gui:get_env(max_gui_package_size_mb, 50) * 1048576).

% Listener id
-define(HTTPS_LISTENER, https_listener).

%% API
-export([start/1, stop/0, restart_and_reload_web_certs/1]).
-export([healthcheck/0, get_cert_chain_pems/0]).
-export([package_hash/1, extract_package/2, read_package/1]).
-export([get_env/1, get_env/2, set_env/2]).

-define(MAX_RESTART_RETRIES, 10).
-define(RESTART_RETRY_DELAY, timer:seconds(1)).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts a HTTPS server based on provided configuration.
%% @end
%%--------------------------------------------------------------------
-spec start(gui_config()) -> ok | {error, term()}.
start(GuiConfig) ->
    ?info("Starting '~p' server...", [?HTTPS_LISTENER]),

    try
        #gui_config{
            port = Port,
            key_file = KeyFile,
            cert_file = CertFile,
            chain_file = ChainFile,
            number_of_acceptors = AcceptorsNum,
            max_keepalive = MaxKeepAlive,
            request_timeout = RequestTimeout,
            inactivity_timeout = InactivityTimeout,
            dynamic_pages = DynamicPages,
            custom_cowboy_routes = CustomRoutes,
            static_root = StaticRoot,
            custom_response_headers = CustomResponseHeaders
        } = GuiConfig,

        save_port(Port),

        DynamicPageRoutes = lists:map(fun({Path, Methods, Handler}) ->
            {Path, dynamic_page_handler, {Methods, Handler}}
        end, DynamicPages),

        StaticRoutes = case StaticRoot of
            undefined -> [];
            _ -> [
                {"/", cowboy_static, {file, filename:join(StaticRoot, "index.html")}},
                {"/#/[...]", cowboy_static, {file, filename:join(StaticRoot, "index.html")}},
                {"/[...]", cowboy_static, {dir, StaticRoot}}
            ]
        end,

        Dispatch = cowboy_router:compile([
            % Matching requests will be redirected to the same address without
            % leading 'www.'. Cowboy does not have a mechanism to match every
            % hostname starting with 'www.' This will match hostnames with up
            % to 9 segments e. g. www.seg2.seg3.seg4.seg5.seg6.seg7.seg8.com
            {"www.:_[.:_[.:_[.:_[.:_[.:_[.:_[.:_]]]]]]]", [
                {'_', redirector_handler, Port}
            ]},
            {'_', lists:flatten([
                DynamicPageRoutes,
                CustomRoutes,
                StaticRoutes
            ])}
        ]),

        SslOpts = [
            {port, Port},
            {num_acceptors, AcceptorsNum},
            {keyfile, KeyFile},
            {certfile, CertFile},
            {ciphers, ssl_utils:safe_ciphers()},
            {connection_type, supervisor},
            {next_protocols_advertised, [<<"http/1.1">>]},
            {alpn_preferred_protocols, [<<"http/1.1">>]}
        ],

        SslOptsWithChain = case filelib:is_regular(ChainFile) of
            true ->
                save_chain(cert_utils:load_ders(ChainFile)),
                [{cacertfile, ChainFile} | SslOpts];
            _ ->
                SslOpts
        end,

        {ok, _} = ranch:start_listener(?HTTPS_LISTENER, ranch_ssl, SslOptsWithChain,
            cowboy_tls, #{
                env => #{dispatch => Dispatch, custom_response_headers => CustomResponseHeaders},
                max_keepalive => MaxKeepAlive,
                request_timeout => RequestTimeout,
                connection_type => supervisor,
                idle_timeout => infinity,
                inactivity_timeout => InactivityTimeout,
                middlewares => [cowboy_router, response_headers_middleware, cowboy_handler]
            }
        ),
        ?info("Server '~p' started successfully", [?HTTPS_LISTENER]),
        ok
    catch
        error:{badmatch, {error, eaddrinuse} = Error} ->
            ?error("Could not start server '~p' due to port being used", [?HTTPS_LISTENER]),
            Error;
        Type:Reason ->
            ?error_stacktrace("Could not start server '~p' - ~p:~p", [
                ?HTTPS_LISTENER, Type, Reason
            ]),
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Stops the HTTPS server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, listener_stop_error}.
stop() ->
    ?info("Stopping '~p' server ...", [?HTTPS_LISTENER]),
    case cowboy:stop_listener(?HTTPS_LISTENER) of
        ok ->
            ?info("Server '~p' stopped", [?HTTPS_LISTENER]),
            ok;
        {error, Error} ->
            ?error("Error stopping server '~p': ~p", [?HTTPS_LISTENER, Error]),
            {error, listener_stop_error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Restarts https_listener and reloads web certs.
%% Erlang ssl implementation caches and reuses web_chain.pem for as long as any
%% connection still exists meaning that newer one will not be loaded until all
%% processes are dead. This is ensured only by restarting listeners.
%% @end
%%--------------------------------------------------------------------
-spec restart_and_reload_web_certs(gui_config()) -> ok | {error, term()}.
restart_and_reload_web_certs(GuiConfig) ->
    case stop() of
        ok ->
            ssl:clear_pem_cache(),
            try_to_start(GuiConfig, ?MAX_RESTART_RETRIES);
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to start gui at max ?MAX_RESTART_RETRIES number of times.
%% Retries are necessary in case of errors like {error, eaddrinuse}
%% that may happen right after stopping gui.
%% @end
%%--------------------------------------------------------------------
-spec try_to_start(gui_config(), non_neg_integer()) -> ok | {error, term()}.
try_to_start(GuiConfig, 1) ->
    start(GuiConfig);
try_to_start(GuiConfig, RetriesLeft) ->
    case start(GuiConfig) of
        ok ->
            ok;
        {error, _} ->
            timer:sleep(?RESTART_RETRY_DELAY),
            try_to_start(GuiConfig, RetriesLeft - 1)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks the status of HTTPS server.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    Endpoint = str_utils:format_bin("https://127.0.0.1:~B", [get_port()]),
    Opts = [{ssl_options, [{secure, false}]}],
    case http_client:get(Endpoint, #{}, <<>>, Opts) of
        {ok, _, _, _} -> ok;
        _ -> {error, server_not_responding}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns intermediate CA chain in PEM format for gui web cert.
%% @end
%%--------------------------------------------------------------------
-spec get_cert_chain_pems() -> [public_key:der_encoded()].
get_cert_chain_pems() ->
    get_chain().


%%--------------------------------------------------------------------
%% @doc
%% Returns SHA256 checksum of given GUI package (by filename or binary).
%% @end
%%--------------------------------------------------------------------
-spec package_hash(package()) ->
    {ok, onedata:gui_hash()} | ?ERROR_BAD_GUI_PACKAGE | ?ERROR_GUI_PACKAGE_TOO_LARGE.
package_hash(Package) ->
    case read_package(Package) of
        {ok, _GuiDirName, Bytes} ->
            {ok, hex_utils:hex(crypto:hash(sha256, Bytes))};
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Extracts given GUI package (by filename or binary) to given working directory.
%% Returns the path to extracted directory with GUI static files.
%% @end
%%--------------------------------------------------------------------
-spec extract_package(package(), Cwd :: file:name_all()) ->
    {ok, file:name_all()} | ?ERROR_BAD_GUI_PACKAGE | ?ERROR_GUI_PACKAGE_TOO_LARGE.
extract_package(Package, Cwd) ->
    case read_package(Package) of
        {ok, GuiDirName, Bytes} ->
            case erl_tar:extract({binary, Bytes}, [compressed, {cwd, Cwd}]) of
                ok ->
                    {ok, filename:join(Cwd, GuiDirName)};
                Other ->
                    ?debug("Cannot extract GUI package: ~p", [Other]),
                    ?ERROR_BAD_GUI_PACKAGE
            end;
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Reads given GUI package (by filename or binary), returns the top level
%% directory name and the package binary content.
%% @end
%%--------------------------------------------------------------------
-spec read_package(package()) ->
    {ok, TopDir :: file:filename(), Bytes :: binary()} |
    ?ERROR_BAD_GUI_PACKAGE | ?ERROR_GUI_PACKAGE_TOO_LARGE.
read_package({binary, Bytes}) ->
    case erl_tar:table({binary, Bytes}, [compressed, verbose]) of
        {ok, [{TopDir, directory, _, _, _, _, _} | _]} ->
            {ok, TopDir, Bytes};
        Other ->
            ?debug("Invalid GUI package tar table: ~p", [Other]),
            ?ERROR_BAD_GUI_PACKAGE
    end;
read_package(Path) ->
    MaxPackageSize = ?MAX_GUI_PACKAGE_SIZE,
    case filelib:file_size(Path) of
        0 ->
            ?ERROR_BAD_GUI_PACKAGE;
        TooLarge when TooLarge > MaxPackageSize ->
            ?ERROR_GUI_PACKAGE_TOO_LARGE;
        _ ->
            case file:read_file(Path) of
                {ok, Bytes} ->
                    read_package({binary, Bytes});
                Other ->
                    ?debug("Cannot read GUI package: ~p", [Other]),
                    ?ERROR_BAD_GUI_PACKAGE
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Wrapper function to get gui env variable.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: atom()) -> undefined | {ok, term()}.
get_env(Key) ->
    application:get_env(gui, Key).


%%--------------------------------------------------------------------
%% @doc
%% Wrapper function to get gui env variable or default.
%% @end
%%--------------------------------------------------------------------
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    application:get_env(gui, Key, Default).


%%--------------------------------------------------------------------
%% @doc
%% Wrapper function to set gui env variable or default.
%% @end
%%--------------------------------------------------------------------
-spec set_env(Key :: atom(), Value :: term()) -> term().
set_env(Key, Value) ->
    application:set_env(gui, Key, Value).


%%===================================================================
%% Internal functions
%%===================================================================


-spec save_port(Port :: non_neg_integer()) -> ok.
save_port(Port) ->
    set_env(listener_port, Port).


-spec get_port() -> Port :: non_neg_integer().
get_port() ->
    get_env(listener_port, 443).


-spec save_chain(CAChain :: [public_key:der_encoded()]) -> ok.
save_chain(CAChain) ->
    set_env(listener_chain, CAChain).


-spec get_chain() -> CAChain :: [public_key:der_encoded()].
get_chain() ->
    get_env(listener_chain, []).
