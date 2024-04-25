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
-type retry_strategy() :: fail_upon_timeout | retry_infinitely.

-export_type([method/0, gui_config/0, package/0, retry_strategy/0]).

% Returns the value converted to bytes.
-define(MAX_GUI_PACKAGE_SIZE, gui:get_env(max_gui_package_size_mb, 50) * 1048576).

% Listener id
-define(HTTPS_LISTENER, https_listener).

%% API
-export([start/1, stop/0, reload_web_certs/1]).
-export([healthcheck/0, get_cert_chain_ders/0]).
-export([package_hash/1, extract_package/2, read_package/1]).
-export([get_env/1, get_env/2, set_env/2]).

% make sure the retries take more than 4 minutes, which is the time required
% for a hanging socket to exit the TIME_WAIT state and terminate
-define(PORT_CHECK_RETRIES, 41).
-define(PORT_CHECK_INTERVAL, timer:seconds(6)).

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
    start(GuiConfig, fail_upon_timeout).


%%--------------------------------------------------------------------
%% @doc
%% Starts a HTTPS server based on provided configuration.
%% @end
%%--------------------------------------------------------------------
-spec start(gui_config(), retry_strategy()) -> ok | {error, term()}.
start(GuiConfig, RetryStrategy) ->
    ?info("Starting '~p' server...", [?HTTPS_LISTENER]),

    try
        Port = GuiConfig#gui_config.port,

        ensure_port_free(Port),
        save_port(Port),

        RanchOpts = build_ranch_opts(GuiConfig),
        CowboyOpts = build_cowboy_opts(GuiConfig),

        start_ranch_listener(RanchOpts, CowboyOpts, initial_retries(RetryStrategy))
    catch Class:Reason:Stacktrace ->
        ?error_exception("Could not start server '~p'", [?HTTPS_LISTENER], Class, Reason, Stacktrace),
        {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Stops the HTTPS server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, listener_stop_error}.
stop() ->
    ?info("Stopping '~p' server...", [?HTTPS_LISTENER]),
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
%% Reloads web certs. In case of changed chain file entire listener is also
%% restarted (due to bugs in chain reloading in ssl cache).
%% @end
%%--------------------------------------------------------------------
-spec reload_web_certs(gui_config()) -> ok | {error, term()}.
reload_web_certs(GuiConfig) ->
    ssl:clear_pem_cache(),
    restart_if_chain_has_changed(GuiConfig).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Erlang ssl properly reloads key and cert when they are changed on disc or
%% ssl_pem_cache is cleared. But the same is not true for chain file. Once
%% loaded it is kept in ssl internal caches (see ssl_manager) as long as at
%% least one connection made using it still exist. Because a lot of connections
%% are long-lasting (e.g. connection between providers) it may never be reloaded.
%% That is why in case of changed chain it is necessary to restart entire
%% ssl and listener.
%% @end
%%--------------------------------------------------------------------
-spec restart_if_chain_has_changed(gui_config()) -> ok | {error, term()}.
restart_if_chain_has_changed(#gui_config{chain_file = ChainFile} = GuiConfig) ->
    case get_chain() == cert_utils:load_ders(ChainFile) of
        true -> ok;
        false -> restart(GuiConfig, retry_infinitely)
    end.


%% @private
-spec restart(gui_config(), retry_strategy()) -> ok | {error, term()}.
restart(GuiConfig, RetryStrategy) ->
    case stop() of
        ok ->
            ssl:stop(),
            ssl:start(),
            start(GuiConfig, RetryStrategy);
        {error, _} = Error ->
            Error
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
%% Returns intermediate CA chain in DER format for gui web cert.
%% @end
%%--------------------------------------------------------------------
-spec get_cert_chain_ders() -> [public_key:der_encoded()].
get_cert_chain_ders() ->
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks whether port is free on localhost.
%% @TODO VFS-7847 Currently the listener ports are not freed correctly and after
%% a restart, they may still be not available for some time. This appears to be
%% triggered by listener healthcheck connections made by hackney, which causes
%% the listener to go into TIME_WAIT state for some duration.
%% @end
%%--------------------------------------------------------------------
-spec ensure_port_free(integer()) -> ok | no_return().
ensure_port_free(Port) ->
    ensure_port_free(Port, ?PORT_CHECK_RETRIES).


%% @private
-spec ensure_port_free(integer(), integer()) -> ok | no_return().
ensure_port_free(Port, AttemptsLeft) ->
    case gen_tcp:listen(Port, [{reuseaddr, true}, {ip, any}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket);
        {error, _} when AttemptsLeft > 1 ->
            ?warning(
                "Port ~B required by the application is not free, attempts left: ~B",
                [Port, AttemptsLeft - 1]
            ),
            timer:sleep(?PORT_CHECK_INTERVAL),
            ensure_port_free(Port, AttemptsLeft - 1);
        {error, Reason} ->
            error({Port, port_unavailable_due_to, Reason})
    end.


%% @private
-spec build_ranch_opts(gui_config()) -> ranch:opts().
build_ranch_opts(#gui_config{
    port = Port,
    key_file = KeyFile,
    cert_file = CertFile,
    chain_file = ChainFile,
    number_of_acceptors = AcceptorsNum
}) ->
    #{
        connection_type => supervisor,
        num_acceptors => AcceptorsNum,
        % options specific for the transport (SSL)
        socket_opts => lists:flatten([
            {ip, any},
            {buffer, 131072},
            {recbuf, 16777216},
            {port, Port},
            {keyfile, KeyFile},
            {certfile, CertFile},
            {ciphers, ssl_utils:safe_ciphers()},
            {next_protocols_advertised, [<<"h2">>, <<"http/1.1">>]},
            {alpn_preferred_protocols, [<<"h2">>, <<"http/1.1">>]},
            case filelib:is_regular(ChainFile) of
                true ->
                    save_chain(cert_utils:load_ders(ChainFile)),
                    {cacertfile, ChainFile};
                _ ->
                    []
            end
        ])
    }.


%% @private
-spec build_cowboy_opts(gui_config()) -> cowboy:opts().
build_cowboy_opts(GuiConfig = #gui_config{
    max_keepalive = MaxKeepAlive,
    request_timeout = RequestTimeout,
    inactivity_timeout = InactivityTimeout,
    custom_response_headers = CustomResponseHeaders
}) ->
    #{
        env => #{
            dispatch => build_dispatch_rules(GuiConfig),
            custom_response_headers => CustomResponseHeaders
        },
        active_n => 24,
        initial_stream_flow_size => 1048576,
        max_received_frame_rate => {100000, 1000},
        idle_timeout => infinity,
        inactivity_timeout => InactivityTimeout,
        max_keepalive => MaxKeepAlive,
        middlewares => [cowboy_router, response_headers_middleware, cowboy_handler],
        request_timeout => RequestTimeout
    }.


%% @private
-spec build_dispatch_rules(gui_config()) -> cowboy_router:dispatch_rules().
build_dispatch_rules(#gui_config{
    port = Port,
    dynamic_pages = DynamicPages,
    custom_cowboy_routes = CustomRoutes,
    static_root = StaticRoot
}) ->
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

    cowboy_router:compile([
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
    ]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to start ranch listener at max ?MAX_RESTART_RETRIES number of times.
%% Retries are necessary in case of errors like {error, eaddrinuse}
%% that may happen right after stopping gui.
%% @end
%%--------------------------------------------------------------------
-spec start_ranch_listener(ranch:opts(), cowboy:opts(), non_neg_integer() | infinity) ->
    ok | {error, term()}.
start_ranch_listener(RanchOpts, CowboyOpts, RetriesLeft) ->
    case start_ranch_listener(RanchOpts, CowboyOpts) of
        ok ->
            ?info("Server '~p' started successfully", [?HTTPS_LISTENER]),
            ok;
        {error, _} = Error when RetriesLeft == 0 ->
            ?error("Could not start server '~p' - due to: ~p", [?HTTPS_LISTENER, Error]),
            Error;
        {error, _} ->
            timer:sleep(?RESTART_RETRY_DELAY),
            start_ranch_listener(RanchOpts, CowboyOpts, leftover_retries(RetriesLeft))
    end.


%% @private
-spec initial_retries(retry_strategy()) -> non_neg_integer() | infinity.
initial_retries(fail_upon_timeout) -> ?MAX_RESTART_RETRIES;
initial_retries(retry_infinitely) -> infinity.


%% @private
-spec leftover_retries(non_neg_integer() | infinity) -> non_neg_integer() | infinity.
leftover_retries(infinity) -> infinity;
leftover_retries(RetriesLeft) -> RetriesLeft - 1.


%% @private
-spec start_ranch_listener(ranch:opts(), cowboy:opts()) -> ok | {error, term()}.
start_ranch_listener(RanchOpts, CowboyOpts) ->
    try
        case ranch:start_listener(?HTTPS_LISTENER, ranch_ssl, RanchOpts, cowboy_tls, CowboyOpts) of
            {ok, _} -> ok;
            {error, _} = Error -> Error
        end
    catch Class:Reason:Stacktrace ->
        ?error_exception("Could not start server '~p'", [?HTTPS_LISTENER], Class, Reason, Stacktrace),
        {error, Reason}
    end.


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
