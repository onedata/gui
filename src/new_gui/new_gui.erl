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
-module(new_gui).
-author("Lukasz Opiola").

-include("new_gui.hrl").
-include("gui_session.hrl").
-include_lib("ctool/include/logging.hrl").

-type method() :: binary(). % <<"GET">> | <<"POST">> etc.
-type gui_config() :: #gui_config{}.
-export_type([method/0, gui_config/0]).


% Listener id
-define(HTTPS_LISTENER, https_listener).

%% API
-export([start/1, stop/0, healthcheck/0, get_cert_chain_pems/0]).
-export([get_env/1, get_env/2, set_env/2]).

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
            custom_cowboy_routes = CustomRoutes
        } = GuiConfig,

        save_port(Port),

        DynamicPageRoutes = lists:map(fun({Path, Methods, Handler}) ->
            {Path, dynamic_page_handler, {Methods, Handler}}
        end, DynamicPages),

        StaticRoot = static_root(GuiConfig),

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
                {"/", cowboy_static, {file, filename:join(StaticRoot, "index.html")}},
                {"/#/[...]", cowboy_static, {file, filename:join(StaticRoot, "index.html")}},
                {"/[...]", cowboy_static, {dir, StaticRoot}}
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
                env => #{dispatch => Dispatch},
                max_keepalive => MaxKeepAlive,
                request_timeout => RequestTimeout,
                connection_type => supervisor,
                idle_timeout => infinity,
                inactivity_timeout => InactivityTimeout,
                middlewares => [cowboy_router, response_headers_middleware, cowboy_handler]
            }
        ),
        ok
    catch
        Type:Reason ->
            ?error_stacktrace("Could not start gui - ~p:~p", [Type, Reason]),
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Stops the HTTPS server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, listener_stop_error}.
stop() ->
    case cowboy:stop_listener(?HTTPS_LISTENER) of
        ok ->
            ok;
        {error, Error} ->
            ?error("Error stopping listener ~p: ~p", [?HTTPS_LISTENER, Error]),
            {error, listener_stop_error}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks the status of HTTPS server.
%% @end
%%--------------------------------------------------------------------
-spec healthcheck() -> ok | {error, server_not_responding}.
healthcheck() ->
    Endpoint = str_utils:format_bin("https://127.0.0.1:~B", [get_port()]),
    Opts = [{ssl_options, [{secure, only_verify_peercert}, {cacerts, get_chain()}]}],
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


%%%%%===================================================================
%%%%% Internal functions
%%%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Resolves GUI static files root based on config - it can be overriden by
%% a custom path or default.
%% @end
%%--------------------------------------------------------------------
-spec static_root(gui_config()) -> Path :: string().
static_root(#gui_config{default_static_root = DefaultRoot, custom_static_root = CustomRoot}) ->
    % Resolve static files root. First, check if there is a non-empty dir
    % located in custom static root. If not, use default.
    case file:list_dir_all(CustomRoot) of
        {error, enoent} -> DefaultRoot;
        {ok, []} -> DefaultRoot;
        {ok, _} -> CustomRoot
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
