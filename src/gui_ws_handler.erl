%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a cowboy websocket handler that handles the connection
%%% between Ember ws_adapter and server. This channel is used for models
%%% synchronization and performing RPC to the server.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_ws_handler).
-author("Lukasz Opiola").
-behaviour(cowboy_websocket_handler).

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% State of the connection. Remembers which data backends were already
%% initialized during current connection.
-record(state, {
    data_backends = maps:new() :: #{}
}).

%% Interface between WebSocket Adapter client and server. Corresponding
%% interface is located in ws_adapter.js.

%% All in-coming JSONs have the following structure (opt = optional field)
%% {
%%   uuid
%%   msgType
%%   resourceType
%%   operation
%%   resourceIds (opt)
%%   data (opt)
%% }
%% All out-coming JSONs have the following structure (opt = optional field)
%% {
%%   uuid (opt, not used in push messages)
%%   msgType
%%   result
%%   data (opt)
%% }

%% Keys corresponding to above structure
-define(KEY_UUID, <<"uuid">>).
-define(KEY_MSG_TYPE, <<"msgType">>).
-define(KEY_RESOURCE_TYPE, <<"resourceType">>).
-define(KEY_OPERATION, <<"operation">>).
-define(KEY_RESOURCE_IDS, <<"resourceIds">>).
-define(KEY_DATA, <<"data">>).
-define(KEY_RESULT, <<"result">>).
%% Message types, identified by ?KEY_MSG_TYPE key
-define(TYPE_MODEL_REQ, <<"modelReq">>).
-define(TYPE_MODEL_RESP, <<"modelResp">>).
-define(TYPE_MODEL_CRT_PUSH, <<"modelPushCreated">>).
-define(TYPE_MODEL_UPT_PUSH, <<"modelPushUpdated">>).
-define(TYPE_MODEL_DLT_PUSH, <<"modelPushDeleted">>).
-define(TYPE_RPC_REQ, <<"RPCReq">>).
-define(TYPE_RPC_RESP, <<"RPCResp">>).
%% Operations on model, identified by ?KEY_OPERATION key
-define(OP_FIND, <<"find">>).
-define(OP_FIND_MANY, <<"findMany">>).
-define(OP_FIND_ALL, <<"findAll">>).
-define(OP_FIND_QUERY, <<"findQuery">>).
-define(OP_CREATE_RECORD, <<"createRecord">>).
-define(OP_UPDATE_RECORD, <<"updateRecord">>).
-define(OP_DELETE_RECORD, <<"deleteRecord">>).
%% Defined concerning session RPC
-define(RESOURCE_TYPE_PUBLIC_RPC, <<"public">>).
-define(RESOURCE_TYPE_PRIVATE_RPC, <<"private">>).
-define(RESOURCE_TYPE_SESSION, <<"session">>).
-define(KEY_SESSION_VALID, <<"sessionValid">>).
-define(KEY_SESSION_DETAILS, <<"sessionDetails">>).
%% Operation results
-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(DATA_INTERNAL_SERVER_ERROR, <<"Internal Server Error">>).

%%%===================================================================
%%% cowboy_webocket_handler API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Upgrades the protocol to WebSocket.
%% @end
%%--------------------------------------------------------------------
-spec init({TransportName, ProtocolName}, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket} when
    TransportName :: tcp | ssl | atom(),
    ProtocolName :: http | atom(),
    Req :: cowboy_req:req(),
    Opts :: any().
init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


%%--------------------------------------------------------------------
%% @doc
%% Initializes the webscoket state for current connection.
%% Accepts only connections from .html pages.
%% @end
%%--------------------------------------------------------------------
-spec websocket_init(TransportName, Req, Opts) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {ok, Req, State, Timeout} | {ok, Req, State, Timeout, hibernate} |
    {shutdown, Req} when
    TransportName :: tcp | ssl | atom(),
    Req :: cowboy_req:req(),
    Opts :: any(),
    State :: #state{},
    Timeout :: timeout().
websocket_init(_TransportName, Req, _Opts) ->
    % @todo geneneric error handling + reporting on client side
    {FullPath, _} = cowboy_req:path(Req),
    case gui_html_handler:is_html_req(FullPath) of
        true ->
            % Initialize context
            g_ctx:init(Req),
            % Check if the client is allowed to connect to WS
            WSRequirements = g_ctx:websocket_requirements(),
            UserLoggedIn = g_session:is_logged_in(),
            Result = case {WSRequirements, UserLoggedIn} of
                {?WEBSOCKET_DISABLED, _} -> error;
                {?SESSION_ANY, _} -> ok;
                {?SESSION_LOGGED_IN, true} -> ok;
                {?SESSION_NOT_LOGGED_IN, false} -> ok;
                {_, _} -> error
            end,
            case Result of
                ok ->
                    {ok, Req, #state{}};
                error ->
                    % The client is not allowed to connect to WS,
                    % send 403 Forbidden.
                    g_ctx:reply(403, [], <<"">>),
                    NewReq = g_ctx:finish(),
                    {shutdown, NewReq}
            end;
        false ->
            % Not a HTML request, send 403 Forbidden.
            {ok, NewReq} = cowboy_req:reply(403, [], <<"">>, Req),
            {shutdown, NewReq}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handles the data received from the Websocket connection.
%% Performs updacking of JSON, follows the data to handler and then encodes
%% its response to JSON and sends it back to the client.
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle(InFrame, Req, State) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {reply, OutFrame | [OutFrame], Req, State} |
    {reply, OutFrame | [OutFrame], Req, State, hibernate} |
    {shutdown, Req, State} when
    InFrame :: {text | binary | ping | pong, binary()},
    Req :: cowboy_req:req(),
    State :: #state{},
    OutFrame :: cowboy_websocket:frame().
websocket_handle({text, MsgJSON}, Req, State) ->
    % @todo geneneric error handling + reporting on client side
    Msg = json_utils:decode(MsgJSON),
    MsgType = proplists:get_value(?KEY_MSG_TYPE, Msg),
    {Resp, NewState} = handle_message(MsgType, Msg, State),
    RespJSON = json_utils:encode(Resp),
    {reply, {text, RespJSON}, Req, NewState};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% Handles any Erlang messages received.
%% Async processes can message the websocket process
%% (push_deleted, push_updated) to push data to the client.
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(Info, Req, State) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {reply, OutFrame | [OutFrame], Req, State} |
    {reply, OutFrame | [OutFrame], Req, State, hibernate} |
    {shutdown, Req, State} when
    Info :: any(),
    Req :: cowboy_req:req(),
    State :: #state{},
    OutFrame :: cowboy_websocket:frame().
websocket_info({push_created, ResourceType, Data}, Req, State) ->
    % @todo geneneric error handling + reporting on client side
    Msg = [
        {?KEY_MSG_TYPE, ?TYPE_MODEL_CRT_PUSH},
        {?KEY_RESOURCE_TYPE, ResourceType},
        {?KEY_DATA, Data}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

websocket_info({push_updated, ResourceType, Data}, Req, State) ->
    % @todo geneneric error handling + reporting on client side
    Msg = [
        {?KEY_MSG_TYPE, ?TYPE_MODEL_UPT_PUSH},
        {?KEY_RESOURCE_TYPE, ResourceType},
        {?KEY_DATA, Data}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

websocket_info({push_deleted, ResourceType, Ids}, Req, State) ->
    % @todo geneneric error handling + reporting on client side
    Msg = [
        {?KEY_MSG_TYPE, ?TYPE_MODEL_DLT_PUSH},
        {?KEY_RESOURCE_TYPE, ResourceType},
        {?KEY_DATA, Ids}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% Performs any necessary cleanup of the state.
%% @end
%%--------------------------------------------------------------------
-spec websocket_terminate(Reason, Req, State) -> ok when
    Reason :: {normal, shutdown | timeout} | {remote, closed} |
    {remote, cowboy_websocket:close_code(), binary()} |
    {error, badencoding | badframe | closed | atom()},
    Req :: cowboy_req:req(),
    State :: #state{}.
websocket_terminate(_Reason, _Req, _State) ->
    gui_async:kill_async_processes(),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handled specific message. Based on the type of message and type of requested
%% resource, decides which handler module should be called.
%% @end
%%--------------------------------------------------------------------
-spec handle_message(MsgType :: binary(), Prop :: proplists:proplist(),
    State :: #state{}) -> {Res :: proplists:proplist(), NewState :: #state{}}.
handle_message(?TYPE_MODEL_REQ, Props, State) ->
    #state{data_backends = DataBackends} = State,
    ResourceType = proplists:get_value(?KEY_RESOURCE_TYPE, Props),
    {Handler, NewDataBackends} = get_data_backend(ResourceType, DataBackends),
    Res = handle_pull_req(Props, Handler),
    {Res, State#state{data_backends = NewDataBackends}};


handle_message(?TYPE_RPC_REQ, Msg, State) ->
    Res = handle_RPC_req(Msg),
    {Res, State}.


%%--------------------------------------------------------------------
%% @doc
%% Resolves data backend for given model synchronization request.
%% Data backends must be initialized on first call, so it uses a map to keep
%% track which backends are already initialized.
%% @end
%%--------------------------------------------------------------------
-spec get_data_backend(ResourceType :: binary(), DataBackends :: #{}) ->
    {Handler :: atom(), NewBackends :: #{}}.
get_data_backend(ResourceType, DataBackends) ->
    case maps:find(ResourceType, DataBackends) of
        {ok, Handler} ->
            {Handler, DataBackends};
        _ ->
            HasSession = g_session:is_logged_in(),
            Handler = ?GUI_ROUTE_PLUGIN:data_backend(HasSession, ResourceType),
            ok = Handler:init(),
            NewBackends = maps:put(ResourceType, Handler, DataBackends),
            {Handler, NewBackends}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handles message of type PULL REQUEST, which is a message requesting data
%% about certain model. Returns a proplist that is later encoded to JSON.
%% @end
%%--------------------------------------------------------------------
-spec handle_pull_req(Props :: proplists:proplist(), Handler :: atom()) ->
    Res :: proplists:proplist().
handle_pull_req(Props, Handler) ->
    MsgUUID = proplists:get_value(?KEY_UUID, Props, null),
    RsrcType = proplists:get_value(?KEY_RESOURCE_TYPE, Props),
    Data = proplists:get_value(?KEY_DATA, Props),
    EntityIdOrIds = proplists:get_value(?KEY_RESOURCE_IDS, Props),
    try
        {Result, RespData} =
            case proplists:get_value(?KEY_OPERATION, Props) of
                ?OP_FIND ->
                    erlang:apply(Handler, find, [RsrcType, [EntityIdOrIds]]);
                ?OP_FIND_MANY ->
                    erlang:apply(Handler, find, [RsrcType, EntityIdOrIds]);
                ?OP_FIND_ALL ->
                    erlang:apply(Handler, find_all, [RsrcType]);
                ?OP_FIND_QUERY ->
                    erlang:apply(Handler, find_query, [RsrcType, Data]);
                ?OP_CREATE_RECORD ->
                    erlang:apply(Handler, create_record, [RsrcType, Data]);
                ?OP_UPDATE_RECORD ->
                    case erlang:apply(Handler, update_record,
                        [RsrcType, EntityIdOrIds, Data]) of
                        ok -> {ok, null};
                        {error, Msg} -> {error, Msg}
                    end;
                ?OP_DELETE_RECORD ->
                    case erlang:apply(Handler, delete_record,
                        [RsrcType, EntityIdOrIds]) of
                        ok -> {ok, null};
                        {error, Msg} -> {error, Msg}
                    end
            end,
        ResultVal = case Result of
            ok -> ?RESULT_OK;
            error -> ?RESULT_ERROR
        end,
        [
            {?KEY_MSG_TYPE, ?TYPE_MODEL_RESP},
            {?KEY_UUID, MsgUUID},
            {?KEY_RESULT, ResultVal},
            {?KEY_DATA, RespData}
        ]
    catch T:M ->
        ?error_stacktrace(
            "Error while handling websocket pull req - ~p:~p", [T, M]),
        [
            {?KEY_MSG_TYPE, ?TYPE_MODEL_RESP},
            {?KEY_UUID, MsgUUID},
            {?KEY_RESULT, ?RESULT_ERROR},
            {?KEY_DATA, ?DATA_INTERNAL_SERVER_ERROR}
        ]
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handles message of type RPC_REQUEST, which is a message requesting that
%% the server performs some operation.
%% Returns a proplist that is later encoded to JSON.
%% @end
%%--------------------------------------------------------------------
-spec handle_RPC_req(Props :: proplists:proplist()) ->
    Res :: proplists:proplist().
handle_RPC_req(Props) ->
    MsgUUID = proplists:get_value(?KEY_UUID, Props, null),
    ResourceType = proplists:get_value(?KEY_RESOURCE_TYPE, Props),
    Operation = proplists:get_value(?KEY_OPERATION, Props),
    Data = proplists:get_value(?KEY_DATA, Props),
    ?dump({ResourceType, Operation}),
    try
        {Result, RespData} = case ResourceType of
            ?RESOURCE_TYPE_SESSION ->
                handle_session_RPC();
            ?RESOURCE_TYPE_PUBLIC_RPC ->
                handle_public_RPC(Operation, Data);
            ?RESOURCE_TYPE_PRIVATE_RPC ->
                case g_session:is_logged_in() of
                    true ->
                        handle_private_RPC(Operation, Data);
                    false ->
                        {error, noSession}
                end
        end,
        ResultVal = case Result of
            ok -> ?RESULT_OK;
            error -> ?RESULT_ERROR
        end,
        OKResp = [
            {?KEY_MSG_TYPE, ?TYPE_RPC_RESP},
            {?KEY_UUID, MsgUUID},
            {?KEY_RESULT, ResultVal},
            {?KEY_DATA, RespData}
        ],
        OKResp
    catch T:M ->
        ?error_stacktrace(
            "Error while handling websocket static data req - ~p:~p", [T, M]),
        ErrorResp = [
            {?KEY_MSG_TYPE, ?TYPE_RPC_RESP},
            {?KEY_UUID, MsgUUID},
            {?KEY_RESULT, ?RESULT_ERROR},
            {?KEY_DATA, ?DATA_INTERNAL_SERVER_ERROR}
        ],
        ErrorResp
    end.


handle_private_RPC(Operation, Data) ->
    Handler = ?GUI_ROUTE_PLUGIN:private_rpc_backend(),
    Handler:handle(Operation, Data).


handle_public_RPC(Operation, Data) ->
    Handler = ?GUI_ROUTE_PLUGIN:public_rpc_backend(),
    Handler:handle(Operation, Data).


handle_session_RPC() ->
    ?dump(handle_session_RPC),
    Data = case g_session:is_logged_in() of
        true ->
            {ok, Props} = ?GUI_ROUTE_PLUGIN:session_details(),
            [
                {?KEY_SESSION_VALID, true},
                {?KEY_SESSION_DETAILS, Props}
            ];
        false ->
            [
                {?KEY_SESSION_VALID, false}
            ]
    end,
    {ok, Data}.
