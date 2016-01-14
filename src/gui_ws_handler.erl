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
%%% synchronization and performing callbacks to the server.
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
-define(MSG_TYPE_KEY, <<"msgType">>).

-define(MSG_TYPE_CALLBACK_REQ, <<"callbackReq">>).
-define(MSG_TYPE_CALLBACK_RESP, <<"callbackResp">>).

-define(MSG_TYPE_PULL_REQ, <<"pullReq">>).
-define(MSG_TYPE_PULL_RESP, <<"pullResp">>).

-define(MSG_TYPE_PUSH_UPDATED, <<"pushUpdated">>).
-define(MSG_TYPE_PUSH_DELETED, <<"pushDeleted">>).

-define(UUID_KEY, <<"uuid">>).

-define(RESOURCE_TYPE_KEY, <<"resourceType">>).

-define(RESOURCE_IDS_KEY, <<"resourceIds">>).

-define(OPERATION_KEY, <<"operation">>).
-define(OPERATION_FIND, <<"find">>).
-define(OPERATION_FIND_MANY, <<"findMany">>).
-define(OPERATION_FIND_ALL, <<"findAll">>).
-define(OPERATION_FIND_QUERY, <<"findQuery">>).
-define(OPERATION_CREATE_RECORD, <<"createRecord">>).
-define(OPERATION_UPDATE_RECORD, <<"updateRecord">>).
-define(OPERATION_DELETE_RECORD, <<"deleteRecord">>).

-define(RESULT_KEY, <<"result">>).
-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(DATA_KEY, <<"data">>).
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
            {ok, Req, #state{}};
        false ->
            {shutdown, Req, no_state}
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
    MsgType = proplists:get_value(?MSG_TYPE_KEY, Msg),
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
websocket_info({Type, Data}, Req, State) ->
    % @todo geneneric error handling + reporting on client side
    MsgType = case Type of
                  push_updated -> ?MSG_TYPE_PUSH_UPDATED;
                  push_deleted -> ?MSG_TYPE_PUSH_DELETED
              end,
    Msg = [
        {?MSG_TYPE_KEY, MsgType},
        {?DATA_KEY, Data}
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
    data_backend:kill_async_processes(),
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
handle_message(?MSG_TYPE_PULL_REQ, Props, State) ->
    #state{data_backends = DataBackends} = State,
    ResourceType = proplists:get_value(?RESOURCE_TYPE_KEY, Props),
    {Handler, NewDataBackends} = get_data_backend(ResourceType, DataBackends),
    Res = handle_pull_req(Props, Handler),
    {Res, State#state{data_backends = NewDataBackends}};


handle_message(?MSG_TYPE_CALLBACK_REQ, Msg, State) ->
    Res = handle_callback_req(Msg),
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
            Handler = ?GUI_ROUTE_PLUGIN:data_backend(ResourceType),
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
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    RsrcType = proplists:get_value(?RESOURCE_TYPE_KEY, Props),
    Data = proplists:get_value(?DATA_KEY, Props),
    EntityIdOrIds = proplists:get_value(?RESOURCE_IDS_KEY, Props),
    try
        {Result, RespData} =
            case proplists:get_value(?OPERATION_KEY, Props) of
                ?OPERATION_FIND ->
                    erlang:apply(Handler, find, [RsrcType, [EntityIdOrIds]]);
                ?OPERATION_FIND_MANY ->
                    erlang:apply(Handler, find, [RsrcType, EntityIdOrIds]);
                ?OPERATION_FIND_ALL ->
                    erlang:apply(Handler, find_all, [RsrcType]);
                ?OPERATION_FIND_QUERY ->
                    erlang:apply(Handler, find_query, [RsrcType, Data]);
                ?OPERATION_CREATE_RECORD ->
                    erlang:apply(Handler, create_record, [RsrcType, Data]);
                ?OPERATION_UPDATE_RECORD ->
                    case erlang:apply(Handler, update_record,
                        [RsrcType, EntityIdOrIds, Data]) of
                        ok -> {ok, null};
                        {error, Msg} -> {error, Msg}
                    end;
                ?OPERATION_DELETE_RECORD ->
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
            {?MSG_TYPE_KEY, ?MSG_TYPE_PULL_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ResultVal},
            {?DATA_KEY, RespData}
        ]
    catch T:M ->
        ?error_stacktrace(
            "Error while handling websocket pull req - ~p:~p", [T, M]),
        [
            {?MSG_TYPE_KEY, ?MSG_TYPE_PULL_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ?RESULT_ERROR},
            {?DATA_KEY, ?DATA_INTERNAL_SERVER_ERROR}
        ]
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handles message of type CALLBACK REQUEST, which is a message requesting that
%% the server performs some operation.
%% Returns a proplist that is later encoded to JSON.
%% @end
%%--------------------------------------------------------------------
-spec handle_callback_req(Props :: proplists:proplist()) ->
    Res :: proplists:proplist().
handle_callback_req(Props) ->
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    ResourceType = proplists:get_value(?RESOURCE_TYPE_KEY, Props),
    Operation = proplists:get_value(?OPERATION_KEY, Props),
    Data = proplists:get_value(?DATA_KEY, Props),
    Handler = ?GUI_ROUTE_PLUGIN:callback_backend(ResourceType),
    try
        {Result, RespData} = Handler:callback(Operation, Data),
        ResultVal = case Result of
                        ok -> ?RESULT_OK;
                        error -> ?RESULT_ERROR
                    end,
        OKResp = [
            {?MSG_TYPE_KEY, ?MSG_TYPE_CALLBACK_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ResultVal},
            {?DATA_KEY, RespData}
        ],
        OKResp
    catch T:M ->
        ?error_stacktrace(
            "Error while handling websocket static data req - ~p:~p", [T, M]),
        ErrorResp = [
            {?MSG_TYPE_KEY, ?MSG_TYPE_CALLBACK_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ?RESULT_ERROR},
            {?DATA_KEY, ?DATA_INTERNAL_SERVER_ERROR}
        ],
        ErrorResp
    end.
