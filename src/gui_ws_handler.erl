-module(gui_ws_handler).

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
    data_backends = maps:new() :: maps:map()
}).

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


init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_TransportName, Req, _Opts) ->
    {FullPath, _} = cowboy_req:path(Req),
    case gui_html_handler:is_html_req(FullPath) of
        true ->
            % Initialize context
            g_ctx:init(Req),
            {ok, Req, #state{}};
        false ->
            {shutdown, Req, no_state}
    end.

websocket_handle({text, MsgJSON}, Req, State) ->
    Msg = g_str:decode_from_json(MsgJSON),
    MsgType = proplists:get_value(?MSG_TYPE_KEY, Msg),
    {Resp, NewState} = handle_message(MsgType, Msg, State),
    RespJSON = g_str:encode_to_json(Resp),
    {reply, {text, RespJSON}, Req, NewState};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.


websocket_info({Type, Data}, Req, State) ->
    ?dump(Type),
    MsgType = case Type of
                  push_updated -> ?MSG_TYPE_PUSH_UPDATED;
                  push_deleted -> ?MSG_TYPE_PUSH_DELETED
              end,
    Msg = [
        {?MSG_TYPE_KEY, MsgType},
        {?DATA_KEY, Data}
    ],
    {reply, {text, g_str:encode_to_json(Msg)}, Req, State};


websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    ?dump({info, _Info}),
    {ok, Req, State}.


websocket_terminate(_Reason, _Req, _State) ->
    data_backend:kill_async_processes(),
    ok.


handle_message(?MSG_TYPE_PULL_REQ, Props, State) ->
    #state{data_backends = DataBackends} = State,
    ResourceType = proplists:get_value(?RESOURCE_TYPE_KEY, Props),
    {Handler, NewDataBackends} = get_data_backend(ResourceType, DataBackends),
    Res = handle_pull_req(Props, Handler),
    {Res, State#state{data_backends = NewDataBackends}};


handle_message(?MSG_TYPE_CALLBACK_REQ, Msg, State) ->
    Res = handle_callback_req(Msg),
    {Res, State}.


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


handle_pull_req(Props,Handler) ->
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    Data = proplists:get_value(?DATA_KEY, Props),
    EntityIdOrIds = proplists:get_value(?RESOURCE_IDS_KEY, Props),
    try
        {Result, RespData} =
            case proplists:get_value(?OPERATION_KEY, Props) of
                ?OPERATION_FIND ->
                    erlang:apply(Handler, find, [[EntityIdOrIds]]);
                ?OPERATION_FIND_MANY ->
                    erlang:apply(Handler, find, [EntityIdOrIds]);
                ?OPERATION_FIND_ALL ->
                    erlang:apply(Handler, find_all, []);
                ?OPERATION_FIND_QUERY ->
                    erlang:apply(Handler, find_query, [Data]);
                ?OPERATION_CREATE_RECORD ->
                    erlang:apply(Handler, create_record, [Data]);
                ?OPERATION_UPDATE_RECORD ->
                    case erlang:apply(Handler, update_record, [EntityIdOrIds, Data]) of
                        ok -> {ok, null};
                        {error, Msg} -> {error, Msg}
                    end;
                ?OPERATION_DELETE_RECORD ->
                    case erlang:apply(Handler, delete_record, [EntityIdOrIds]) of
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


handle_callback_req(Props) ->
    % TOdo moze cos co rzuca jak nie ma klucza, a potem handler u gory
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    ResourceType = proplists:get_value(?RESOURCE_TYPE_KEY, Props),
    Operation = proplists:get_value(?OPERATION_KEY, Props),
    Data = proplists:get_value(?DATA_KEY, Props),
    % Todo merge with get_handler_module
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


