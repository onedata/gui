-module(gui_ws_handler).

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(MSG_TYPE_KEY, <<"msgType">>).
-define(MSG_TYPE_PULL_REQ, <<"pullReq">>).
-define(MSG_TYPE_PULL_RESP, <<"pullResp">>).
-define(MSG_TYPE_STATIC_DATA_REQ, <<"staticDataReq">>).
-define(MSG_TYPE_STATIC_DATA_RESP, <<"staticDataResp">>).

-define(UUID_KEY, <<"uuid">>).

-define(ENTITY_TYPE_KEY, <<"entityType">>).

-define(ENTITY_IDS_KEY, <<"entityIds">>).

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
            {ok, Req, maps:new()};
        false ->
            {shutdown, Req, no_state}
    end.

websocket_handle({text, MsgJSON}, Req, DataBackends) ->
    Msg = g_str:decode_from_json(MsgJSON),
    MsgType = proplists:get_value(?MSG_TYPE_KEY, Msg),
    {Resp, NewDataBackends} =
        case MsgType of
            ?MSG_TYPE_PULL_REQ ->
                {_Res, _NewDataBackends} = handle_pull_req(Msg, DataBackends);
            ?MSG_TYPE_STATIC_DATA_REQ ->
                Res = handle_static_data_req(Msg),
                % State does not need modification
                {Res, DataBackends}
        end,
    RespJSON = g_str:encode_to_json(Resp),
    {reply, {text, RespJSON}, Req, NewDataBackends};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({push, Data}, Req, State) ->
    ?dump(websocket_info_push),
    Roz = [
        {<<"msgType">>, <<"pushReq">>},
        {<<"data">>, Data}
    ],
    {reply, {text, g_str:encode_to_json(Roz)}, Req, State};

websocket_info({timeout, _Ref, Msg}, Req, State) ->
%%     erlang:start_timer(1000, opn_cowboy_bridge:get_socket_pid(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    ?dump({info, _Info}),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    data_backend:kill_async_processes(),
    ok.


get_handler_module(EntityType, DataBackends) ->
    case maps:find(EntityType, DataBackends) of
        {ok, Handler} ->
            {Handler, DataBackends};
        _ ->
            Handler = ?GUI_ROUTE_PLUGIN:data_backend(EntityType),
            ok = Handler:init(),
            NewBackends = maps:put(EntityType, Handler, DataBackends),
            {Handler, NewBackends}
    end.


handle_pull_req(Props, DataBackends) ->
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    Data = proplists:get_value(?DATA_KEY, Props),
    EntityType = proplists:get_value(?ENTITY_TYPE_KEY, Props),
    EntityIdOrIds = proplists:get_value(?ENTITY_IDS_KEY, Props),
    {Handler, NewBackends} = get_handler_module(EntityType, DataBackends),
    try
        {Result, RespData} =
            case proplists:get_value(?OPERATION_KEY, Props) of
                ?OPERATION_FIND ->
                    erlang:apply(Handler, find, [EntityIdOrIds]);
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
        OKResp = [
            {?MSG_TYPE_KEY, ?MSG_TYPE_PULL_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ResultVal},
            {?DATA_KEY, RespData}
        ],
        {OKResp, NewBackends}
    catch T:M ->
        ?error_stacktrace(
            "Error while handling websocket pull req - ~p:~p", [T, M]),
        ErrorResp = [
            {?MSG_TYPE_KEY, ?MSG_TYPE_PULL_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ?RESULT_ERROR},
            {?DATA_KEY, ?DATA_INTERNAL_SERVER_ERROR}
        ],
        {ErrorResp, NewBackends}
    end.


handle_static_data_req(Props) ->
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    EntityType = proplists:get_value(?ENTITY_TYPE_KEY, Props),
    Handler = ?GUI_ROUTE_PLUGIN:static_data_backend(),
    try
        {Result, RespData} = Handler:find(EntityType),
        ResultVal = case Result of
                        ok -> ?RESULT_OK;
                        error -> ?RESULT_ERROR
                    end,
        OKResp = [
            {?MSG_TYPE_KEY, ?MSG_TYPE_STATIC_DATA_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ResultVal},
            {?DATA_KEY, RespData}
        ],
        OKResp
    catch T:M ->
        ?error_stacktrace(
            "Error while handling websocket static data req - ~p:~p", [T, M]),
        ErrorResp = [
            {?MSG_TYPE_KEY, ?MSG_TYPE_STATIC_DATA_RESP},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ?RESULT_ERROR},
            {?DATA_KEY, ?DATA_INTERNAL_SERVER_ERROR}
        ],
        ErrorResp
    end.


