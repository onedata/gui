-module(gui_ws_handler).

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(MSG_TYPE_KEY, <<"msgType">>).
-define(PULL_REQ_VAL, <<"pullReq">>).
-define(PULL_RESP_VAL, <<"pullResp">>).

-define(UUID_KEY, <<"uuid">>).

-define(ENTITY_TYPE_KEY, <<"entityType">>).

-define(ENTITY_IDS_KEY, <<"entityIds">>).

-define(OPERATION_KEY, <<"operation">>).
-define(FIND_VAL, <<"find">>).
-define(FIND_MANY_VAL, <<"findMany">>).
-define(FIND_ALL_VAL, <<"findAll">>).
-define(FIND_QUERY_VAL, <<"findQuery">>).
-define(CREATE_RECORD_VAL, <<"createRecord">>).
-define(UPDATE_RECORD_VAL, <<"updateRecord">>).
-define(DELETE_RECORD_VAL, <<"deleteRecord">>).

-define(RESULT_KEY, <<"result">>).
-define(OK_VAL, <<"ok">>).
-define(ERROR_VAL, <<"error">>).

-define(DATA_KEY, <<"data">>).
-define(INTERNAL_SERVER_ERROR_VAL, <<"Internal Server Error">>).

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
    throw(test),
    Msg = g_str:decode_from_json(MsgJSON),
%%     ?dump(Msg),
    {Resp, NewDataBackends} = handle_ws_req(Msg, DataBackends),
%%     ?dump(Resp),
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
    ?dump(terminate),
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


handle_ws_req(Props, DataBackends) ->
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    ?PULL_REQ_VAL = proplists:get_value(?MSG_TYPE_KEY, Props),
    Data = proplists:get_value(?DATA_KEY, Props),
    EntityType = proplists:get_value(?ENTITY_TYPE_KEY, Props),
    EntityIdOrIds = proplists:get_value(?ENTITY_IDS_KEY, Props),
    {Handler, NewBackends} = get_handler_module(EntityType, DataBackends),
    try
        {Result, RespData} =
            case proplists:get_value(?OPERATION_KEY, Props) of
                ?FIND_VAL ->
                    erlang:apply(Handler, find, [EntityIdOrIds]);
                ?FIND_MANY_VAL ->
                    erlang:apply(Handler, find, [EntityIdOrIds]);
                ?FIND_ALL_VAL ->
                    erlang:apply(Handler, find_all, []);
                ?FIND_QUERY_VAL ->
                    erlang:apply(Handler, find_query, [Data]);
                ?CREATE_RECORD_VAL ->
                    erlang:apply(Handler, create_record, [Data]);
                ?UPDATE_RECORD_VAL ->
                    case erlang:apply(Handler, update_record, [EntityIdOrIds, Data]) of
                        ok -> {ok, null};
                        {error, Msg} -> {error, Msg}
                    end;
                ?DELETE_RECORD_VAL ->
                    case erlang:apply(Handler, delete_record, [EntityIdOrIds]) of
                        ok -> {ok, null};
                        {error, Msg} -> {error, Msg}
                    end
            end,
        ResultVal = case Result of
                        ok -> ?OK_VAL;
                        error -> ?ERROR_VAL
                    end,
        OKResp = [
            {?MSG_TYPE_KEY, ?PULL_RESP_VAL},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ResultVal},
            {?DATA_KEY, RespData}
        ],
        {OKResp, NewBackends}
    catch T:M ->
        ?error_stacktrace("Error while handling websocket message - ~p:~p",
            [T, M]),
        ErrorResp = [
            {?MSG_TYPE_KEY, ?PULL_RESP_VAL},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ?ERROR_VAL},
            {?DATA_KEY, ?INTERNAL_SERVER_ERROR_VAL}
        ],
        {ErrorResp, NewBackends}
    end.
