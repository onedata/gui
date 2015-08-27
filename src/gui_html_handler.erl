-module(gui_html_handler).

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

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


-export([is_html_req/1, maybe_handle_html_req/1, handle_ws_req/1]).


is_html_req(<<"/">>) ->
    true;

is_html_req(<<?WEBSOCKET_PREFIX_PATH, Path/binary>>) ->
    is_html_req(Path);

is_html_req(<<"/", Path/binary>>) ->
    is_html_req(Path);

is_html_req(Path) ->
    case byte_size(Path) >= 5 andalso binary:split(Path, <<"/">>) =:= [Path] of
        false ->
            false;
        true ->
            case binary_part(Path, {byte_size(Path), -5}) of
                <<".html">> ->
                    true;
                _ ->
                    false
            end
    end.


maybe_handle_html_req(Req) ->
    {FullPath, _} = cowboy_req:path(Req),
    case is_html_req(FullPath) of
        true ->
            % Initialize context, run page's init code,
            % let cowboy static handler server the html
            handle_html_req(Req);
        false ->
            % Just let the cowboy static handler serve a static file
            {continue, Req}
    end.


handle_html_req(Req) ->
    g_ctx:init(Req),
    % Coalesce possible results from page_init.
    InitResult =
        case page_init_callback() of
            serve_html ->
                {serve_html, []};
            {serve_body, Bd} ->
                {reply, 200, Bd, [{<<"content-type">>, <<"text/plain">>}]};
            {serve_body, Bd, Hdrs} ->
                {reply, 200, Bd, Hdrs};
            display_404_page ->
                g_ctx:set_html_file(?GUI_ROUTE_PLUGIN:error_404_html_file()),
                {serve_html, []};
            display_500_page ->
                g_ctx:set_html_file(?GUI_ROUTE_PLUGIN:error_500_html_file()),
                {serve_html, []};
            {redirect, URL} ->
                Hdrs = [
                    {<<"location">>, URL},
                    {<<"content-type">>, <<"text/html">>}
                ],
                {reply, 307, <<"">>, Hdrs};
            Other ->
                Other
        end,
    % Process the page_init results.
    Result =
        case InitResult of
            {serve_html, Headers} ->
                case g_ctx:get_html_file() of
                    undefined ->
                        ?error("HTML file for page ~p is not defined.",
                            [g_ctx:get_path()]),
                        g_ctx:reply(500, [], <<"">>),
                        finish;
                    Path ->
                        HtmlFileToServe = <<"/", (Path)/binary>>,
                        g_ctx:set_path(HtmlFileToServe),
                        g_ctx:set_resp_headers(Headers),
                        continue
                end;
            {reply, Code, Body, Headers} ->
                g_ctx:reply(Code, Headers, Body),
                finish
        end,
    NewReq = g_ctx:finish(),
    {Result, NewReq}.


handle_ws_req(Props) ->
    MsgUUID = proplists:get_value(?UUID_KEY, Props, null),
    try
        ?PULL_REQ_VAL = proplists:get_value(?MSG_TYPE_KEY, Props),
        Data = proplists:get_value(?DATA_KEY, Props),
        EntityType = proplists:get_value(?ENTITY_TYPE_KEY, Props),
        EntityIdOrIds = proplists:get_value(?ENTITY_IDS_KEY, Props),
        {Result, RespData} =
            case proplists:get_value(?OPERATION_KEY, Props) of
                ?FIND_VAL ->
                    handle_find_req(EntityType, [EntityIdOrIds]);
                ?FIND_MANY_VAL ->
                    handle_find_req(EntityType, EntityIdOrIds);
                ?FIND_ALL_VAL ->
                    handle_find_all_req(EntityType);
                ?FIND_QUERY_VAL ->
                    handle_find_query_req(EntityType, Data);
                ?CREATE_RECORD_VAL ->
                    handle_create_record_req(EntityType, Data);
                ?UPDATE_RECORD_VAL ->
                    handle_update_record_req(EntityType, EntityIdOrIds, Data);
                ?DELETE_RECORD_VAL ->
                    handle_delete_record_req(EntityType, EntityIdOrIds)
            end,
        ResultVal = case Result of
                        ok -> ?OK_VAL;
                        error -> ?ERROR_VAL
                    end,
        [
            {?MSG_TYPE_KEY, ?PULL_RESP_VAL},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ResultVal},
            {?DATA_KEY, RespData}
        ]
    catch T:M ->
        ?error_stacktrace("Error while handling websocket message - ~p:~p",
            [T, M]),
        [
            {?MSG_TYPE_KEY, ?PULL_RESP_VAL},
            {?UUID_KEY, MsgUUID},
            {?RESULT_KEY, ?ERROR_VAL},
            {?DATA_KEY, ?INTERNAL_SERVER_ERROR_VAL}
        ]
    end.


handle_find_req(EntityType, Ids) ->
    dynamic_backend_callback(find, [EntityType, Ids]).

handle_find_all_req(EntityType) ->
    dynamic_backend_callback(find_all, [EntityType]).

handle_find_query_req(EntityType, Data) ->
    dynamic_backend_callback(find_query, [EntityType, Data]).

handle_create_record_req(EntityType, Data) ->
    dynamic_backend_callback(create_record, [EntityType, Data]).

handle_update_record_req(EntityType, Id, Data) ->
    case dynamic_backend_callback(update_record, [EntityType, Id, Data]) of
        ok -> {ok, null};
        {error, Msg} -> {error, Msg}
    end.

handle_delete_record_req(EntityType, Id) ->
    case dynamic_backend_callback(delete_record, [EntityType, Id]) of
        ok -> {ok, null};
        {error, Data} -> {error, Data}
    end.


page_init_callback() ->
    case g_ctx:get_page_backend() of
        undefined ->
            % No backend specified - just serve the HTML.
            serve_html;
        Module ->
            % Backend is specified - run the page_init fun. Crash if it
            % is undefined.
            Module:page_init()
    end.

% Dynamic pages must implement the complete dynamic_backend_behaviour.
% This function will crash if they do not.
dynamic_backend_callback(Fun, Args) ->
    Module = g_ctx:get_page_backend(),
    erlang:apply(Module, Fun, Args).