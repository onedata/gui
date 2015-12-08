-module(gui_html_handler).

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([is_html_req/1, maybe_handle_html_req/1]).



%@todo
-type cookie_opts() :: cow_cookie:cookie_opts().
-export_type([cookie_opts/0]).

-type content_decode_fun() :: fun((binary())
    -> {ok, binary()}
| {error, atom()}).
-type transfer_decode_fun() :: fun((binary(), any())
    -> cow_http_te:decode_ret()).

-type body_opts() :: [{continue, boolean()}
| {length, non_neg_integer()}
| {read_length, non_neg_integer()}
| {read_timeout, timeout()}
| {transfer_decode, transfer_decode_fun(), any()}
| {content_decode, content_decode_fun()}].
-export_type([body_opts/0]).

-type resp_body_fun() :: fun((any(), module()) -> ok).
-type send_chunk_fun() :: fun((iodata()) -> ok | {error, atom()}).
-type resp_chunked_fun() :: fun((send_chunk_fun()) -> ok).

-record(http_req, {
    %% Transport.
    socket = undefined :: any(),
    transport = undefined :: undefined | module(),
    connection = keepalive :: keepalive | close,

    %% Request.
    pid = undefined :: pid(),
    method = <<"GET">> :: binary(),
    version = 'HTTP/1.1' :: cowboy:http_version(),
    peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
    host = undefined :: undefined | binary(),
    host_info = undefined :: undefined | cowboy_router:tokens(),
    port = undefined :: undefined | inet:port_number(),
    path = undefined :: binary(),
    path_info = undefined :: undefined | cowboy_router:tokens(),
    qs = undefined :: binary(),
    qs_vals = undefined :: undefined | list({binary(), binary() | true}),
    bindings = undefined :: undefined | cowboy_router:bindings(),
    headers = [] :: cowboy:http_headers(),
    p_headers = [] :: [any()],
    cookies = undefined :: undefined | [{binary(), binary()}],
    meta = [] :: [{atom(), any()}],

    %% Request body.
    body_state = waiting :: waiting | done | {stream, non_neg_integer(),
        transfer_decode_fun(), any(), content_decode_fun()},
    buffer = <<>> :: binary(),
    multipart = undefined :: undefined | {binary(), binary()},

    %% Response.
    resp_compress = false :: boolean(),
    resp_state = waiting :: locked | waiting | waiting_stream
    | chunks | stream | done,
    resp_headers = [] :: cowboy:http_headers(),
    resp_body = <<>> :: iodata() | resp_body_fun()
    | {non_neg_integer(), resp_body_fun()}
    | {chunked, resp_chunked_fun()},

    %% Functions.
    onresponse = undefined :: undefined | already_called
    | cowboy:onresponse_fun()
}).



is_html_req(<<"/">>) ->
    true;

is_html_req(<<?WEBSOCKET_PREFIX_PATH>>) ->
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
            % Initialize context, run page's init code, reply, redirect or just
            % let cowboy static handler serve the html.
            % Catch all errors here
            try
                handle_html_req(Req)
            catch T:M ->
                % If an error occured, display error 505 page (will be served
                % by cowboy_static).
                ?error_stacktrace("Error while handling a HTTP request - ~p:~p",
                    [T, M]),
                Page505File = ?GUI_ROUTE_PLUGIN:error_500_html_file(),
                Req2 = cowboy_req:set([{path, <<"/", Page505File/binary>>}], Req),
                Req3 = cowboy_req:set([{path_info, [Page505File]}], Req2),
                {continue, Req3}
            end;
        false ->
            % Just let the cowboy static handler serve a static file
            {continue, Req}
    end.


handle_html_req(Req) ->
    % Initialize request context
    g_ctx:init(Req),
    % Check if the user is permitted to see the page
    % If so, call page_init/0 callback
    % If not, redirect to another page
    ReqrsSess = g_ctx:session_requirements(),
    LoggedIn = g_session:is_logged_in(),
    PageInitResult =
        case {ReqrsSess, LoggedIn} of
            {?SESSION_ANY, _} ->
                page_init_callback();
            {?SESSION_LOGGED_IN, true} ->
                page_init_callback();
            {?SESSION_LOGGED_IN, false} ->
                {redirect_relative, ?GUI_ROUTE_PLUGIN:login_page_path()};
            {?SESSION_NOT_LOGGED_IN, false} ->
                page_init_callback();
            {?SESSION_NOT_LOGGED_IN, true} ->
                {redirect_relative, ?GUI_ROUTE_PLUGIN:default_page_path()}
        end,
    % Coalesce possible results from page_init
    CoalescedResult =
        case PageInitResult of
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
            {redirect_relative, URL} ->
                % @todo https: moze automatycznie wykryc, a nie hardocodowane
                FullURL = <<"https://", (g_ctx:get_requested_hostname())/binary, URL/binary>>,
                {reply, 307, <<"">>, [{<<"location">>, FullURL}]};
            {redirect_absolute, AbsURL} ->
                {reply, 307, <<"">>, [{<<"location">>, AbsURL}]};
            Other ->
                Other
        end,
    ?dump({g_ctx:get_path(), CoalescedResult}),
    % Process the page_init results.
    Result =
        case CoalescedResult of
            {serve_html, Headers} ->
                case g_ctx:get_html_file() of
                    undefined ->
                        ?error("HTML file for page ~p is not defined.",
                            [g_ctx:get_path()]),
                        Page500Path = ?GUI_ROUTE_PLUGIN:error_500_html_file(),
                        HtmlFileToServe = <<"/", (Page500Path)/binary>>,
                        g_ctx:set_path(HtmlFileToServe),
                        continue;
                    Path ->
                        HtmlFileToServe = <<"/", (Path)/binary>>,
                        g_ctx:set_path(HtmlFileToServe),
                        g_ctx:set_resp_headers(Headers),
                        ?dump({HtmlFileToServe,Headers}),
                        continue
                end;
            {reply, Code, Body, Headers} ->
                g_ctx:reply(Code, Headers, Body),
                finish
        end,
    NewReq = g_ctx:finish(),
    ?dump(NewReq#http_req.resp_state),
    {Result, NewReq}.

page_init_callback() ->
    case g_ctx:get_page_backend() of
        undefined ->
            % No backend specified - just serve the HTML.
            {serve_html, []};
        Module ->
            % Backend is specified - run the page_init fun. Crash if it
            % is undefined.
            Module:page_init()
    end.