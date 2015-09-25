%%%-------------------------------------------------------------------
%%% @author lopiola
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2015 11:33
%%%-------------------------------------------------------------------
-module(g_ctx).
-author("lopiola").

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

% Key in process dicitonary to store request reply (it is performed on finish)
-define(REPLY_KEY, reply).

%% API
-export([init/1, finish/0]).
-export([session_requirements/0, user_logged_in/0]).
-export([get_html_file/0, set_html_file/1]).
-export([get_page_backend/0, set_page_backend/1]).
% Cowboy req manipulation
-export([get_path/0, set_path/1]).
-export([get_session_id/0, set_resp_session_id/2]).
-export([get_cookie/1, set_resp_cookie/3]).
-export([get_header/1, set_resp_header/2, set_resp_headers/1]).
-export([get_requested_hostname/0]).
-export([get_url_param/1]).
-export([reply/3]).


init(Req) ->
    % Set request context, but skip session id
    set_cowboy_req(Req),
    Path = case get_path() of
               <<"/ws", P/binary>> -> P;
               P -> P
           end,
    try ?GUI_ROUTE_PLUGIN:route(Path) of
        GuiRoute ->
            set_gui_route(GuiRoute),
            g_session:init(get_session_id()),
            ok
    catch
        error:function_clause ->
            % No such route was found - serve page 404.
            Page404File = ?GUI_ROUTE_PLUGIN:error_404_html_file(),
            set_gui_route(#gui_route{html_file = Page404File}),
            ?dump(404),
            g_session:init(get_session_id()),
            ok;
        error:undef ->
            ?error(
                "~p module could not be found. It's required for GUI to work.",
                [?GUI_ROUTE_PLUGIN]),
            throw(cannot_init_context);
        T:M ->
            ?error_stacktrace("Unexpected error in GUI context init - ~p:~p",
                [T, M]),
            throw(cannot_init_context)
    end.



finish() ->
    {SessionID, Options} = g_session:finish(),
    g_ctx:set_resp_session_id(SessionID, Options),
    Req = get_cowboy_req(),
    % Check if something was staged for reply
    case get(?REPLY_KEY) of
        {Code, Headers, Body} ->
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            Req2;
        _ ->
            Req
    end.


session_requirements() ->
    #gui_route{requires_session = Reqs} = get_gui_route(),
    Reqs.


user_logged_in() ->
    g_session:is_logged_in().


get_html_file() ->
    #gui_route{html_file = File} = get_gui_route(),
    File.


set_html_file(File) ->
    GuiRoute = get_gui_route(),
    set_gui_route(GuiRoute#gui_route{html_file = File}),
    ok.


get_page_backend() ->
    #gui_route{page_backend = Mod} = get_gui_route(),
    Mod.


set_page_backend(Mod) ->
    GuiRoute = get_gui_route(),
    set_gui_route(GuiRoute#gui_route{page_backend = Mod}),
    ok.


get_path() ->
    Req = get_cowboy_req(),
    {Path, _} = cowboy_req:path(Req),
    Path.


set_path(<<"/", PathNoSlash/binary>> = Path) ->
    PathInfo = binary:split(PathNoSlash, <<"/">>),
    Req = get_cowboy_req(),
    Req2 = cowboy_req:set([{path, Path}], Req),
    Req3 = cowboy_req:set([{path_info, PathInfo}], Req2),
    set_cowboy_req(Req3).


get_session_id() ->
    get_cookie(?SESSION_COOKIE_KEY).


set_resp_session_id(SessionID, Options) ->
    set_resp_cookie(?SESSION_COOKIE_KEY, SessionID, Options).


%%--------------------------------------------------------------------
%% @doc Returns cookie value for given cookie name. Undefined if no such cookie was sent.
%% NOTE! This should be used instead of cowboy_req:cookie as it contains a bug.
%% @end
%%--------------------------------------------------------------------
-spec get_cookie(Name :: binary()) -> binary() | undefined.
get_cookie(Name) ->
    try
        Req = get_cowboy_req(),
        {Value, _Req} = cowboy_req:cookie(Name, Req),
        Value
    catch _:_ ->
        undefined
    end.


set_resp_cookie(Key, Value, Options) ->
    Req = get_cowboy_req(),
    NewReq = cowboy_req:set_resp_cookie(Key, Value, Options, Req),
    set_cowboy_req(NewReq),
    ok.


set_gui_route(GUIRoute) ->
    put(gui_route, GUIRoute),
    ok.


get_gui_route() ->
    get(gui_route).


set_cowboy_req(Req) ->
    put(req, Req),
    ok.


get_cowboy_req() ->
    get(req).


get_header(Name) ->
    Req = get_cowboy_req(),
    {Header, _} = cowboy_req:header(Name, Req, undefined),
    Header.


set_resp_header(Name, Value) ->
    Req = get_cowboy_req(),
    Req2 = cowboy_req:delete_resp_header(Name, Req),
    Req3 = cowboy_req:set_resp_header(Name, Value, Req2),
    set_cowboy_req(Req3).


%%--------------------------------------------------------------------
%% @doc Sets response headers, but prevents duplicate entries. Headers must
%% be normalized to lowercase (e. g. content-type and not Content-Type)
%% @end
%%--------------------------------------------------------------------
-spec set_resp_headers([{Name :: binary(), Value :: binary()}]) -> ok.
set_resp_headers(Headers) ->
    lists:foreach(
        fun({Name, Value}) ->
            set_resp_header(Name, Value)
        end, Headers),
    ok.


get_requested_hostname() ->
    get_header(<<"host">>).


get_url_param(Key) ->
    Req = get_cowboy_req(),
    {Val, _} = cowboy_req:qs_val(Key, Req, undefined),
    Val.

reply(Code, Headers, Body) ->
    % Stage data for reply
    put(?REPLY_KEY, {Code, Headers, Body}),
    ok.