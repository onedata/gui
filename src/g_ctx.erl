%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module keeps the context for gui processes. This allows for easy
%%% sharing context between websocket requests and HTTP requests.
%%% All operations on underlying cowboy req must be done using this module,
%%% which stores the cowboy req in process dictionary, modifies it and puts
%%% it back into the dicitonary.
%%% @end
%%%-------------------------------------------------------------------
-module(g_ctx).
-author("Lukasz Opiola").

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-type reply() :: {Code :: integer(), Headers :: http_client:headers(),
    Body :: binary()} | undefined.

-record(ctx, {
    req = undefined :: cowboy_req:req() | undefined,
    gui_route = undefined :: #gui_route{} | undefined,
    reply = undefined :: reply()
}).

%% API
-export([init/1, finish/0]).
-export([session_requirements/0, user_logged_in/0]).
-export([get_html_file/0, set_html_file/1]).
-export([get_page_backend/0, set_page_backend/1]).
% Cowboy req manipulation
-export([get_path/0, set_path/1]).
-export([get_cookie/1, set_resp_cookie/3]).
-export([get_header/1, set_resp_header/2, set_resp_headers/1]).
-export([get_requested_hostname/0]).
-export([get_url_param/1]).
-export([reply/3]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the context.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req()) -> ok.
init(Req) ->
    % Set empty request context - or else context from previous requests
    % could be accidentally used (when connection is kept alive).
    set_ctx(#ctx{}),
    % Set cowboy req in the context
    set_cowboy_req(Req),
    Path = case get_path() of
               <<"/ws", P/binary>> -> P;
               P -> P
           end,
    try ?GUI_ROUTE_PLUGIN:route(Path) of
        GuiRoute ->
            set_gui_route(GuiRoute),
            % Initialize session
            g_session:init(),
            ok
    catch
        error:function_clause ->
            % No such route was found - serve page 404.
            Page404File = ?GUI_ROUTE_PLUGIN:error_404_html_file(),
            set_gui_route(#gui_route{html_file = Page404File}),
            g_session:init(),
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


%%--------------------------------------------------------------------
%% @doc
%% Called just before response is sent to finalize the context.
%% @end
%%--------------------------------------------------------------------
-spec finish() -> cowboy_req:req().
finish() ->
    % Sets proper response cookie
    g_session:finish(),
    Req = get_cowboy_req(),
    % Check if something was staged for reply
    case get_reply() of
        {Code, Headers, Body} ->
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            Req2;
        _ ->
            Req
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the session_requirements of requested page
%% (should the user be logged in, as returned by gui_route_plugin:route/1).
%% @end
%%--------------------------------------------------------------------
-spec session_requirements() ->
    ?SESSION_ANY | ?SESSION_LOGGED_IN | ?SESSION_NOT_LOGGED_IN.
session_requirements() ->
    #gui_route{requires_session = Reqs} = get_gui_route(),
    Reqs.


%%--------------------------------------------------------------------
%% @doc
%% Checks if the user is logged in.
%% @end
%%--------------------------------------------------------------------
-spec user_logged_in() -> boolean().
user_logged_in() ->
    g_session:is_logged_in().


%%--------------------------------------------------------------------
%% @doc
%% Returns the html file that is bound to current route.
%% (as returned by gui_route_plugin:route/1).
%% @end
%%--------------------------------------------------------------------
-spec get_html_file() -> binary() | undefined.
get_html_file() ->
    #gui_route{html_file = File} = get_gui_route(),
    File.


%%--------------------------------------------------------------------
%% @doc
%% Sets the html file for the current route.
%% @end
%%--------------------------------------------------------------------
-spec set_html_file(binary() | undefined) -> ok.
set_html_file(File) ->
    GuiRoute = get_gui_route(),
    set_gui_route(GuiRoute#gui_route{html_file = File}),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns the page backend module that is bound to current route.
%% (as returned by gui_route_plugin:route/1).
%% @end
%%--------------------------------------------------------------------
-spec get_page_backend() -> atom() | undefined.
get_page_backend() ->
    #gui_route{page_backend = Mod} = get_gui_route(),
    Mod.


%%--------------------------------------------------------------------
%% @doc
%% Sets the page backend module for the current route.
%% @end
%%--------------------------------------------------------------------
-spec set_page_backend(atom() | undefined) -> ok.
set_page_backend(Mod) ->
    GuiRoute = get_gui_route(),
    set_gui_route(GuiRoute#gui_route{page_backend = Mod}),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns the requested path of request.
%% @end
%%--------------------------------------------------------------------
-spec get_path() -> binary().
get_path() ->
    Req = get_cowboy_req(),
    {Path, _} = cowboy_req:path(Req),
    Path.


%%--------------------------------------------------------------------
%% @doc
%% Updates the requested path of request.
%% @end
%%--------------------------------------------------------------------
-spec set_path(binary()) -> ok.
set_path(<<"/", PathNoSlash/binary>> = Path) ->
    PathInfo = binary:split(PathNoSlash, <<"/">>),
    Req = get_cowboy_req(),
    Req2 = cowboy_req:set([{path, Path}], Req),
    Req3 = cowboy_req:set([{path_info, PathInfo}], Req2),
    set_cowboy_req(Req3).


%%--------------------------------------------------------------------
%% @doc
%% Returns cookie value for given cookie name.
%% Undefined if no such cookie was sent.
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


%%--------------------------------------------------------------------
%% @doc
%% Sets a response cookie.
%% @end
%%--------------------------------------------------------------------
-spec set_resp_cookie(Key :: binary(), Value :: binary(),
    Options :: cowboy_req:cookie_opts()) -> ok.
set_resp_cookie(Key, Value, Options) ->
    Req = get_cowboy_req(),
    NewReq = cowboy_req:set_resp_cookie(Key, Value, Options, Req),
    set_cowboy_req(NewReq),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns request header by name or undefined if no such header was sent.
%% @end
%%--------------------------------------------------------------------
-spec get_header(Name :: binary()) -> binary() | undefined.
get_header(Name) ->
    Req = get_cowboy_req(),
    {Header, _} = cowboy_req:header(Name, Req, undefined),
    Header.


%%--------------------------------------------------------------------
%% @doc
%% Sets a response header by name. Header must
%% be normalized to lowercase (e. g. content-type and not Content-Type).
%% @end
%%--------------------------------------------------------------------
-spec set_resp_header(Name :: binary(), Value :: binary()) -> ok.
set_resp_header(Name, Value) ->
    Req = get_cowboy_req(),
    Req2 = cowboy_req:delete_resp_header(Name, Req),
    Req3 = cowboy_req:set_resp_header(Name, Value, Req2),
    set_cowboy_req(Req3).


%%--------------------------------------------------------------------
%% @doc
%% Sets response headers, but prevents duplicate entries. Headers must
%% be normalized to lowercase (e. g. content-type and not Content-Type)
%% @end
%%--------------------------------------------------------------------
-spec set_resp_headers(http_client:headers()) -> ok.
set_resp_headers(Headers) ->
    lists:foreach(
        fun({Name, Value}) ->
            set_resp_header(Name, Value)
        end, Headers),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns the requested hostname or undefined if host headers was not sent.
%% @end
%%--------------------------------------------------------------------
-spec get_requested_hostname() -> binary() | undefined.
get_requested_hostname() ->
    get_header(<<"host">>).


%%--------------------------------------------------------------------
%% @doc
%% Returns a URL param by Key, or undefined if no such param was found.
%% @end
%%--------------------------------------------------------------------
-spec get_url_param(Key :: binary()) -> binary() | undefined.
get_url_param(Key) ->
    Req = get_cowboy_req(),
    {Val, _} = cowboy_req:qs_val(Key, Req, undefined),
    Val.


%%--------------------------------------------------------------------
%% @doc
%% Stages a reply that will be performed when responding to current request.
%% @end
%%--------------------------------------------------------------------
-spec reply(Code :: integer(), Headers :: http_client:headers(),
    Body :: binary()) -> ok.
reply(Code, Headers, Body) ->
    % Stage data for reply
    set_reply({Code, Headers, Body}),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores the request context record in process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec set_ctx(Ctx :: #ctx{}) -> ok.
set_ctx(Ctx) ->
    put(ctx, Ctx).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the request context record from process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec get_ctx() -> #ctx{}.
get_ctx() ->
    get(ctx).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores the requests @gui_route record in process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec set_gui_route(GUIRoute :: #gui_route{} | undefined) -> ok.
set_gui_route(GUIRoute) ->
    Ctx = get_ctx(),
    set_ctx(Ctx#ctx{gui_route = GUIRoute}),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the requests @gui_route record from process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec get_gui_route() -> #gui_route{} | undefined.
get_gui_route() ->
    Ctx = get_ctx(),
    Ctx#ctx.gui_route.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores the cowboy #req record in process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec set_cowboy_req(Ctx :: cowboy_req:req()) -> ok.
set_cowboy_req(Req) ->
    Ctx = get_ctx(),
    set_ctx(Ctx#ctx{req = Req}),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the cowboy #req record from process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec get_cowboy_req() -> cowboy_req:req().
get_cowboy_req() ->
    Ctx = get_ctx(),
    Ctx#ctx.req.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the request reply from process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec set_reply(Reply :: reply()) -> ok.
set_reply(Reply) ->
    Ctx = get_ctx(),
    set_ctx(Ctx#ctx{reply = Reply}),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores the request reply in process dictionary.
%% @end
%%--------------------------------------------------------------------
-spec get_reply() -> reply().
get_reply() ->
    Ctx = get_ctx(),
    Ctx#ctx.reply.
