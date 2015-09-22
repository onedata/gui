%%%-------------------------------------------------------------------
%%% @author lopiola
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Aug 2015 13:27
%%%-------------------------------------------------------------------
-module(gui_route_plugin_behaviour).
-author("lopiola").

-include("gui.hrl").


%%--------------------------------------------------------------------
%% @doc
%% Should return a gui_route record per every page that a user can visit.
%% If the Path is not valid, error_404_gui_route/0 function will be used
%% to retrieve gui_route.
%% @end
%%--------------------------------------------------------------------
-callback route(Path :: binary()) -> #gui_route{}.


%%--------------------------------------------------------------------
%% @doc
%% Should return a gui_route record per every page that a user can visit.
%% If the Path is not valid, error_404_gui_route/0 function will be used
%% to retrieve gui_route.
%% @end
%%--------------------------------------------------------------------
-callback data_backend(Identifier :: binary()) -> HandlerModule :: module().


%%--------------------------------------------------------------------
%% @doc
%% Should return login page where the user will be redirected if he requests
%% a page that can only be visited when logged in.
%% @end
%%--------------------------------------------------------------------
-callback login_page_path() -> Path :: binary().


%%--------------------------------------------------------------------
%% @doc
%% Should return a default page where the user will be redirected if
%% he requests a page that he cannot currently visit (for example login page
%% when the user is already logged in).
%% @end
%%--------------------------------------------------------------------
-callback default_page_path() -> Path :: binary().


%%--------------------------------------------------------------------
%% @doc
%% Should return a file name of the HTML file that displays error 404 page.
%% @end
%%--------------------------------------------------------------------
-callback error_404_html_file() -> FileName :: binary().


%%--------------------------------------------------------------------
%% @doc
%% Should return a file name of the HTML file that displays error 404 page.
%% @end
%%--------------------------------------------------------------------
-callback error_500_html_file() -> FileName :: binary().

