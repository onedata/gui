%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies an API for gui routing logic - a module that
%%% decides to which handler modules GUI requests should be routed.
%%% The implementing module must be called ?GUI_ROUTE_PLUGIN.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_route_plugin_behaviour).
-author("Lukasz Opiola").

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
%% Should return a module that implements data_backend_behaviour and
%% will be called for models synchronization over websocket.
%% @end
%%--------------------------------------------------------------------
-callback data_backend(Identifier :: binary()) -> HandlerModule :: module().


%%--------------------------------------------------------------------
%% @doc
%% Should return a module that implements callback_backend_behaviour and
%% will be called to handle calls from the GUI that do not regard models.
%% @end
%%--------------------------------------------------------------------
-callback callback_backend(Identifier :: binary()) -> HandlerModule :: module().


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
%% Should return a file name of the HTML file that displays error 500 page.
%% @end
%%--------------------------------------------------------------------
-callback error_500_html_file() -> FileName :: binary().

