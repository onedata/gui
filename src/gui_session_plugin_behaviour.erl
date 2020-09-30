%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies an API for session logic - a module,
%%% that is capable of persisting GUI session.
%%% The implementing module must be called ?NEW_GUI_SESSION_PLUGIN.
%%% Note that gui logic does NOT perform clearing of outdated sessions.
%%% This should be done by the application that uses gui sessions.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_session_plugin_behaviour).
-author("Lukasz Opiola").

%%--------------------------------------------------------------------
%% @doc
%% Creates a new session with given id.
%% @end
%%--------------------------------------------------------------------
-callback create(gui_session:id(), gui_session:details()) -> ok | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a session by Id.
%% @end
%%--------------------------------------------------------------------
-callback get(gui_session:id()) -> {ok, gui_session:details()} | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Updates a session using given diff function.
%% @end
%%--------------------------------------------------------------------
-callback update(gui_session:id(), fun((gui_session:details()) -> gui_session:details())) ->
    {ok, gui_session:details()} | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Deletes a session by Id.
%% @end
%%--------------------------------------------------------------------
-callback delete(gui_session:id()) -> ok | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Returns current timestamp that will be used to track session validity.
%% @end
%%--------------------------------------------------------------------
-callback timestamp() -> time_utils:seconds().


%%--------------------------------------------------------------------
%% @doc
%% Returns the key of session cookies.
%% @end
%%--------------------------------------------------------------------
-callback session_cookie_key() -> binary().
