%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies an API for session logic - a module,
%%% that is capable of persisting GUI sessions (in ETS, DB or anything else).
%%% Such module will be called from gui_session_handler.
%%% The implementing module must be called ?GUI_SESSION_PLUGIN.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_session_plugin_behaviour).
-author("Lukasz Opiola").

%%--------------------------------------------------------------------
%% @doc
%% Initializes the session_logic module. Any setup such as ets creation
%% should be performed in this function.
%% @end
%%--------------------------------------------------------------------
-callback init() -> ok.


%%--------------------------------------------------------------------
%% @doc
%% Performs any cleanup, such as deleting the previously created ets tables.
%% @end
%%--------------------------------------------------------------------
-callback cleanup() -> ok.


%%--------------------------------------------------------------------
%% @doc
%% Should create a new session under SessionID key.
%% The session is valid up to given moment (Expires).
%% Expires is expressed in number of seconds since epoch.
%% CustomArgs are the args that are passed to g_session:log_in/1 function,
%% they are application specific arguments that are needed to create a session.
%% @end
%%--------------------------------------------------------------------
-callback create_session(CustomArgs) ->
    {ok, SessionId} | {error, term()} when
    CustomArgs :: [term()], SessionId :: binary().


%%--------------------------------------------------------------------
%% @doc
%% Should save session data under SessionID key. Updates the session memory.
%% If there is no record of session
%% with id SessionID, error atom should be returned.
%% @end
%%--------------------------------------------------------------------
-callback update_session(SessionID, Memory) -> ok | {error, term()}
    when SessionID :: binary(),
    Memory :: [{Key :: binary(), Value :: binary}].


%%--------------------------------------------------------------------
%% @doc
%% Should lookup a session by given SessionID key.
%% On success, returns session memory,
%% or undefined if given session does not exist.
%% @end
%%--------------------------------------------------------------------
-callback lookup_session(SessionId :: binary()) -> {ok, Memory} | undefined
    when Memory :: [{Key :: binary(), Value :: binary}].


%%--------------------------------------------------------------------
%% @doc
%% Should delete a session by SessionID key.
%% @end
%%--------------------------------------------------------------------
-callback delete_session(SessionID :: binary()) -> ok | {error, term()}.


%%--------------------------------------------------------------------
%% @doc
%% Should return cookies time to live in seconds.
%% @end
%%--------------------------------------------------------------------
-callback get_cookie_ttl() -> integer() | {error, term()}.