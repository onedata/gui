%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module keeps the session context for GUI processes. The context
%%% includes session_id (saved in cookies and database), is-session
%%% key-value memory and the login status of user. This module calls
%%% gui_session_plugin undearneath to perform application-specific
%%% operations like saving or retireving sessions.
%%% @end
%%%-------------------------------------------------------------------
-module(g_session).
-author("Lukasz Opiola").

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

% Session cookie id
-define(SESSION_COOKIE_KEY, <<"session_id">>).
% Value of cookie when there is no session
-define(NO_SESSION_COOKIE, <<"no_session">>).

%% API
-export([init/0, finish/0]).
-export([get_session_id/0]).
-export([put_value/2, get_value/1]).
-export([log_in/1, log_out/0, is_logged_in/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the session context.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    SessionId = g_ctx:get_cookie(?SESSION_COOKIE_KEY),
    case call_lookup_session(SessionId) of
        undefined ->
            set_logged_in(false),
            set_session_id(?NO_SESSION_COOKIE);
        Memory ->
            set_logged_in(true),
            % Updatings session will refresh its expiration time
            ok = call_update_session(SessionId, Memory),
            set_session_id(SessionId)
    end,
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Finalizes the session context, sets proper response cookie.
%% @end
%%--------------------------------------------------------------------
-spec finish() -> ok.
finish() ->
    {SessionId, Options} =
        case is_logged_in() of
            false ->
                % Session is not valid, send no_session cookie
                Opts = [
                    {path, <<"/">>},
                    {max_age, 0},
                    {secure, true},
                    {http_only, true}
                ],
                {?NO_SESSION_COOKIE, Opts};
            true ->
                % Session is valid, set cookie to SessionId
                SID = case get_session_id() of
                          ?NO_SESSION_COOKIE ->
                              throw(missing_session_id);
                          OldSessionId ->
                              OldSessionId
                      end,
                Opts = [
                    {path, <<"/">>},
                    {max_age, call_get_cookie_ttl()},
                    {secure, true},
                    {http_only, true}
                ],
                {SID, Opts}
        end,
    g_ctx:set_resp_cookie(?SESSION_COOKIE_KEY, SessionId, Options),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Returns the session id or undefined.
%% @end
%%--------------------------------------------------------------------
-spec get_session_id() -> binary() | undefined.
get_session_id() ->
    get(session_id).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets the session id of current request.
%% @end
%%--------------------------------------------------------------------
-spec set_session_id(SessionId :: binary()) -> ok.
set_session_id(SessionId) ->
    put(session_id, SessionId),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Saves a value in session memory.
%% @end
%%--------------------------------------------------------------------
-spec put_value(Key :: term(), Value :: term()) -> ok.
put_value(Key, Value) ->
    SessionId = get_session_id(),
    case call_lookup_session(SessionId) of
        undefined ->
            throw(user_not_logged_in);
        Memory ->
            NewMemory = [{Key, Value} | proplists:delete(Key, Memory)],
            ok = call_update_session(SessionId, NewMemory)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a value from session memory.
%% @end
%%--------------------------------------------------------------------
-spec get_value(Key :: term()) -> Value :: term().
get_value(Key) ->
    SessionId = get_session_id(),
    case call_lookup_session(SessionId) of
        undefined ->
            throw(user_not_logged_in);
        Memory ->
            proplists:get_value(Key, Memory, undefined)
    end.


%%--------------------------------------------------------------------
%% @doc
%% When called, marks the current session as logged in.
%% CustomArgs will be passed to gui_session_plugin:create_session/1.
%% @end
%%--------------------------------------------------------------------
-spec log_in(CustomArgs :: term()) -> {ok, SessionId :: binary()}.
log_in(CustomArgs) ->
    case get_session_id() of
        ?NO_SESSION_COOKIE ->
            ok;
        _ ->
            throw(user_already_logged_in)
    end,
    {ok, SessionId} = call_create_session(CustomArgs),
    set_session_id(SessionId),
    set_logged_in(true),
    {ok, SessionId}.


%%--------------------------------------------------------------------
%% @doc
%% When called, marks the current session as logged out.
%% @end
%%--------------------------------------------------------------------
-spec log_out() -> ok.
log_out() ->
    case get_session_id() of
        ?NO_SESSION_COOKIE ->
            throw(user_already_logged_out);
        _ ->
            ok
    end,
    ok = call_delete_session(get_session_id()),
    set_logged_in(false),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Indicates if the current session is logged in.
%% @end
%%--------------------------------------------------------------------
-spec is_logged_in() -> boolean().
is_logged_in() ->
    % get(?LOGGED_IN_KEY) can return true, false or undefined
    get(logged_in) =:= true.


% Private so noone can tinker
set_logged_in(Flag) ->
    put(logged_in, Flag).


%%%===================================================================
%%% Internal functions
%%%===================================================================

call_create_session(Args) ->
    case ?GUI_SESSION_PLUGIN:create_session(Args) of
        {ok, SessionId} ->
            {ok, SessionId};
        {error, Error} ->
            ?error("Cannot create GUI session: ~p", [Error]),
            {error, Error}
    end.


call_update_session(SessionId, Memory) ->
    case ?GUI_SESSION_PLUGIN:update_session(SessionId, Memory) of
        ok ->
            ok;
        {error, Error} ->
            ?error("Cannot update GUI session (~p): ~p", [SessionId, Error]),
            {error, Error}
    end.


%%--------------------------------------------------------------------
%% @doc Calls back to session logic module to lookup a session. Will not make
%% senseless calls, such as those when session cookie yields no session.
%% @end
%%--------------------------------------------------------------------
-spec call_lookup_session(SessionId :: binary()) ->
    [{Key :: binary(), Val :: binary()}] | undefined.
call_lookup_session(SessionId) ->
    case SessionId of
        undefined ->
            undefined;
        ?NO_SESSION_COOKIE ->
            undefined;
        _ ->
            case ?GUI_SESSION_PLUGIN:lookup_session(SessionId) of
                undefined -> undefined;
                {ok, Memory} -> Memory
            end
    end.


%%--------------------------------------------------------------------
%% @doc Calls back to session logic module to delete a session. Will not make
%% senseless calls, such as those when session cookie yields no session.
%% @end
%%--------------------------------------------------------------------
-spec call_delete_session(SessionId :: binary()) -> ok.
call_delete_session(SessionId) ->
    case SessionId of
        undefined ->
            ok;
        ?NO_SESSION_COOKIE ->
            ok;
        _ ->
            case ?GUI_SESSION_PLUGIN:delete_session(SessionId) of
                ok ->
                    ok;
                {error, Error} ->
                    ?error("Cannot delete GUI session (~p): ~p", [SessionId, Error]),
                    {error, Error}
            end
    end.


call_get_cookie_ttl() ->
    ?GUI_SESSION_PLUGIN:get_cookie_ttl().
