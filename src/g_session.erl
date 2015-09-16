%%%-------------------------------------------------------------------
%%% @author lopiola
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Aug 2015 13:07
%%%-------------------------------------------------------------------
-module(g_session).
-author("lopiola").

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

% Key to process dictionary, which holds information of current
% SessionId in the context.
-define(SESSION_ID_KEY, session_id).
% Key to process dictionary, which holds information if
% the current session is valid (user hasn't logged out etc).
-define(LOGGED_IN_KEY, logged_in).

-export([init/1, finish/0]).
-export([put_value/2, get_value/1]).
-export([log_in/1, log_out/0, is_logged_in/0]).
-export([clear_expired_sessions/0]).

init(SessionId) ->
    ?dump({init, SessionId}),
    case call_lookup_session(SessionId) of
        undefined ->
            put(?LOGGED_IN_KEY, false),
            put(?SESSION_ID_KEY, ?NO_SESSION_COOKIE);
        Props ->
            put(?LOGGED_IN_KEY, true),
            % Updaet session will refresh its expiration time
            ok = call_update_session(SessionId, Props),
            put(?SESSION_ID_KEY, SessionId)
    end,
    ok.


finish() ->
    case is_logged_in() of
        false ->
            % Session is not valid, send no_session cookie
            Options = [
                {path, <<"/">>},
                {max_age, 0},
                {secure, true},
                {http_only, true}
            ],
            {?NO_SESSION_COOKIE, Options};
        true ->
            % Session is valid, set cookie to SessionId
            SessionId = case get(?SESSION_ID_KEY) of
                            ?NO_SESSION_COOKIE ->
                                throw(missing_session_id);
                            OldSessionId ->
                                OldSessionId
                        end,
            Options = [
                {path, <<"/">>},
                {max_age, ?GUI_SESSION_PLUGIN:get_cookie_ttl()},
                {secure, true},
                {http_only, true}
            ],
            {SessionId, Options}
    end.


put_value(Key, Value) ->
    SessionId = get(?SESSION_ID_KEY),
    case call_lookup_session(SessionId) of
        undefined ->
            throw(user_not_logged_in);
        Props ->
            NewProps = [{Key, Value} | proplists:delete(Key, Props)],
            ok = call_update_session(SessionId, NewProps)
    end.


get_value(Key) ->
    SessionId = get(?SESSION_ID_KEY),
    case call_lookup_session(SessionId) of
        undefined ->
            throw(user_not_logged_in);
        Props ->
            proplists:get_value(Key, Props, undefined)
    end.


log_in(CustomArgs) ->
    case get(?SESSION_ID_KEY) of
        ?NO_SESSION_COOKIE ->
            ok;
        _ ->
            throw(user_already_logged_in)
    end,
    {ok, SessionId} = call_create_session(CustomArgs),
    put(?SESSION_ID_KEY, SessionId),
    put(?LOGGED_IN_KEY, true),
    {ok, SessionId}.


log_out() ->
    case get(?SESSION_ID_KEY) of
        ?NO_SESSION_COOKIE ->
            throw(user_already_logged_out);
        _ ->
            ok
    end,
    ok = call_delete_session(get(?SESSION_ID_KEY)),
    put(?LOGGED_IN_KEY, false),
    ok.


is_logged_in() ->
    get(?LOGGED_IN_KEY) =:= true.


%%--------------------------------------------------------------------
%% @doc Deletes all sessions that have expired. Every session is saved
%% with a Expires arg, that marks a point in time when it expires
%% (in secs since epoch).
%% It has to be periodically called as it is NOT performed automatically.
%% @end
%%--------------------------------------------------------------------
-spec clear_expired_sessions() -> ok.
clear_expired_sessions() ->
    ?GUI_SESSION_PLUGIN:clear_expired_sessions().


%%%===================================================================
%%% Internal functions
%%%===================================================================


call_create_session(Args) ->
    ?GUI_SESSION_PLUGIN:create_session(get_expiration_time(), Args).


call_update_session(SessionId, Props) ->
    ?GUI_SESSION_PLUGIN:save_session(SessionId, Props, get_expiration_time()).


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
            ?GUI_SESSION_PLUGIN:lookup_session(SessionId)
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
            ?GUI_SESSION_PLUGIN:delete_session(SessionId)
    end.


get_expiration_time() ->
    {Megaseconds, Seconds, _} = now(),
    Megaseconds * 1000000 + Seconds + ?GUI_SESSION_PLUGIN:get_cookie_ttl().