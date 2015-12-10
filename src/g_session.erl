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

% Session cookie id
-define(SESSION_COOKIE_KEY, <<"session_id">>).
% Value of cookie when there is no session
-define(NO_SESSION_COOKIE, <<"no_session">>).

-export([init/0, finish/0]).
-export([get_session_id/0]).
-export([put_value/2, get_value/1]).
-export([log_in/1, log_out/0, is_logged_in/0]).

init() ->
    SessionId = g_ctx:get_cookie(?SESSION_COOKIE_KEY),
    ?dump(SessionId),
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


finish() ->
    {SessionId, Options} = case is_logged_in() of
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


get_session_id() ->
    get(session_id).

% Private so noone can tinker
set_session_id(SessionId) ->
    put(session_id, SessionId).


put_value(Key, Value) ->
    SessionId = get_session_id(),
    case call_lookup_session(SessionId) of
        undefined ->
            throw(user_not_logged_in);
        Memory ->
            NewMemory = [{Key, Value} | proplists:delete(Key, Memory)],
            ok = call_update_session(SessionId, NewMemory)
    end.


get_value(Key) ->
    SessionId = get_session_id(),
    case call_lookup_session(SessionId) of
        undefined ->
            throw(user_not_logged_in);
        Memory ->
            proplists:get_value(Key, Memory, undefined)
    end.


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
