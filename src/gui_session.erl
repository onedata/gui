%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is used for GUI session manipulation.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_session).
-author("Lukasz Opiola").

-include("gui_session.hrl").
-include_lib("ctool/include/logging.hrl").

% Session id carried by cookie
-type id() :: binary().
% Random nonce that is regularly refreshed
-type nonce() :: binary().
% Cookie - a concatenation of nonce and id
-type cookie() :: binary().
% Opaque term, understood by the gui_session_plugin
-type details() :: #gui_session{}.
-type client() :: term().
% Possible errors returned from API
-type session_error() :: {error, invalid | no_session_cookie}.

-export_type([id/0, details/0, client/0]).

-define(SESSION_ID_LENGTH, gui:get_env(session_id_length, 16)).
-define(NONCE_LENGTH, gui:get_env(session_nonce_length, 16)).

-define(COOKIE_TTL, gui:get_env(session_cookie_ttl, 604800)). % 7 days
-define(COOKIE_REFRESH_INTERVAL, gui:get_env(session_cookie_refresh_interval, 3600)). % 1 hour
-define(COOKIE_GRACE_PERIOD, gui:get_env(session_cookie_grace_period, 20)).

-define(NOW(), ?GUI_SESSION_PLUGIN:timestamp()).
-define(RANDOM_SESSION_ID(), str_utils:rand_hex(?SESSION_ID_LENGTH)).
-define(RANDOM_NONCE(), str_utils:rand_hex(?NONCE_LENGTH)).

%% API
-export([log_in/2, log_out/1, validate/1]).
-export([get_session_id/1]).
-export([cookie_ttl/0, is_expired/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Logs given user in, creating new session and setting the session cookie in
%% http response.
%% @end
%%--------------------------------------------------------------------
-spec log_in(client(), cowboy_req:req()) -> cowboy_req:req() | {error, term()}.
log_in(Client, Req) ->
    Nonce = ?RANDOM_NONCE(),
    SessionId = ?RANDOM_SESSION_ID(),
    GuiSession = #gui_session{
        client = Client,
        last_refresh = ?NOW(),
        nonce = Nonce
    },
    case ?GUI_SESSION_PLUGIN:create(SessionId, GuiSession) of
        ok ->
            Cookie = nonce_and_id_to_cookie(Nonce, SessionId),
            set_session_cookie(Cookie, ?COOKIE_TTL, Req);
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Logs out the user that performed given request, deletes the session and
%% clears the session cookie.
%% @end
%%--------------------------------------------------------------------
-spec log_out(cowboy_req:req()) -> cowboy_req:req() | session_error().
log_out(Req) ->
    case get_session_cookie(Req) of
        undefined ->
            Req;
        Cookie ->
            case cookie_to_nonce_and_id(Cookie) of
                {ok, _Nonce, Id} ->
                    ?GUI_SESSION_PLUGIN:delete(Id),
                    unset_session_cookie(Req);
                {error, invalid} ->
                    Req
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Validates session carried by given cowboy req. Refreshes the session cookie
%% if needed.
%% @end
%%--------------------------------------------------------------------
-spec validate(cowboy_req:req()) ->
    {ok, client(), cookie(), cowboy_req:req()} | session_error().
validate(Req) ->
    case get_session_cookie(Req) of
        undefined ->
            {error, no_session_cookie};
        Cookie ->
            case examine_validity(Cookie) of
                {valid, Client} ->
                    {ok, Client, Cookie, Req};
                {refreshed, Client, NewCookie} ->
                    NewReq = set_session_cookie(NewCookie, ?COOKIE_TTL, Req),
                    {ok, Client, NewCookie, NewReq};
                {error, Error} ->
                    {error, Error}
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the session id carried by given cookie or cowboy request.
%% @end
%%--------------------------------------------------------------------
-spec get_session_id(cookie() | cowboy_req:req()) -> id().
get_session_id(Cookie) when is_binary(Cookie) ->
    {ok, _Nonce, SessionId} = cookie_to_nonce_and_id(Cookie),
    SessionId;
get_session_id(Req) ->
    <<_/binary>> = Cookie = get_session_cookie(Req),
    get_session_id(Cookie).


%%--------------------------------------------------------------------
%% @doc
%% Returns the configured cookie TTL in seconds.
%% @end
%%--------------------------------------------------------------------
-spec cookie_ttl() -> non_neg_integer().
cookie_ttl() ->
    ?COOKIE_TTL.


%%--------------------------------------------------------------------
%% @doc
%% Returns if given session is currently expired.
%% Can be checked based solely on LastRefresh (TTL is universal for GUI sessions).
%% @end
%%--------------------------------------------------------------------
-spec is_expired(details() | non_neg_integer()) -> boolean().
is_expired(#gui_session{last_refresh = LastRefresh}) ->
    is_expired(LastRefresh);
is_expired(LastRefresh) ->
    LastRefresh + ?COOKIE_TTL < ?NOW().


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns if the nonce included in session is currently expired.
%% Can be checked based solely on LastRefresh (nonce TTL is universal for GUI sessions).
%% @end
%%--------------------------------------------------------------------
-spec is_nonce_expired(details() | non_neg_integer()) -> boolean().
is_nonce_expired(#gui_session{last_refresh = LastRefresh}) ->
    is_nonce_expired(LastRefresh);
is_nonce_expired(LastRefresh) ->
    LastRefresh + ?COOKIE_REFRESH_INTERVAL < ?NOW().


%% @private
-spec examine_validity(cookie()) ->
    {valid, client()} | {refreshed, client(), NewCookie :: cookie()} | session_error().
examine_validity(Cookie) ->
    case cookie_to_nonce_and_id(Cookie) of
        {ok, Nonce, Id} -> examine_validity(Nonce, Id);
        {error, _} = Error -> Error
    end.


%% @private
-spec examine_validity(nonce(), id()) ->
    {valid, client()} | {refreshed, client(), NewCookie :: cookie()} | session_error().
examine_validity(Nonce, Id) ->
    case ?GUI_SESSION_PLUGIN:get(Id) of
        {ok, GuiSession = #gui_session{client = Client}} ->
            case examine_ttl(Nonce, GuiSession) of
                valid ->
                    {valid, Client};
                invalid ->
                    {error, invalid};
                expired ->
                    ok = ?GUI_SESSION_PLUGIN:delete(Id),
                    {error, invalid};
                refresh ->
                    UpdateFun = fun(GS = #gui_session{nonce = CurrentNonce}) ->
                        % Make sure nonce was not refreshed in the meantime to
                        % avoid race conditions.
                        case is_nonce_expired(GS) of
                            true ->
                                GS#gui_session{
                                    last_refresh = ?NOW(),
                                    nonce = ?RANDOM_NONCE(),
                                    previous_nonce = CurrentNonce
                                };
                            false ->
                                GS
                        end
                    end,
                    {ok, #gui_session{
                        nonce = NewNonce
                    }} = ?GUI_SESSION_PLUGIN:update(Id, UpdateFun),
                    {refreshed, Client, nonce_and_id_to_cookie(NewNonce, Id)}
            end;
        {error, _} ->
            {error, invalid}
    end.


%% @private
-spec examine_ttl(nonce(), details()) -> valid | refresh | invalid | expired.
examine_ttl(Nonce, #gui_session{nonce = Nonce} = SessionDetails) ->
    case is_expired(SessionDetails) of
        true ->
            expired;
        false ->
            case is_nonce_expired(SessionDetails) of
                true -> refresh;
                false -> valid
            end
    end;
examine_ttl(Nonce, #gui_session{last_refresh = LastRefresh, previous_nonce = Nonce}) ->
    case LastRefresh + ?COOKIE_GRACE_PERIOD >= ?NOW() of
        true -> valid;
        false -> invalid
    end;
examine_ttl(_, _) ->
    invalid.


%% @private
-spec nonce_and_id_to_cookie(nonce(), id()) -> cookie().
nonce_and_id_to_cookie(Nonce, Id) ->
    <<Nonce/binary, "|", Id/binary>>.


%% @private
-spec cookie_to_nonce_and_id(cookie()) -> {ok, nonce(), id()} | {error, invalid}.
cookie_to_nonce_and_id(Cookie) ->
    case binary:split(Cookie, <<"|">>) of
        [Nonce, Id] -> {ok, Nonce, Id};
        _ -> {error, invalid}
    end.


%% @private
-spec get_session_cookie(cowboy_req:req()) -> undefined | binary().
get_session_cookie(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case proplists:get_value(?SESSION_COOKIE_KEY, Cookies, ?NO_SESSION) of
        ?NO_SESSION -> undefined;
        Cookie -> Cookie
    end.


%% @private
-spec set_session_cookie(SessionId :: binary(), TTL :: non_neg_integer(), cowboy_req:req()) ->
    cowboy_req:req().
set_session_cookie(SessionId, TTL, Req) ->
    Options = #{
        path => <<"/">>,
        max_age => TTL,
        secure => true,
        http_only => true
    },
    cowboy_req:set_resp_cookie(?SESSION_COOKIE_KEY, SessionId, Req, Options).


%% @private
-spec unset_session_cookie(cowboy_req:req()) -> cowboy_req:req().
unset_session_cookie(Req) ->
    set_session_cookie(?NO_SESSION, 0, Req).
