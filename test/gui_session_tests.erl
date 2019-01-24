%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This suite contains unit tests for functions in gui_session module.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_session_tests).
-author("Lukasz Opiola").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("gui_session.hrl").

-define(MOCKED_COOKIE_TTL, 7).
-define(MOCKED_COOKIE_REFRESH_INTERVAL, 4).
-define(MOCKED_COOKIE_GRACE_PERIOD, 2).

%%%===================================================================
%%% Tests functions
%%%===================================================================


gui_session_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"log in", fun log_in/0},
            {"log out", fun log_out/0},
            {"bad cookie", fun bad_cookie/0},
            {"cookie expiry", fun cookie_expiry/0},
            {"cookie refresh", fun cookie_refresh/0},
            {"regular cookie refreshing prolongs session infinitely",
                fun regular_cookie_refreshing_prolongs_session_infinitely/0},
            {"old cookie grace period", fun old_cookie_grace_period/0}
        ]
    }.


log_in() ->
    Client = <<"user1">>,
    Req = mocked_cowboy_req(undefined),
    Req2 = gui_session:log_in(Client, Req),
    Cookie = parse_resp_cookie(Req2),
    ?assert(is_binary(Cookie)),
    Req3 = simulate_next_http_request(Req2),
    ?assertMatch({ok, Client, Cookie, Req3}, gui_session:validate(Req3)).


log_out() ->
    Client = <<"user1">>,
    Req = mocked_cowboy_req(undefined),
    Req2 = gui_session:log_in(Client, Req),
    Cookie = parse_resp_cookie(Req2),
    SessionId = gui_session:get_session_id(Cookie),
    ?assertMatch({ok, #gui_session{}}, get_session_mock(SessionId)),
    Req3 = simulate_next_http_request(Req2),
    ?assertMatch({ok, Client, _, Req3}, gui_session:validate(Req3)),
    Req4 = gui_session:log_out(Req3),
    Req5 = simulate_next_http_request(Req4),
    ?assertMatch({error, no_session_cookie}, gui_session:validate(Req5)),
    % Make sure session was deleted
    ?assertMatch({error, not_found}, get_session_mock(SessionId)).


bad_cookie() ->
    Req1 = mocked_cowboy_req(undefined),
    ?assertMatch({error, no_session_cookie}, gui_session:validate(Req1)),

    Req2 = mocked_cowboy_req(<<"bad-cookie">>),
    ?assertMatch({error, invalid}, gui_session:validate(Req2)),

    Req3 = mocked_cowboy_req(<<"bad-nonce|bad-id">>),
    ?assertMatch({error, invalid}, gui_session:validate(Req3)).


cookie_expiry() ->
    Client = <<"user1">>,
    Req = mocked_cowboy_req(undefined),
    Req2 = gui_session:log_in(Client, Req),
    Cookie = parse_resp_cookie(Req2),
    SessionId = gui_session:get_session_id(Cookie),
    ?assertMatch({ok, #gui_session{}}, get_session_mock(SessionId)),
    Req3 = simulate_next_http_request(Req2),
    simulate_time_passing(?MOCKED_COOKIE_TTL + 1),
    ?assertMatch({error, invalid}, gui_session:validate(Req3)),
    % Make sure session was deleted
    ?assertMatch({error, not_found}, get_session_mock(SessionId)).


cookie_refresh() ->
    Client = <<"user1">>,
    Req = mocked_cowboy_req(undefined),
    Req2 = gui_session:log_in(Client, Req),
    Cookie = parse_resp_cookie(Req2),
    Req3 = simulate_next_http_request(Req2),
    simulate_time_passing(?MOCKED_COOKIE_REFRESH_INTERVAL + 1),
    Result = gui_session:validate(Req3),
    ?assertMatch({ok, Client, _, _}, Result),
    {ok, Client, NewCookie, Req4} = Result,
    ?assert(Cookie /= NewCookie),
    ?assertMatch(NewCookie, parse_resp_cookie(Req4)),
    Req5 = simulate_next_http_request(Req4),
    ?assertMatch({ok, Client, NewCookie, _}, gui_session:validate(Req5)).


regular_cookie_refreshing_prolongs_session_infinitely() ->
    Client = <<"user1">>,
    Req = mocked_cowboy_req(undefined),
    Req2 = gui_session:log_in(Client, Req),
    Req3 = simulate_next_http_request(Req2),

    RefreshManyTimes = fun Fun(CurrentReq, Count) ->
        simulate_time_passing(?MOCKED_COOKIE_TTL - 1),
        Result = gui_session:validate(CurrentReq),
        ?assertMatch({ok, Client, _, _}, Result),
        case Count of
            0 ->
                ok;
            _ ->
                {ok, _, _, RespReq} = Result,
                NewReq = simulate_next_http_request(RespReq),
                Fun(NewReq, Count - 1)
        end
    end,

    RefreshManyTimes(Req3, 30).


old_cookie_grace_period() ->
    Client = <<"user1">>,
    Req = mocked_cowboy_req(undefined),
    Req2 = gui_session:log_in(Client, Req),
    OldCookie = parse_resp_cookie(Req2),
    Req3 = simulate_next_http_request(Req2),
    simulate_time_passing(?MOCKED_COOKIE_REFRESH_INTERVAL + 1),
    {ok, Client, NewCookie, _} = gui_session:validate(Req3),
    ?assert(OldCookie /= NewCookie),

    Req4 = mocked_cowboy_req(OldCookie),
    simulate_time_passing(?MOCKED_COOKIE_GRACE_PERIOD - 1),
    ?assertMatch({ok, Client, OldCookie, _}, gui_session:validate(Req4)),

    simulate_time_passing(2),
    ?assertMatch({error, invalid}, gui_session:validate(Req4)),

    Req5 = mocked_cowboy_req(NewCookie),
    ?assertMatch({ok, Client, NewCookie, _}, gui_session:validate(Req5)).


setup() ->
    meck:new(?GUI_SESSION_PLUGIN, [non_strict]),

    meck:expect(?GUI_SESSION_PLUGIN, create, fun(Id, GuiSession) ->
        put(Id, GuiSession),
        ok
    end),

    meck:expect(?GUI_SESSION_PLUGIN, get, fun get_session_mock/1),
    meck:expect(?GUI_SESSION_PLUGIN, update, fun update_session_mock/2),
    meck:expect(?GUI_SESSION_PLUGIN, delete, fun delete_session_mock/1),
    meck:expect(?GUI_SESSION_PLUGIN, timestamp, fun timestamp_mock/0),

    gui:set_env(session_cookie_ttl, ?MOCKED_COOKIE_TTL),
    gui:set_env(session_cookie_refresh_interval, ?MOCKED_COOKIE_REFRESH_INTERVAL),
    gui:set_env(session_cookie_grace_period, ?MOCKED_COOKIE_GRACE_PERIOD),

    ok.


teardown(_) ->
    ?assert(meck:validate([?GUI_SESSION_PLUGIN])),
    meck:unload().


get_session_mock(Id) ->
    case get(Id) of
        undefined -> {error, not_found};
        GuiSession -> {ok, GuiSession}
    end.


update_session_mock(Id, Diff) ->
    case get(Id) of
        undefined ->
            {error, not_found};
        GuiSession ->
            Updated = Diff(GuiSession),
            put(Id, Updated),
            {ok, Updated}
    end.


delete_session_mock(Id) ->
    put(Id, undefined),
    ok.


timestamp_mock() ->
    case get(mocked_time) of
        undefined -> 1500000000; % starting timestamp
        Val -> Val
    end.


simulate_time_passing(Seconds) ->
    put(mocked_time, timestamp_mock() + Seconds).


mocked_cowboy_req(Cookie) -> #{
    resp_headers => #{},
    headers => case Cookie of
        undefined ->
            #{};
        _ ->
            #{<<"cookie">> => <<?SESSION_COOKIE_KEY/binary, "=", Cookie/binary>>}
    end
}.


parse_resp_cookie(#{resp_cookies := #{?SESSION_COOKIE_KEY := CookieIoList}}) ->
    Cookie = iolist_to_binary(CookieIoList),
    CookieKey = ?SESSION_COOKIE_KEY,
    CookieLen = byte_size(?SESSION_COOKIE_KEY),
    [<<CookieKey:CookieLen/binary, "=", Sid/binary>> | _] = binary:split(Cookie, <<";">>, [global, trim_all]),
    Sid;
parse_resp_cookie(_) ->
    undefined.


simulate_next_http_request(Req) ->
    case parse_resp_cookie(Req) of
        undefined -> Req;
        Cookie -> mocked_cowboy_req(Cookie)
    end.


-endif.
