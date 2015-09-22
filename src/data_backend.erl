%%%-------------------------------------------------------------------
%%% @author lopiola
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Sep 2015 12:28
%%%-------------------------------------------------------------------
-module(data_backend).
-author("lopiola").

-include_lib("ctool/include/logging.hrl").
%% API
-export([aync_process/1, push/1]).

-define(WEBSCOKET_PROCESS_KEY, ws_process).


aync_process(Fun) ->
    % Prevent async proc from killing the calling proc on crash
    process_flag(trap_exit, true),
    WSPid = opn_cowboy_bridge:get_socket_pid(),
    Pid = spawn_link(fun() -> async_init(WSPid, Fun) end),
    {ok, Pid}.


push(Data) ->
    get(?WEBSCOKET_PROCESS_KEY) ! {push, Data}.


async_init(WSPid, Fun) ->
    put(?WEBSCOKET_PROCESS_KEY, WSPid),
    Fun().
