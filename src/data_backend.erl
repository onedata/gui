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
-export([aync_process/1, push/1, kill_async_processes/0]).

-define(WEBSCOKET_PROCESS_KEY, ws_process).
-define(ASYNC_PROCESSES_KEY, async_processes).


aync_process(Fun) ->
    % Prevent async proc from killing the calling proc on crash
    process_flag(trap_exit, true),
    WSPid = self(),
    Pid = spawn_link(fun() -> async_init(WSPid, Fun) end),
    AsyncProcesses = case get(?ASYNC_PROCESSES_KEY) of
                         undefined ->
                             [];
                         List when is_list(List) ->
                             List
                     end,
    put(?ASYNC_PROCESSES_KEY, [Pid | AsyncProcesses]),
    {ok, Pid}.


push(Data) ->
    get(?WEBSCOKET_PROCESS_KEY) ! {push, Data}.


kill_async_processes() ->
    lists:foreach(
        fun(Pid) ->
            exit(Pid, kill)
        end, get(?ASYNC_PROCESSES_KEY)),
    ok.


async_init(WSPid, Fun) ->
    put(?WEBSCOKET_PROCESS_KEY, WSPid),
    Fun().
