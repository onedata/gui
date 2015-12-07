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
-export([async_process/1, kill_async_processes/0]).
-export([push_updated/1, push_deleted/1]).

-define(WEBSCOKET_PROCESS_KEY, ws_process).
-define(ASYNC_PROCESSES_KEY, async_processes).


async_process(Fun) ->
    % Prevent async proc from killing the calling proc on crash
    process_flag(trap_exit, true),
    WSPid = self(),
    Pid = spawn_link(fun() -> async_init(WSPid, Fun) end),
    append_async_process(Pid),
    {ok, Pid}.


kill_async_processes() ->
    lists:foreach(
        fun(Pid) ->
            exit(Pid, kill)
        end, get_async_processes()),
    ok.


push_updated(Data) ->
    get(?WEBSCOKET_PROCESS_KEY) ! {push_updated, Data}.


push_deleted(Id) ->
    get(?WEBSCOKET_PROCESS_KEY) ! {push_deleted, Id}.


async_init(WSPid, Fun) ->
    put(?WEBSCOKET_PROCESS_KEY, WSPid),
    Fun().


get_async_processes() ->
    case get(?ASYNC_PROCESSES_KEY) of
        undefined ->
            [];
        List when is_list(List) ->
            List
    end.


append_async_process(Pid) ->
    put(?ASYNC_PROCESSES_KEY, [Pid | get_async_processes()]).
