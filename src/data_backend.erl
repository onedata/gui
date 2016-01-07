%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for easy creation and management of asynchronous
%%% processes that can aid in pushing information about
%%% model changes to client side.
%%% @end
%%%-------------------------------------------------------------------
-module(data_backend).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
%% API
-export([async_process/1, kill_async_processes/0]).
-export([push_updated/1, push_deleted/1]).

% Keys in process dictionary used to store PIDs of processes.
-define(WEBSCOKET_PROCESS_KEY, ws_process).
-define(ASYNC_PROCESSES_KEY, async_processes).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an asynchronous process that can communicate with the calling
%% process. Functions push_updated/1 and push_deleted/1 can be called from
%% the async process to push information through websocket
%% channel to the client about model changes.
%% @end
%%--------------------------------------------------------------------
-spec async_process(Fun :: fun()) -> {ok, Pid :: pid()}.
async_process(Fun) ->
    % Prevent async proc from killing the calling proc on crash
    process_flag(trap_exit, true),
    WSPid = self(),
    Pid = spawn_link(fun() -> async_init(WSPid, Fun) end),
    append_async_process(Pid),
    {ok, Pid}.


%%--------------------------------------------------------------------
%% @doc
%% Kills all sync processes that have been spawned by the calling process.
%% @end
%%--------------------------------------------------------------------
-spec kill_async_processes() -> ok.
kill_async_processes() ->
    lists:foreach(
        fun(Pid) ->
            exit(Pid, kill)
        end, get_async_processes()),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about model update to the client via websocket channel.
%% The Data is a proplist that will be translated to JSON, it must include
%% <<"id">> field. It might also be the updated data of many records.
%% @end
%%--------------------------------------------------------------------
-spec push_updated(Data :: proplists:proplist()) -> ok.
push_updated(Data) ->
    get(?WEBSCOKET_PROCESS_KEY) ! {push_updated, Data},
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about record deletion from model to the client
%% via websocket channel.
%% @end
%%--------------------------------------------------------------------
-spec push_deleted(IdOrIds :: binary() | [binary()]) -> ok.
push_deleted(IdOrIds) ->
    Ids = case IdOrIds of
              Bin when is_binary(Bin) ->
                  [Bin];
              List when is_list(List) ->
                  List
          end,
    get(?WEBSCOKET_PROCESS_KEY) ! {push_deleted, Ids},
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function called to initialize async_process and store the websocket
%% process key in its dictionary. Then, its main function is evaluated.
%% @end
%%--------------------------------------------------------------------
-spec async_init(WSPid :: pid(), Fun :: fun()) -> term().
async_init(WSPid, Fun) ->
    put(?WEBSCOKET_PROCESS_KEY, WSPid),
    Fun().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all async processes spawned by the calling process.
%% @end
%%--------------------------------------------------------------------
-spec get_async_processes() -> [pid()].
get_async_processes() ->
    case get(?ASYNC_PROCESSES_KEY) of
        undefined ->
            [];
        List when is_list(List) ->
            List
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add the pid of an async process to the list of processes started by
%% the calling process.
%% @end
%%--------------------------------------------------------------------
-spec append_async_process(Pid :: pid()) -> ok.
append_async_process(Pid) ->
    put(?ASYNC_PROCESSES_KEY, [Pid | get_async_processes()]),
    ok.
