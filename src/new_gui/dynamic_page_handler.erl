%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles dynamic page requests.
%%% @end
%%%-------------------------------------------------------------------
-module(dynamic_page_handler).
-author("Lukasz Opiola").

-behaviour(cowboy_handler).

-include_lib("ctool/include/logging.hrl").

-export([init/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy handler callback.
%% Handles a dynamic page request. Checks if requested METHOD is
%% valid for given endpoint, and if so, calls a handler specified in
%% dynamic_pages part of gui config.
%% @end
%%--------------------------------------------------------------------
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(#{method := Method} = Req, {Methods, Handler}) ->
    NewReq = case lists:member(Method, Methods) of
        true ->
            try
                Handler:handle(Method, Req)
            catch Type:Reason ->
                ?error_stacktrace("Error in dynamic page handler - ~p:~p", [
                    Type, Reason
                ]),
                cowboy_req:reply(500, Req)
            end;
        false ->
            <<", ", Allow/binary>> = <<<<", ", M/binary>> || M <- Methods>>,
            cowboy_req:reply(405, #{<<"allow">> => Allow}, Req)
    end,
    {ok, NewReq, no_state}.
