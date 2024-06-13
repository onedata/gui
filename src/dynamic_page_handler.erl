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
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").

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
            catch
                throw:{error, _} = Error ->
                    cowboy_req:reply(
                        errors:to_http_code(Error),
                        #{?HDR_CONNECTION => <<"close">>},
                        json_utils:encode(#{<<"error">> => errors:to_json(Error)}),
                        Req
                    );
                Type:Reason:Stacktrace ->
                    ?error_exception(?autoformat(Handler), Type, Reason, Stacktrace),
                    cowboy_req:reply(
                        ?HTTP_500_INTERNAL_SERVER_ERROR,
                        #{?HDR_CONNECTION => <<"close">>},
                        Req
                    )
            end;
        false ->
            cowboy_req:reply(
                ?HTTP_405_METHOD_NOT_ALLOWED,
                #{?HDR_ALLOW => str_utils:join_binary(Methods, <<", ">>)},
                Req
            )
    end,
    {ok, NewReq, no_state}.
