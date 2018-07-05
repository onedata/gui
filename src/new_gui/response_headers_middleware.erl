%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Cowboy middleware to add default headers to each response.
%%% @end
%%%-------------------------------------------------------------------
-module(response_headers_middleware).
-author("Lukasz Opiola").

%% API
-export([execute/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% {@link cowboy_middleware} callback execute/2.
%% @end
%%--------------------------------------------------------------------
-spec execute(Req, Env) -> {ok, Req, Env} | {stop, Req} when
    Req :: cowboy_req:req(), Env :: cowboy_middleware:env().
execute(Req, Env) ->
    Headers = new_gui:get_env(default_response_headers, []),
    {ok, cowboy_req:set_resp_headers(maps:from_list(Headers), Req), Env}.
