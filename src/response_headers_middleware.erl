%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Cowboy middleware to add headers to each response. Two types of headers are
%%% taken into account:
%%%     1) default_response_headers defined as gui env variable
%%%     2) headers returned by the function specified in gui config as
%%%         custom_response_headers, if any.
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
execute(Req, Env = #{custom_response_headers := undefined}) ->
    DefaultHeaders = gui:get_env(default_response_headers, #{}),
    {ok, cowboy_req:set_resp_headers(DefaultHeaders, Req), Env};
execute(Req, Env = #{custom_response_headers := CustomHeadersFun}) ->
    NewReq = cowboy_req:set_resp_headers(CustomHeadersFun(), Req),
    execute(NewReq, Env#{custom_response_headers => undefined}).
