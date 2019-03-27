%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Cowboy middleware to add headers to each response. Headers to add are taken
%%% from two sources:
%%%     1) default_response_headers env variable in gui app
%%%     2) headers returned by the custom_response_headers function
%%%        (if specified in the #gui_config{} record)
%%% Custom headers override default headers.
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
execute(Req, Env = #{custom_response_headers := CustomHeadersFun}) ->
    CustomHeaders = case CustomHeadersFun of
        undefined -> #{};
        Fun when is_function(Fun, 0) -> CustomHeadersFun()
    end,
    DefaultHeaders = gui:get_env(default_response_headers, #{}),
    AllRespHeaders = maps:merge(DefaultHeaders, CustomHeaders),
    {ok, cowboy_req:set_resp_headers(AllRespHeaders, Req), Env}.
