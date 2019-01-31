%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license 
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides functions related to HTTP CORS handling.
%%% (Cross Origin Resource Sharing).
%%% @end
%%%-------------------------------------------------------------------
-module(gui_cors).
-author("Lukasz Opiola").

-export([allow_origin/2, options_response/4]).

-define(JOIN_WITH_COMMAS(Term), str_utils:join_binary(Term, <<", ">>)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds Access-Control-Allow-Origin response header to the cowboy Req.
%% @end
%%--------------------------------------------------------------------
-spec allow_origin(origin:binary(), cowboy_req:req()) -> cowboy_req:req().
allow_origin(Origin, Req) ->
    cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req).


%%--------------------------------------------------------------------
%% @doc
%% Responds to an OPTIONS request. In case it is a CORS pre-flight request,
%% adds proper Access-Control-Allow-* headers.
%% @end
%%--------------------------------------------------------------------
-spec options_response(AllowOrigin :: binary(), AllowMethods :: [binary()],
    AllowHeaders :: [binary()], cowboy_req:req()) -> cowboy_req:req().
options_response(AllowOrigin, AllowMethods, AllowHeaders, Req) ->
    case cowboy_req:header(<<"access-control-request-method">>, Req, undefined) of
        undefined ->
            % Not a CORS pre-flight request - return allowed methods
            cowboy_req:reply(200, #{
                <<"allow">> => ?JOIN_WITH_COMMAS(AllowMethods)
            }, Req);
        _ ->
            cowboy_req:reply(200, #{
                <<"access-control-allow-origin">> => AllowOrigin,
                <<"access-control-allow-methods">> => ?JOIN_WITH_COMMAS([<<"OPTIONS">> | AllowMethods]),
                <<"access-control-allow-headers">> => ?JOIN_WITH_COMMAS(AllowHeaders)
            }, Req)
    end.

