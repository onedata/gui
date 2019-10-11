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

-include_lib("ctool/include/http/headers.hrl").

-export([allow_origin/2, allow_frame_origin/2, options_response/4]).

-define(JOIN_WITH_COMMAS(Term), str_utils:join_binary(Term, <<", ">>)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds Access-Control-Allow-Origin response header to the cowboy Req.
%% @end
%%--------------------------------------------------------------------
-spec allow_origin(AllowOrigin :: binary(), cowboy_req:req()) -> cowboy_req:req().
allow_origin(AllowOrigin, Req) ->
    cowboy_req:set_resp_header(?HDR_ACCESS_CONTROL_ALLOW_ORIGIN, AllowOrigin, Req).


%%--------------------------------------------------------------------
%% @doc
%% Adds X-Frame-Options response header to the cowboy Req.
%% @end
%%--------------------------------------------------------------------
-spec allow_frame_origin(AllowOrigin :: binary(), cowboy_req:req()) -> cowboy_req:req().
allow_frame_origin(AllowOrigin, Req) ->
    cowboy_req:set_resp_header(?HDR_X_FRAME_OPTIONS, <<"allow-from ", AllowOrigin/binary>>, Req).


%%--------------------------------------------------------------------
%% @doc
%% Responds to an OPTIONS request. In case it is a CORS pre-flight request,
%% adds proper Access-Control-Allow-* headers.
%% @end
%%--------------------------------------------------------------------
-spec options_response(AllowOrigin :: binary(), AllowMethods :: [binary()],
    AllowHeaders :: [binary()], cowboy_req:req()) -> cowboy_req:req().
options_response(AllowOrigin, AllowMethods, AllowHeaders, Req) ->
    case cowboy_req:header(?HDR_ACCESS_CONTROL_REQUEST_METHOD, Req, undefined) of
        undefined ->
            % Not a CORS pre-flight request - return allowed methods
            cowboy_req:reply(200, #{
                ?HDR_ALLOW => ?JOIN_WITH_COMMAS(AllowMethods)
            }, Req);
        _ ->
            cowboy_req:reply(200, #{
                ?HDR_ACCESS_CONTROL_ALLOW_ORIGIN => AllowOrigin,
                ?HDR_ACCESS_CONTROL_ALLOW_METHODS => ?JOIN_WITH_COMMAS([<<"OPTIONS">> | AllowMethods]),
                ?HDR_ACCESS_CONTROL_ALLOW_HEADERS => ?JOIN_WITH_COMMAS(AllowHeaders)
            }, Req)
    end.

