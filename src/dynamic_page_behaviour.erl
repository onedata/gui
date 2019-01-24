%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour defines the API of dynamic_page module which will be called
%%% upon request received on path specified in dynamic_pages part of gui config.
%%% @end
%%%-------------------------------------------------------------------
-module(dynamic_page_behaviour).
-author("Lukasz Opiola").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Called upon received request, should handle it and respond returning a new
%% cowboy_req record.
%% @end
%%--------------------------------------------------------------------
-callback handle(gui:method(), cowboy_req:req()) -> cowboy_req:req().
