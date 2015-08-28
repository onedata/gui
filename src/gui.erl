%%%-------------------------------------------------------------------
%%% @author lopiola
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Aug 2015 15:16
%%%-------------------------------------------------------------------
-module(gui).
-author("lopiola").

-include("gui.hrl").

%% API
-export([init/0, cleanup/0]).


init() ->
    ?GUI_SESSION_PLUGIN:init().


cleanup() ->
    ?GUI_SESSION_PLUGIN:cleanup().