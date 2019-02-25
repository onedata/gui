%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Records and macros concerning gui session.
%%% @end
%%%-------------------------------------------------------------------
-author("Lukasz Opiola").

-ifndef(GUI_SESSION_HRL).
-define(GUI_SESSION_HRL, 1).

-define(SESSION_COOKIE_KEY, ?GUI_SESSION_PLUGIN:session_cookie_key()).
% Value of cookie when there is no session
-define(NO_SESSION, <<"undefined">>).

-define(GUI_SESSION_PLUGIN, gui_session_plugin).

% Record carrying information related to GUI session
-record(gui_session, {
    client :: term(),
    last_refresh = 0 :: non_neg_integer(),
    nonce = <<"">> :: binary(),
    previous_nonce = <<"">> :: binary()
}).

-endif.