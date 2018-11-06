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

-define(SESSION_COOKIE_KEY, <<"SID">>).
% Value of cookie when there is no session
-define(NO_SESSION, <<"undefined">>).

-define(NEW_GUI_SESSION_PLUGIN, new_gui_session_plugin).

% Record carrying information related to GUI session
-record(gui_session, {
    client :: term(),
    last_refresh = 0 :: non_neg_integer(),
    nonce = <<"">> :: binary(),
    previous_nonce = <<"">> :: binary()
}).