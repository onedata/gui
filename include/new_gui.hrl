%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Common defines used in gui modules.
%%% @end
%%%-------------------------------------------------------------------
-author("Lukasz Opiola").

% Record holding config of gui listener
-record(gui_config, {
    port = 443 :: integer(),
    key_file :: filename:filename(),
    cert_file :: filename:filename(),
    % Will be omitted if the path does not exist
    chain_file :: undefined | filename:filename(),
    number_of_acceptors = 100 :: integer(),
    max_keepalive = 30 :: integer(),
    request_timeout = timer:seconds(10) :: integer(),
    inactivity_timeout = timer:minutes(5) :: integer(),
    dynamic_pages = [] :: [{Path :: string(), AllowedMethods :: [new_gui:method()], module()}],
    custom_cowboy_routes = [] :: [{Path :: string() | binary(), module(), State :: term()}],
    default_static_root :: filename:filename(),
    static_root_override :: filename:filename()
}).

% Session cookie id
-define(SESSION_COOKIE_KEY, <<"session_id">>).
