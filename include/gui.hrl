-author("lopiola").

-define(SESSION_ANY, any).
-define(SESSION_LOGGED_IN, logged_in).
-define(SESSION_NOT_LOGGED_IN, not_logged_in).

% Prefix of paths requested by webscoket clients.
-define(WEBSOCKET_PREFIX_PATH, "/ws/").


% Session cookie id
-define(SESSION_COOKIE_KEY, <<"session_id">>).
% Value of cookie when there is no session
-define(NO_SESSION_COOKIE, <<"no_session">>).


% GUI routing plugin
-define(GUI_ROUTE_PLUGIN, gui_route_plugin).
-define(GUI_SESSION_PLUGIN, gui_session_plugin).

-record(gui_route, {
    % Does this resource require being logged in?
    requires_session = ?SESSION_ANY ::
    ?SESSION_ANY | ?SESSION_LOGGED_IN, ?SESSION_NOT_LOGGED_IN,
    % HTML file connected to this resource
    % (just the name, not file path).
    % `undefined` value can be used if the page does not have a html file.
    html_file = undefined :: binary() | undefined,
    % Erlang handler module for the page server logic (if exists),
    % implementing page_backend_behaviour.
    % `undefined` value can be used if the page does not have a backend logic,
    % then simply HTML file will be served.
    page_backend = undefined :: module()
}).