-author("lopiola").

%% Types of session requirements
-define(SESSION_ANY, any).
-define(SESSION_LOGGED_IN, logged_in).
-define(SESSION_NOT_LOGGED_IN, not_logged_in).

%% Prefix of paths requested by webscoket clients.
-define(WEBSOCKET_PREFIX_PATH, "/ws/").

%% GUI routing plugin - module of such name is expected in
%% the application including gui.
%% @todo this could be configurable from env - do we need so?
-define(GUI_ROUTE_PLUGIN, gui_route_plugin).
%% GUI session plugin - module of such name is expected in
%% the application including gui.
%% @todo this could be configurable from env - do we need so?
-define(GUI_SESSION_PLUGIN, gui_session_plugin).

%% Record used to define GUI routes, their requirements and logic.
-record(gui_route, {
    % Does this resource require being logged in?
    requires_session = ?SESSION_ANY ::
    ?SESSION_ANY | ?SESSION_LOGGED_IN | ?SESSION_NOT_LOGGED_IN,
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