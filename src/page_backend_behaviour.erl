%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour defines a page_backend module which will be called before
%%% a html file is served, if corresponding gui_route specifies so.
%%% @end
%%%-------------------------------------------------------------------
-module(page_backend_behaviour).
-author("Lukasz Opiola").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Called before the page (HTML) is served. Used to decide how to
%% respond to page query (see returned values).
%% @end
%%--------------------------------------------------------------------
-callback page_init() ->
    %% Will serve the HTML file defined in ?GUI_ROUTE_PLUGIN
    serve_html |
    %% Same as above, adding given headers to default ones
    {serve_html, Headers} |
    %% Will serve explicite body
    {serve_body, Body} |
    %% Same as above, with given headers
    {serve_body, Body, Headers} |
    %% Will display the 404 page specified in ?GUI_ROUTE_PLUGIN
    display_404_page |
    %% Will display the 500 page specified in ?GUI_ROUTE_PLUGIN
    display_500_page |
    %% Will reply with given code, body and headers
    {reply, Code, Body, Headers} |
    %% Will send a 307 redirect back to the client,
    %% given URL must be relative to current domain, e.g. /images/image.png
    {redirect_relative, URL} |
    %% Will send a 307 redirect back to the client,
    %% given URL must be full, e.g. https://google.com/images/image.png
    {redirect_absolute, URL} when
    Code :: integer(),
    Body :: binary(),
    Headers :: [{binary(), binary()}],
    URL :: binary().

