%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This behaviour describes a module that will serve as callback handler.
%%% Callbacks are in practive calls to server from client side (ember).
%%% They can be performed using websocket adapter's (ws_adapter.js)
%%% callback function. The first argument is the identifier of callback
%%% resource, which will be used in gui_route_plugin:callback_backend/1 to
%%% resolve handling module. The second argument is the name of the function.
%%% The third argument is data (function args in json).
%%% Calling ws_adapter.callback('resource', 'function', '{"id":4,"text":"msg"}')
%%% will result in call to gui_route_plugin:callback_backend('resource'),
%%% wchich should return Module, and then
%%% Module:callback(<<"function">>, <<"{\"id\":4,\"text\":\"msg\"}">>)
%%% will be called to handle the callback.
%%% @end
%%%-------------------------------------------------------------------
-module(rpc_backend_behaviour).
-author("Lukasz Opiola").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Called to handle a callback from client side.
%% RequestData is JSON received from client and decoded.
%% ResponseData will be encoded into JSON and sent back to the client.
%% @end
%%--------------------------------------------------------------------
-callback callback(FunctionID :: binary(), RequestData :: term()) ->
    {ok, ResponseData :: term()} |
    {error, ResponseProplist :: term()}.
