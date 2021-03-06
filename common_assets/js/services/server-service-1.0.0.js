// jshint esnext: true

/**
 * Provides communication with Server.
 * @module services/server
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// This file should be linked to app/services/server.js

import Ember from 'ember';

export default Ember.Service.extend({
  store: Ember.inject.service('store'),
  adapter: function () {
    return this.get('store').adapterFor('application');
  }.property(),

  /**
   * Forces the WebSocket adapter to initialize a WebSocket connection.
   * Can register onOpen, onError, onClose callbacks that will be called after
   * the connection is established, refused or closed.
   *
   * onOpen, onError and onClose arguments are event handlers for WebSocket events
   *
   * See WebSocket events on: https://developer.mozilla.org/en-US/docs/Web/Events
   */
  initWebSocket: function (onOpen, onError, onClose) {
    this.get('adapter').initWebSocket(onOpen, onError, onClose);
  },

  clearWebsocket() {
    this.get('adapter').clearWebsocket();
  },

  closeWebsocket() {
    this.get('adapter').closeWebsocket();
  },

  /**
   * Sends a RPC call via WebSocket asking for session data, i.e. if the session
   * is valid and session details such as user name.
   * Returns a promise that will be called with received data.
   */
  sessionRPC: function () {
    return this.get('adapter').RPC('session');
  },

  /**
   * Sends an RPC call to the server for a publicly available resource.
   * Returns a promise that will be called with received data.
   */
  publicRPC: function (operation, data) {
    return this.get('adapter').RPC('public', operation, data);
  },

  /**
   * Sends an RPC call to the server for a resource that is restricted to
   * logged in clients.
   * Returns a promise that will be called with received data.
   */
  privateRPC: function (operation, data) {
    return this.get('adapter').RPC('private', operation, data);
  }
});
