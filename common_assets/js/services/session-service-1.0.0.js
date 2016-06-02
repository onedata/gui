// jshint esnext: true

/**
 * Provides a session abstraction using ember-simple-auth. The session validity
 * is resolved via WebSocket.
 * @module services/server
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// This file should be linked to app/services/session.js

import Ember from 'ember';
import SessionService from 'ember-simple-auth/services/session';

export default SessionService.extend({
  server: Ember.inject.service('server'),

  sessionInitResolve: null,
  sessionInitReject: null,
  sessionRestoreResolve: null,
  sessionRestoreReject: null,

  // This flag indicates if the client has active session. Null when
  // the session validity hasn't been resolved yet.
  sessionValid: null,
  // Generic session details (object) returned from the server, containing
  // user name etc.
  sessionDetails: null,

  /** Returns a promise that will be resolved when the client has resolved
   * its session using WebSocket.
   * NOTE: This requires server service and WebSocket adapter.
   * If this is called, session data from WebSocket will resolve session
   * restoration rather than run authenticate. */
  initSession: function () {
    let session = this;
    let onOpen = () => {
      // Ask the server for session details when the WebSocket connection
      // is established
      session.resolveSession();
    };
    let onError = () => {
      // Reject session restoration if WebSocket connection
      // could not be established
      let initRejectFunction = this.get('sessionInitReject');
      if (initRejectFunction) {
        console.debug("SESSION INIT REJECTED");
        initRejectFunction();
      }
      let restoreRejectFunction = this.get('sessionRestoreReject');
      if (restoreRejectFunction) {
        console.debug("SESSION RESTORE REJECTED");
        restoreRejectFunction();
      }
      this.set('sessionInitResolve', null);
      this.set('sessionInitReject', null);
      this.set('sessionRestoreResolve', null);
      this.set('sessionRestoreReject', null);
    };
    this.get('server').initWebSocket(onOpen, onError);
    return new Ember.RSVP.Promise((resolve, reject) => {
      // This promise will be resolved when WS connection is established
      // and session details are sent via WS.
      this.set('sessionInitResolve', resolve);
      this.set('sessionInitReject', reject);
    });
  },

  /** If this is called, session data from WebSocket will resolve session
   * restoration rather than run authenticate. */
  tryToRestoreSession: function () {
    return new Ember.RSVP.Promise((resolve, reject) => {
      console.debug('tryToRestoreSession, sessionValid = ', this.get('sessionValid'));
      if (this.get('sessionValid') === true) {
        resolve();
      } else {
        // This promise will be resolved when WS connection is established
        // and session details are sent via WS.
        this.set('sessionRestoreResolve', resolve);
        this.set('sessionRestoreReject', reject);
      }
    });
  },

  /** Performs an RPC call and registers a promise that will resolve
   * client session when WebSocket is established and it has send session
   * details. */
  resolveSession: function () {
    console.debug('session.resolveSession');
    // Request session data
    this.get('server').sessionRPC().then((data) => {
      console.debug("RESOLVE SESSION REQ");
      console.debug('data: ' + JSON.stringify(data));
      if (data.sessionValid === true) {
        this.set('sessionDetails', data.sessionDetails);
        let sessionRestoreResolveFun = this.get('sessionRestoreResolve');
        if (sessionRestoreResolveFun) {
          console.debug("SESSION VALID, RESTORED");
          sessionRestoreResolveFun();
        } else {
          console.debug("SESSION VALID, AUTHENTICATED");
          this.get('session').authenticate('authenticator:basic');
        }
      } else {
        console.debug("SESSION INVALID");
        let sessionRestoreRejectFun = this.get('sessionRestoreReject');
        if (sessionRestoreRejectFun) {
          console.debug("RESTORE REJECTED");
          sessionRestoreRejectFun();
        }
      }
      let resolveFunction = this.get('sessionInitResolve');
      resolveFunction();
      this.set('sessionInitResolve', null);
      this.set('sessionInitReject', null);
      this.set('sessionRestoreResolve', null);
      this.set('sessionRestoreReject', null);
    });
  }
});
