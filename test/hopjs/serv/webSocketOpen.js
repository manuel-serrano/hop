/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/webSocketOpen.js  */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Tue Sep  15 12:42:26 2015 (serrano)               */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Test repetitive open/close of a WebSocket                        */
/*=====================================================================*/

var hop = require( 'hop' );
var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/wsOpenClient.js' );

var NUMCLIENTS = 1; // number of concurrent clients
var NUMCALLS = 1000; // number of service invocations per client
var TIMEOUT = 10000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware

// The WebSocket server
// accepts all connections,
// echo the received message.

var connections = 0;

var serv = new WebSocketServer( {path: 'serv'} );
serv.onconnection = function( event ) {
   connections++;
   console.log( 'WebSocketServer new connection: %s', connections );
   var ws = event.value;
   ws.onmessage = function( event ) {
      console.log( 'WebSocketServer processing message', event.data );
      ws.send( event.data );
   };
   ws.onclose = function() {
      connections--;
      console.log( 'WeSocketServer closing connection: %s', connections );
   }
};

runTest( clientModule, NUMCLIENTS, NUMCALLS, TIMEOUT );
