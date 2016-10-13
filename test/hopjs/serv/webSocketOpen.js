/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/serv/webSocketOpen.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Thu Oct 13 08:38:55 2016 (serrano)                */
/*    Copyright   :  2015-16 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Test repetitive open/close of a WebSocket                        */
/*=====================================================================*/

var assert = require( 'assert' );
var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/wsOpenClient.js' );

var NUMCLIENTS = 5; // number of concurrent clients
var NUMCALLS = 30; // number of service invocations per client
var TIMEOUT = 10000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware

//NUMCLIENTS = 2;
//NUMCALLS = 10;

// The WebSocket server
// accepts all connections,
// echo the received message.

var connections = 0;

var serv = new WebSocketServer( {path: 'serv'} );
serv.onconnection = function( event ) {
   connections++;
   // console.log( 'WebSocketServer new connection: %s', connections );
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

// checks that sockets are closed server side.
function onSuccess() {
   setTimeout( function() {
      assert.equal( connections, 0 );
      console.log( 'server: all connections are closed' );
      process.exit( 0 );
   }, 5000 );
}

runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	   onSuccess: onSuccess } );
