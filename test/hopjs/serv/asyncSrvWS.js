/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/asyncSrvWS.js    */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Tue Sep  15 12:42:26 2015 (serrano)               */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Test asynchronous responses in services proxying WebSockets      */
/*=====================================================================*/

var hop = require( 'hop' );
var assert = require( 'assert' );
var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/stdClient.js' );

var NUMCLIENTS = 10; // number of concurrent clients
var NUMCALLS = 100; // number of service invocations per client
var TIMEOUT = 10000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware

// The WebSocket server
// accepts all connections,
// echo the received message.

var connections = 0;

var serv = new WebSocketServer( {path: 'serv'} );
serv.onconnection = function( event ) {
   //console.log( 'WebSocketServer accepting a new connection' );
   connections++;
   var ws = event.value;
   ws.onmessage = function( event ) {
   //   console.log( 'WebSocketServer processing message', event.data );
      ws.send( event.data );
   };
   ws.onclose = function() {
      connections--;
   };
};

service toTest( clientId, num ) {
   var result;
   //console.log( 'Service received request', clientId, num );
   return hop.HTTPResponseAsync( function( sendResponse ) {
      var ws = new WebSocket( 'ws://localhost:' + hop.port + '/hop/serv' );
      // comment the line below (console.log) blocks the execution
      //console.log( 'Service created WS', clientId, num, ws );
      ws.onopen = function() {
	 ws.send( JSON.stringify( {clientId: clientId, num: num } ));
	 //	 console.log( 'Service forwarding request', clientId, num );
      };
      ws.onclose = function() {
//	 console.log( 'Service sends result', result );
	 sendResponse( result );
      };
      ws.onmessage = function( event ) {
//	 console.log( 'Service received WS message', event.data );
	 result = JSON.parse( event.data );
	 ws.close();
      };
   }, this );
}

function onSuccess() {
   setTimeout( function() {
      assert.equal( connections, 0 );
      console.log( 'WebSocket Server: all connections closed' );
      process.exit( 0 );
   }, 100 );
}

runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	   onSuccess: onSuccess } );
