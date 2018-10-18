/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/serv/webSocket.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Thu Sep 17 11:43:00 2015                          */
/*    Last change :  Tue Oct 10 12:48:36 2017 (serrano)                */
/*    Copyright   :  2015-17 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Stress test for webSockets                                       */
/*=====================================================================*/

var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/webSocketClient.js' );

var serv = new WebSocketServer( { path: "serv" } );

var messages = 0;
serv.onconnection = function( event ) {
   var ws = event.value;
   ws.onmessage = function( event ) {
      //console.log( 'server:', event.data );
      messages++;
      ws.send( event.data );
   };
};

var NUMCLIENTS = 5; // number of concurrent clients
var NUMCALLS = 1000; // number of ws messages sent per client
var TIMEOUT = 20000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware ( ~ 500 requests/s on a laptop)

function onFailure() {
   console.log( 'server has processed %s messages', messages );
   process.exit( 1 );
}

runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	   onFailure: onFailure,
	 } );
