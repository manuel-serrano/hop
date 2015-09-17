/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/serv/webSocket.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Thu Sep 17 11:43:00 2015                          */
/*    Last change :  Tue Sep 15 16:15:03 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Stress test for webSockets                                       */
/*=====================================================================*/

var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/webSocketClient.js' );

var serv = new WebSocketServer( { path: "serv" } );

serv.onconnection = function( event ) {
   var ws = event.value;
   ws.onmessage = function( event ) {
      //      console.log( 'server:', event.data );
      ws.send( event.data );
   };
};

var NUMCLIENTS = 5; // number of concurrent clients
var NUMCALLS = 200; // number of ws messages sent per client
var TIMEOUT = 10000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware ( ~ 500 requests/s on a laptop)


runTest( clientModule, NUMCLIENTS, NUMCALLS, TIMEOUT );
