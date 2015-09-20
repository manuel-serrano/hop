/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/webSocket.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Thu Sep 17 11:43:00 2015                          */
/*    Last change :  Fri Sep 18 16:11:43 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Stress test for webSockets                                       */
/*=====================================================================*/

var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/webSocketClient.js' );

var serv = new WebSocketServer( { path: "serv" } );

serv.onconnection = function( event ) {
/*    console.error( ">>> onconnection");                              */
   var ws = event.value;
   ws.onmessage = function( event ) {
      //console.log( 'server:', event.data );
      ws.send( event.data );
   };
/*    console.error( "<<< onconnection");                              */
};

var NUMCLIENTS = 5; // number of concurrent clients
var NUMCALLS = 200; // number of ws messages sent per client
var TIMEOUT = 6000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware ( ~ 500 requests/s on a laptop)

NUMCLIENTS = 5;
NUMCALLS = 200;

runTest( clientModule, NUMCLIENTS, NUMCALLS, TIMEOUT );
