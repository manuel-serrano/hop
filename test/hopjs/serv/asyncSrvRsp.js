/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/serv/asyncSrvRsp.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Sat Apr  8 12:36:46 2017 (serrano)                */
/*    Copyright   :  2015-17 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Test asynchronous responses in services                          */
/*=====================================================================*/

var hop = require( 'hop' );
var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/stdClient.js' );

// number of concurrent clients
var NUMCLIENTS = 5;
// number of service invocations per client
var NUMCALLS = 200;
// set delay for asynchronous response
var DELAY = 5;
//global timeout (test will fail if not completed by then)
var TIMEOUT = 10000; 
// change TIMEOUT value to match your hardware
// (~ 500 requests/s on a laptop for synchronous responses)

var requests = 0;

service toTest( clientId, num ) {
   requests++;
   console.log( "toTest req=", requests );
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 setTimeout( function () {
	    var r = { clientId: clientId, num: num };
	    console.log( "sending response r=", r );
	    sendResponse( r );
	 }, DELAY );
      }, this );
}

function onTimeout() {
   console.log( 'timeout (%s ms): server has processed %s requests',
		TIMEOUT, requests );
   process.exit( 1 );
}

function onFailure() {
   console.log( 'failure: server has processed %s requests', requests );
   process.exit( 1 );
}

runTest( { clientModule: clientModule,
	   numClients: NUMCLIENTS,
	   numCalls: NUMCALLS,
	   timeout: TIMEOUT,
	   onTimeout: onTimeout,
	   onFailure: onFailure,
	 } );
