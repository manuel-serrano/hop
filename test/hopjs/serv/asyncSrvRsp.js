/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/asyncSrvRsp.js    */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Tue Sep  15 12:42:26 2015 (serrano)               */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Test asynchronous responses in services                          */
/*=====================================================================*/

var hop = require( 'hop' );
var runTest = require( './aux/launchWorkers.js' ).runTest;
var clientModule = require.resolve( './aux/stdClient.js' );

var NUMCLIENTS = 5; // number of concurrent clients
var NUMCALLS = 200; // number of service invocations per client
var DELAY = 10; // set delay for asynchronous response
var TIMEOUT = 10000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware
// ( ~ 500 requests/s on a laptop for synchronous responses )

var count = 1;

service toTest( clientId, num ) {
   //   console.log( id );
   console.log( 'server', count );
   count++;
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 setTimeout( function () {
	    sendResponse( { clientId: clientId, num: num } );
	 }, DELAY );
      }, this );
}

runTest( clientModule, NUMCLIENTS, NUMCALLS, TIMEOUT );
