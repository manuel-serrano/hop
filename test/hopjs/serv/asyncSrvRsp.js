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

var NUMCLIENTS = 10; // number of concurrent clients
var NUMCALLS = 200; // number of service invocations per client
var DELAY = 10; // set delay for asynchronous response
var TIMEOUT = 20000; //global timeout (test will fail if not completed by then)
// change TIMEOUT value to match your hardware
// ( ~ 500 requests/s on a laptop for synchronous responses )

service toTest( clientId, num ) {
//   console.log( id );
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 setTimeout( function () {
	    sendResponse( { clientId: clientId, num: num } );
	 }, DELAY );
      }, this );
}


setTimeout( function() {
   console.log( 'Timeout: %sms, test failed', TIMEOUT );
   consol.log( 'Change TIMEOUT value in source file' );
   process.exit( 1 );
}, TIMEOUT );

runTest( NUMCLIENTS, NUMCALLS);
