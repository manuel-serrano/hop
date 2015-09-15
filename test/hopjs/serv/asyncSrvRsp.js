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

var NUMCLIENTS = 12; // number of concurrent clients
var NUMCALLS = 100; // number of service invocations per client
var DELAY = 100; // set delay for asynchronous response
var TIMEOUT = 20000; //global timeout (test will fail if not completed by then)
// ensure that TIMEOUT >>  NUMCALLS * DELAY

service toTest( clientId, num ) {
//   console.log( id );
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 setTimeout( function () {
	    sendResponse( { clientId: clientId, num: num } );
	 }, DELAY );
      }, this );
}


function runTest() {

   var doneWithClients = 0;
   function checkCompletion() {
      console.log( 'checkCompletion', doneWithClients );
      if ( doneWithClients == NUMCLIENTS ) {
	 console.log( 'All tests passed. exiting' );
	 process.exit( 0 );
      };
   }
   
   var clients = [];
   for ( var i = 0; i  < NUMCLIENTS; i++ ) {
      console.log( 'main: preparing client #%s', i );
      var client = new Worker( './aux/asyncSrvRspClient.js' );
      clients.push( client );
      client.onmessage = function( e ) {
	 doneWithClients++;
	 console.log( 'client %s done', e.data );
	 checkCompletion();
      };
   };
   console.log( 'main: launching test' );
   clients.forEach( function( client, id ) {
      client.postMessage( { clientId: id, num: NUMCALLS } );
   });
   checkCompletion();
}


setTimeout( function() {
   console.log( 'Timeout: %sms, test failed', TIMEOUT );
   process.exit( 1 );
}, TIMEOUT );

runTest();
