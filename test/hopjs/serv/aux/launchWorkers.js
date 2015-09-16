/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/aux/launchWorkers.js*/
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Tue Sep  15 12:42:26 2015 (serrano)               */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    A generic launcher for stress test workers                       */
/*=====================================================================*/


function runTest( numClients, numCalls ) {

   var doneWithClients = 0;
   function checkCompletion() {
      console.log( 'checkCompletion', doneWithClients );
      if ( doneWithClients == numClients ) {
	 console.log( 'All tests passed. exiting' );
	 process.exit( 0 );
      };
   }
   
   var clients = [];
   for ( var i = 0; i  < numClients; i++ ) {
      console.log( 'main: preparing client #%s', i );
      var client = new Worker( './stdClient.js' );
      clients.push( client );
      client.onmessage = function( e ) {
	 doneWithClients++;
	 console.log( 'client %s done', e.data );
	 checkCompletion();
      };
   };
   console.log( 'main: launching test' );
   clients.forEach( function( client, id ) {
      client.postMessage( { clientId: id, num: numCalls } );
   });
   checkCompletion();
}


exports.runTest = runTest;
