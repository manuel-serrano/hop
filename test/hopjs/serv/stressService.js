/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/serv/stressService.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  14 11:43:00 2015                         */
/*    Last change :  Tue Sep 15 16:15:03 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Stress test for services                                         */
/*=====================================================================*/

/* Usage: set <numClients> to specify how many clients will send
 * service requests */

service toTest( { str: undefined, id: undefined, file: undefined } ) {
//   console.log( id );
   return true;
}

var numClients = 8;
//var numClients = 1;
var doneWithClients = 0;
var clients = [];

function prepareClient( id ) {
   if ( id == numClients ) {
      launchTest();
   } else {
      console.log( 'main: init client', id );
      var client = new Worker( './aux/stressClient.js' );
      clients.push( client );
      client.onmessage = function( e ) {
	 doneWithClients++;
	 console.log( 'client %s done', e.data );
	 checkCompletion();
      };
      prepareClient( id + 1 );
   }
}

function launchTest() {
   console.log( 'main: launchTest' );
   clients.forEach( function( client, id ) {
      client.postMessage( id );
   });
   checkCompletion();
}

function checkCompletion() {
   console.log( 'checkCompletion', doneWithClients );
   if ( doneWithClients == numClients ) {
      console.log( 'All tests passed. exiting' );
      process.exit( 0 );
   };
}

prepareClient( 0 );

