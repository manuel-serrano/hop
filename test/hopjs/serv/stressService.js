/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/serv/stressService.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  14 11:43:00 2015                         */
/*    Last change :  Wed Dec 22 13:52:59 2021 (serrano)                */
/*    Copyright   :  2015-21 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Stress test for services                                         */
/*=====================================================================*/
"use hopscript";

/* Usage: set <numClients> to specify how many clients will send
 * service requests */

service toTest( o ) {
   console.log("toTest o.id=", o.id);
   return true;
}

let numClients = 8;
//let numClients = 1;
let doneWithClients = 0;
let clients = [];

function prepareClient( id ) {
   if ( id == numClients ) {
      launchTest();
   } else {
      console.log( 'main: init client', id );
      const client = new Worker( './aux/stressClient.js' );
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

