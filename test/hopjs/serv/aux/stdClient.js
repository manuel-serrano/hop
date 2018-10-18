/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/serv/aux/stdClient.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Tue Sep  15 11:43:00 2015                         */
/*    Last change :  Wed May 17 10:02:29 2017 (serrano)                */
/*    Copyright   :  2015-17 Inria                                     */
/*    -------------------------------------------------------------    */
/*    simple worker to stress test services                            */
/*=====================================================================*/

// This worker iterates <num> service invocations, then
// post a message to inform the main thread of completion

service toTest();

function test( id, num ) {
   if ( num == 0 ) {
      postMessage( { messageType: 'done' });
   } else {
      console.log( 'client #%s: call #%s url=', id, num, toTest( id, num ) );
/*       try {                                                         */
	 toTest( id, num ).post( function( result ) {
	    test( id, num - 1 );
	 }  );
/* 				 , {                                   */
/* 	    fail: function( error ) {                                  */
/* 	       console.log( "error=", error );                         */
/* #:tprint( error );                                                  */
/* 	       console.log( 'Service invocation failed for client #%s, #%s', */
/* 			    id, num );                                 */
/* 	       postMessage( { messageType: 'failure' } );              */
/* 	    }});                                                       */
/*       }                                                             */
/*       catch( e ) {                                                  */
/* 	 console.log( 'client %s cannot post at %s', id, num );        */
/* 	 postMessage( { messageType: 'failure' } );                    */
/*       }                                                             */
   }
}


/* Protocol with workers launcher */
onmessage = function( e ) {
   switch (e.data.messageType) {
      case 'params':
	 id = e.data.clientId;
	 num = e.data.num;
	 postMessage( { messageType: 'ready' } );
	 break;
      case 'run':
	 test( id, num );
   }
};
