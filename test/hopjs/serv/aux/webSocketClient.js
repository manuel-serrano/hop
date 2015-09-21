/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/serv/aux/webSocketClient.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Thu Sep  17 11:43:00 2015                         */
/*    Last change :  Sun Sep 20 07:23:38 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    simple worker to stress test a WebSocket server                  */
/*=====================================================================*/

/* This worker iterates service invocations for <num> times, then
 * post a message to inform the main thread of completion */

var hop = require( 'hop' );
var assert = require( 'assert' );

function test( id, num ) {
   var ws;
   
   try {
      // console.log( 'client #%s: WebSocket open', id );
      ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv' );
   }
   catch( e ) {
      console.log( 'client #%s: failed to open WebSocket', id );
      process.exit( 1 );
   };
   
   function loop( num ) {
      if ( num == 0 ) {
	 postMessage( id );
      } else {
	 // console.log( 'client #%s: call #%s', id, num );
	 ws.send( JSON.stringify( { id: id, num: num } ));
      }
   }

   ws.onmessage = function( event ) {
      var data = JSON.parse( event.data );
      assert.equal( id, data.id );
      loop( data.num - 1 );
   };
   
   ws.onopen = function() {
      loop( num );
   };
}

/* Protocol with workers launcher */
onmessage = function( e ) {
   var id = e.data.clientId;
   var num = e.data.num;
   //console.log( 'client start', id, num );
   test( id, num );
};
