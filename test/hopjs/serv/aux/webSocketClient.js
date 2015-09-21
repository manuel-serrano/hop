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

/* This worker opens a WebSocket, then sends <num> messages, then post
 * a message to inform the main thread of completion */

var hop = require( 'hop' );
var assert = require( 'assert' );

var id;
var num;
var ws;

function loop( num ) {
   if ( num == 0 ) {
      postMessage( { messageType: 'done' } );
   } else {
      // console.log( 'client #%s: call #%s', id, num );
      ws.send( JSON.stringify( { id: id, num: num } ));
   }
}


/* Protocol with workers launcher */
onmessage = function( e ) {
   switch (e.data.messageType) {
   case 'params':
      id = e.data.clientId;
      num = e.data.num;
      try {
	 ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv' );
      }
      catch (e) {
	 console.log( 'error creating socket', e );
	 process.exit( 1 );
      };
      ws.onopen = function() {
	 postMessage( { messageType: 'ready' } );
      };
      ws.onmessage = function( event ) {
	 var data = JSON.parse( event.data );
	 assert.equal( id, data.id );
	 loop( data.num - 1 );
      };
   
      break;
   case 'run':
      loop( num );
   }
};
