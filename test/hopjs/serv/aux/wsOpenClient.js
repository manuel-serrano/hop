/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/serv/aux/wsOpenClient.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Thu Sep  17 11:43:00 2015                         */
/*    Last change :  Fri Dec 11 20:25:16 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    simple worker to open/close a WebSocket                          */
/*=====================================================================*/

/* This worker iterates create/send/receive/close on WebSockets for
 * <num> times, then post a message to inform the main thread of
 * completion */

var hop = require( 'hop' );
var assert = require( 'assert' );

var id;
var num;

function test( id, num ) {
   var ws;
   
   function loop( num ) {
      if ( num == 0 ) {
	 // console.error( "<<<" + id + " done" );
	 postMessage( { messageType: 'done' } );
      } else {
	 // console.log( 'client #%s: call #%s', id, num );
	 ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv' );
	 ws.onopen = function() {
	    // console.error( "onopen " + id + " num=" + num );
	    ws.send( JSON.stringify( { id: id, num: num } ));
	 };
	 ws.onmessage = function( event ) {
	    var data = JSON.parse( event.data );
	    // console.error( "onmsg " + id + " num=" + num, " data.id=", data.id );
	    assert.equal( id, data.id );
	    ws.close();
	 };
	 ws.onclose = function() {
	    // console.error( "onclose " + id + " num=" + num );
	    loop( num - 1 );
	 };
	 ws.onerror = function( e ) {
	    console.error( "*** ERROR: ", e.data );
	 }
      };
   }
   loop( num );
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
