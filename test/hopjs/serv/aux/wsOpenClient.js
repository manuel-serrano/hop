/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/aux/wsOpenClient.js*/
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Thu Sep  17 11:43:00 2015                         */
/*    Last change :  Tue Sep  15 12:42:26 2015 (serrano)               */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    simple worker to open/close a WebSocket                          */
/*=====================================================================*/

/* This worker iterates create/send/receive/close on WebSockets for
 * <num> times, then post a message to inform the main thread of
 * completion */

var hop = require( 'hop' );
var assert = require( 'assert' );

function test( id, num ) {
   var ws;
   
   function loop( num ) {
      if ( num == 0 ) {
	 postMessage( id );
      } else {
	 console.log( 'client #%s: call #%s', id, num );
	 ws = new WebSocket( 'ws://localhost:'+ hop.port + '/hop/serv' );
	 ws.onopen = function() {
	    ws.send( JSON.stringify( { id: id, num: num } ));
	 };
	 ws.onmessage = function( event ) {
	    var data = JSON.parse( event.data );
	    assert.equal( id, data.id );
	    ws.close();
	 };
	 ws.onclose = function() {
	    loop( num - 1 );
	 };
      };
   }
   loop( num );
}

/* Protocol with workers launcher */
onmessage = function( e ) {
   var id = e.data.clientId;
   var num = e.data.num;
   console.log( 'client start', id, num );
   test( id, num );
};

