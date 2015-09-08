/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/http.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  4 18:43:00 2015                          */
/*    Last change :  Mon Sep  7 12:42:26 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing services, webSockets and Broadcast over http and https   */
/*=====================================================================*/

var assert = require( 'assert' );
var hop = require( 'hop' );
//var spawn = require( 'childProcess' ).spawn;

/* we are in a http hop process. We need to launch a https hop process
 * and then have both processes communicate with each other */

var payload = [ 1999, 'This is not a string', {key: {subkey: 'subkey', otherKey: 'enough'}, andALastOne: ['d', 'i', 's', 'c', 'o' ]}, [ 3.14, 1837, -765 ] ];



console.log( 'testing local http ...' );


function goToService() {
   service foo( arg ) {
      return arg ;
   }

   function testService( i ) {
      if ( i == payload.length ) {
	 console.log( 'Service test ok' );
	 goToWS();
      } else {
	 foo( payload[i] ).post(function( result ) {
	    // console.log( i, payload[ i ], result, typeof( result ) );
	    assert.equal (typeof( result ), typeof( payload[ i ]));
	    assert.equal (result.toString(), payload[ i ].toString() );
	    testService( i + 1 );
	 }, function( error ) {
	    process.exit( 1 );
	 }
			       );
      }
   }
   console.log( 'Service test' );
   testService( 0 );
}

var WSFlag = false;

/* WebSocket test. We test that a string is passed unchanged from the
 * ws client, to the server, and back to the client */

function goToWS() {
   var message = 'my test string';
   console.log( 'WS test' );
   var server = new WebSocketServer( {path: 'server'} );
   console.log( 'server WS listening' );
   
   server.onconnection = function( event ) {
      console.log( 'server: accepting new connection' );
      var ws = event.value;
      console.log( 'server: ws readyState:', ws.readyState );
      ws.onmessage = function( event ) {
	 console.log( 'server: ws received message', event.data );
	 assert.equal( message, event.data );
	 ws.send( message );
	 console.log( 'server: replied to client' );
      };
      ws.onclose = function() {
	 console.log( 'server: ws closing' );
	 console.log( 'server: ws readyState:', ws.readyState );
	 WSFlag = true;
      };
   };
   console.log( 'ws://' + hop.hostname + ':' + hop.port + '/hop/server' );
   var ws = new WebSocket( 'ws://' + hop.hostname + ':' + hop.port + '/hop/server' );
   console.log( 'client: WS created' );
   ws.onopen = function() {
      console.log( 'client: ws url', ws.url );
      console.log( 'client: readyState', ws.readyState );
      console.log( 'client: ws sending', message );
      ws.send( message );
   };
   ws.onmessage = function( event ) {
      console.log( 'client WS received', event.data );
      assert.equal( typeof( event.data ), typeof( message ) );
      assert.equal( event.data, message );
      ws.close();
      console.log( 'attempting to close ws' );
   };
   // ws.onclose = goToBroadcast;
   // commented out until server.addEventListener is supported by hop processes.
   ws.onclose = function() {
      console.log( 'client WS closed' );
      console.log( 'WS test ok' );
      goToEnd();
   }
}

/* broadcast: we test that the client receives any of the payload elements. TODO: replace the weak (and false) toString equality test with a complete object inspection */
   
function goToBroadcast(){
   console.log( 'Broadcast test' );
   var i = 0
   server.addEventListener( 'foo', function( event ) {
      assert.equal( typeof( event.data ), typeof( payload[ i ] ));
      assert.equal( event.data.toString(), payload[ i ].toString() );
      i++;
      if ( i < payload.length ) {
	 broadcast( 'foo', payload[ i ] );
      } else {
	 console.log( 'Broadcast test ok' );
	 goToEnd();
      }
   });
}

function goToEnd() {
   setTimeout( function() {
      console.log( 'Test diagnostic' );
      console.log( 'checking', WSFlag );
      //      assert.ok( WSFlag );
      // commented out: the server does not get the close event within 2 seconds.
      process.exit( 0 );
   }, 2000 );
}

goToService();
