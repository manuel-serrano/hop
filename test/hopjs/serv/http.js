/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/serv/http.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  4 18:43:00 2015                          */
/*    Last change :  Thu Oct 13 08:11:10 2016 (serrano)                */
/*    Copyright   :  2015-16 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Testing services, webSockets and Broadcast over http and https   */
/*=====================================================================*/

var assert = require( 'assert' );
var hop = require( 'hop' );
var config = require( hop.config );

var objectEqual = require( './aux/objectEqual.js' );

var payload = [ 1999, 'This is not a string', {key: {subkey: 'subkey', otherKey: 'enough'}, andALastOne: ['d', 'i', 's', 'c', 'o' ]}, [ 3.14, 1837, -765 ],
		[ { foo: 'oof', bar : 'rab' }, { gee: 'eeg' } ]];

var ssl;
var wsScheme;

if (config.HTTPSPort) {
   ssl = true;
   wsScheme = 'wss';
} else {
   ssl = false;
   wsScheme = 'ws';
};


service foo( arg ) {
   return arg ;
}

function goToService() {
   
   function testService( i ) {
      if ( i == payload.length ) {
	 console.log( 'Service test ok' );
	 goToServiceNamedArgs();
      } else {
	 foo( payload[i] ).post(function( result ) {
	    assert.ok( objectEqual( result, payload[i] ) );
	    testService( i + 1 );
	 }, { fail: function( error ) {
	    process.exit( 1 );
	 },
	      ssl: ssl
	    });
      }
   }
   console.log( 'Service test' );
   testService( 0 );
}

function goToServiceNamedArgs() {
   service bar( o ) {
      return o ? o.arg : undefined;
   }
   function testService( i ) {
      if ( i == payload.length ) {
	 console.log( 'Service (named arguments) test ok' );
	 goToWS();
      } else {
	 bar( { arg: payload[i] } ).post(function( result ) {
	    assert.ok( result, payload[ i ] );
	    testService( i + 1 );
	 }, { fail: function( error ) {
	    process.exit( 1 );
	 },
	      ssl: ssl
	    });
      }
   }
   console.log( 'Service test (named arguments)' );
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
   var ws = new WebSocket( wsScheme + '://localhost:' + hop.port + '/hop/server' );
   console.log( 'client: WS created' );
   ws.onopen = function() {
      console.log( 'client: ws url', ws.url );
      console.log( 'client: readyState', ws.readyState );
      console.log( 'client: ws sending', message );
      ws.send( message );
   };
   ws.onmessage = function( event ) {
      console.log( 'client WS received', event.data );
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

/* broadcast: we test that the client receives any of the payload
 * elements. TODO: replace the weak (and false) toString equality test
 * with a comprehensive object comparison */
   
function goToBroadcast(){
   console.log( 'Broadcast test' );
   var i = 0
   server.addEventListener( 'foo', function( event ) {
      assert.ok( event.data, payload[ i ] );
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
      assert.ok( WSFlag );
      console.log( 'All tests passed' );
      process.exit( 0 );
   }, 200 );
}


goToService();

setTimeout( function() {
   console.log( 'TIMEOUT while running tests. Abort' );
   process.exit( 1 );
}, 2000 );


