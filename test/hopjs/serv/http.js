/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/serv/http.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  4 18:43:00 2015                          */
/*    Last change :  Tue Jun 18 13:48:26 2024 (serrano)                */
/*    Copyright   :  2015-24 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Testing services, webSockets and Broadcast over http and https   */
/*=====================================================================*/
"use hopscript";

setTimeout( function() {
   if( hop.compilerDriver.pending > 0 ) {
      hop.compilerDriver.addEventListener( "all", function( e ) {
         checkCompletion();
      } );
   } else {
      checkCompletion();
   }
}, 1000 );


function checkCompletion() {
   setTimeout( function() {
      console.log( 'TIMEOUT while running tests. Abort' );
      process.exit( 1 );
   }, 2000 );
}

const assert = require( 'assert' );
const hop = require( 'hop' );
const config = require( '@hop/config' );

const objectEqual = require( './aux/objectEqual.js' );

const payload = [ 1999, 'This is not a string', {key: {subkey: 'subkey', otherKey: 'enough'}, andALastOne: ['d', 'i', 's', 'c', 'o' ]}, [ 3.14, 1837, -765 ],
		[ { foo: 'oof', bar : 'rab' }, { gee: 'eeg' } ]];

let ssl;
let wsScheme;

if (config.HTTPSPort) {
   ssl = true;
   wsScheme = 'wss';
} else {
   ssl = false;
   wsScheme = 'ws';
};


service foo( arg ) {
   return arg;
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
		 console.log( error );
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



let WSFlag = false;

/* WebSocket test. We test that a string is passed unchanged from the
 * ws client, to the server, and back to the client */

function goToWS() {
   const message = 'my test string';
   console.log( 'WS test' );
   const server = new WebSocketServer( {path: 'server'} );
   console.log( 'server WS listening' );

   server.onconnection = function( event ) {
      console.log( 'server: accepting new connection' );
      const ws = event.value;
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
   const ws = new WebSocket( wsScheme + '://localhost:' 
			   + (hop.ports.http || hop.ports.https) 
			   + '/hop/server' );
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
   // ws.server = server;
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
   let i = 0
   this.server.addEventListener( 'foo', function( event ) {
      assert.ok( event.data, payload[ i ] );
      i++;
      if ( i < payload.length ) {
	 hop.broadcast( 'foo', payload[ i ] );
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
