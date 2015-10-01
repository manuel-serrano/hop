/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/webSocketErrors.js */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Oct 02 00:43:00 2015                          */
/*    Last change :  Fri Sep 18 16:11:43 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Negative test for webSockets                                     */
/*=====================================================================*/

var hop = require( 'hop' );

var serv = new WebSocketServer( { path: "serv" } );

serv.onconnection = function( event ) {
   var ws = event.value;
   console.log( 'server: new connection' );
   ws.onmessage = function( event ) {
      console.log( 'server received %s command', event.data );
      switch (event.data) {
      case 'testClose': ws.onclose = function() {
	 console.log( 'server: received close event' );
	 pass();
      };
	 break;
      case 'sendClose':
	 console.log( 'server: closing socket' );
	 ws.close();
	 break;
      case 'receiveClose':
	 console.log( 'server setting receiveClose commands' );
	 ws.onclose = function() {
	    console.log( 'server: received close event' );
	    pass();
	 };
	 ws.onerror = function() {
	    console.log( 'server: received error message' );
	    fail();
	 };
	 ws.send( 'sendClose' );
	 break;
      default: console.log( 'server received ping');
	 ws.send( 'pong' );
      }
   };
};


var passed = 0
var nextTest = 0;

function next() {
   var testFunction = testSuite[ nextTest ];
   console.log( 'running test', nextTest );
   nextTest ++;
   testFunction();
}

function pass() {
   passed++;
   if ( passed == testSuite.length ) {
      console.log( 'All tests passed' );
      process.exit( 0 );
   } else {
      next();
   };
}

function fail() {
   console.log( 'Test failed' );
   process.exit( 1 );
}

function ignore() {
   console.log( 'Test ignored' );
   pass();
}

var testSuite = [
   function() {
      console.log( 'tries to open a ws on a wrong URL, should raise an exception' );
      return ignore();; // IGNORE TEST
      var ws;
      try {
	 ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/wrongURL' );
	 console.log( 'Exception not raised' );
	 fail();
      }
      catch (e) {
	 console.log( 'Exception raised. ok' );
	 pass()
      };
   },
   function() {
      console.log( 'server initiated close, test that close event is received on the server side' );
      var ws;
      ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv' );
      console.log( 'client, ws created' );
      ws.onopen = function() {
	 console.log( 'client sending testClose request' );
	 ws.send( 'testClose' );
	 console.log( 'client sending sendClose request' );
	 ws.send( 'sendClose' );
      };
   },
   function() {
      console.log( 'server initiated close, test that close event is received on the client side' );
      var ws;
      ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv' );
      console.log( 'client, ws created' );
      ws.onclose = function() {
	 console.log( 'client received close' );
	 pass();
      };
      ws.onopen = function() {
	 console.log( 'client sending sendClose request' );
	 ws.send( 'sendClose' );
      };
      ws.onerror = function() {
	 console.log( 'client received error event' );
	 fail();
      };
   },
   function() {
      console.log( 'client initiated close, test that close event is received on the client side' );
      var ws;
      ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv' );
      console.log( 'client, ws created' );
      ws.onclose = function() {
	 console.log( 'client received close, ok' );
	 pass();
      };
      ws.onopen = function() {
	 console.log( 'client closing websocket' );
	 ws.close();
      };
      ws.onerror = function() {
	 console.log( 'client received error event' );
	 fail();
      };
   },
   function() {
      console.log( 'client initiated close, test that close event is received on the server side' );
      var ws;
      return ignore();; // IGNORE TEST
      ws = new WebSocket( 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv' );
      console.log( 'client, ws created' );
      ws.onopen = function() {
	 console.log( 'client sending close request' );
	 ws.send( 'receiveClose' );
      };
      ws.onerror = function() {
	 console.log( 'client received error event' );
	 fail();
      };
      ws.onmessage = function() {
	 console.log( 'client received server ack' );
	 ws.close();
      };
   },
      
];

next();
