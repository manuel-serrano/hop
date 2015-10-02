/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/serv/webSocketErrors.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Oct 02 00:43:00 2015                          */
/*    Last change :  Fri Oct  2 14:36:46 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Negative test for webSockets                                     */
/*=====================================================================*/

var hop = require( 'hop' );

var serv = new WebSocketServer( { path: "serv" } );

var wrongURL = 'ws://' + hop.hostname + ':' + hop.port + '/hop/wrongURL';
var wrongURL2 = 'ws://' + hop.hostname + ':' + (hop.port + 10) + '/hop/wrongURL';
var wrongURL3 = 'ws://' + hop.hostname + "_doesnotexist" + ':' + hop.port + '/hop/wrongURL';
var URL = 'ws://' + hop.hostname + ':'+ hop.port + '/hop/serv';

serv.onconnection = function( event ) {
   var ws = event.value;
   console.log( 'server: new connection' );
   ws.onmessage = function( event ) {
      console.log( 'server received %s command', event.data );
      switch (event.data) {
	 
      case 'exitOnClose':
	 this.onclose = function() {
	    console.log( 'server received close event' );
	    console.log( 'state( server side):', this.readyState );
	    this.onclose = function() {
	       console.log( 'WARNING: duplicate close event');
	    };
	    setTimeout( pass, 200 );
	 };
	 this.send( 'ready' );
	 break;
	 
      case 'sendClose':
	    console.log( 'server: closing socket' );
	    this.send( "foobar" );
	 this.close();
	 console.log( 'state( server side):', this.readyState );
	 break;
      default: console.log( 'server received ping');
	 this.send( 'pong' );
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

function mkTest( wrongURL ) {
   return function() {
      console.log( 'TEST.0: tries to open a ws on a wrong URL, should raise an exception or return an error and/or close event.' );
      // return ignore();; // IGNORE TEST
      var ws;
      var timer = setTimeout( function () {
	 console.log( 'client state: %s. Timeout', ws.readyState );
	 fail();
      }, 2000 );
      try {
	 ws = new WebSocket( wrongURL );
	 console.log( 'client state:', ws.readyState );
	 ws.onerror = function() {
	    console.log( 'client received error' );
	    clearTimeout( timer );
	    pass();
	 };
	 ws.onclose = function() {
	    console.log( 'client received close' );
	    clearTimeout( timer );
	    pass();
	 };
      }
      catch (e) {
	 console.log( 'Exception raised. ok' );
	 clearTimeout( timer );
	 pass();
      };
      console.log( 'client listeners are listening ...' );
   }
}

var testSuite = [
   mkTest( wrongURL ),
   mkTest( wrongURL2 ),
   mkTest( wrongURL3 ),
   
   function() {
      console.log( 'TEST.1: server initiated close, test that close event is received on the server side' );
      var ws;
      ws = new WebSocket( URL );
      console.error( "URL=", URL );
      console.log( 'client state:', ws.readyState );
      ws.onopen = function() {
	 console.log( 'client state:', ws.readyState );
	 console.log( 'client sending exitOnClose request' );
	 ws.send( 'exitOnClose' );
	 console.log( 'client sending sendClose request' );
	 ws.send( 'sendClose' );
      };
      console.log( 'client listeners are listening ...' );
   },
   
   function() {
      console.log( 'TEST.2: server initiated close, test that close event is received on the client side' );
      var ws;
      ws = new WebSocket( URL );
      console.log( 'client state:', ws.readyState );
      ws.onclose = function() {
	 console.log( 'client state:', this.readyState );
	 console.log( 'client received close' );
	 pass();
      };
      ws.onopen = function() {
	 console.log( 'client state:', this.readyState );
	 console.log( 'client sending sendClose request' );
	 this.send( 'sendClose' );
	 setTimeout( function() {
	    console.log( 'client sending a ping message' );
	    this.send( 'ping' ); //send a message to detect that the socket is down
	 }.bind( this ), 100 );
      };
      ws.onerror = function() {
	 console.log( 'client received error event' );
	 fail();
      };
   },

   function() {
      console.log( 'TEST.3: client initiated close, test that close event is received on the client side' );
      var ws;
      ws = new WebSocket( URL );
      console.log( 'client state:', ws.readyState );
      ws.onclose = function() {
	 console.log( 'client state:', this.readyState );
	 console.log( 'client received close, ok' );
	 this.onclose = function() {
	    console.log( 'WARNING: client: duplicate close event' );
	 };
	 setTimeout( function() {
	    pass();
	 }, 200 );
      };
      ws.onopen = function() {
	 console.log( 'client state:', ws.readyState );
	 setTimeout( function() {
	    console.log( 'client closing websocket' );
	    this.close();
	    console.log( 'client state:', this.readyState );
	 }.bind( this ), 500 );
      };
      ws.onerror = function() {
	 console.log( 'client received error event' );
	 fail();
      };
   },

   function() {
      console.log( 'TEST.4: client initiated close, test that close event is received on the server side' );
      var ws;
      ws = new WebSocket( URL );
      console.log( 'client state:', ws.readyState );
      ws.onopen = function() {
	 console.log( 'client state:', this.readyState );
	 console.log( 'client sending exitOnClose request' );
	 this.send( 'exitOnClose' );
      };
      ws.onerror = function() {
	 console.log( 'client received error event' );
	 fail();
      };
      ws.onmessage = function() {
	 console.log( 'client closing WebSocket' );
	 this.close();
      };
   },
      
];

next();
