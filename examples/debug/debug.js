/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/debug/debug.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Tue Jul 15 19:39:16 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Illustrates multitier error reporting                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g debug.js                                          */
/*    browser: http://localhost:8080/hop/debug                         */
/*=====================================================================*/

var hop = require( "hop" );

service debug() {
   return <HTML> {
      <HEAD> {
	 require: "./bugs.js"
      },
      ~{ var BUGS = require( "./bugs.js" ) },
      <DIV> {
	 "Examples of error reporting.",
	 <OL> {
	    <LI> {
	       <BUTTON> {
		  onclick: ~{ BUGS.raise( event.type ) },
		  "raise"
	       },
	       " onclick throw"
	    },
	    <LI> {
	       <BUTTON> {
		  onclick: ~{ BUGS.unbound() },
		  "raise"
	       },
	       " onclick -> variable unbound error"
	    },
	    <LI> {
	       <BUTTON> {
		  onclick: ~{ BUGS.interval() },
		  "raise"
	       },
	       " onclick -> interval -> exception"
	    },
	    <LI> {
	       <BUTTON> {
		  id: "direct-button",
		  onclick: ~{ throw new Error( "direct error" ); },
		  "raise"
	       },
	       " direct throw"
	    },
	    <LI> {
	       <BUTTON> {
		  id: "srv-button",
		  onclick: ~{ ${bug}( { y: 4 } ).post() },
		  "raise"
	       },
	       " server side service error",
	    },
	    <LI> {
	       <BUTTON> {
		  id: "srv-button",
		  onclick: ~{ setTimeout( function() {
		     ${bug}( { y: 4 } ).post() }, 0 ) },
		  "raise"
	       },
	       " server side service error within timeout"
	    },
	    <LI> {
	       <BUTTON> {
		  id: "srv-button",
		  onclick: ~{ setTimeout( function() {}, 0 );
			      ${bug}( { y: 4 } ).post() },
		  "raise"
	       },
	       " server side service error after timeout"
	    }
	 }
      }
   }
}

service bug( query ) {
   return query[ "x" ][ "car" ];
}

if( hop.debug < 1 ) {
   console.log( "This weblet needs debug mode." );
   console.log( "Re-run hop with \"-g\" option." );
   process.exit( 1 );
} else {
   console.log( "Go to \"http://%s:%d/%s\"",
		hop.hostname, hop.port, debug().toString() );
}

