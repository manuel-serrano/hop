/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/keepalive/keepalive.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed Dec 17 17:37:35 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    This shows how to combine keep-alive HTTP and async responses    */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g keepalive.js                                      */
/*    browser: http://localhost:8080/hop/keepalive                     */
/*=====================================================================*/
var hop = require( "hop" );

service keepalive() {
   return hop.HTTPResponseAsync(
      function( reply ) {
	 setTimeout(
	    function() {
	       reply( <HTML> {
		  <HEAD> {
		     css: keepalive.resource( "keepalive.hss" )
		  },
		  <DIV> { "I'm still alive" }
	       } );
	    },
	    4000 );
      },
   this );
}

console.log( "Go to \"http://%s:%d/hop/keepalive\" (and wait 4 sec).", hop.hostname, hop.port );
