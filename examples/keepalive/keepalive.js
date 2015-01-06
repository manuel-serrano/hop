/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/keepalive/keepalive.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Mon Jan  5 17:36:16 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    This shows how to combine keep-alive HTTP and async responses    */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g keepalive.js                                      */
/*    browser: http://localhost:8080/hop/keepalive                     */
/*=====================================================================*/
var hop = require( "hop" );

service keepalive() {
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 setTimeout(
	    function() {
	       sendResponse( <HTML> {
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
