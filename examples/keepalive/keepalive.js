/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/keepalive/keepalive.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed Jul 23 16:31:57 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Simple example showing how to implement keep-alive HTTP          */
/*    will using asynchronous computations.                            */
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
		     <DIV> {
			"I'm still alive"
		     }
	       } );
	    },
	    3000 );
      },
   hop.currentRequest() );
}

console.log( "Go to \"http://%s:%d/hop/keepalive\" (and wait 3 sec).", hop.hostname, hop.port );
