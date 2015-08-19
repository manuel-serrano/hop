/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/keepalive/keepalive.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed Aug 19 13:59:06 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    This shows how to combine keep-alive HTTP and async responses    */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g keepalive.js                                      */
/*    browser: http://localhost:8080/hop/keepalive                     */
/*=====================================================================*/
var hop = require( "hop" );

service keepalive() {
   return new Promise(
      function( resolve, reject ) {
	 setTimeout(
	    function() {
	       resolve( <html>
		 <head css=${keepalive.resource( "keepalive.hss" )}/>
		 <div>I am still alive</div>
	       </html> ) },
	    4000 );
      }
   )
}

console.log( "Go to \"http://%s:%d/hop/keepalive\" (and wait 4 sec).", hop.hostname, hop.port );
