/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/hss/hss.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Thu Aug 13 13:18:26 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    basic HSS example                                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g hss.js                                            */
/*    browser: http://localhost:8080/hop/hss                           */
/*=====================================================================*/
var hop = require( 'hop' );

service hss() {
   return <html>
     <head css=${hss.resource( "hss.hss" )}/>
     ${[0, 1, 2, 3, 4, 5, 6, 7, 8, 9].map(
	 function( i ) {
	    return <div class=${"div" + i}>${i}</div>;
	 } )}
   </html>;
}

console.log( "Go to \"http://%s:%d/hop/hss\"", hop.hostname, hop.port );
