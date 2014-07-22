/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/hss/hss.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Thu Jul  3 17:25:15 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    basic HSS example                                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g hss.js                                            */
/*    browser: http://localhost:8080/hop/hss                           */
/*=====================================================================*/

var hop = require( 'hop' );

service hss() {
   return <HTML> {
      <HEAD> {
	 css: hss.resource( "hss.hss" )
      },
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].map(
	 function( i ) {
	    return <DIV> {
	       class: "div" + i,
	       i
	    }
	 } )
   }
}

console.log( "Go to \"http://%s:%d/hop/hss\"", hop.hostname, hop.port );
