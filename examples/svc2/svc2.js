/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc2/svc2.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Sat Dec 20 10:12:28 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services declarations.            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc2.js                                           */
/*    browser: http://localhost:8080/hop/svc2                          */
/*=====================================================================*/
var hop = require( 'hop' );
require( "./extern.js" );

service svc2() {
   return <HTML> {
      <BUTTON> {
	 onclick: ~{
	    ${dummy}( { b: 22 } )
	       .post( function( r ) {
		  document.body.appendChild(
		     <TABLE> {
			r.map( function( e ) {
			   return <TR> { <TH> { e.head }, <TD> { e.data } }
			} )
		     } );
	       } )
	 },
	 "add"
      }
   }
}

import service dummy( { a: 10, b: 11 } );

console.log( "Go to \"http://%s:%d/hop/svc2\"", hop.hostname, hop.port );
