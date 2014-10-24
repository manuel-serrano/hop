/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc3/svc3.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Fri Oct 24 13:34:23 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services declarations.            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc3.js                                           */
/*    browser: http://localhost:8080/hop/svc3                          */
/*=====================================================================*/

var hop = require( 'hop' );

service svc3() {
   return <HTML> {
      <BUTTON> {
	 onclick: ~{
	    ${foo}( 1 )
	       .post( function( r ) {
		  document.body.appendChild( r );
	       } )
	 },
	 "click"
      }
   }
}

service foo( x ) {
   console.log( "in foo x=", x );
   return hop.HTTPResponseAsync(
      function( reply )  {
	 bar( x + 1 ).post( function( e ) {
	    reply( e );
	 } )
      } );
}

service bar( x ) {
   console.log( "in bar x=", x );
   return <DIV> { x + 1 };
}

console.log( "Go to \"http://%s:%d/hop/svc3\"", hop.hostname, hop.port );
