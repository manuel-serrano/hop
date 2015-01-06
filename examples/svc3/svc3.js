/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc3/svc3.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Mon Jan  5 17:36:38 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
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
      function( sendResponse ) {
	 bar( x + 1 ).post( function( e ) {
	    sendResponse( e );
	 } )
      }, this );
}

service bar( x ) {
   console.log( "in bar x=", x );
   return <DIV> { x + 1 };
}

console.log( "Go to \"http://%s:%d/hop/svc3\"", hop.hostname, hop.port );
