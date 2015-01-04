/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/pair/pair.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Dec 20 08:07:01 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Hop pair API example                                             */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g pair.js                                           */
/*    browser: http://localhost:8080/hop/pair                          */
/*=====================================================================*/
var hop = require( "hop" );

service pair() {
   var l = hop.List( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
   return <HTML> {
      l.map( function( n ) { return <DIV> { n } } ),
      <BUTTON> {
	 onclick: ~{
	    var hop = require( "hop" );
	    
	    ${reverse}( ${l} )
	       .post( function( r ) {
		  hop.List( "A", "B", "C" ).concat( r ).forEach( function( n ) {
		     document.body.appendChild( <DIV> {
			onclick: ~{ alert( "n=" + ${n} ) }, n } );
	          } );
	       } );
	 },
	 "Click to mirror."
      } </BUTTON>
   }
}

service reverse( p ) {
   return p.reverse();
}

console.log( "Go to \"http://%s:%d/hop/pair\"", hop.hostname, hop.port );
