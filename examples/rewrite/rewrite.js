/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/rewrite/rewrite.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Thu Jun 12 16:17:24 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    String response example                                          */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g rewrite.js                                        */
/*    browser: http://localhost:8080/hop/rewrite                       */
/*=====================================================================*/

var hop = require( "hop" );
var fs = require( "fs" );

var size = 96000;

service rewrite() {
   var url = <INPUT> { size: 60, value: "http://www.inria.fr" }

   return <HTML> {
      url,
      <BUTTON> {
	 onclick: ~{ window.open( ${rewriteNow}( ${url}.value ) ) },
	 "Rewrite now"
      }
   }
}

service rewriteNow( url ) {
   var fd = fs.openSync( url, "r" );
   var buf = new Buffer( size );
   fs.readSync( fd, buf, 0, size, 0 );
   console.log( buf.toString( "ascii", 0, size ) );
   return hop.HTTPResponseString( 
      buf.toString( "ascii", 0, size ).replace( /Inria/gi, "l'Inria" ),
      { contentType: "text/html" } );
}
