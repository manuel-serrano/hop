/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/examples/doc.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 19 10:32:06 2014                          */
/*    Last change :  Mon Jan  5 12:01:36 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Read and fontify the examples source codes.                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g examples.js                                       */
/*    browser: http://localhost:8080/hop/examples                      */
/*=====================================================================*/
var hop = require( "hop" );
var fs = require( "fs" );
var path = require( "path" );
var wiki = require( hop.wiki );

/*---------------------------------------------------------------------*/
/*    examplesDoc ...                                                  */
/*---------------------------------------------------------------------*/
service examplesDoc( o ) {
   if( "description" in o ) {
      return <DIV> { o.description };
   } else {
      var p = path.join( o.dir, "doc.wiki" );
      if( fs.existsSync( p ) ) {
	 return hop.HTTPResponseAsync(
	    function( reply ) {
	       fs.readFile( p, function( err, buf ) {
		  reply( <DIV> { wiki.parse( buf ) } );
	       } );
	    }, this );
      } else {
	 return <DIV> { "" };
      }
   }
}

