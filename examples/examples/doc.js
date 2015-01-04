/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/examples/doc.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 19 10:32:06 2014                          */
/*    Last change :  Sun Dec 21 13:17:04 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
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
   if( "doc" in o ) {
      return o.doc;
   } else {
      return hop.HTTPResponseAsync(
	 function( reply ) {
	    var p = path.join( o.dir, "doc.wiki" );
	    fs.readFile( p, function( err, buf ) {
	       reply( <DIV> { wiki.parse( buf ) } );
	    } );
	 }, this );
   }
}

