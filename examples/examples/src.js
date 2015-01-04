/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/examples/src.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 19 10:32:06 2014                          */
/*    Last change :  Sat Dec 20 10:19:29 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Read and fontify the examples source codes.                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g examples.js                                       */
/*    browser: http://localhost:8080/hop/examples                      */
/*=====================================================================*/
var hop = require( "hop" );
var fs = require( "fs" );
var fontifier = require( hop.fontifier );

/*---------------------------------------------------------------------*/
/*    examplesSrc ...                                                  */
/*---------------------------------------------------------------------*/
service examplesSrc( path ) {
   return hop.HTTPResponseAsync(
      function( reply ) {
	 var fontify = fontifier.hopscriptFontifier;
	 var lbegin = 14;
	 
	 if( path.match( /[.]hss$/ ) ) {
	    fontify = fontifier.hssFontifier;
	    lbegin = 11;
	 } else if( path.match( /[.]json$/ ) ) {
	    lbegin = 0;
	 } else if( path.match( /[.]hop$/ ) ) {
	    fontify = fontifier.hopFontifier;
	    lbegin = 0;
	 }
	 
	 fs.readFile( path, function( err, buf ) {
	    reply( <PRE> {
	       class: "fontifier-prog",
	       fontifier.lineNumber( fontify( buf, lbegin ) )
	    } );
	 } );
      }, this );
}

