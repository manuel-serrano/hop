/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/examples/src.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 19 10:32:06 2014                          */
/*    Last change :  Tue Sep 15 16:30:31 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
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
      function( sendResponse ) {
	 var fontify = fontifier.hopscript;
	 var lbegin = 14;
	 
	 if( path.match( /[.]hss$/ ) ) {
	    fontify = fontifier.hss;
	    lbegin = 11;
	 } else if( path.match( /[.]json$/ ) ) {
	    lbegin = 0;
	 } else if( path.match( /[.]html$/ ) ) {
	    lbegin = 0;
	    fontify = fontifier.xml;
	 } else if( path.match( /[.]hop$/ ) ) {
	    fontify = fontifier.hop;
	    lbegin = 0;
	 }

	 fs.readFile( path, function( err, buf ) {
	    sendResponse( <PRE> {
	       class: "fontifier-prog",
	       fontifier.lineNumber( fontify( buf, lbegin ) )
	    } );
	 } );
      }, this );
}

