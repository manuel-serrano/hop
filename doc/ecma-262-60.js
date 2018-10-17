/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/doc/ecma-262-60.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Oct 26 11:41:07 2015                          */
/*    Last change :  Mon Oct 26 18:23:12 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Parses the ECMA-2620-60 documentation for generating the index.  */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    import                                                           */
/*---------------------------------------------------------------------*/
var path = require( "path" );

/*---------------------------------------------------------------------*/
/*    chapterTitle ...                                                 */
/*---------------------------------------------------------------------*/
var chapterTitle = "ecma-262";

/*---------------------------------------------------------------------*/
/*    compileECMA262Index ...                                          */
/*---------------------------------------------------------------------*/
function compileECMA262Index( file ) {
   var ast = require( file, "html" );

   var entries = [];
   var base = path.dirname( file );

   function firstChildIs( children, tag ) {
      for( var i = 0; i < children.length; i++ ) {
	 if( children[ i ].nodeType == 1 ) {
	    if( children[ i ].tagName == tag ) {
	       return children[ i ];
	    } else {
	       return false;
	    }
	 }
      }
      return false;
   };

   // find all the As child of TD
   var sections = ast.getElementsByTagName( "section" )
       .forEach( function( el, idx=undefined, arr=undefined ) {
	  var f = firstChildIs( el.childNodes, "h1" );
	  if( f ) {
	     if( f.childNodes[ 0 ].tagName == "span" ) {
		var span = f.childNodes[ 0 ];
		if( span.childNodes[ 0 ].tagName == "a" ) {
		   if( f.childNodes[ 1 ].nodeType == 3 ) {
		      console.log( "[" +
				   f.childNodes[ 1 ]
				   .toString()
				   .replace( /(?:^[ \t\n]+|[ \t\n]+$)/g, '' )
				   + "]" );
		   }
		}
	     }
	  }
       } );

   console.log( JSON.stringify( entries ) );
}

compileECMA262Index( process.argv[ 2 ] );
