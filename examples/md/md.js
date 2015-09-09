/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/md/md.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Tue Sep  8 15:29:06 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example combining Markdown and HSS                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g md.js                                             */
/*=====================================================================*/
var hop = require( "hop" );
var md = require( hop.markdown );
var hss = require( hop.hss );

function compileNode( node, css, media ) {
   
   function isVisible( style ) {
      return style.display != "none" && style.visibility != "hidden";
   }
   
   var style = css.getComputedStyle( node );

   if( isVisible( style ) ) {
      var buf = "";
      
      // before
      if( style.before ) {
	 buf += style.before.content;
      }
      // body
      switch( node.nodeType ) {
      case 1:
      case 11:
	 node.childNodes.forEach( function( n ) {
	    buf += compileNode( n, css )
	 } );
	 break;
	 
      case 3:
	 buf += node.data;
	 break;
      }

      // after
      if( style.after ) {
	 buf += style.after.content;
      }

      return buf;
   } else {
      return "";
   }
}

var texhss = hss.load( require.resolve( "./tex.hss" ), "tex" );
var mdhss = hss.load( require.resolve( "./markdown.hss" ) );
var doc = md.load( require.resolve( "./README.md" ) ).XML;

var n = doc.getElementById( "foo" );

console.log( "tex..." );
console.log( compileNode( doc, texhss ) );

console.log();

console.log( "markdown..." );
console.log( compileNode( doc, mdhss ) );
