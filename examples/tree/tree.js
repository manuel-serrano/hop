/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/tree/tree.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Wed Jul 15 12:41:41 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    TREE widget example                                              */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g tree.js                                           */
/*    browser: http://localhost:8080/hop/tree?dir=/tmp                 */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );
var hop = require( 'hop' );
var TR = require( hop.tree );

function base( dir ) {
   return dir.replace( /.*\//g, "" );
}

function dirToTree( dir ) {
   return <TR.TREE> {
      multiselect: true,
      value: dir,
      <TR.TRHEAD> { base( dir ) },
      <TR.TRBODY> {
	 service () {
	    return fs.readdirSync( dir ).map(
	       function( p ) {
		  var fp = path.join( dir, p );
		  if( fs.lstatSync( fp ).isDirectory() ) {
		     return dirToTree( fp );
		  } else {
		     return <TR.TRLEAF> { value: fp, p }
		  }
	       } );
	 }
      } </TR.TRBODY>
   } </TR.TREE>
}

service tree( { dir: path.dirname( path.dirname( module.filename ) ) } ) {
   var t = dirToTree( dir );
   return <HTML> {
      <HEAD> {
	 css: TR.css,
	 jscript: TR.jscript
      },
      <BODY> {
	 <DIV> {
	    <BUTTON> {
	       onclick: ~{ HopTree.open( ${t} ) },
	       "Open tree"
	    },
	    <BUTTON> {
	       onclick: ~{ HopTree.close( ${t} ) },
	       "Close tree"
	    },
	    <BUTTON> {
	       onclick: ~{ console.log( HopTree.selection( ${t} ) ) },
	       "Log selection"
	    },
	    <BUTTON> {
	       onclick: ~{ HopTree.reset( ${t} ) },
	       "Reset selection"
	    }
	 },
	 t
      }
   }
}

console.log( "Go to \"http://%s:%d/hop/tree\"", hop.hostname, hop.port );
