/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/tree/tree.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Thu Jul 24 14:55:37 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
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
      <TR.TRHEAD> {
	 base( dir )
      },
      <TR.TRBODY> {
	 service() {
	    return fs.readdirSync( dir ).map(
	       function( p ) {
		  if( fs.lstatSync( p ).isDirectory( p ) ) {
		     return dirToTree( p );
		  } else {
		     return <TR.TRLEAF> {
			value: p,
			base( p )
		     }
		  }
	       } );
	 }
      } </TR.TRBODY>
   } </TR.TREE>
}
	    
			
service tree( { dir: path.dirname( path.dirname( module.filename ) ) } ) {
   return <HTML> {
      <HEAD> {
	 css: TR.css,
	 jscript: TR.jscript
      },
      <BODY> {
	 dirToTree( dir )
      }
   }
}

console.log( "Go to \"http://%s:%d/hop/tree\"", hop.hostname, hop.port );
