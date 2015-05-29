/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/spage/spage.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Fri May 29 10:22:23 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    SPAGE widget example                                             */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g spage.js                                          */
/*    browser: http://localhost:8080/hop/spage?dir=/tmp                */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );
var hop = require( 'hop' );
var SP = require( hop.spage );

function base( dir ) {
   return dir.replace( /.*\//g, "" );
}

function dirToSpage( dir ) {
   return <SP.SPTAB> {
      <SP.SPTABHEAD> { base( dir ) },
      service () {
	 return fs.readdirSync( dir ).map(
	    function( p ) {
	       var fp = path.join( dir, p );
	       if( fs.lstatSync( fp ).isDirectory() ) {
		  return dirToSpage( fp );
	       } else {
		  return <DIV> { value: fp, p }
	       }
	    } );
      }
   } </SP.SPTAB>
}

service spage( { dir: path.dirname( path.dirname( module.filename ) ) } ) {
   return <HTML> {
      <HEAD> { css: SP.css, jscript: SP.jscript },
      <BODY> { <SP.SPAGE> { <SP.SPHEAD> { dir }, dirToSpage( dir ) } }
   }
}

console.log( "Go to \"http://%s:%d/hop/spage\"", hop.hostname, hop.port );
