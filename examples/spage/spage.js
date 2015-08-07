/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/spage/spage.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Fri Aug  7 07:16:25 2015 (serrano)                */
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
   return <SP.sptab>
     <SP.sptabhead>${ base( dir ) }</SP.sptabhead>
     ${service () {
	return fs.readdirSync( dir ).map(
	   function( p ) {
	      var fp = path.join( dir, p );
	      if( fs.lstatSync( fp ).isDirectory() ) {
		 return dirToSpage( fp );
	      } else {
		 return <div value=${fp}>${p}</div>;
	      }
	   } );
     } }
   </SP.sptab>
}

service spage( { dir: path.dirname( path.dirname( module.filename ) ) } ) {
   return <html>
     <head css=${SP.css} jscript=${SP.jscript}/>
     <body>
       <SP.spage>
	 <SP.sphead>${ dir }</SP.sphead>
	 ${dirToSpage( dir )}
       </SP.spage>
     </body>
   </html>;
   }

console.log( "Go to \"http://%s:%d/hop/spage\"", hop.hostname, hop.port );
