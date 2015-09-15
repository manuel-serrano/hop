/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/spage/spage.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Tue Sep 15 08:01:03 2015 (serrano)                */
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
   var d = <span>0</span>;
   return <html>
     <head css=${SP.css} jscript=${SP.jscript}/>
     <body>
       <div>depth: ${d}</div>
       <SP.spage id="sp" onchange=~{${d}.innerHTML = HopSpage.depth( "sp" )}>
	 <SP.sphead>${ dir }</SP.sphead>
	 ${dirToSpage( dir )}
       </SP.spage>
     </body>
   </html>;
   }

console.log( "Go to \"http://%s:%d/hop/spage\"", hop.hostname, hop.port );
