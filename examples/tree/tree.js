/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/tree/tree.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Sun Aug  2 14:49:12 2015 (serrano)                */
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
var tr = require( hop.tree );

function base( dir ) {
   return dir.replace( /.*\//g, "" );
}

function dirToTree( dir ) {
   return <tr.tree multiselect=true value=${dir}>
      <tr.head>${base( dir )}</tr.head>
      <tr.body>
         ${service () {
	    return fs.readdirSync( dir ).map(
	       function( p ) {
		  var fp = path.join( dir, p );
		  if( fs.lstatSync( fp ).isDirectory() ) {
		     return dirToTree( fp );
		  } else {
		     return <tr.leaf value=${fp}>${p}<tr.leaf>
		  }
	       } );
	 }}
      </tr.body>
   </tr.tree>
}

service tree( { dir: path.dirname( path.dirname( module.filename ) ) } ) {
   var t = dirToTree( dir );
   return <html>
     <head css=${tr.css} jscript=${tr.jscript}/>
     <body>
       <div>
        <button onclick=~{ HopTree.open( ${t} ) }>Open tree</button>
        <button onclick=~{ HopTree.close( ${t} ) }>Close tree</button>
	<button onclick=~{ console.log( HopTree.selection( ${t} ) ) }>Log</button>
	<button onclick=~{ HopTree.reset( ${t} ) }>Reset</button>
       </div>
       ${t}
     </body>
   </html>;
}

console.log( "Go to \"http://%s:%d/hop/tree\"", hop.hostname, hop.port );
