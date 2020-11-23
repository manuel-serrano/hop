/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/spage/spage.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Tue Sep 15 08:01:03 2015 (serrano)                */
/*    Copyright   :  2014-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    SPAGE widget example                                             */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g spage.js                                          */
/*    browser: http://localhost:8080/hop/spage?dir=/tmp                */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );
var sp = require( hop.spage );

function base( dir ) {
   return dir.replace( /.*\//g, "" );
}

service spageDir( dir ) {
   return fs.readdirSync( dir ).map(
      function( p ) {
	 var fp = path.join( dir, p );
	 if( fs.lstatSync( fp ).isDirectory() ) {
	    return dirToSpage( fp );
	 } else {
	    return <div value=${fp}>${p}</div>;
	 }
      } );
}
   
function dirToSpage( dir ) {
   return <sp.sptab svc=${spageDir} arg=${dir}>
     <sp.sptabhead>${ base( dir ) }</sp.sptabhead>
   </sp.sptab>
}

service spage( o ) {
   var dir = o && "dir" in o ?
       o.dir : path.dirname( path.dirname( module.filename ) );
   var d = <span>0</span>;
   
   return <html>
     
     <head>
       <link href=${sp.css} rel="stylesheet" type="text/css"/>
       <script src=${sp.script} type="application/x-javascript"/>
     </head>
     
     <body>
       <div>
	 transition style
	 <button onclick=~{node_style_set( document.getElementById( "sp" ).spstyle, "cursor", "help" )}>Fade</button>
	 <button onclick=~{node_style_set( document.getElementById( "sp" ).spstyle, "cursor", "move" )}>Slide</button>
       </div>
       
       <div>depth: ${d}</div>
       <br/>
       <div style="width: 400px; border: 1px solid #ccc">
       	 <sp.spage id="sp" onchange=~{${d}.innerHTML = document.getElementById( "sp" ).depth}>
           <sp.sphead>${ dir }</sp.sphead>
           ${dirToSpage( dir )}
       	 </sp.spage>
       </div>
     </body>
   </html>;
}

console.log( "Go to \"http://%s:%d/hop/spage\"", hop.hostname, hop.port );
