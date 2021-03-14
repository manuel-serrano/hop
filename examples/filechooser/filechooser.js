/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/filechooser/filechooser.js   */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Tue Sep 15 08:01:03 2015 (serrano)                */
/*    Copyright   :  2014-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    FILECHOOSER widget example                                       */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g filechooser.js                                    */
/*    browser: http://localhost:8080/hop/filechooser?dir=/tmp          */
/*=====================================================================*/
var fs = require( 'fs' );
var path = require( 'path' );
var fc = require( hop.filechooser );

service filechooser( o ) {
   var dir = o && "dir" in o ?
       o.dir : path.dirname( path.dirname( module.filename ) );
   var d = <span>0</span>;
   
   return <html>
     
     <head>
       <link href=${fc.css} rel="stylesheet" type="text/css"/>
       <script src=${fc.script} type="application/x-javascript"/>
     </head>
     
     <body>
       <fc.filechooser onselect=~{alert( "selection=" + this.value )}/>
     </body>
   </html>;
}

console.log( "Go to \"http://%s:%d/hop/filechooser\"", hop.hostname, hop.port );
