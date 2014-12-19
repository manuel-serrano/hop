/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/fs/fs.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Wed Dec 17 16:54:07 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    basic example FS examples                                        */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g file.js                                           */
/*    browser: http://localhost:8080/hop/fs                            */
/*=====================================================================*/
var hop = require( "hop" );
var sys = require( "fs" );

function readCheck( name ) {
   var fd = sys.openSync( name, "r" );
   var buf = new Buffer( 100 );
   sys.readSync( fd, buf, 0, 100, 0 );
   
   return buf.toString( "ascii", 0, 100 ).match( "^/[*]=" ) != null;
}

service fs() {
   return <HTML> {
      <TABLE> {
         <TR> {
	    <TH> { "exists" },
	    <TD> { sys.existsSync( fs.resource( "fs.js" ) ) ? "ok" : "error" }
	 },
         <TR> {
            <TH> { "read" },
            <TD> { readCheck( fs.resource( "fs.js" ) ) ? "ok" : "error" }
	 }
      }
   }
}

console.log( "Go to \"http://%s:%d/hop/fs\"", hop.hostname, hop.port );
