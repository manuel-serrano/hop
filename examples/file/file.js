/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/file/file.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Thu Jul  3 14:28:29 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    basic example that shows to the ship a file to the client        */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g file.js                                           */
/*    browser: http://localhost:8080/hop/file                          */
/*=====================================================================*/

var hop = require( "hop" );

service file() {
   var pre = <PRE> {};

   return <HTML> {
      ~{
	 var entityMap = {
	    "&": "&amp;",
	    "<": "&lt;",
	    ">": "&gt;",
	    '"': '&quot;',
	    "'": '&#39;',
	    "/": '&#x2F;'
	 };
	 
	 function escapeHTML( string ) {
	    return String( string ).replace(
		  /[&<>"'\/]/g,
	       function ( s ) {
		  return entityMap[s];
	       } );
	 }
      },

      <BUTTON> {
	 onclick: ~{
	    var file = ${fileGet.resource( "file.js" )};

	    ${fileGet}( file )
	       .post( function( txt ) { ${pre}.innerHTML = escapeHTML( txt ) } );
	 },
	 "click me"
      },
      pre
   }
}

service fileGet( path ) {
   return hop.HTTPResponseFile( path );
}
					  
console.log( "Go to \"http://%s:%d/hop/file\"", hop.hostname, hop.port );
