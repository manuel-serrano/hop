/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/file/file.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Mon Jun  1 18:47:13 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    basic example that shows how to ship files                       */
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
   return hop.HTTPResponseFile( path,
				{ contentType: "text/plain",
				  charset: hop.locale } );
}
					  
console.log( "Go to \"http://%s:%d/hop/file\"", hop.hostname, hop.port );
