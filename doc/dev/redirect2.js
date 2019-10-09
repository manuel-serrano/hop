"use strict";

const config = require( hop.config );

hop.addRequestFilterFirst( req => {
      if( config.HTTPSPort && !req.socket.ssl ) {
	 console.log( "redirection..." );
      	 return hop.HTTPResponseString( "",
            {
               startLine: "HTTP/1.0 307 Temporary Redirect",
               header: { "location": `https://${req.host}:${config.HTTPSPort}${req.abspath}` }
            } );
      } else {
      	 return false;
      }
   } );

service foo() {
   console.log( this.socket.ssl );
   
   if( this.socket.ssl ) {
      return "ssl";
   } else {
      return "unsecure";
   }
}
