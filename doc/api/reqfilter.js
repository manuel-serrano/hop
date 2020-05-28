"use strict";

hop.addRequestFilter( req => {
      console.log( "*** req.path=", req.abspath );
      return undefined;
   } );

hop.addRequestFilter( req => {
      if( req.abspath.match( "^/hop" ) ) {
      	 console.log( "+++ req.header=", req.header );
      }
   } );

service foo() {
   if( !this.socket.ssl ) {
      return "dangerous";
   } else {
      return "secure";
   }
}
