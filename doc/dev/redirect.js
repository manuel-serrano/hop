service mySvc() {
   return HTTPtoHTTPs( this ) || ...;
}

function HTTPtoHTTPS( req ) {
   if( config.HTTPSPort && !req.socket.ssl ) {
      return hop.HTTPResponseString( "",
         {
            startLine: "HTTP/1.0 307 Temporary Redirect",
            header: { "location": `https://${req.host}:${config.HTTPSPort}${req.abspath}` }
         } );
   } else {
      return false;
   }
}
