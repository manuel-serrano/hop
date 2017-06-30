/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/wspost/wsserver.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Mon Jun 26 10:00:53 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    WebSocket server example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 -v -g wsserver.js                               */
/*=====================================================================*/

var serv = new WebSocketServer( { path: "serv", protocol: "foo" } );

service obj( o ) {
   o.a++;
   o.b = "obj ok";
   return o;
}

service str( o ) {
   if( o.a > 10 ) {
      return "ok strict";
   } else {
      return hop.HTTPResponseString( "error string",
				     { startLine: "HTTP/1.0 404 File not found" } );
   }
}

service asyn( o ) {
   o.a++;
   o.b = "asyn ok";
   return new Promise( function( resolve, reject ) {
      setTimeout( function( e ) { resolve( o ) }, 1000 );
   } );
}

console.log( "wsserver ready" );
