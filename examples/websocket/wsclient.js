/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/websocket/wsclient.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Sun Dec 21 07:40:12 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    WebSocket client example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -v -- wsclient.js 9999                                  */
/*=====================================================================*/

var port = parseInt( process.execArgv[ process.execArgv.length - 1 ] );
var ws = new WebSocket( "ws://localhost:" + port + "/hop/wss", [ "bar", "foo" ] );

console.error( "ws=", "ws://localhost:" + port + "/hop/wss" );
ws.onopen = function( event ) {
   this.send( "toto n'est pas content" );
   this.send( "tutu non plus" );
   this.send( "close" );
};

ws.onmessage = function( event ) {
   console.log( "client received [%s]", event.data );
   if( event.data == "close" ) {
      ws.close();
   }
};

ws.onclose = function( event ) {
   console.error( "client websocket closed." );
}
