/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/websocket/wsclient.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Tue Dec  9 07:29:36 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    WebSocket client example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g wsclient.js                                       */
/*    (this assumes an echo WebSocket server at localhost:9999)        */
/*=====================================================================*/

var ws = new WebSocket( "ws://localhost:9999/hop/wss", [ "bar", "foo" ] );

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
   console.log( "client websocket closed." );
}
