/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/websocket/wsclient.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Thu Nov  6 16:13:14 2014 (serrano)                */
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
};

ws.onmessage = function( event ) {
   console.log( "received [%s]", event.data );
};
