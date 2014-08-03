/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/websocket/wsclient.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Sat Aug  2 07:25:39 2014 (serrano)                */
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
};

ws.onmessage = function( event ) {
   console.log( "received [%s]", event.value );
};
