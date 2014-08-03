/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/websocket/wsserver.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Sat Aug  2 07:26:48 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    WebSocket server example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 -v -g wsserver.js                               */
/*=====================================================================*/

var wss = new WebSocketServer( { path: "wss", protocol: "foo" } );

wss.onconnection = function( event ) {
   var ws = event.value;

   console.log( "connection established:", ws.socket );

   ws.onmessage = function( event ) {
      console.log('received [%s]', event.value);
   };
   ws.send('something');
};
