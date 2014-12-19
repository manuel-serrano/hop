/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/websocket/wsserver.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Tue Dec  9 07:43:45 2014 (serrano)                */
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
      console.log( "server received [%s]", event.data );
      if( event.data == "close" ) {
	 setTimeout( function() { wss.close(); }, 500 );
      }
   };

   ws.onclose = function( event ) {
      console.log( "client socket closed." );
   }
   
   ws.send( "something" );
   ws.send( "close" );
   setTimeout( function() { ws.close(); }, 200 );
};


wss.onclose = function() {
   console.log( "server websocket closed." );
}
