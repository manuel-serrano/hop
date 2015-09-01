/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/examples/websocket/wsserver.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Tue Sep  1 15:26:31 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    WebSocket server example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 -v -g wsserver.js                               */
/*=====================================================================*/

var serv = new WebSocketServer( { path: "serv", protocol: "foo" } );

serv.onconnection = function( event ) {
   var ws = event.value;

   console.error( "connection established:", ws.socket );

   ws.onmessage = function( event ) {
      console.log( "server received [%s]", event.data );
   };

   ws.onclose = function( event ) {
      console.log( "client socket closed." );
   }
   
   ws.send( "something" );
};
