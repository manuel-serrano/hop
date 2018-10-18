/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/wsclient/wsclient.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Tue Jun 23 15:45:03 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    WebSocket client example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g wsclient.js                                       */
/*=====================================================================*/
service wsclient () {
   return <html>
      <button onclick=~{
	    var ws = new WebSocket( "ws://"
				    + ${hop.hostname} + ":"
				    + ${hop.port} + "/hop/wss",
				    [ "bar", "foo" ] );
	    
	    ws.onopen = function( event ) {
	       document.body.appendChild( (<div>WebSocket opened</div> ) );
	       this.send( "toto n'est pas content" );
	       this.send( "tutu non plus" );
	    };

	    ws.onmessage = function( event ) {
	       document.body.appendChild( <div>${event.data}</div> );
	    };
      }>
	 Open WebSocket
      </button>
   </html>
}

var wss = new WebSocketServer( { path: "wss", protocol: ["foo"] } );

wss.onconnection = function( event ) {
   var ws = event.value;

   console.log( "server: connection established:", ws.socket );

   ws.onmessage = function( event ) {
      console.log( "server received [%s]", event.value );
   };

   console.log( "server sends something" );
   ws.send( "something" );
   ws.send( "2something2" );
};

console.log( "Go to \"http://%s:%d/hop/wsclient\"", hop.hostname, hop.port );
