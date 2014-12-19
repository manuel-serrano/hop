/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/wsclient/wsclient.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 14 17:02:10 2014                          */
/*    Last change :  Wed Dec  3 18:54:25 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    WebSocket client example                                         */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g wsclient.js                                       */
/*=====================================================================*/
var hop = require ("hop");

service wsclient () {
   return <HTML> {
      <BUTTON> {
	 onclick: ~{
	    var ws = new WebSocket( "ws://localhost:" + ${hop.port} + "/hop/wss",
				    [ "bar", "foo" ] );
	    
	    ws.onopen = function( event ) {
	       document.body.appendChild( (<DIV> { "WebSocket opened" } ) );
	       this.send( "toto n'est pas content" );
	       this.send( "tutu non plus" );
	    };

	    ws.onmessage = function( event ) {
	       document.body.appendChild( <DIV> { event.data } );
	    };
	 },
	 "Open WebSocket"
      }}};

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
