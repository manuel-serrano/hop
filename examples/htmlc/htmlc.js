/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/htmlc/htmlc.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  4 09:19:16 2014                          */
/*    Last change :  Wed Jul 29 15:15:35 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HTML client-side attributes                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g htmlc.js                                          */
/*    browser: http://localhost:8080/hop/htmlc                         */
/*=====================================================================*/
var hop = require( "hop" );

service htmlc() {
   return <HTML>
      ~{ function getTitle() { return "client-side attributes"; } }
      ~{ function getStyle() { return "color: blue"; } }
      <DIV title="server-side attributes"
	 style="color: green; border: 1px solid green">
	 server-side attributes
      </DIV>
      <DIV title=~{ getTitle() } style=~{ getStyle() }>
	 client-side attributes
      </DIV>
      <DIV title=~{ "client-side attributes" }
	 style=~{ "color: red; border: 1px solid red" }>
	 client-side attributes
      </DIV>
      <IFRAME src=${iframe( "server-side src" )}/>
      <IFRAME src=~{ ${iframe}( "client-side src" )}/>
   </HTML>;   
}

service iframe( n ) {
   return <HTML>${n}</HTML>;
}

iframe.path = "/hop/htmlc/iframe";
	 
console.log( "Go to \"http://%s:%d/hop/htmlc\"", hop.hostname, hop.port );
