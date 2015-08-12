/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/htmlc/htmlc.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  4 09:19:16 2014                          */
/*    Last change :  Sun Aug  9 06:26:15 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HTML client-side attributes                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g htmlc.js                                          */
/*    browser: http://localhost:8080/hop/htmlc                         */
/*=====================================================================*/
var hop = require( "hop" );

service htmlc() {
   return <html>
      ~{ function getTitle() { return "client-side attributes"; } }
      ~{ function getStyle() { return "color: blue"; } }
      <div title="server-side attributes"
	 style="color: green; border: 1px solid green">
	 server-side attributes
      </div>
      <div title=~{ getTitle() } style=~{ getStyle() }>
	 client-side attributes
      </div>
      <div title=~{ "client-side attributes" }
	 style=~{ "color: red; border: 1px solid red" }>
	 client-side attributes
      </div>
      <iframe src=${iframe( "server-side src" )}/>
      <iframe src=~{ ${iframe}( "client-side src" )}/>
   </html>;   
}

service iframe( n ) {
   return <html>${n}</html>;
}

iframe.path = "/hop/htmlc/iframe";
	 
console.log( "Go to \"http://%s:%d/hop/htmlc\"", hop.hostname, hop.port );
