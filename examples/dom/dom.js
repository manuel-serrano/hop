/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/dom/dom.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Wed Jul 29 15:33:22 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Multitier dom manipulations                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g dom.js                                            */
/*    browser: http://localhost:8080/hop/dom                           */
/*=====================================================================*/
var hop = require( 'hop' );

service dom() {
   var el = <UL><LI>foo</LI><LI>bar</LI><LI>gee</LI></UL>;
   
   return <HTML>
      <DIV>
        ${el}
   <BUTTON onclick=~{
	       var c0 = ${el}.childNodes[ 0 ];
	       var c1 = ${el}.childNodes[ 1 ];

	       ${el}.removeChild( c0 );
	       ${el}.appendChild( c0 );
	}>rotate</BUTTON>
      </DIV>
   </HTML>;
}

console.log( "Go to \"http://%s:%d/hop/dom\"", hop.hostname, hop.port );
