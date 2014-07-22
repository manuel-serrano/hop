/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/dom/dom.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 17 08:51:31 2014                          */
/*    Last change :  Mon Jul 14 09:34:33 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Multitier dom manipulations                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g dom.js                                            */
/*    browser: http://localhost:8080/hop/dom                           */
/*=====================================================================*/

var hop = require( 'hop' );

service dom() {
   var el = <UL> {
      <LI> { "foo" },
      <LI> { "bar" },
      <LI> { "gee" }
   };
   
   return <HTML> {
      <HEAD> {},
      <DIV> {
	 el,
	 <BUTTON> {
	    onclick: ~{
	       var c0 = ${el}.childNodes[ 0 ];
	       var c1 = ${el}.childNodes[ 1 ];
	    
	       ${el}.replaceChild( c1, c0 );
	       ${el}.appendChild( c0 );
	    },
	    "rotate"
	 } </BUTTON>
      } </DIV>
   } </HTML>
}

console.log( "Go to \"http://%s:%d/hop/dom\"", hop.hostname, hop.port );

