/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/htmlr/htmlr.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun  4 09:19:16 2014                          */
/*    Last change :  Fri Aug 14 07:22:47 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Requiring HTML ast                                               */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g html.js                                           */
/*    browser: http://localhost:8080/hop/html                          */
/*=====================================================================*/
var hop = require( "hop" );

service htmlr() {
   return require( "./htmlr.html" );
}
	 
console.log( "Go to \"http://%s:%d/hop/htmlr\"", hop.hostname, hop.port );
