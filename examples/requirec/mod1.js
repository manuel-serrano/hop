/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirec/mod1.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Dec 20 09:47:32 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    multitier require                                                */
/*    -------------------------------------------------------------    */
/*    see requirec.js                                                  */
/*=====================================================================*/

var mod2 = require( "./mod2.js" );

var s = "";

s += "hello";

exports.hello = function( x ) {
   return mod2.hello( s );
}
