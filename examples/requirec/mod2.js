/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/requirec/mod2.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Feb  3 09:28:56 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    multitier require                                                */
/*    -------------------------------------------------------------    */
/*    see requirec.js                                                  */
/*=====================================================================*/

exports.hello = function( s ) {
   return <button onclick=~{ alert( "s=" + s ) }>${s}</button>;
}
