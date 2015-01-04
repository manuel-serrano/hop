/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirec/mod2.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Dec 20 09:47:27 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    multitier require                                                */
/*    -------------------------------------------------------------    */
/*    see requirec.js                                                  */
/*=====================================================================*/

exports.hello = function( s ) {
   return <DIV> {
      onclick: ~{ alert( "s=" + s ); },
      s
   }
}
