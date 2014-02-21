/*=====================================================================*/
/*    serrano/prgm/project/hop/2.6.x/nodejs/console.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Sep 18 14:56:55 2013                          */
/*    Last change :  Wed Jan  8 10:15:03 2014 (serrano)                */
/*    Copyright   :  2013-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Basic console facilities                                         */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    The module                                                       */
/*---------------------------------------------------------------------*/
// (module __nodejs_console (library hopscript))

exports.log = function() {
   for( var i = 0; i < arguments.length; i++ ) {
      #:display( #:js-tostring( arguments[ i ] ) );
   }
   #:newline();
}
