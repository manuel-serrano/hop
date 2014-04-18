/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/nodejs/console.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Sep 18 14:56:55 2013                          */
/*    Last change :  Wed Apr 16 11:24:30 2014 (serrano)                */
/*    Copyright   :  2013-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Basic console facilities                                         */
/*=====================================================================*/

var mutex = #:make-mutex();

exports.log = function() {
   #:mutex-lock!( mutex );
   try {
      for( var i = 0; i < arguments.length; i++ ) {
         #:display( #:js-tostring( arguments[ i ], #:%this ) );
      }
      #:newline();
   } finally {
      #:mutex-unlock!( mutex );
   }
}
