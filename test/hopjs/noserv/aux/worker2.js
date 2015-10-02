/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/noserv/aux/worker2.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  28 18:43:00 2015                         */
/*    Last change :  Tue Sep  8 18:10:37 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing workers                                                  */
/*=====================================================================*/

onmessage = function( e ) {
   console.log( 'worker processing ', e.data );
   postMessage( 'Response' );
   close();
};
