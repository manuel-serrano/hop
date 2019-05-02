/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/noserv/aux/worker1.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  28 18:43:00 2015                         */
/*    Last change :  Thu May  2 14:16:01 2019 (serrano)                */
/*    Copyright   :  2015-19 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Testing workers                                                  */
/*=====================================================================*/

onmessage = function( e ) {
   console.log( 'worker processing ', e.data );
   postMessage( 'Response' );
};
