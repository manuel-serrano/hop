/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/noserv/aux/worker2.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  28 18:43:00 2015                         */
/*    Last change :  Fri Jan 26 06:21:56 2018 (serrano)                */
/*    Copyright   :  2015-18 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Testing workers                                                  */
/*=====================================================================*/

onmessage = function( e ) {
   if( e.data.length == 6 ) {
      e.data.push( 7 );
      postMessage( e.data );
   } else {
      throw "bad slave value " + e.data;
   }
   close();
};
