/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/noserv/aux/worker1.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  28 18:43:00 2015                         */
/*    Last change :  Mon Jul 15 14:01:03 2024 (serrano)                */
/*    Copyright   :  2015-24 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Testing workers                                                  */
/*=====================================================================*/

onmessage = function(e) {
   console.log('worker processing ', e.data);
   postMessage('Response');
};
