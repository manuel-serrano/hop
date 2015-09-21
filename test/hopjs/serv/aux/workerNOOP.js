/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/serv/aux/workerNOOP.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  21 11:43:00 2015                         */
/*    Last change :  Sun Sep 20 07:23:38 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    simple worker that does nothing                                  */
/*=====================================================================*/

/* This worker iterates service invocations for <num> times, then
 * post a message to inform the main thread of completion */

var id;
var num;

function test( id, num ) {
   postMessage( { messageType: 'done' } );
}

/* Protocol with workers launcher */
onmessage = function( e ) {
   switch (e.data.messageType) {
   case 'params':
      id = e.data.clientId;
      num = e.data.num;
      postMessage( { messageType: 'ready' } );
      break;
   case 'run':
      test( id, num );
   }
};

