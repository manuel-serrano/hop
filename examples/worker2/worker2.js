/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker2/worker2.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Fri Oct 24 18:49:40 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Server side worker thread example                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker.js                                         */
/*=====================================================================*/

console.log( "starting worker-master..." );

var w = new Worker( "./slave.js" );

// wait one second and send a message
setTimeout( function() {
   console.log( "sending hello slave" );
   w.postMessage( "hello slave" );

   // wait one other 2 seconds and kill the worker
   setTimeout( function() {
      console.log( "terminating slave" );
      w.terminate();
   }, 2000 );
}, 1000 );

w.onmessage = function ( e ) {
   console.log( "master received '%s'", e.data );
};
