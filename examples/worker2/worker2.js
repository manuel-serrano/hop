/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker2/worker2.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Sun Dec 21 11:07:02 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Server side worker thread example                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker.js                                         */
/*=====================================================================*/

console.log( "Master creating a Worker" );
var w = new Worker( "./slave.js" );

// can post as soon as the Worker object is created.
console.log( "Master sending a Work Order" );
w.postMessage( "Work Order" );


w.onmessage = function ( e ) {
   console.log( "Master received '%s'", e.data );
   console.log( "Master terminating worker" );
   w.terminate();
};

//w.onexit is useful to track worker self termination (not the case here).
w.onexit = function() {
   console.log( "Worker is terminated, I knew that already" );
}
