/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/noserv/worker1.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  28 18:43:00 2015                         */
/*    Last change :  Tue Sep  8 18:10:37 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing workers                                                  */
/*=====================================================================*/


var w = new Worker( './aux/worker1.js' );

var testResult = false;

w.onmessage = function( e ) {
   console.log( 'Received result', e.data );
   testResult = true;
   w.terminate();
};

w.onexit = function() {
   console.log( 'worker has left' );
   if ( testResult ) {
      console.log( 'test passed' );
      process.exit( 0 );
   } else {
      console.log( 'test failed' );
      process.exit( 1 );
   }
};

w.onerror = function() {
   console.log( 'worker error' );
   process.exit( 1 );
};


setTimeout( function() {
   console.log( 'timeout' );
   process.exit( 1 );
}, 5000 );
	 
console.log( 'Sending Payload' );

w.postMessage( 'Payload' );

