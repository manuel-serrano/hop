/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/worker2.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Sep  28 18:43:00 2015                         */
/*    Last change :  Fri Jan 26 06:20:55 2018 (serrano)                */
/*    Copyright   :  2015-18 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Testing workers                                                  */
/*=====================================================================*/

var w = new Worker( './aux/worker2.js' );

var testResult = false;

w.onmessage = function( e ) {
   if( e.data.length != 7 ) {
      throw "bad master value " + e.data;
   }
   testResult = true;
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

let v = [ 1, 2, 3, 4, 5 ];
v.push( 6 );
w.postMessage( v );

