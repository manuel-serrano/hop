/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker/worker.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Tue Jan 13 16:40:19 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Server side worker thread example                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker.js                                         */
/*=====================================================================*/

console.log( "starting worker-master..." );
var bar = require( "./bar.js" );

var w = new Worker( "./slave.js" );

w.onmessage = function( e ) {
   console.log( "received from the slave: ", e.data );
   console.log( "onmsg master bar=", bar.count( 1 ), " (expect 16)" );
}

w.postMessage( "hello slave" );

console.log( "master, bar=", bar.count( 10 ), " (expect 10)" );
console.log( "master, bar=", bar.count( 5 ), " (expect 15)" );

console.log( "master done...");
