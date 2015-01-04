/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker/slave.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec 21 10:55:12 2014                          */
/*    Last change :  Sun Dec 21 10:55:19 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Worker slave                                                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker.js                                         */
/*=====================================================================*/

console.log( "starting worker-slave..." );

var bar = require( "./bar.js" );

onmessage = function( e ) {
   console.log( "received from master: ", e.data );
   postMessage( "what master?" );
}

postMessage( "hi master" );

console.log( "slave bar=", bar.count( 100 ), " (expect 100)" );
console.log( "slave bar=", bar.count( 3 ), " (expect 103)" );
