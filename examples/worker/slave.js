console.log( "starting worker-slave..." );

var bar = require( "./bar.js" );

onmessage = function( e ) {
   console.log( "received from master: ", e.data );
   postMessage( "what master?" );
}

postMessage( "hi master" );

console.log( "slave bar=", bar.count( 100 ), " (expect 100)" );
console.log( "slave bar=", bar.count( 3 ), " (expect 103)" );
