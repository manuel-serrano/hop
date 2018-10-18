var worker;
var err = 0;
var assert = require( "assert" );

try {
   worker = new Worker('./aux/faultySubmodule.js' );
   worker.onerror = function( e ) { err++; process.exit( 0 ) };
};

setTimeout( function() {
   assert.ok( err, 1 );
}, 1000 );
