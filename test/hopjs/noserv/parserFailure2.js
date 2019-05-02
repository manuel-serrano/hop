var worker;
var err = 0;
var assert = require( "assert" );

try {
   worker = new Worker('./aux/faultySubmodule.js' );
   worker.onerror = function( e ) { err++; process.exit( 0 ) };
};

setTimeout( function() {
   console.log( "error=", err );
   try {
      assert.ok( err, 1, "parseFailure2" );
   } catch( e ) {
      console.log( "e=", e.name );
      #:js-debug-object( e, "e=" );
   }
}, 1000 );
