service foo() {
   return true;
}

try {
   require('./aux/legacyImportSubmodule.js' );
} catch ( e ) {
   console.log( 'syntax fail' );
   process.exit( 1 );
};

setTimeout( function() {
   console.log( 'exit on timeout' );
   process.exit( 1 );
}, 1000 );

