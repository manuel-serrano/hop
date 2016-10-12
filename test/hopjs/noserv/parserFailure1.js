try {
   require('./aux/faultySubmodule.js' );
}
catch (e) {
   console.log( 'catched a failure' );
   process.exit( 0 );
};

setTimeout( function() {
   console.log( 'exit on timeout' );
   process.exit( 1 );
}, 1000 );

