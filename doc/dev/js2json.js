const fs = require('fs');
var exec = require('child_process').exec;
const hopc = require( hop.hopc );

service js2http( { ast, config } ) {
   const prg = hopc.intern( ast );
   
   return new Promise( (res, rej) => {
         fs.writeFile("/tmp/test.json", hopc.extern( prg ), function( err ) {
            // should call the analyzer here.
            var child = exec( 'cat /tmp/test.json' );
            var result = '';

            child.stdout.on( 'data', function( data ) {
	       // analyser result.
	       result += data;
            });

            child.on( 'close', function() {
	       var o = JSON.parse( result );
	       // pass the result to the next step.
	       res( hopc.intern( o ) );
            });
         });
      });
}
