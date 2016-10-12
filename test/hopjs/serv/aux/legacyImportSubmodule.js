import service foo();

foo().post( function() {
   console.log( 'service is ok' );
   process.exit();
},
	    function( error ) {
	       console.log( 'service is broken' );
	       process.exit( 1 );
	    });
