var fs = require( 'fs' );
var assert = require( 'assert' );

var res;

var size1 = 24; // small buffer
var buf1 = new Buffer( size1 );
for ( var i = 0; i < size1; i++ ) {
   buf1[ i ] = i;
};

var size2 = 17000; // large buffer
var buf2 = new Buffer( size2 );
for ( var i = 0; i < size2; i++ ) {
    buf2[ i ] = i % 256;
};

console.log( 'buffers initialized' );

function checkBuffer( buf, reference ) {
   assert.ok( typeof( buf ) == 'object' );
   assert.ok( Buffer.isBuffer( buf ));
   assert.equal( buf.length, reference.length );
   for ( var i = 0; i < reference.length; i++ ) {
      assert.equal( buf[ i ], reference[ i ]);
   }
   return true;
}

service passSmallBuffer( data ) {
   return checkBuffer( data, buf1 );
}

service passLargeBuffer( data ) {
   return checkBuffer( data, buf2 );
}

service passTwoBuffers( data1, data2 ) {
   return checkBuffer( data1, buf1 ) && checkBuffer( data2, buf2 );
}

function test() {
   res = 0;
   passSmallBuffer( buf1 ).post( function( result ) {
      if ( result ) res++;
   });
   
   passLargeBuffer( buf2 ).post( function( result ) {
      if ( result ) res++;
   });
   

   passTwoBuffers( buf1, buf2 ).post( function( result ) {
      if ( result ) res++;
   });
}

setTimeout( function() {
   process.exit( res === 3 ? 0 : 1 );
}, 1000 );

test();
