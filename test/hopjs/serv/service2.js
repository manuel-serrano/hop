var assert = require( "assert" );

service iserv0( val ) {
   assert.ok( val == 3 );
   return val + 1;
}

service iserv1( {n: 10, m: 20} ) {
   assert.ok( n > 0 );
   assert.ok( m > n );
   
   return n + m;
}

service iserv2( val ) {
   assert.ok( val === arguments[ 0 ] );
   return arguments.length;
}

if( process.argv.indexOf( "serv/service2.js" ) ) {
   process.exit( 0 );
}
