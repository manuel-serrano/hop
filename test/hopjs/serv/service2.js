var assert = require( "assert" );

service serv2( val ) {
   assert.ok( val == 3 );
   return val + 1;
}

service serv3( {n: 10, m: 20} ) {
   assert.ok( n > 0 );
   assert.ok( m > n );
   
   return n + m;
}

if( process.argv.indexOf( "serv/service2.js" ) ) {
   process.exit( 0 );
}
