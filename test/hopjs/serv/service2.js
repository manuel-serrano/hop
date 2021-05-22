"use hopscript";

const assert = require( "assert" );

service iserv0( val ) {
   assert.ok( val == 3 );
   return val + 1;
}

service iserv1( o ) {
   const n = ("n" in o) ? o.n : 10;
   const m = ("m" in o) ? o.m : 20;
   assert.ok( n > 0 );
   assert.ok( m > n );
   
   return n + m;
}

service iserv2( val ) {
   assert.ok( val === arguments[ 0 ] );
   return arguments.length;
}

const iserv3 = new Service( function( o ) {
   const n = ("n" in o) ? o.n : 10;
   const m = ("m" in o) ? o.m : 20;
   assert.ok( n > 0 );
   assert.ok( m > n );
   
   return n + m;
}, "iserv3" );

if( process.argv.indexOf( "serv/service2.js" ) ) {
   process.exit( 0 );
}
