/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/serv/service.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jan 11 18:14:33 2015                          */
/*    Last change :  Sat May 22 07:00:25 2021 (serrano)                */
/*    Copyright   :  2015-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing server-to-server services                                */
/*=====================================================================*/
"use hopscript";

const assert = require( "assert" );
let res = 0;

require( "./service2.js" );
 		
service serv0( val ) {
   assert.ok( val == 3 );
   res++;
   return val + 1;
}

service serv1( o ) {
   const n = ("n" in o) ? o.n : 10;
   const m = ("m" in o) ? o.m : 20;
   assert.ok( n > 0 );
   assert.ok( m > n );
   res++;
   
   return n + m;
}

service serv2( val ) {
   assert.ok( val === arguments[ 0 ] );
   return arguments.length;
}


service iserv0();
service iserv1();
service iserv2();
service iserv3();

function test() {
   serv0( 3 ).post( function( v ) { assert.ok( v == 4 ); res++; } );

   serv1().post( function( v ) { assert.ok( v == 30 ); res++; } );
   serv1( {} ).post( function( v ) { assert.ok( v == 30 ); res++; } );
   serv1( {n: 15} ).post( function( v ) { assert.ok( v == 35 ); res++; } );
   serv1( {m: 15} ).post( function( v ) { assert.ok( v == 25 ); res++; } );
   serv1( {n: 15, m: 25} ).post( function( v ) { assert.ok( v == 40 ); res++; } );
   serv2().post( function( v ) { assert.ok( v === 0 ); } );
   serv2( 1 ).post( function( v ) { assert.ok( v === 1 ); res++ } );
   serv2( 1, 2 ).post( function( v ) { assert.ok( v === 2 ); res++ } );

   iserv0( 3 ).post( function( v ) { assert.ok( v == 4 ); res++; } );

   iserv1().post( function( v ) { assert.ok( v == 30 ); res++; } );
   iserv1( {} ).post( function( v ) { assert.ok( v == 30 ); res++; } );
   iserv1( {n: 15} ).post( function( v ) { assert.ok( v == 35 ); res++; } );
   iserv1( {m: 15} ).post( function( v ) { assert.ok( v == 25 ); res++; } );
   iserv1( {n: 15, m: 25} ).post( function( v ) { assert.ok( v == 40 ); res++; } );
   iserv2( 1 ).post( function( v ) { assert.ok( v === 1 ); res++ } );
   iserv2( 1, 2 ).post( function( v ) { assert.ok( v === 2 ); res++ } );
   
   iserv3( 1 ).post( function( v ) { assert.ok( v === 1 ); res++ } );
   iserv3( 1, 2 ).post( function( v ) { assert.ok( v === 2 ); res++ } );
   
   iserv0( 3 ).post( function( v ) { assert.ok( v == 4 ); res++; } );
}

setTimeout( function() {
   assert.ok( res === 22 );
   process.exit( res === 22 ? 0 : 1 );
}, 200 );

test();


