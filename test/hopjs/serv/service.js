/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/service.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jan 11 18:14:33 2015                          */
/*    Last change :  Wed Nov 25 20:34:00 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing server-to-server services                                */
/*=====================================================================*/
var assert = require( "assert" );
var res = 0;

require( "./service2.js" );
 		
service serv0( val ) {
   assert.ok( val == 3 );
   res++;
   return val + 1;
}

service serv1( o ) {
   var n = ("n" in o) ? o.n : 10;
   var m = ("m" in o) ? o.m : 20;
   assert.ok( n > 0 );
   assert.ok( m > n );
   res++;
   
   return n + m;
}

service serv2( val ) {
   assert.ok( val === arguments[ 0 ] );
   return arguments.length;
}


import service iserv0();
import service iserv1();
import service iserv2();
import service iserv3();

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


