/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/service.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jan 11 18:14:33 2015                          */
/*    Last change :  Thu Jan 15 22:07:25 2015 (serrano)                */
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

service serv1( { n: 10, m: 20 } ) {
   assert.ok( n > 0 );
   assert.ok( m > n );
   res++;
   
   return n + m;
}

import service serv2();
import service serv3();

function test() {
   serv0( 3 ).post( function( v ) { assert.ok( v == 4 ); res++; } );

   serv1().post( function( v ) { assert.ok( v == 30 ); res++; } );
   serv1( {} ).post( function( v ) { assert.ok( v == 30 ); res++; } );
   serv1( {n: 15} ).post( function( v ) { assert.ok( v == 35 ); res++; } );
   serv1( {m: 15} ).post( function( v ) { assert.ok( v == 25 ); res++; } );
   serv1( {n: 15, m: 25} ).post( function( v ) { assert.ok( v == 40 ); res++; } );

   serv2( 3 ).post( function( v ) { assert.ok( v == 4 ); res++; } );

   serv3().post( function( v ) { assert.ok( v == 30 ); res++; } );
   serv3( {} ).post( function( v ) { assert.ok( v == 30 ); res++; } );
   serv3( {n: 15} ).post( function( v ) { assert.ok( v == 35 ); res++; } );
   serv3( {m: 15} ).post( function( v ) { assert.ok( v == 25 ); res++; } );
   serv3( {n: 15, m: 25} ).post( function( v ) { assert.ok( v == 40 ); res++; } );
}

setTimeout( function() {
   assert.ok( res === 18 );
   process.exit( res === 18 ? 0 : 1 );
}, 200 );

test();


