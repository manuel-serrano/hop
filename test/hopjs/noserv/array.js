/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/array.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Tue Nov 15 08:28:44 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing arrays                                                   */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

var s = [ 1, 2, 3, 4, 5 ];
var s2 = s.splice( 1, 0, 256 );

assert.deepEqual( s2, [] );
assert.deepEqual( s, [ 1, 256, 2, 3, 4, 5 ] );

function fun( x ) {
   return x;
}

var v = [];
v.push( 1 );

assert.ok( fun.apply( undefined, v ) === 1 );

/*---------------------------------------------------------------------*/
/*    preventExtensions                                                */
/*---------------------------------------------------------------------*/
var o = [ 1, 2 ];
Object.preventExtensions( o );

try {
   o.push( 3 );
   assert.fail( "object not extensible" );
} catch( _ ) {
   ;
}

var o2 = [ 1, 2 ];
Object.preventExtensions( o2 );

try {
   o2[ 2 ] = 3;
   assert.fail( "object not extensible" );
} catch( _ ) {
   ;
}

/*---------------------------------------------------------------------*/
/*    expand                                                           */
/*---------------------------------------------------------------------*/
function expander( v, nlen ) {
   for( let i = 0; i < nlen; i++ ) {
      v[ i ] = i;
   }

   return v;
}

function expanderLength( v, len ) {
   expander( v, len );
   let p = Object.getOwnPropertyDescriptor( v, "length" );
   
   return v.length == len;
}
   
function expanderProp( v, len ) {
   expander( v, len );
   let p = Object.getOwnPropertyDescriptor( v, "length" );
   
   return p.value == len;
}
   
assert.ok( expanderLength( new Array( 7 ), 8 ), "expand length" );
assert.ok( expanderLength( new Array( 7 ), 9 ), "expand length" );
assert.ok( expanderLength( new Array( 7 ), 10 ), "expand length" );

assert.ok( expanderProp( new Array( 7 ), 8 ), "expand prop" );
assert.ok( expanderProp( new Array( 7 ), 9 ), "expand prop" );
assert.ok( expanderProp( new Array( 7 ), 10 ), "expand prop" );
