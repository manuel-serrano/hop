/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/array.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Thu Nov 24 10:10:40 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing arrays                                                   */
/*=====================================================================*/
"use strict";
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

   return v.length == len;
}
   
function expanderProp( v, len ) {
   expander( v, len );
   let p = Object.getOwnPropertyDescriptor( v, "length" );

   return p.value == len;
}

function expanderRonly() {
   var a = [ 1, 2, 3];
   Object.defineProperty( a, "length", {writable: false} );
   a.length == 5;
   return a.length == 3;
}

assert.ok( expanderLength( new Array( 5 ), 7 ), "expand length .7" );
assert.ok( expanderLength( new Array( 5 ), 8 ), "expand length .8" );
assert.ok( expanderLength( new Array( 5 ), 9 ), "expand length .9" );
assert.ok( expanderLength( new Array( 5 ), 10 ), "expand length .10" );

assert.ok( expanderProp( new Array( 5 ), 7 ), "expand prop .7" );
assert.ok( expanderProp( new Array( 5 ), 8 ), "expand prop .8" );
assert.ok( expanderProp( new Array( 5 ), 9 ), "expand prop .9" );
assert.ok( expanderProp( new Array( 5 ), 10 ), "expand prop .10" );

assert.ok( expanderRonly(), "expand read-only" );

/*---------------------------------------------------------------------*/
/*    properties                                                       */
/*---------------------------------------------------------------------*/
function props() {
   var arrObj = [ 0, 1 ];
   Object.defineProperty( arrObj, "1", {
      value: 1,
      configurable: false
   } );
   var d = Object.getOwnPropertyDescriptor( arrObj, "length" );
   return d.value == 2;
}

assert.ok( props(), "length.configurable" );

/*---------------------------------------------------------------------*/
/*    iterations                                                       */
/*---------------------------------------------------------------------*/
function checkA( a, len ) {
   if( a.length != len ) { return false };
   for( let i = 0; i < len; i++ ) {
      if( !(i in a) ) { return false }
   }
   return true;
}

function forShrink( a ) {
   let l = a.length;
   if( !checkA( a, 5 ) ) return -1;
   
   for( let i = 0; i < l; i++ ) {
      if( i == 2 ) { a.length = 3; }
      if( i == 3 ) {
	 if( i in a ) { console.log( "a=", a ); return -2; }
	 if( Object.getOwnPropertyDescriptor( a, "" + i ) ) return -3;
	 if( Object.hasOwnProperty( a, "" + i ) ) return -4;
	 if( a[ i ] !== undefined ) { console.log( a[ i ] ); return -5; }
	 return 0;
      };
   }
}

function forDelete( a ) {
   let l = a.length;
   if( !checkA( a, 5 ) ) return -1;
   
   for( let i = 0; i < l; i++ ) {
      if( i == 2 ) { delete a[ 3 ]; }
      if( i == 3 ) {
	 if( i in a ) { console.log( "a=", a ); return -2; }
	 if( Object.getOwnPropertyDescriptor( a, "" + i ) ) return -3;
	 if( Object.hasOwnProperty( a, "" + i ) ) return -4;
	 if( a[ i ] !== undefined ) { console.log( "a=", a ); return -5; }
	 return 0;
      };
   }
}

function forExpand( a ) {
   let l = a.length;
   if( !checkA( a, 5 ) ) return -1;

   for( let i = 0; i < l; i++ ) {
      if( i == 2 ) { a.length = l + 3; a[ l + 2 ] = 4 };
      if( i == 3 ) {
	 if( (l+1) in a ) return -2;
	 if( Object.getOwnPropertyDescriptor( a, "" + (l+1) ) ) return -3;
	 if( Object.hasOwnProperty( a, "" + (l+1) ) ) return -4;
	 if( a[ l+1 ] !== undefined ) return -5;
	 if( a[ l+2 ] !== 4 ) { console.log( "a=", a, " v=", a[ l + 2 ] ); return -6; }
	 return 0;
      };
   }
}

function vecExpand( a ) {
   let l = a.length;
   if( !checkA( a, 5 ) ) return -1;
   
   if( !a[ l + 2 ] == undefined ) return -2;
   if( !a.length == l + 3 ) return -3;
   if( l in a ) return -4;
   if( Object.getOwnPropertyDescriptor( a, "" + l ) ) return -5;
   if( Object.hasOwnProperty( a, "" + l ) ) return -6;
   return 0;
}

function findDelete( a ) {
   var x = -2;
   a.find( function( val, idx, arr ) {
      if( idx == 2 ) { delete arr[ 4 ]; x++; };
      if( idx == 4 ) { x++ };
   } );

   return x;
}

function foreachDelete( a ) {
   var x = 0;
   a.forEach( function( val, idx, arr ) {
      if( idx == 2 ) { delete arr[ 4 ]; x = 0 };
      if( val == 5 ) { x = -2 };
   } );

   return x;
}


function deleteLarge( a ) {
   if( ((a.length -1) in a) ) return -1;
   if( ((a.length -2) in a) ) return -2;

   a[ a.length -1 ] = 1;
   a[ a.length -2 ] = 2;
   
   if( !((a.length -1) in a) ) return -3;
   if( !((a.length -2) in a) ) return -4;

   a[ 0 ] = 0;
   a[ 1 ] = 1;
   
   if( !("0" in a) ) return -5;
   if( !("1" in a) ) return -6;

   return 0;
}

function run( proc, msg ) {
   let r = proc();
   assert.strictEqual( r, 0, msg + " [" + r + "]" );
}

run( () => forShrink( [ 1, 2, 3, 4, 5] ), "forShrink" );
run( () => forDelete( [ 1, 2, 3, 4, 5] ), "forDelete" );
run( () => forExpand( [ 1, 2, 3, 4, 5] ), "forExpand" );
run( () => vecExpand( [ 1, 2, 3, 4, 5] ), "vecExpand" );
run( () => findDelete( [ 1, 2, 3, 4, 5] ), "findDelete" );
run( () => foreachDelete( [ 1, 2, 3, 4, 5] ), "foreachDelete" );
run( () => deleteLarge( new Array( Math.pow( 2, 32 ) -1 ) ), "deleteLarge" );
