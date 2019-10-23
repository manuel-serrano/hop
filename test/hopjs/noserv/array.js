/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/array.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Wed Oct 23 14:39:41 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing arrays                                                   */
/*=====================================================================*/
"use strict";
"use hopscript";

var assert = require( "assert" );

var s = [ 1, 2, 3, 4, 5 ];
var s2 = s.splice( 1, 0, 256 );

assert.deepEqual( s2, [], "splice empty" );
assert.deepEqual( s, [ 1, 256, 2, 3, 4, 5 ], "splice" );

function fun( x ) {
   return x;
}

var v = [];
v.push( 1 );

assert.ok( fun.apply( undefined, v ) === 1 );

var s3 = new Array( 5 );

assert.ok( !(0 in s3), "0 should not be in s3" );
assert.ok( !("0" in s3), "0 should not be in s3" );

var pp = {
   get x() { return -1 },
   set x( v ) { return undefined;}
}

var oo = {__proto__: pp};

oo.x = 3;

assert.ok( oo.x, -1 );

Object.defineProperty( oo, "x", { value: 100 } );

assert.ok( oo.x, 100, "oo.x" );

/*---------------------------------------------------------------------*/
/*    preventExtensions                                                */
/*---------------------------------------------------------------------*/
var o = [ 1, 2 ];
Object.preventExtensions( o );

assert.throws( () => o.push( 3 ), undefined, "push, object not extensible" );

var o2 = [ 1, 2 ];
Object.preventExtensions( o2 );

assert.throws( () => o2[ 2 ] = 3, undefined, "set, object not extensible" );

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

function expanderSparse( len ) {
   var a = new Array( 10 );
   a.length = len;
   expander( a, len );
   return a.length == len && a[ len - 1 ] == len -1 && a[ 10 ] == 10 && a[ 0 ] == 0;
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

assert.ok( expanderSparse( 20 ), "expand sparse.20" );
assert.ok( expanderSparse( 22 ), "expand sparse.22" );

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

var a1 = [ 1, 2 ];

Object.defineProperty( a1, "length", { value: 3, writable: false } );
assert.throws( () => a1.push( "toto" ), undefined, "ronly length on push" );

var a2 = [ 1, 2 ];

Object.defineProperty( a2, "length", { value: 2, writable: false } );
assert.throws( () => a2.push( "toto" ), undefined, "ronly length on push" );
assert.throws( () => a2.pop(), undefined, "ronly length on pop" );

var a3 = [ 1, 2 ];
Object.defineProperty( a1, "length", { value: 3, writable: false } );
a3.push = function( a ) { return true; };
a3.pop = function( a ) { return true; };

assert.ok( a3.push( "toto" ), "push overriden" );
assert.ok( a3.pop( "toto" ), "pop overriden" );

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
	 if( a.hasOwnProperty( "" + i ) ) return -4;
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
	 if( a.hasOwnProperty( "" + i ) ) return -4;
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
	 if( a.hasOwnProperty( "" + (l+1) ) ) return -4;
	 if( a[ l+1 ] !== undefined ) return -5;
	 if( a[ l+2 ] !== 4 ) { console.log( "a=", a, " v=", a[ l + 2 ] ); return -6; }
	 return 0;
      };
   }
}

function forExpand2( a ) {
   let l = a.length;

   for( let i = 0; i < l; i++ ) {
      if( i == 2 ) { a[ 4 ] = 3; }
      if( i in a && i == 4 && a[ i ] == 3 ) {
	 return 0;
      }
   }

   return -1;
}

function vecExpand( a ) {
   let l = a.length;
   if( !checkA( a, 5 ) ) return -1;
   
   if( !a[ l + 2 ] == undefined ) return -2;
   if( !a.length == l + 3 ) return -3;
   if( l in a ) return -4;
   if( Object.getOwnPropertyDescriptor( a, "" + l ) ) return -5;
   if( a.hasOwnProperty( "" + l ) ) return -6;
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
run( () => forExpand2( new Array( 5 ) ), "forExpand2" );
run( () => vecExpand( [ 1, 2, 3, 4, 5] ), "vecExpand" );
run( () => findDelete( [ 1, 2, 3, 4, 5] ), "findDelete" );
run( () => foreachDelete( [ 1, 2, 3, 4, 5] ), "foreachDelete" );
run( () => deleteLarge( new Array( Math.pow( 2, 32 ) -1 ) ), "deleteLarge" );

/*---------------------------------------------------------------------*/
/*    prototypes                                                       */
/*---------------------------------------------------------------------*/
function testProto( Array, msg ) {
   var a = new Array();

   assert.ok( a.__proto__ == Array.prototype, msg );

   a[ 1 ] = 2;
   var b = a.map( x => x );

   assert.ok( b.__proto__ == Array.prototype, msg );
}

testProto( Array, "default proto" );

assert.throws( function() {
   var ap = { name: "glop" };
   Array.prototype = ap;
}, undefined, "custom proto" );

/*---------------------------------------------------------------------*/
/*    overflow                                                         */
/*---------------------------------------------------------------------*/
var o = [ "x" ];

assert.ok( o[ 4294967296 ] == undefined );

/*---------------------------------------------------------------------*/
/*    push                                                             */
/*---------------------------------------------------------------------*/
var p = [];

[1,2,3,4,5,6,7,8,9,10].forEach( n => assert.ok( p.push( true ) === n ) );

function testCtorPush() {
   let a = new Array( 10 );
   
   let n = a.push( "foo" );
   return !Object.getOwnPropertyDescriptor( a, "0" ) && n === 11;
}

assert.ok( testCtorPush(), "CtorPush" );

/*---------------------------------------------------------------------*/
/*    holey                                                            */
/*---------------------------------------------------------------------*/
function holey() {
   var a = [];
   var k = 0; 

   a[ 1 ] = 3;

   for( var i = 0; i < a.length; i++ ) {
      k += i;
   }

   return k;
}

assert.ok( holey() === 1, "holey" );

/*---------------------------------------------------------------------*/
/*    expansion                                                        */
/*---------------------------------------------------------------------*/
function expansion() {
   var foo = [];

   foo[ 1 ] = true;
   foo[ 0 ] = true;
   foo[ 2 ] = true;

   return foo[ 2 ] ;
}

assert.ok( expansion(), "expansion" );

/*---------------------------------------------------------------------*/
/*    optimization                                                     */
/*---------------------------------------------------------------------*/
function optimInObject() {
   const a = new Array( 10 );
   a[ 0 ] = 10;
   
   a.fill = function() { }
   
   a.fill( 20 );
   
   return a[ 0 ] === 10;
}

function optimInProto() {
   const a = new Array( 10 );
   a[ 0 ] = 10;
   
   Array.prototype.fill = function() { }
   
   a.fill( 20 );
   
   return a[ 0 ] === 10;
}

function optimInNewProto() {
   const a = new Array( 10 );
   a[ 0 ] = 10;
   const p = { fill: function() {}, __proto__: Array.prototype };
   
   a.__proto__ = p;
   
   a.fill( 20 );
   
   return a[ 0 ] === 10;
}

assert.ok( optimInObject(), "optimInObject" );
assert.ok( optimInProto(), "optimInProto" );
assert.ok( optimInProto(), "optimInNewProto" );

/*---------------------------------------------------------------------*/
/*    extensible                                                       */
/*---------------------------------------------------------------------*/
function extensible() {
   var a = new Array();
   
   Object.defineProperty( a, "length", { value: 5, writable: false } );
   
   a[ 3 ] = 3;
   
   try { 
      a[ 6 ] = 6;
      return false;
   } catch( e ) {
      return a[ 3 ] === 3;
   }
}

assert.ok( extensible(), "extensible" );

/*---------------------------------------------------------------------*/
/*    prototype chain                                                  */
/*---------------------------------------------------------------------*/
function protoChain() {
   var a = new Array();
   var b = new Array();
	 
   Object.defineProperty( Object.prototype, "0", { 
      set: function( v ) { return; } 
   } );
   
   Object.defineProperty( Object.prototype, "1", { 
      value: 10, writable: false
   } );
   
   a[ 0 ] = 3;
   
   try {
      a[ 1 ] = 20;
      return false;
   } catch( e ) {
      return a[ 0 ] === undefined;
   }
}

function protoChainPush() {
   var a = new Array( 10 );
	 
   Object.defineProperty( Object.prototype, "10", { 
      set: function( v ) { return; }, get: function() { return -1 }
   } );
   
   Object.defineProperty( Object.prototype, "11", { 
      value: 10, writable: false
   } );
   
   a.push( 3 );

   try {
      a.push( 4 );
      return false;
   } catch( e ) {
      return a[ 10 ] === -1 && a.length === 11;
   }
}

assert.ok( protoChain(), "protoChain" );
assert.ok( protoChainPush(), "protoChainPush" );

function testConstant() {
   
   function funvec( v ) {
      v[ 2 ] = 3.6;
   }
   for( let i = 0; i < 10; i++ ) {
      const o = [1, 2, 34];
      if( i == 3 ) return o[ 2 ] === 34;
      funvec( o );
   }
}

assert.ok( testConstant(), "testConstant" );

/*---------------------------------------------------------------------*/
/*    splice                                                           */
/*---------------------------------------------------------------------*/
function testSplice() {
   var o = [ 34, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 8, 7 ];
   var o2 = o.splice( 10 );
   
   return o.length == 10 && o2.length == 5;
}

assert.ok( testSplice(), "testSplice" );
