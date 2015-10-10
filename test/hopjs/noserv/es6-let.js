/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/es6-let.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Sat Oct 10 20:30:03 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 let construct                             */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

var acc = ""

acc += "111 ";
var _ZZZ = (acc += "222 ",  123);
const ZZZ = (acc += "333", _ZZZ);

function simple( z, a ) {
   let xxx = 1;
   if( z > 0 ) {
      let f = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + ZZZ;
   }
}

function multiple( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); },
	  g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
}

function multipleReturn( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); },
	  g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
   // after the returns
   let ____ = 35;
}

function split( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); };
      let g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
}

function split2( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); };
      if( xxx > tttt ) tttt = g;
      let g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
}

function errForward() {
   let f = function( x ) { return h( x + 1 ); };
   f( 34 );
   let h = function( y ) { return y + 10; };
   
   return h( 45 );
}

function errForward2() {
   let f = function( x ) { return h( x + 1 ); };
   let h = function( y ) { return i( y ); };
   f( 34 );
   let i = function( y ) { return y + 10; };
   
   return h( 45 );
}

function arity() {
   var x = 0;
   
   let f = function( x, a, b ) { return x; }

   f( 0, x++, x++ );

   return x;
}

assert.strictEqual( acc, "111 222 333" );

assert.strictEqual( simple( 10, 20 ), -10 );
assert.strictEqual( simple( -10, 20 ), 1 + ZZZ );

assert.strictEqual( multiple( 10, 20 ), -10 );
assert.strictEqual( multiple( -10, 20 ), 3 );

assert.strictEqual( multipleReturn( 10, 20 ), -10 );
assert.strictEqual( multipleReturn( -10, 20 ), 3 );

assert.strictEqual( split( 10, 20 ), -10 );
assert.strictEqual( split( -10, 20 ), 3 );

assert.strictEqual( split2( 10, 20 ), -10 );
assert.strictEqual( split2( -10, 20 ), 3 );

assert.throws( errForward );
assert.throws( errForward2 );

assert.ok( arity() == 2 );
