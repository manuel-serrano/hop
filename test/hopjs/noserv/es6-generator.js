/*=====================================================================*/
/*    .../project/hop/3.1.x/test/hopjs/noserv/es6-generator.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 30 17:54:07 2015                          */
/*    Last change :  Fri Nov  6 12:54:06 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 generators                                */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    g ...                                                            */
/*---------------------------------------------------------------------*/
var g;

/*---------------------------------------------------------------------*/
/*    equal ...                                                        */
/*---------------------------------------------------------------------*/
function equal( v1, v2 ) {
   return v1.value == v2.value && v1.done == v2.done;
}

/*---------------------------------------------------------------------*/
/*    bar                                                              */
/*---------------------------------------------------------------------*/
function bar( x ) {
   return x + 1;
}

function bar2( x, y ) {
   return x - y;
}
   
/*---------------------------------------------------------------------*/
/*    basic                                                            */
/*---------------------------------------------------------------------*/
function* gen1a() {
   yield bar( 1 );
}

function* gen1b() {
   yield bar( 1 );
   yield bar( 2 );
}

function* gen1c( i ) {
   yield bar( i );
   yield bar( 2 );
}

function* gen1d( i ) {
   yield bar( i );
   {
      i++;
      yield bar( 2 );
      yield i;
   }
}

function* gen1e( i ) {
   yield i + 1;
}

function* gen1f()  {
   yield;
}

console.log( "basic..." );

console.log( "   gen1a()" );
g = gen1a();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen1b()" );
g = gen1b();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen1c(10)" );
g = gen1c( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen1d(10)" );
g = gen1d( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen1e(10)" );
g = gen1e( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen1f()" );
g = gen1f();
assert.ok( equal( g.next(), { value: undefined, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    nested                                                           */
/*---------------------------------------------------------------------*/
function* gen2a() {
   yield( yield 23 );
}

function* gen2b() {
   yield( 5 + (yield 23) );
}

function* gen2c( i ) {
   yield( yield( yield i ) );
}

function* gen2d( i ) {
   return( yield i );
}

function* gen2e( i ) {
   return( 1 + (yield i) );
}

function* gen2f( i ) {
   return bar2( yield i + 1, 3 );
}

console.log( "nested..." );

console.log( "   gen2a()" );
g = gen2a();
assert.ok( equal( g.next(), { value: 23, done: false } ) );
assert.ok( equal( g.next( 10 ), { value: 10, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen2b()" );
g = gen2b();
assert.ok( equal( g.next(), { value: 23, done: false } ) );
assert.ok( equal( g.next( 10 ), { value: 15, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen2d()" );
g = gen2d( 10 );
assert.ok( equal( g.next( 1 ), { value: 10, done: false } ) );
assert.ok( equal( g.next( 2 ), { value: 2, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen2e()" );
g = gen2e( 10 );
assert.ok( equal( g.next( 1 ), { value: 10, done: false } ) );
assert.ok( equal( g.next( 2 ), { value: 3, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen2f()" );
g = gen2f( 10 );
assert.ok( equal( g.next( 1 ), { value: 11, done: false } ) );
assert.ok( equal( g.next( 2 ), { value: -1, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    if                                                               */
/*---------------------------------------------------------------------*/
function *gen3a( i ) {
   x = 1111;
   yield i;
   if( i > 10 ) {
      yield 2222;
   }
}

function *gen3b( i ) {
   x = 1111;
   yield i;
   if( i > 10 ) {
      yield 2222;
   }
   yield 3333;
   yield 4444;
}

function *gen3c( i ) {
   x = 1111;
   yield i;
   if( i > 10 ) {
      yield 2222;
   }
   if( i > 2 ) {
      yield 3333;
   }
   yield 4444;
   yield 5555;
}

function *gen3d( i ) {
   x = 1111;
   yield i;
   if( i > 10 ) {
      yield 2222;
      yield 3333;
   }
   yield 4444;
   yield 5555;
}

function *gen3e( i ) {
   x = 1111;
   yield i;
   if( i > 10 ) {
   } else {
      yield 2222;
   }
   yield 3333;
   yield 4444;
}

console.log( "if..." );

console.log( "   gen3a(5)" );
g = gen3a( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3a(11)" );
g = gen3a( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3b(5)" );
g = gen3b( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3b(11)" );
g = gen3b( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3c(5)" );
g = gen3c( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3c(11)" );
g = gen3c( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3d(5)" );
g = gen3d( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3d(11)" );
g = gen3d( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen3e(5)" );
g = gen3e( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
console.log( "   gen3e(11)" );
g = gen3e( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
   
/*---------------------------------------------------------------------*/
/*    assig ...                                                        */
/*---------------------------------------------------------------------*/
var glo;

function* gen4a() {
   glo = yield bar( 1 );
}

function* gen4b() {
   var x = yield bar( 1 );
   yield x;
}

console.log( "assig..." );

console.log( "   gen4a()" );
g = gen4a();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 23 ), { value: undefined, done: true } ) );
assert.ok( equal( glo, 23 ) );

console.log( "   gen4b()" );
g = gen4b();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 14 ), { value: 14, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    function call ...                                                */
/*---------------------------------------------------------------------*/
function* gen5a() {
   var f = yield bar( 1 );
   yield f( 2 );
}

function* gen5b() {
   yield (yield bar( 1 ))( 2 );
}

function* gen5c( i ) {
   yield bar( yield i );
}

function* gen5d( i ) {
   yield bar2( yield i, yield 45 );
}

console.log( "function call..." );

console.log( "   gen5a()" );
g = gen5a();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( bar ), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen5b()" );
g = gen5b();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( bar ), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen5c(2)" );
g = gen5c( 2 );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 20 ), { value: 21, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen5d(2)" );
g = gen5d( 2 );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 8 ), { value: 45, done: false } ) );
assert.ok( equal( g.next( 9 ), { value: -1, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    sequence                                                         */
/*---------------------------------------------------------------------*/
function* gen6() {
   yield (1, 2, 3);
   (yield 4, yield 5, yield 6);
}

console.log( "sequence..." );

console.log( "   gen6()" );
g = gen6();
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 6, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    cond ...                                                         */
/*---------------------------------------------------------------------*/
function* gen7a( i ) {
   yield (i > 2 ? 1 : 3);
}

function* gen7b( i ) {
   yield (i > 2 ? 1 : 3);
   yield 2;
}

function* gen7c( i ) {
   yield( i > 2 ? yield 1 : yield 3 );
}

function* gen7d( i ) {
   yield 0;
   yield( i > 2 ? yield 1 : yield 3 );
   yield 4;
}

function* gen7e() {
   (yield( 0 ) > 2) ? yield 1 : yield 3;
   yield 4;
}

function* gen7f() {
   (yield( 0 ) > 2) ? (yield 1, yield 11) : (yield 3, yield 33);
   yield 4;
}

console.log( "cond..." );

console.log( "   gen7a(1)" );
g = gen7a(1);
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7a(3)" );
g = gen7a(3);
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7b(1)" );
g = gen7b(1);
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7b(3)" );
g = gen7b(3);
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7c(1)" );
g = gen7c(1);
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next( 14 ), { value: 14, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7c(3)" );
g = gen7c(3);
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next( 14 ), { value: 14, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7d(1)" );
g = gen7d(1);
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(5), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7d(3)" );
g = gen7d(3);
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(5), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7e()" );
g = gen7e();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(3), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen7e()" );
g = gen7f();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(3), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    for ...                                                          */
/*---------------------------------------------------------------------*/
function* gen8a() {
   for( var i = 0; i < 3; i++ ) {
      yield i;
   }
}

function* gen8b() {
   var k =  10;
   for( var i = 0; i < 3; i = yield( i ) ) {
      k++;
   }
   return k;
}

function* gen8c() {
   var k =  10;
   for( var i = 0; (yield( i )) < 3; i++ ) {
      k++;
   }
   return k;
}

function* gen8d() {
   var k =  10;
   for( var i = 0; yield( i ) < 3; i++ ) {
      k++;
   }
   return k;
}

function* gen8e() {
   for( var i = 0; i < 3; i++ ) {
      yield i;
   }
   yield i;
}

function* gen8f( j ) {
   var k =  10;
   for( var i = 0; i < 10; i++ ) {
      yield i;
      if( i >= j ) {
	 break;
      }
   }
   return k;
}

function* gen8g( j ) {
   var i;
   for( (yield), i = 0; i < 3; i++ ) {
      yield i;
   }
   yield i;
}
	
function* gen8h() {
   var i;
   for( i = 0; i < 3; i++ ) {
      yield i;
      continue;
   }
}
	
console.log( "for..." );

console.log( "   gen8a()" );
g = gen8a();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen8b()" );
g = gen8b();
assert.ok( equal( g.next(1), { value: 0, done: false } ) );
assert.ok( equal( g.next(2), { value: 2, done: false } ) );
assert.ok( equal( g.next(3), { value: 12, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen8c()" );
g = gen8c();
assert.ok( equal( g.next(1), { value: 0, done: false } ) );
assert.ok( equal( g.next(2), { value: 1, done: false } ) );
assert.ok( equal( g.next(3), { value: 11, done: true } ) );
assert.ok( equal( g.next(4), { value: undefined, done: true } ) );
assert.ok( equal( g.next(5), { value: undefined, done: true } ) );

console.log( "   gen8d()" );
g = gen8d();
assert.ok( equal( g.next(1), { value: true, done: false } ) );
assert.ok( equal( g.next(2), { value: true, done: false } ) );
assert.ok( equal( g.next(3), { value: true, done: false } ) );
assert.ok( equal( g.next(4), { value: false, done: false } ) );
assert.ok( equal( g.next(), { value: 13, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen8e()" );
g = gen8e();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen8f()" );
g = gen8f( 2 );
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 10, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen8g()" );
g = gen8g( 2 );
assert.ok( equal( g.next(), { value: undefined, done: false } ) );
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen8h()" );
g = gen8h();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
