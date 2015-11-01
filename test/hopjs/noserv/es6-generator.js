/*=====================================================================*/
/*    .../project/hop/3.1.x/test/hopjs/noserv/es6-generator.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 30 17:54:07 2015                          */
/*    Last change :  Sun Nov  1 15:26:32 2015 (serrano)                */
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

/*---------------------------------------------------------------------*/
/*    nested                                                           */
/*---------------------------------------------------------------------*/
function* gen2a() {
   yield( yield 23 );
}

function* gen2b() {
   yield( 5 + (yield 23) );
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

/*---------------------------------------------------------------------*/
/*    cond                                                             */
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

console.log( "cond..." );

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

