/*=====================================================================*/
/*    .../project/hop/3.1.x/test/hopjs/noserv/es6-generator.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 30 17:54:07 2015                          */
/*    Last change :  Sat Jan  2 07:32:14 2016 (serrano)                */
/*    Copyright   :  2015-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 generators                                */
/*=====================================================================*/
"use strict";

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
/*    seq ...                                                          */
/*---------------------------------------------------------------------*/
function* seq1() {
   yield bar( 1 );
}

function* seq2() {
   yield bar( 1 );
   yield bar( 2 );
}

function* seq3( i ) {
   yield bar( i );
   yield bar( 2 );
}

function* seq4( i ) {
   yield bar( i );
   {
      i++;
      yield bar( 2 );
      yield i;
   }
}

function* seq5( i ) {
   yield i + 1;
}

function* seq6()  {
   yield;
}

function* seq7( i )  {
   i++;
   {
      { i += yield i; }
      i++;
   }
   return i;
}

function* seq8( i )  {
   {
      { yield i; }
      i++;
      i++;
   }
   return i;
}

function* seq9( i )  {
   {
      { i += (yield i++); }
   }
   return i;
}

function* seq10( i )  {
   i++;
   yield bar( i );
   {
      i++;
      yield bar( 2 );
      yield i;
   }
   i++;
   {
      { yield i; }
      i++;
   }
   return i;
}

function* seq11( i )  {
   i++;
   yield i;
}

console.log( "seq..." );

console.log( "   seq1()" );
g = seq1();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq2()" );
g = seq2();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq3(10)" );
g = seq3(10);
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq4(10)" );
g = seq4( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq5(10)" );
g = seq5( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq6()" );
g = seq6();
assert.ok( equal( g.next(), { value: undefined, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq7(10)" );
g = seq7(10);
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(23), { value: 35, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq8(10)" );
g = seq8(10);
assert.ok( equal( g.next(), { value: 10, done: false } ) );
assert.ok( equal( g.next(), { value: 12, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq9(10)" );
g = seq9(10);
assert.ok( equal( g.next(), { value: 10, done: false } ) );
assert.ok( equal( g.next(23), { value: 34, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq10(10)" );
g = seq10(10);
assert.ok( equal( g.next(), { value: 12, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 12, done: false } ) );
assert.ok( equal( g.next(), { value: 13, done: false } ) );
assert.ok( equal( g.next(), { value: 14, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   seq11(10)" );
g = seq11(10);
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    basic                                                            */
/*---------------------------------------------------------------------*/
function* basic1() {
   yield bar( 1 );
}

function* basic2() {
   yield bar( 1 );
   yield bar( 2 );
}

function* basic3( i ) {
   yield bar( i );
   yield bar( 2 );
}

function* basic4( i ) {
   yield bar( i );
   {
      i++;
      yield bar( 2 );
      yield i;
   }
}

function* basic5( i ) {
   yield i + 1;
}

function* basic6()  {
   yield;
}

function* basic7() {
   return (yield 1, yield 2);
}

function* basic8() {
   return bar2( yield 1, yield 2 );
}

function* basic9() {
   var x = 1 + (2, yield 3);
   return x;
}

function* basic10( i ) {
   var x = ((yield 1), i);
}

console.log( "basic..." );

console.log( "   basic1()" );
g = basic1();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   basic2()" );
g = basic2();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   basic3(10)" );
g = basic3( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   basic4(10)" );
g = basic4( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   basic5(10)" );
g = basic5( 10 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   basic6()" );
g = basic6();
assert.ok( equal( g.next(), { value: undefined, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   basic7()" );
g = basic7();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 3 ), { value: 3, done: true } ) );

console.log( "   basic8()" );
g = basic8();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next( 10 ), { value: 2, done: false } ) );
assert.ok( equal( g.next( 20 ), { value: -10, done: true } ) );

console.log( "   basic9()" );
g = basic9();
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next( 4 ), { value: 5, done: true } ) );

console.log( "   basic10()" );
g = basic10();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    nested                                                           */
/*---------------------------------------------------------------------*/
function* nested1() {
   yield( yield 23 );
}

function* nested2() {
   yield( 5 + (yield 23) );
}

function* nested3( i ) {
   yield( yield( yield i ) );
}

function* nested4( i ) {
   return( yield i );
}

function* nested5( i ) {
   return( 1 + (yield i) );
}

function* nested6( i ) {
   return bar2( yield i + 1, 3 );
}

console.log( "nested..." );

console.log( "   nested1()" );
g = nested1();
assert.ok( equal( g.next(), { value: 23, done: false } ) );
assert.ok( equal( g.next( 10 ), { value: 10, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   nested1()" );
g = nested2();
assert.ok( equal( g.next(), { value: 23, done: false } ) );
assert.ok( equal( g.next( 10 ), { value: 15, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   nested3()" );
g = nested3( 10 );
assert.ok( equal( g.next( 1 ), { value: 10, done: false } ) );
assert.ok( equal( g.next( 2 ), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   nested4()" );
g = nested4( 10 );
assert.ok( equal( g.next( 1 ), { value: 10, done: false } ) );
assert.ok( equal( g.next( 2 ), { value: 2, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   nested5()" );
g = nested5( 10 );
assert.ok( equal( g.next( 1 ), { value: 10, done: false } ) );
assert.ok( equal( g.next( 2 ), { value: 3, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   nested6()" );
g = nested6( 10 );
assert.ok( equal( g.next( 1 ), { value: 11, done: false } ) );
assert.ok( equal( g.next( 2 ), { value: -1, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    if                                                               */
/*---------------------------------------------------------------------*/
function *ifa( i ) {
   var x = 1111;
   yield i;
   if( i > 10 ) {
      yield 2222;
   }
}

function *ifb( i ) {
   var x = 1111;
   yield i;
   if( i > 10 ) {
      yield 2222;
   }
   yield 3333;
   yield 4444;
}

function *ifc( i ) {
   var x = 1111;
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

function *ifd( i ) {
   var x = 1111;
   yield i;
   if( i > 10 ) {
      yield 2222;
      yield 3333;
   }
   yield 4444;
   yield 5555;
}

function *ife( i ) {
   var x = 1111;
   yield i;
   if( i > 10 ) {
   } else {
      yield 2222;
   }
   yield 3333;
   yield 4444;
}

function* iff( i ) {
   if( (yield( i )) < 3 ) i = 45; else i = 55;
   return i;
}

function *ifg( i ) {
   var x = 1111;
   yield i;
   if( i <= 10 ) {
   } else {
      yield 2222;
   }
}

console.log( "if..." );

console.log( "   ifa(5)" );
g = ifa( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifa(11)" );
g = ifa( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifb(5)" );
g = ifb( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifb(11)" );
g = ifb( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifc(5)" );
g = ifc( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifc(11)" );
g = ifc( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifd(5)" );
g = ifd( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifd(11)" );
g = ifd( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ife(5)" );
g = ife( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ife(11)" );
g = ife( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   iff(10)" );
g = iff( 10 );
assert.ok( equal( g.next(), { value: 10, done: false } ) );
assert.ok( equal( g.next(), { value: 55, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifg(5)" );
g = ifg( 5 );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   ifg(11)" );
g = ifg( 11 );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    assig ...                                                        */
/*---------------------------------------------------------------------*/
var glo;

function* assiga() {
   glo = yield bar( 1 );
}

function* assigb() {
   var x = yield 2;
   yield x;
}

function* assigc() {
   var x = yield bar( 1 );
   yield x;
}

console.log( "assig..." );

console.log( "   assiga()" );
g = assiga();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 23 ), { value: undefined, done: true } ) );
assert.ok( equal( glo, 23 ) );

console.log( "   assigb()" );
g = assigb();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 14 ), { value: 14, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   assigc()" );
g = assigc();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 14 ), { value: 14, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    function call ...                                                */
/*---------------------------------------------------------------------*/
function* calla() {
   var f = yield bar( 1 );
   yield f( 2 );
}

function* callb() {
   yield (yield bar( 1 ))( 2 );
}

function* callc( i ) {
   yield bar( yield i );
}

function* calld( i ) {
   yield bar2( yield i, yield 45 );
}

console.log( "call..." );

console.log( "   calla()" );
g = calla();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( bar ), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   callb()" );
g = callb();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( bar ), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   callc(2)" );
g = callc( 2 );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 20 ), { value: 21, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   calld(2)" );
g = calld( 2 );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next( 8 ), { value: 45, done: false } ) );
assert.ok( equal( g.next( 9 ), { value: -1, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    sequence                                                         */
/*---------------------------------------------------------------------*/
function* seqa() {
   yield (1, 2, 3);
   (yield 4, yield 5, yield 6);
}

console.log( "sequence..." );

console.log( "   seqa()" );
g = seqa();
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 6, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    cond ...                                                         */
/*---------------------------------------------------------------------*/
function* conda( i ) {
   yield (i > 2 ? 1 : 3);
}

function* condb( i ) {
   yield (i > 2 ? 1 : 3);
   yield 2;
}

function* condc( i ) {
   yield( i > 2 ? yield 1 : yield 3 );
}

function* condd( i ) {
   yield 0;
   yield( i > 2 ? yield 1 : yield 3 );
   yield 4;
}

function* conde() {
   (yield( 0 ) > 2) ? yield 1 : yield 3;
   yield 4;
}

function* condf() {
   (yield( 0 ) > 2) ? (yield 1, yield 11) : (yield 3, yield 33);
   yield 4;
}

console.log( "cond..." );

console.log( "   conda(1)" );
g = conda(1);
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   conda(3)" );
g = conda(3);
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   condb(1)" );
g = condb(1);
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   condb(3)" );
g = condb(3);
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   condc(1)" );
g = condc(1);
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next( 14 ), { value: 14, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   condc(3)" );
g = condc(3);
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next( 14 ), { value: 14, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   condd(1)" );
g = condd(1);
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(5), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   condd(3)" );
g = condd(3);
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(5), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   conde()" );
g = conde();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(3), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   conde()" );
g = condf();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(3), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 11, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    for ...                                                          */
/*---------------------------------------------------------------------*/
function* fora() {
   for( var i = 0; i < 3; i++ ) {
      yield i;
   }
}

function* forb() {
   var k =  10;
   for( var i = 0; i < 3; i = yield( i ) ) {
      k++;
   }
   return k;
}

function* forc() {
   var k =  10;
   for( var i = 0; (yield( i )) < 3; i++ ) {
      k++;
   }
   return k;
}

function* ford() {
   var k =  10;
   for( var i = 0; yield( i ) < 3; i++ ) {
      k++;
   }
   return k;
}

function* fore() {
   for( var i = 0; i < 3; i++ ) {
      yield i;
   }
   yield i;
}

function* forf( j ) {
   var k =  10;
   for( var i = 0; i < 10; i++ ) {
      yield i;
      if( i >= j ) {
	 break;
      }
   }
   return k;
}

function* forg( j ) {
   var i;
   for( (yield), i = 0; i < 3; i++ ) {
      yield i;
   }
   yield i;
}

function* forh() {
   var i;
   for( i = 0; i < 3; i++ ) {
      yield i;
      continue;
   }
}

console.log( "for..." );

console.log( "   fora()" );
g = fora();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forb()" );
g = forb();
assert.ok( equal( g.next(1), { value: 0, done: false } ) );
assert.ok( equal( g.next(2), { value: 2, done: false } ) );
assert.ok( equal( g.next(3), { value: 12, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forc()" );
g = forc();
assert.ok( equal( g.next(1), { value: 0, done: false } ) );
assert.ok( equal( g.next(2), { value: 1, done: false } ) );
assert.ok( equal( g.next(3), { value: 11, done: true } ) );
assert.ok( equal( g.next(4), { value: undefined, done: true } ) );
assert.ok( equal( g.next(5), { value: undefined, done: true } ) );

console.log( "   ford()" );
g = ford();
assert.ok( equal( g.next(1), { value: true, done: false } ) );
assert.ok( equal( g.next(2), { value: true, done: false } ) );
assert.ok( equal( g.next(3), { value: true, done: false } ) );
assert.ok( equal( g.next(4), { value: false, done: false } ) );
assert.ok( equal( g.next(), { value: 13, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   fore()" );
g = fore();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forf()" );
g = forf( 2 );
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 10, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forg()" );
g = forg( 2 );
assert.ok( equal( g.next(), { value: undefined, done: false } ) );
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forh()" );
g = forh();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    while                                                            */
/*---------------------------------------------------------------------*/
function* whilea() {
   var i = 0;

   while( i < 3 ) {
      yield i;
      i++;
   }
}

function* whileb() {
   var i = 0;
   var k =  10;

   while( i < 3 ) {
      i = yield i;
      k++;
   }
   return k;
}

function* whilec() {
   var i = 0;

   while( i < 3 ) {
      yield i++;
   }
   yield i;
}

function* whiled( j ) {
   var k =  10;
   var i = 0;

   while( i < 10 ) {
      yield i;
      if( i >= j ) {
	 break;
      }
      i++;
   }
   return k;
}

function* whilee() {
   var i = 0;

   while( i < 3 ) {
      yield i;
      i++;
      continue;
   }
}

function* whilef() {
   var i = 0;

   while( yield i ) {
      i++;
   }
}

function* whileg() {
   var i = 0;
   var k = 0;

   while( i < 3 ) {
      yield i;
      i++;

      if( i > 1 ) {
	 continue;
      }

      k++;
   }

   return k;
}

console.log( "while..." );

console.log( "   whilea()" );
g = whilea();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   whileb()" );
g = whileb();
assert.ok( equal( g.next(1), { value: 0, done: false } ) );
assert.ok( equal( g.next(2), { value: 2, done: false } ) );
assert.ok( equal( g.next(3), { value: 12, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   whilec()" );
g = whilec();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   whiled()" );
g = whiled( 2 );
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 10, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   whilee()" );
g = whilee();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   whilef()" );
g = whilef();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(1), { value: 1, done: false } ) );
assert.ok( equal( g.next(2), { value: 2, done: false } ) );
assert.ok( equal( g.next(3), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   whileg()" );
g = whileg();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(1), { value: 1, done: false } ) );
assert.ok( equal( g.next(2), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    do                                                               */
/*---------------------------------------------------------------------*/
function* doa() {
   var i = 0;

   do {
      yield i;
      i++;
   } while( i < 3 )
}

function* dob() {
   var i = 0;
   var k =  10;

   do {
      i = yield i;
      k++;
   } while( i < 3 )
   return k;
}

function* doc() {
   var i = 0;

   do {
      yield i++;
   } while( i < 3 )
   yield i;
}

function* dod( j ) {
   var k =  10;
   var i = 0;

   do {
      yield i;
      if( i >= j ) {
	 break;
      }
      i++;
   } while( i < 10 )
   return k;
}

function* doe() {
   var i = 0;

   do {
      yield i;
      i++;
      continue;
   } while( i < 3 )
}

function* dof() {
   var i = 0;

   do {
      i++;
   } while( yield i )
}

console.log( "do..." );

console.log( "   doa()" );
g = doa();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   dob()" );
g = dob();
assert.ok( equal( g.next(1), { value: 0, done: false } ) );
assert.ok( equal( g.next(2), { value: 2, done: false } ) );
assert.ok( equal( g.next(3), { value: 12, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   doc()" );
g = doc();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   dod()" );
g = dod( 2 );
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 10, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   doe()" );
g = doe();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   dof()" );
g = dof();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(1), { value: 2, done: false } ) );
assert.ok( equal( g.next(2), { value: 3, done: false } ) );
assert.ok( equal( g.next(3), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    let ...                                                          */
/*---------------------------------------------------------------------*/
function* gen11a() {
   "use strict";
   let x = yield 0;
   let y = yield 1;
   yield 2;
   yield 3;
}

function* gen11b() {
   "use strict";
   let xxx = yield 0;
   let yyy = 1;
   let zzz = yield yyy;
   yield 2;
   yield 3;
}

function* gen11c() {
   "use strict";
   let aaa = 0;
   let xxx = yield aaa;
   let yyy = 1;
   let zzz = yield yyy;
   yield 2;
   yield 3;
}

function* gen11d() {
   "use strict";
   let aaa = 0;
   let xxx = yield aaa;
   let yyy = xxx;
   let zzz = yield yyy;
   yield 2;
   yield 3;
}

console.log( "let..." );

console.log( "   gen11a()" );
g = gen11a();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen11b()" );
g = gen11b();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen11c()" );
g = gen11c();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   gen11d()" );
g = gen11d();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next( 1 ), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    const ...                                                        */
/*---------------------------------------------------------------------*/
function* consta() {
   "use strict";
   const x = yield 0;
   const y = yield 1;
   yield 2;
   yield 3;
}

function* constb() {
   "use strict";
   const xxx = yield 0;
   const yyy = 1;
   const zzz = yield yyy;
   yield 2;
   yield 3;
}

function* constc() {
   "use strict";
   const aaa = 0;
   const xxx = yield aaa;
   const yyy = 1;
   const zzz = yield yyy;
   yield 2;
   yield 3;
}

function* constd() {
   "use strict";
   const aaa = 0;
   const xxx = yield aaa;
   const yyy = xxx;
   const zzz = yield yyy;
   yield 2;
   yield 3;
}

console.log( "const..." );

console.log( "   consta()" );
g = consta();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   constb()" );
g = constb();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   constc()" );
g = constc();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   constd()" );
g = constd();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next( 1 ), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    try                                                              */
/*---------------------------------------------------------------------*/
function* trya() {
   try {
      1;
   } catch( e ) {
      ;
   } finally {
      yield 0;
      yield 1;
      yield 2;
   }
   return 3;
}

function* tryb() {
   try {
      yield 0;
   } catch( e ) {
      ;
   } finally {
      yield 1;
      yield 2;
   }
   return 3;
}

function* tryc() {
   try {
      throw 1;
   } catch( e ) {
      yield 0;
      yield 1;
   } finally {
      yield 2;
      yield 3;
   }
   return 4;
}

function* tryd() {
   try {
      throw yield 0;
   } catch( e ) {
      yield 1;
      yield 2;
   } finally {
      yield 3;
      yield 4;
   }
   return 5;
}

function* trye() {
   try {
      throw 14;
      yield 0;
   } catch( e ) {
      yield 2;
   }
   return 3;
}

function* tryf() {
   try {
      throw 14;
      yield 0;
   } catch( e ) {
      yield 2;
   } finally {
      yield 1;
   }
   return 3;
}

function* tryg() {
   try {
      yield 1111;
      yield 2222;
   } finally {
      yield 3333;
      yield 4444
   }
   return 5555;
}

function* tryh() {
   try {
      try {
	 yield 1111;
	 throw 3;
      } finally {
      }
   } catch( _ ) {
      ;
   }
   return 5555;
}

function* tryi() {
   try {
      yield 1111;
      throw 4;
      yield 2222;
   } catch( _ ) {
      ;
   } finally {
      yield 3333;
      yield 4444
   }
   return 5555;
}

function* tryj() {
   try {
      yield 1111;
      yield 2222;
      yield 3333;
      throw 4;
   } catch( _ ) {
      yield 4444;
   } finally {
      yield 5555;
   }
   yield 6666;
   return 7777;
}

console.log( "try..." );

console.log( "   trya()" );
g = trya();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryb()" );
g = tryb();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryc()" );
g = tryc();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryd()" );
g = tryd();
assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: 5, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   trye()" );
g = trye();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryf()" );
g = tryf();
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryg()" );
g = tryg();
assert.ok( equal( g.next(), { value: 1111, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryh()" );
g = tryh();
assert.ok( equal( g.next(), { value: 1111, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryi()" );
g = tryi();
assert.ok( equal( g.next(), { value: 1111, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   tryj()" );
g = tryj();
assert.ok( equal( g.next(), { value: 1111, done: false } ) );
assert.ok( equal( g.next(), { value: 2222, done: false } ) );
assert.ok( equal( g.next(), { value: 3333, done: false } ) );
assert.ok( equal( g.next(), { value: 4444, done: false } ) );
assert.ok( equal( g.next(), { value: 5555, done: false } ) );
assert.ok( equal( g.next(), { value: 6666, done: false } ) );
assert.ok( equal( g.next(), { value: 7777, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    switch                                                           */
/*---------------------------------------------------------------------*/
function* switcha() {
   var v = 0;
   switch( yield 1 ) {
      case 10: v++;
      case 20: v++;
      default: v++;
   }
   return v;
}

function* switchb() {
   var v = 0;
   switch( yield 1 ) {
      case 10: v += 1; break;
      case 20: v += 3; break;
      default: v += 7;
   }
   return v;
}

function* switchc() {
   switch( yield 1 ) {
      case 10: return 100;
      case 20: return 200;
      default: return 300;
   }
}

function* switchd() {
   switch( yield 1 ) {
      case 10: return 100;
      case 20: yield 200; return 201;
      default: return 300;
   }
}

function* switche( x ) {
   switch( x ) {
      case 14: return 24;
      case yield 1: return 25;
      case 16: return 26;
   }
}

function* switchf( x ) {
   switch( x ) {
      case 14: return 24;
      case yield 1: break
      case 16: return 26;
      default: return 30;
   }
   return 25;
}

function* switchg( x ) {
   switch( x ) {
      case 14: return 24;
      case yield 1: break
      case 16: return 26;
      default: return 30;
   }
   return 25;
}

console.log( "switch" );

console.log( "   switcha()" );
g = switcha();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(20), { value: 2, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   switchb()" );
g = switchb();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(20), { value: 3, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   switchc()" );
g = switchc();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(20), { value: 200, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   switchd()" );
g = switchd();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(20), { value: 200, done: false } ) );
assert.ok( equal( g.next(), { value: 201, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   switche()" );
g = switche( 15 );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(20), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

g = switche( 15 );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(15), { value: 25, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   switchf()" );
g = switchf( 15 );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(15), { value: 25, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

g = switchf( 15 );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(20), { value: 30, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   switchg()" );
g = switchg( 15 );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(15), { value: 25, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

g = switchg( 15 );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(20), { value: 30, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    for..in                                                          */
/*---------------------------------------------------------------------*/
function* forina( x ) {
   for( var y in x ) {
      yield y;
   }
}

function* forinb() {
   for( var y in yield 1 ) {
      yield y;
   }
}

console.log( "forin..." );

console.log( "   forina()" );
g = forina( { a: 15, b: 20 } );
assert.ok( equal( g.next(), { value: 'a', done: false } ) );
assert.ok( equal( g.next(), { value: 'b', done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forina2()" );
g = forina( [1, 2, 3] );
assert.ok( equal( g.next(), { value: '0', done: false } ) );
assert.ok( equal( g.next(), { value: '1', done: false } ) );
assert.ok( equal( g.next(), { value: '2', done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forinb()" );
g = forinb();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next( { a: 15, b: 20 } ), { value: 'a', done: false } ) );
assert.ok( equal( g.next(), { value: 'b', done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   forina2()" );
g = forinb();
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next( [1, 2, 3] ), { value: '0', done: false } ) );
assert.ok( equal( g.next(), { value: '1', done: false } ) );
assert.ok( equal( g.next(), { value: '2', done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function* mdna() {
   var index = 0;

   while( index < 3 )
      yield index++;
}

function* mdnb1() {
  yield 2;
  yield 3;
  yield 4;
}

function* mdnb() {
  yield 1;
  yield* mdnb1();
  yield 5;
}

function* mdnc() {
  yield* [1, 2, 3];
  return "foo";
}

var mdncresult;

function* mdnc2() {
  mdncresult = yield* mdnc();
}

function* mdnd() {
  yield* [1, 2];
  yield* "34";
  yield* Array.from( arguments );
}

var string = 'A\uD835\uDC68C';
var mdne = string[Symbol.iterator]();

console.log( "mdn..." );

console.log( "   mdna()" );
g = mdna();

assert.ok( equal( g.next(), { value: 0, done: false } ) );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   mdnb()" );
g = mdnb();

assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   mdnc()" );
g = mdnc();

assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: "foo", done: true } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   mdnc2()" );
g = mdnc2();

assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );
assert.ok( equal( mdncresult, "foo" ) );

console.log( "   mdnd()" );
g = mdnd( 5, 6 );
assert.ok( equal( g.next(), { value: 1, done: false } ) );
assert.ok( equal( g.next(), { value: 2, done: false } ) );
assert.ok( equal( g.next(), { value: 3, done: false } ) );
assert.ok( equal( g.next(), { value: 4, done: false } ) );
assert.ok( equal( g.next(), { value: 5, done: false } ) );
assert.ok( equal( g.next(), { value: 6, done: false } ) );
assert.ok( equal( g.next(), { value: undefined, done: true } ) );

console.log( "   mdne()");
assert.ok( equal( mdne.next(), { value: "A", done: false } ) );
assert.ok( equal( mdne.next(), { value: "\uD835\uDC68", done: false } ) );
assert.ok( equal( mdne.next(), { value: "C", done: false } ) );
assert.ok( equal( mdne.next(), { value: undefined, done: true } ) );
assert.ok( equal( mdne.next(), { value: undefined, done: true } ) );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*    -------------------------------------------------------------    */
/*    http://kangax.github.io/compat-table/es6/                        */
/*---------------------------------------------------------------------*/
function kangaxa() {
   function *generator() {
      yield 5; yield 6;
   };
   
   var iterator = generator();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxb() {
   var generator = function * (){
      yield 5; yield 6;
   };
   var iterator = generator();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxc() {
   function * generator(){
      yield this.x; yield this.y;
   };
   var iterator = { g: generator, x: 5, y: 6 }.g();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxd() {
   function * generator(){
      yield this.x; yield this.y;
   };
   try {
      (new generator()).next();
   }
   catch (e) {
      return true;
   }
}

function kangaxe() {
   var sent;
   function * generator(){
      sent = [yield 5, yield 6];
   };
   var iterator = generator();
   iterator.next();
   iterator.next("foo");
   iterator.next("bar");
   return sent[0] === "foo" && sent[1] === "bar";
}

function kangaxf() {
   function * generatorFn(){}
   var ownProto = Object.getPrototypeOf(generatorFn());
   var passed = ownProto === generatorFn.prototype;

   var sharedProto = Object.getPrototypeOf(ownProto);
   passed &= sharedProto !== Object.prototype &&
      sharedProto === Object.getPrototypeOf(function*(){}.prototype) &&
      sharedProto.hasOwnProperty('next');

   return passed;
}

function kangaxg() {
   function * g (){}
   var iterator = new g.constructor("a","b","c","yield a; yield b; yield c;")(5,6,7);
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 7 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxh() {
   var passed = false;
   function * generator(){
      try {
	 yield 5; yield 6;
      } catch(e) {
	 passed = (e === "foo");
      }
   };
   var iterator = generator();
   iterator.next();
   iterator.throw("foo");
   return passed;   
}

function kangaxi() {
   function * generator(){
      yield 5; yield 6;
   };
   var iterator = generator();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.return("quxquux");
   passed    &= item.value === "quxquux" && item.done === true;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxj() {
   var passed;
   function * generator(){
      passed = yield 0 ? true : false;
   };
   var iterator = generator();
   iterator.next();
   iterator.next(true);
   return passed;   
}

function kangaxk() {
   var iterator = (function * generator() {
      yield * [5, 6];
   }());
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;   
}

function kangaxl() {
   var iterator = (function * generator() {
      yield * [,,];
   }());
   var item = iterator.next();
   var passed = item.value === undefined && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxm() {
   var iterator = (function * generator() {
      yield * "56";
   }());
   var item = iterator.next();
   var passed = item.value === "5" && item.done === false;
   item = iterator.next();
   passed    &= item.value === "6" && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxn() {
   var iterator = (function * generator() {
      yield * "𠮷𠮶";
   }());
   var item = iterator.next();
   var passed = item.value === "𠮷" && item.done === false;
   item = iterator.next();
   passed    &= item.value === "𠮶" && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;   
}

function kangaxo() {
   var iterator = (function * generator() {
      yield * (function*(){ yield 5; yield 6; yield 7; }());
   }());
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 7 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;   
}

function kangaxr() {
   var iterator = (function * generator() {
      yield * [5];
   }());
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   iterator = (function * generator() {
      yield * 5;
   }());
   try {
      iterator.next();
   } catch (e) {
      return passed;
   }
}

/* function kangaxu() {                                                */
/*    var o = {                                                        */
/* 	 * generator() {                                               */
/* 	    yield 5; yield 6;                                          */
/* 	 },                                                            */
/*    };                                                               */
/*    var iterator = o.generator();                                    */
/*    var item = iterator.next();                                      */
/*    var passed = item.value === 5 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === 6 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === undefined && item.done === true;     */
/*    return passed;                                                   */
/* }                                                                   */
/*                                                                     */
/* function kangaxv() {                                                */
/*    var o = {                                                        */
/* 	 * "foo bar"() {                                               */
/* 	    yield 5; yield 6;                                          */
/* 	 },                                                            */
/*    };                                                               */
/*    var iterator = o["foo bar"]();                                   */
/*    var item = iterator.next();                                      */
/*    var passed = item.value === 5 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === 6 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === undefined && item.done === true;     */
/*    return passed;                                                   */
/* }                                                                   */
/*                                                                     */
/* function kangaxw() {                                                */
/*    var garply = "generator";                                        */
/*    var o = {                                                        */
/* 	 * [garply] () {                                               */
/* 	    yield 5; yield 6;                                          */
/* 	 },                                                            */
/*    };                                                               */
/*    var iterator = o.generator();                                    */
/*    var item = iterator.next();                                      */
/*    var passed = item.value === 5 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === 6 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === undefined && item.done === true;     */
/*    return passed;                                                   */
/* }                                                                   */

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true );

console.log( "   kangaxb()");
assert.equal( kangaxb(), true );

console.log( "   kangaxc()");
assert.equal( kangaxc(), true );

console.log( "   kangaxd()");
assert.equal( kangaxd(), undefined );

console.log( "   kangaxe()");
assert.equal( kangaxe(), true );

console.log( "   kangaxf()");
assert.equal( kangaxf(), true );

console.log( "   kangaxg()");
assert.equal( kangaxg(), true );

console.log( "   kangaxh()");
assert.equal( kangaxh(), true );

console.log( "   kangaxi()");
assert.equal( kangaxi(), true );

console.log( "   kangaxj()");
assert.equal( kangaxj(), true );

console.log( "   kangaxk()");
assert.equal( kangaxk(), true );

console.log( "   kangaxl()");
assert.equal( kangaxl(), true );

console.log( "   kangaxm()");
assert.equal( kangaxm(), true );

console.log( "   kangaxn()");
assert.equal( kangaxn(), true );

console.log( "   kangaxo()");
assert.equal( kangaxo(), true );

console.log( "   kangaxr()");
assert.equal( kangaxr(), true );

/* console.log( "   kangaxu()");                                       */
/* assert.equal( kangaxu(), true );                                    */
/*                                                                     */
/* console.log( "   kangaxv()");                                       */
/* assert.equal( kangaxv(), true );                                    */
/*                                                                     */
/* console.log( "   kangaxw()");                                       */
/* assert.equal( kangaxw(), true );                                    */
/*                                                                     */


