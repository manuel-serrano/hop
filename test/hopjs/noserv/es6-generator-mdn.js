/*=====================================================================*/
/*    .../project/hop/3.1.x/test/hopjs/noserv/es6-generator-mdn.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Feb  4 09:22:38 2017                          */
/*    Last change :  Sat Feb  4 09:27:21 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    MDN generator tests                                              */
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


