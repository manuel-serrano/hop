/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/es6-rest.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Sat Oct 10 20:31:38 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 rest arguments                            */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    Utility functions                                                */
/*---------------------------------------------------------------------*/
function id( f ) { return f; }
function obj( f ) { return { f: f } };

var A, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, o;

/*---------------------------------------------------------------------*/
/*    equal ...                                                        */
/*---------------------------------------------------------------------*/
function equal( left, right ) {
   return JSON.stringify( left ) === JSON.stringify( right );
}

/*---------------------------------------------------------------------*/
/*    0 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo0 = function( ... a ) {
   assert.ok( equal( A, a ) );
}
var foo0 = _foo0;
o = obj( _foo0 );

assert.ok( _foo0.length == 0 );

A = [];
console.log( "0.0..." );
_foo0();
foo0();
o.f();
(id( foo0 ))();
_foo0.apply( undefined, [] );
foo0.apply( undefined, [] );
o.f.apply( o, [] );
_foo0.call( o );

A = [ 1 ];
console.log( "0.1..." );
_foo0( 1 );
foo0( 1 );
o.f( 1 );
(id( foo0 ))( 1 );
_foo0.apply( undefined, [ 1 ] );
foo0.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo0.call( o, 1 );

A = [ 1, 2 ];
console.log( "0.2..." );
_foo0( 1, 2 );
foo0( 1, 2 );
o.f( 1, 2 );
(id( foo0 ))( 1, 2 );
_foo0.apply( undefined, [ 1, 2 ] );
foo0.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo0.call( o, 1, 2 );

/*---------------------------------------------------------------------*/
/*    1 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo1 = function( a0, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A, a ) );
}
var foo1 = _foo1;
o = obj( _foo1 );

assert.ok( _foo1.length == 1 );

A0 = undefined;
A = [];
console.log( "1.0..." );
_foo1();
foo1();
o.f();
(id( foo1 ))();
_foo1.apply( undefined, [] );
foo1.apply( undefined, [] );
o.f.apply( o, [] );
_foo1.call( o );

A0 = 1;
A = [];
console.log( "1.1..." );
_foo1( 1 );
foo1( 1 );
o.f( 1 );
(id( foo1 ))( 1 );
_foo1.apply( undefined, [ 1 ] );
foo1.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo1.call( o, 1 );

A0 = 1;
A = [ 2 ];
console.log( "1.2..." );
_foo1( 1, 2 );
foo1( 1, 2 );
o.f( 1, 2 );
(id( foo1 ))( 1, 2 );
_foo1.apply( undefined, [ 1, 2 ] );
foo1.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo1.call( o, 1, 2 );

A0 = 1;
A = [ 2, 3 ];
console.log( "1.3..." );
_foo1( 1, 2, 3 );
foo1( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo1 ))( 1, 2, 3 );
_foo1.apply( undefined, [ 1, 2, 3 ] );
foo1.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo1.call( o, 1, 2, 3 );

/*---------------------------------------------------------------------*/
/*    2 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo2 = function( a0, a1, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A, a ) );
}
var foo2 = _foo2;
o = obj( _foo2 );

assert.ok( _foo2.length == 2 );

A0 = undefined;
A1 = undefined;
A = [];
console.log( "2.0..." );
_foo2();
foo2();
o.f();
(id( foo2 ))();
_foo2.apply( undefined, [] );
foo2.apply( undefined, [] );
o.f.apply( o, [] );
_foo2.call( o );

A0 = 1;
A1 = undefined;
A = [];
console.log( "2.1..." );
_foo2( 1 );
foo2( 1 );
o.f( 1 );
(id( foo2 ))( 1 );
_foo2.apply( undefined, [ 1 ] );
foo2.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo2.call( o, 1 );

A0 = 1;
A1 = 2;
A = [];
console.log( "2.2..." );
_foo2( 1, 2 );
foo2( 1, 2 );
o.f( 1, 2 );
(id( foo2 ))( 1, 2 );
_foo2.apply( undefined, [ 1, 2 ] );
foo2.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo2.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A = [ 3 ];
console.log( "2.3..." );
_foo2( 1, 2, 3 );
foo2( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo2 ))( 1, 2, 3 );
_foo2.apply( undefined, [ 1, 2, 3 ] );
foo2.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo2.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A = [ 3, 4 ];
console.log( "2.4..." );
_foo2( 1, 2, 3, 4 );
foo2( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo2 ))( 1, 2, 3, 4 );
_foo2.apply( undefined, [ 1, 2, 3, 4 ] );
foo2.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo2.call( o, 1, 2, 3, 4 );


/*---------------------------------------------------------------------*/
/*    3 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo3 = function( a0, a1, a2, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A, a ) );
}
var foo3 = _foo3;
o = obj( _foo3 );

assert.ok( _foo3.length == 3 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A = [];
console.log( "3.0..." );
_foo3();
foo3();
o.f();
(id( foo3 ))();
_foo3.apply( undefined, [] );
foo3.apply( undefined, [] );
o.f.apply( o, [] );
_foo3.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A = [];
console.log( "3.1..." );
_foo3( 1 );
foo3( 1 );
o.f( 1 );
(id( foo3 ))( 1 );
_foo3.apply( undefined, [ 1 ] );
foo3.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo3.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A = [];
console.log( "3.2..." );
_foo3( 1, 2 );
foo3( 1, 2 );
o.f( 1, 2 );
(id( foo3 ))( 1, 2 );
_foo3.apply( undefined, [ 1, 2 ] );
foo3.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo3.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A = [];
console.log( "3.3..." );
_foo3( 1, 2, 3 );
foo3( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo3 ))( 1, 2, 3 );
_foo3.apply( undefined, [ 1, 2, 3 ] );
foo3.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo3.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A = [ 4 ];
console.log( "3.4..." );
_foo3( 1, 2, 3, 4 );
foo3( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo3 ))( 1, 2, 3, 4 );
_foo3.apply( undefined, [ 1, 2, 3, 4 ] );
foo3.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo3.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A = [ 4, 5 ];
console.log( "3.5..." );
_foo3( 1, 2, 3, 4, 5 );
foo3( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo3 ))( 1, 2, 3, 4, 5 );
_foo3.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo3.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo3.call( o, 1, 2, 3, 4, 5 );

/*---------------------------------------------------------------------*/
/*    4 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo4 = function( a0, a1, a2, a3, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A, a ) );
}
var foo4 = _foo4;
o = obj( _foo4 );

assert.ok( _foo4.length == 4 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A = [];
console.log( "4.0..." );
_foo4();
foo4();
o.f();
(id( foo4 ))();
_foo4.apply( undefined, [] );
foo4.apply( undefined, [] );
o.f.apply( o, [] );
_foo4.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A = [];
console.log( "4.1..." );
_foo4( 1 );
foo4( 1 );
o.f( 1 );
(id( foo4 ))( 1 );
_foo4.apply( undefined, [ 1 ] );
foo4.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo4.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A = [];
console.log( "4.3..." );
_foo4( 1, 2 );
foo4( 1, 2 );
o.f( 1, 2 );
(id( foo4 ))( 1, 2 );
_foo4.apply( undefined, [ 1, 2 ] );
foo4.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo4.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A = [];
console.log( "4.3..." );
_foo4( 1, 2, 3 );
foo4( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo4 ))( 1, 2, 3 );
_foo4.apply( undefined, [ 1, 2, 3 ] );
foo4.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo4.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A = [];
console.log( "4.4..." );
_foo4( 1, 2, 3, 4 );
foo4( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo4 ))( 1, 2, 3, 4 );
_foo4.apply( undefined, [ 1, 2, 3, 4 ] );
foo4.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo4.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A = [ 5 ];
console.log( "4.5..." );
_foo4( 1, 2, 3, 4, 5 );
foo4( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo4 ))( 1, 2, 3, 4, 5 );
_foo4.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo4.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo4.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A = [ 5, 6 ];
console.log( "4.6..." );
_foo4( 1, 2, 3, 4, 5, 6 );
foo4( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo4 ))( 1, 2, 3, 4, 5, 6 );
_foo4.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo4.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo4.call( o, 1, 2, 3, 4, 5, 6 );

/*---------------------------------------------------------------------*/
/*    5 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo5 = function( a0, a1, a2, a3, a4, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A4, a4 ) );
   assert.ok( equal( A, a ) );
}
var foo5 = _foo5;
o = obj( _foo5 );

assert.ok( _foo5.length == 5 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A = [];
console.log( "5.0..." );
_foo5();
foo5();
o.f();
(id( foo5 ))();
_foo5.apply( undefined, [] );
foo5.apply( undefined, [] );
o.f.apply( o, [] );
_foo5.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A = [];
console.log( "5.1..." );
_foo5( 1 );
foo5( 1 );
o.f( 1 );
(id( foo5 ))( 1 );
_foo5.apply( undefined, [ 1 ] );
foo5.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo5.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A = [];
console.log( "5.3..." );
_foo5( 1, 2 );
foo5( 1, 2 );
o.f( 1, 2 );
(id( foo5 ))( 1, 2 );
_foo5.apply( undefined, [ 1, 2 ] );
foo5.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo5.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A4 = undefined;
A = [];
console.log( "5.3..." );
_foo5( 1, 2, 3 );
foo5( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo5 ))( 1, 2, 3 );
_foo5.apply( undefined, [ 1, 2, 3 ] );
foo5.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo5.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = undefined;
A = [];
console.log( "5.4..." );
_foo5( 1, 2, 3, 4 );
foo5( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo5 ))( 1, 2, 3, 4 );
_foo5.apply( undefined, [ 1, 2, 3, 4 ] );
foo5.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo5.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A = [];
console.log( "5.5..." );
_foo5( 1, 2, 3, 4, 5 );
foo5( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo5 ))( 1, 2, 3, 4, 5 );
_foo5.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo5.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo5.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A = [ 6 ];
console.log( "5.6..." );
_foo5( 1, 2, 3, 4, 5, 6 );
foo5( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo5 ))( 1, 2, 3, 4, 5, 6 );
_foo5.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo5.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo5.call( o, 1, 2, 3, 4, 5, 6 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A = [ 6, 7 ];
console.log( "5.7..." );
_foo5( 1, 2, 3, 4, 5, 6, 7 );
foo5( 1, 2, 3, 4, 5, 6, 7 );
o.f( 1, 2, 3, 4, 5, 6, 7 );
(id( foo5 ))( 1, 2, 3, 4, 5, 6, 7 );
_foo5.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
foo5.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
_foo5.call( o, 1, 2, 3, 4, 5, 6, 7 );

/*---------------------------------------------------------------------*/
/*    6 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo6 = function( a0, a1, a2, a3, a4, a5, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A4, a4 ) );
   assert.ok( equal( A5, a5 ) );
   assert.ok( equal( A, a ) );
}
var foo6 = _foo6;
o = obj( _foo6 );

assert.ok( _foo6.length == 6 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A = [];
console.log( "6.0..." );
_foo6();
foo6();
o.f();
(id( foo6 ))();
_foo6.apply( undefined, [] );
foo6.apply( undefined, [] );
o.f.apply( o, [] );
_foo6.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A = [];
console.log( "6.1..." );
_foo6( 1 );
foo6( 1 );
o.f( 1 );
(id( foo6 ))( 1 );
_foo6.apply( undefined, [ 1 ] );
foo6.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo6.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A = [];
console.log( "6.3..." );
_foo6( 1, 2 );
foo6( 1, 2 );
o.f( 1, 2 );
(id( foo6 ))( 1, 2 );
_foo6.apply( undefined, [ 1, 2 ] );
foo6.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo6.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A = [];
console.log( "6.3..." );
_foo6( 1, 2, 3 );
foo6( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo6 ))( 1, 2, 3 );
_foo6.apply( undefined, [ 1, 2, 3 ] );
foo6.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo6.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = undefined;
A5 = undefined;
A = [];
console.log( "6.4..." );
_foo6( 1, 2, 3, 4 );
foo6( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo6 ))( 1, 2, 3, 4 );
_foo6.apply( undefined, [ 1, 2, 3, 4 ] );
foo6.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo6.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = undefined;
A = [];
console.log( "6.5..." );
_foo6( 1, 2, 3, 4, 5 );
foo6( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo6 ))( 1, 2, 3, 4, 5 );
_foo6.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo6.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo6.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A = [];
console.log( "6.6..." );
_foo6( 1, 2, 3, 4, 5, 6 );
foo6( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo6 ))( 1, 2, 3, 4, 5, 6 );
_foo6.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo6.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo6.call( o, 1, 2, 3, 4, 5, 6 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A = [ 7 ];
console.log( "6.7..." );
_foo6( 1, 2, 3, 4, 5, 6, 7 );
foo6( 1, 2, 3, 4, 5, 6, 7 );
o.f( 1, 2, 3, 4, 5, 6, 7 );
(id( foo6 ))( 1, 2, 3, 4, 5, 6, 7 );
_foo6.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
foo6.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
_foo6.call( o, 1, 2, 3, 4, 5, 6, 7 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A = [ 7, 8 ];
console.log( "6.8..." );
_foo6( 1, 2, 3, 4, 5, 6, 7, 8 );
console.log( "ok.1" );
foo6( 1, 2, 3, 4, 5, 6, 7, 8 );
console.log( "ok.2" );
o.f( 1, 2, 3, 4, 5, 6, 7, 8 );
console.log( "ok.3" );
(id( foo6 ))( 1, 2, 3, 4, 5, 6, 7, 8 );
console.log( "ok.4" );
_foo6.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
console.log( "ok.5" );
foo6.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
console.error( "ok.6" );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
_foo6.call( o, 1, 2, 3, 4, 5, 6, 7, 8 );

/*---------------------------------------------------------------------*/
/*    7 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo7 = function( a0, a1, a2, a3, a4, a5, a6, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A4, a4 ) );
   assert.ok( equal( A5, a5 ) );
   assert.ok( equal( A6, a6 ) );
   assert.ok( equal( A, a ) );
}
var foo7 = _foo7;
o = obj( _foo7 );

assert.ok( _foo7.length == 7 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A = [];
console.log( "7.0..." );
_foo7();
foo7();
o.f();
(id( foo7 ))();
_foo7.apply( undefined, [] );
foo7.apply( undefined, [] );
o.f.apply( o, [] );
_foo7.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A = [];
console.log( "7.1..." );
_foo7( 1 );
foo7( 1 );
o.f( 1 );
(id( foo7 ))( 1 );
_foo7.apply( undefined, [ 1 ] );
foo7.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo7.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A = [];
console.log( "7.3..." );
_foo7( 1, 2 );
foo7( 1, 2 );
o.f( 1, 2 );
(id( foo7 ))( 1, 2 );
_foo7.apply( undefined, [ 1, 2 ] );
foo7.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo7.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A = [];
console.log( "7.3..." );
_foo7( 1, 2, 3 );
foo7( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo7 ))( 1, 2, 3 );
_foo7.apply( undefined, [ 1, 2, 3 ] );
foo7.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo7.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A = [];
console.log( "7.4..." );
_foo7( 1, 2, 3, 4 );
foo7( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo7 ))( 1, 2, 3, 4 );
_foo7.apply( undefined, [ 1, 2, 3, 4 ] );
foo7.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo7.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = undefined;
A6 = undefined;
A = [];
console.log( "7.5..." );
_foo7( 1, 2, 3, 4, 5 );
foo7( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo7 ))( 1, 2, 3, 4, 5 );
_foo7.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo7.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo7.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = undefined;
A = [];
console.log( "7.6..." );
_foo7( 1, 2, 3, 4, 5, 6 );
foo7( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo7 ))( 1, 2, 3, 4, 5, 6 );
_foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo7.call( o, 1, 2, 3, 4, 5, 6 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A = [];
console.log( "7.7..." );
_foo7( 1, 2, 3, 4, 5, 6, 7 );
foo7( 1, 2, 3, 4, 5, 6, 7 );
o.f( 1, 2, 3, 4, 5, 6, 7 );
(id( foo7 ))( 1, 2, 3, 4, 5, 6, 7 );
_foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
_foo7.call( o, 1, 2, 3, 4, 5, 6, 7 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A = [ 8 ];
console.log( "7.8..." );
_foo7( 1, 2, 3, 4, 5, 6, 7, 8 );
foo7( 1, 2, 3, 4, 5, 6, 7, 8 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8 );
(id( foo7 ))( 1, 2, 3, 4, 5, 6, 7, 8 );
_foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
_foo7.call( o, 1, 2, 3, 4, 5, 6, 7, 8 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A = [ 8, 9 ];
console.log( "7.9..." );
_foo7( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
foo7( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
(id( foo7 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
_foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
foo7.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
_foo7.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9 );

/*---------------------------------------------------------------------*/
/*    8 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo8 = function( a0, a1, a2, a3, a4, a5, a6, a7, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A4, a4 ) );
   assert.ok( equal( A5, a5 ) );
   assert.ok( equal( A6, a6 ) );
   assert.ok( equal( A7, a7 ) );
   assert.ok( equal( A, a ) );
}
var foo8 = _foo8;
o = obj( _foo8 );

assert.ok( _foo8.length == 8 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A = [];
console.log( "8.0..." );
_foo8();
foo8();
o.f();
(id( foo8 ))();
_foo8.apply( undefined, [] );
foo8.apply( undefined, [] );
o.f.apply( o, [] );
_foo8.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A = [];
console.log( "8.1..." );
_foo8( 1 );
foo8( 1 );
o.f( 1 );
(id( foo8 ))( 1 );
_foo8.apply( undefined, [ 1 ] );
foo8.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo8.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A = [];
console.log( "8.3..." );
_foo8( 1, 2 );
foo8( 1, 2 );
o.f( 1, 2 );
(id( foo8 ))( 1, 2 );
_foo8.apply( undefined, [ 1, 2 ] );
foo8.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo8.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A = [];
console.log( "8.3..." );
_foo8( 1, 2, 3 );
foo8( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo8 ))( 1, 2, 3 );
_foo8.apply( undefined, [ 1, 2, 3 ] );
foo8.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo8.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A = [];
console.log( "8.4..." );
_foo8( 1, 2, 3, 4 );
foo8( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo8 ))( 1, 2, 3, 4 );
_foo8.apply( undefined, [ 1, 2, 3, 4 ] );
foo8.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo8.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A = [];
console.log( "8.5..." );
_foo8( 1, 2, 3, 4, 5 );
foo8( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo8 ))( 1, 2, 3, 4, 5 );
_foo8.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo8.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo8.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = undefined;
A7 = undefined;
A = [];
console.log( "8.6..." );
_foo8( 1, 2, 3, 4, 5, 6 );
foo8( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo8 ))( 1, 2, 3, 4, 5, 6 );
_foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo8.call( o, 1, 2, 3, 4, 5, 6 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = undefined;
A = [];
console.log( "8.7..." );
_foo8( 1, 2, 3, 4, 5, 6, 7 );
foo8( 1, 2, 3, 4, 5, 6, 7 );
o.f( 1, 2, 3, 4, 5, 6, 7 );
(id( foo8 ))( 1, 2, 3, 4, 5, 6, 7 );
_foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
_foo8.call( o, 1, 2, 3, 4, 5, 6, 7 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A = [];
console.log( "8.8..." );
_foo8( 1, 2, 3, 4, 5, 6, 7, 8 );
foo8( 1, 2, 3, 4, 5, 6, 7, 8 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8 );
(id( foo8 ))( 1, 2, 3, 4, 5, 6, 7, 8 );
_foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
_foo8.call( o, 1, 2, 3, 4, 5, 6, 7, 8 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A = [ 9 ];
console.log( "8.9..." );
_foo8( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
foo8( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
(id( foo8 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
_foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
_foo8.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A = [ 9, 10 ];
console.log( "8.10..." );
_foo8( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
foo8( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
(id( foo8 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
_foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
foo8.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
_foo8.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );

/*---------------------------------------------------------------------*/
/*    9 or more                                                        */
/*---------------------------------------------------------------------*/
const _foo9 = function( a0, a1, a2, a3, a4, a5, a6, a7, a8, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A4, a4 ) );
   assert.ok( equal( A5, a5 ) );
   assert.ok( equal( A6, a6 ) );
   assert.ok( equal( A7, a7 ) );
   assert.ok( equal( A8, a8 ) );
   assert.ok( equal( A, a ) );
}
var foo9 = _foo9;
o = obj( _foo9 );

assert.ok( _foo9.length == 9 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.0..." );
_foo9();
foo9();
o.f();
(id( foo9 ))();
_foo9.apply( undefined, [] );
foo9.apply( undefined, [] );
o.f.apply( o, [] );
_foo9.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.1..." );
_foo9( 1 );
foo9( 1 );
o.f( 1 );
(id( foo9 ))( 1 );
_foo9.apply( undefined, [ 1 ] );
foo9.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo9.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.3..." );
_foo9( 1, 2 );
foo9( 1, 2 );
o.f( 1, 2 );
(id( foo9 ))( 1, 2 );
_foo9.apply( undefined, [ 1, 2 ] );
foo9.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo9.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.3..." );
_foo9( 1, 2, 3 );
foo9( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo9 ))( 1, 2, 3 );
_foo9.apply( undefined, [ 1, 2, 3 ] );
foo9.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo9.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.4..." );
_foo9( 1, 2, 3, 4 );
foo9( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo9 ))( 1, 2, 3, 4 );
_foo9.apply( undefined, [ 1, 2, 3, 4 ] );
foo9.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo9.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.5..." );
_foo9( 1, 2, 3, 4, 5 );
foo9( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo9 ))( 1, 2, 3, 4, 5 );
_foo9.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo9.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo9.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.6..." );
_foo9( 1, 2, 3, 4, 5, 6 );
foo9( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo9 ))( 1, 2, 3, 4, 5, 6 );
_foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo9.call( o, 1, 2, 3, 4, 5, 6 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = undefined;
A8 = undefined;
A = [];
console.log( "9.7..." );
_foo9( 1, 2, 3, 4, 5, 6, 7 );
foo9( 1, 2, 3, 4, 5, 6, 7 );
o.f( 1, 2, 3, 4, 5, 6, 7 );
(id( foo9 ))( 1, 2, 3, 4, 5, 6, 7 );
_foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
_foo9.call( o, 1, 2, 3, 4, 5, 6, 7 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = undefined;
A = [];
console.log( "9.8..." );
_foo9( 1, 2, 3, 4, 5, 6, 7, 8 );
foo9( 1, 2, 3, 4, 5, 6, 7, 8 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8 );
(id( foo9 ))( 1, 2, 3, 4, 5, 6, 7, 8 );
_foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
_foo9.call( o, 1, 2, 3, 4, 5, 6, 7, 8 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A = [];
console.log( "9.9..." );
_foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
(id( foo9 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
_foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
_foo9.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A = [ 10 ];
console.log( "9.10..." );
_foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
(id( foo9 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
_foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
_foo9.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A = [ 10, 11 ];
console.log( "9.11..." );
_foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
(id( foo9 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
_foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
foo9.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
_foo9.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

/*---------------------------------------------------------------------*/
/*    10 or more                                                       */
/*---------------------------------------------------------------------*/
const _foo10 = function( a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A4, a4 ) );
   assert.ok( equal( A5, a5 ) );
   assert.ok( equal( A6, a6 ) );
   assert.ok( equal( A7, a7 ) );
   assert.ok( equal( A8, a8 ) );
   assert.ok( equal( A9, a9 ) );
   assert.ok( equal( A, a ) );
}
var foo10 = _foo10;
o = obj( _foo10 );

assert.ok( _foo10.length == 10 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.0..." );
_foo10();
foo10();
o.f();
(id( foo10 ))();
_foo10.apply( undefined, [] );
foo10.apply( undefined, [] );
o.f.apply( o, [] );
_foo10.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.1..." );
_foo10( 1 );
foo10( 1 );
o.f( 1 );
(id( foo10 ))( 1 );
_foo10.apply( undefined, [ 1 ] );
foo10.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo10.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.3..." );
_foo10( 1, 2 );
foo10( 1, 2 );
o.f( 1, 2 );
(id( foo10 ))( 1, 2 );
_foo10.apply( undefined, [ 1, 2 ] );
foo10.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo10.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.3..." );
_foo10( 1, 2, 3 );
foo10( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo10 ))( 1, 2, 3 );
_foo10.apply( undefined, [ 1, 2, 3 ] );
foo10.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo10.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.4..." );
_foo10( 1, 2, 3, 4 );
foo10( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo10 ))( 1, 2, 3, 4 );
_foo10.apply( undefined, [ 1, 2, 3, 4 ] );
foo10.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo10.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.5..." );
_foo10( 1, 2, 3, 4, 5 );
foo10( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo10 ))( 1, 2, 3, 4, 5 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo10.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.6..." );
_foo10( 1, 2, 3, 4, 5, 6 );
foo10( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo10 ))( 1, 2, 3, 4, 5, 6 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo10.call( o, 1, 2, 3, 4, 5, 6 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.7..." );
_foo10( 1, 2, 3, 4, 5, 6, 7 );
foo10( 1, 2, 3, 4, 5, 6, 7 );
o.f( 1, 2, 3, 4, 5, 6, 7 );
(id( foo10 ))( 1, 2, 3, 4, 5, 6, 7 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
_foo10.call( o, 1, 2, 3, 4, 5, 6, 7 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = undefined;
A9 = undefined;
A = [];
console.log( "10.8..." );
_foo10( 1, 2, 3, 4, 5, 6, 7, 8 );
foo10( 1, 2, 3, 4, 5, 6, 7, 8 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8 );
(id( foo10 ))( 1, 2, 3, 4, 5, 6, 7, 8 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
_foo10.call( o, 1, 2, 3, 4, 5, 6, 7, 8 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = undefined;
A = [];
console.log( "10.9..." );
_foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
(id( foo10 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
_foo10.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = 10;
A = [];
console.log( "10.10..." );
_foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
(id( foo10 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
_foo10.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = 10;
A = [ 11 ];
console.log( "10.11..." );
_foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
(id( foo10 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
_foo10.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = 10;
A = [ 11, 12 ];
console.log( "10.12..." );
_foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
foo10( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
(id( foo10 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
_foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
foo10.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
_foo10.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );

/*---------------------------------------------------------------------*/
/*    11 or more                                                       */
/*---------------------------------------------------------------------*/
const _foo11 = function( a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, ... a ) {
   "use strict";
   assert.ok( equal( A0, a0 ) );
   assert.ok( equal( A1, a1 ) );
   assert.ok( equal( A2, a2 ) );
   assert.ok( equal( A3, a3 ) );
   assert.ok( equal( A4, a4 ) );
   assert.ok( equal( A5, a5 ) );
   assert.ok( equal( A6, a6 ) );
   assert.ok( equal( A7, a7 ) );
   assert.ok( equal( A8, a8 ) );
   assert.ok( equal( A9, a9 ) );
   assert.ok( equal( A10, a10 ) );
   assert.ok( equal( A, a ) );
}
var foo11 = _foo11;
o = obj( _foo11 );

assert.ok( _foo11.length == 11 );

A0 = undefined;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.0..." );
_foo11();
foo11();
o.f();
(id( foo11 ))();
_foo11.apply( undefined, [] );
foo11.apply( undefined, [] );
o.f.apply( o, [] );
_foo11.call( o );

A0 = 1;
A1 = undefined;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.1..." );
_foo11( 1 );
foo11( 1 );
o.f( 1 );
(id( foo11 ))( 1 );
_foo11.apply( undefined, [ 1 ] );
foo11.apply( undefined, [ 1 ] );
o.f.apply( undefined, [ 1 ] );
_foo11.call( o, 1 );

A0 = 1;
A1 = 2;
A2 = undefined;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.3..." );
_foo11( 1, 2 );
foo11( 1, 2 );
o.f( 1, 2 );
(id( foo11 ))( 1, 2 );
_foo11.apply( undefined, [ 1, 2 ] );
foo11.apply( undefined, [ 1, 2 ] );
o.f.apply( undefined, [ 1, 2 ] );
_foo11.call( o, 1, 2 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = undefined;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.3..." );
_foo11( 1, 2, 3 );
foo11( 1, 2, 3 );
o.f( 1, 2, 3 );
(id( foo11 ))( 1, 2, 3 );
_foo11.apply( undefined, [ 1, 2, 3 ] );
foo11.apply( undefined, [ 1, 2, 3 ] );
o.f.apply( undefined, [ 1, 2, 3 ] );
_foo11.call( o, 1, 2, 3 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = undefined;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.4..." );
_foo11( 1, 2, 3, 4 );
foo11( 1, 2, 3, 4 );
o.f( 1, 2, 3, 4 );
(id( foo11 ))( 1, 2, 3, 4 );
_foo11.apply( undefined, [ 1, 2, 3, 4 ] );
foo11.apply( undefined, [ 1, 2, 3, 4 ] );
o.f.apply( undefined, [ 1, 2, 3, 4 ] );
_foo11.call( o, 1, 2, 3, 4 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = undefined;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.5..." );
_foo11( 1, 2, 3, 4, 5 );
foo11( 1, 2, 3, 4, 5 );
o.f( 1, 2, 3, 4, 5 );
(id( foo11 ))( 1, 2, 3, 4, 5 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5 ] );
_foo11.call( o, 1, 2, 3, 4, 5 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = undefined;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.6..." );
_foo11( 1, 2, 3, 4, 5, 6 );
foo11( 1, 2, 3, 4, 5, 6 );
o.f( 1, 2, 3, 4, 5, 6 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = undefined;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.7..." );
_foo11( 1, 2, 3, 4, 5, 6, 7 );
foo11( 1, 2, 3, 4, 5, 6, 7 );
o.f( 1, 2, 3, 4, 5, 6, 7 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6, 7 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6, 7 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = undefined;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.8..." );
_foo11( 1, 2, 3, 4, 5, 6, 7, 8 );
foo11( 1, 2, 3, 4, 5, 6, 7, 8 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6, 7, 8 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6, 7, 8 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = undefined;
A10 = undefined;
A = [];
console.log( "10.9..." );
_foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = 10;
A10 = undefined;
A = [];
console.log( "10.10..." );
_foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = 10;
A10 = 11;
A = [ ];
console.log( "10.11..." );
_foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = 10;
A10 = 11;
A = [ 12 ];
console.log( "10.12..." );
_foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );

A0 = 1;
A1 = 2;
A2 = 3;
A3 = 4;
A4 = 5;
A5 = 6;
A6 = 7;
A7 = 8;
A8 = 9;
A9 = 10;
A10 = 11;
A = [ 12, 13 ];
console.log( "10.13..." );
_foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 );
foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 );
o.f( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 );
(id( foo11 ))( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 );
_foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ] );
foo11.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ] );
o.f.apply( undefined, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ] );
_foo11.call( o, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 );

