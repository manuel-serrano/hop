/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es6-arrow.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Thu May  6 16:19:10 2021 (serrano)                */
/*    Copyright   :  2015-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 arrow functions                           */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

var f0a = (() => 1);
var f0b = (() => { return 1; } );

assert.strictEqual( f0a(), 1 );
assert.strictEqual( f0b(), 1 );

var f1a = (x => x + 1);
var f1b = (x => { return x + 1 });

assert.strictEqual( f1a( 1 ), 2 );
assert.strictEqual( f1b( 1 ), 2 );

var f2a = ((x, y) => x - y);
var f2b = ((x, y) => { return x - y });

assert.strictEqual( f2a( 5, 3 ), 2 );
assert.strictEqual( f2b( 5, 3 ), 2 );

function Person( age ) {
   this.age = age;

   this.inc = (v => { this.age += v; });
   this.get = (() => this.age);
}

var p = new Person( 10 );
assert.strictEqual( p.get(), 10 );
p.inc( 3 );
p.inc( 2 );
assert.strictEqual( p.get(), 15 );

Person.prototype.fun = function( a ) {
   // check if the arrow function captures lexical this correctly
   let res = undefined;
   a.forEach( (x) => { res = this; } );
   return res;
}

var p = new Person( 20 );
assert.strictEqual( p.fun( [ 10 ] ).age, 20 );

/*---------------------------------------------------------------------*/
/*    vararg                                                           */
/*---------------------------------------------------------------------*/
const vararg = (a = 0, ...b ) => a + b.reduce( (x,y) => x+y, 0 );

assert.ok( vararg() === 0, "vararg()" );
assert.ok( vararg( 1 ) === 1, "vararg( 1 )" );
assert.ok( vararg( 1, 2 ) === 3, "vararg( 1, 2 )" );
assert.ok( vararg( 1, 2, 3 ) === 6, "vararg( 1, 2, 3 )" );

/*---------------------------------------------------------------------*/
/*    spread                                                           */
/*---------------------------------------------------------------------*/
const spread = ({a,b}, [{c,d}]) => a + b + c + d;

assert.ok( spread( {b: 2, a: 1}, [{a: 1, c: 6, e: 5, d: 3}] ) === 12, "spread" );
