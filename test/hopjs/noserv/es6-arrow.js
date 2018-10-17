/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-arrow.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Wed Oct 11 19:03:16 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 arrow functions                           */
/*=====================================================================*/
"use hopscript";

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

