/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/function.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Fri Apr 17 15:28:05 2020 (serrano)                */
/*    Copyright   :  2014-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing functions                                                */
/*=====================================================================*/
var assert = require( "assert" );

var f = function ( x ) {
   if( !this instanceof f ) throw "Bad prototype";
}

var g = f.bind();
var p = new f("f");
var o = new g("g");

assert.ok( (Object.getPrototypeOf(p) === f.prototype), "f.prototype" );
assert.ok( undefined === g.prototype, "g.prototype" );
assert.ok( (Object.getPrototypeOf(o) === f.prototype), "f.bind.prototype" );

function f2() {
   if( !this instanceof f2 ) throw "Bad prototype";
}

var proto = {};
f2.prototype = proto;

var g2 = f2.bind();
var p2 = new f2("f2");
var o2 = new g2("g2");

assert.ok( (Object.getPrototypeOf(p2) === f2.prototype), "f2.prototype" );
assert.ok( undefined === g2.prototype, "g2.prototype" );
assert.ok( (Object.getPrototypeOf(o2) === f2.prototype), "f2.bind.prototype" );
assert.ok( (Object.getPrototypeOf(o2) === proto), "f2.bind.prototype" );

var proto2 = {};
var g3 = f2.bind();
g3.prototype = proto2;

var o3 = new g3("g3");

assert.ok( proto2 === g3.prototype, "g3.prototype" );
assert.ok( (Object.getPrototypeOf(o3) === f2.prototype), "f3.bind.prototype" );
assert.ok( (Object.getPrototypeOf(o3) !== proto2), "f3.bind.prototype" );

function argtest() {
   return function( obj, n ) {
      var l = arguments.length;
      return arguments[ n ].a === 1;
   }
}

assert.ok( argtest()( { a: 1 }, { b: 2 } ), "arguments" );


