/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/function.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Mon Oct  7 15:41:01 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing functions                                                */
/*=====================================================================*/
var assert = require( "assert" );

var f = function (x ) {
   if( !this instanceof f ) throw "Bad prototype";
}

var g = f.bind();
var p = new f("f");
var o = new g("g");

assert.ok( (Object.getPrototypeOf(p) === f.prototype), "f.prototype" );
assert.ok( (Object.getPrototypeOf(o) === f.prototype), "f.bind.prototype" );

