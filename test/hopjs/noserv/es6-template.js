/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/noserv/es6-template.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Aug 14 09:43:13 2015                          */
/*    Last change :  Wed Aug 19 11:23:05 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ES6 template strings                                     */
/*=====================================================================*/
var assert = require( "assert" );

// simple tests
assert.strictEqual( `toto
tutu`, "toto\ntutu" );

var res = "";
var expect = "";
   
for( var i = 0; i < 3; i++ ) {
   res += `res-${i + 10}-${i}`;
   expect += "res-" + ((i + 10) + "") + "-" + i;
}

assert.strictEqual( res, expect );

// tag
function bar( strs, val0, val1, val2 ) {
   assert.deepEqual( strs, [ "foo$", "", "bar$", "" ] );
   assert.equal( val0, 0 );
   assert.equal( val1, 1 );
   assert.equal( val2, 2 );
   return strs;
}
bar`foo$${0}${1}bar$${2}`;

// $$
assert.strictEqual( `foo$`, "foo$" );
assert.strictEqual( `foo$$`, "foo$$" );
assert.strictEqual( `foo$bar`, "foo$bar" );
assert.strictEqual( `foo$$bar`, "foo$$bar" );
assert.strictEqual( `foo$${1+0}`, "foo$1" );
assert.strictEqual( `foo$${1+0}$${2+0}`, "foo$1$2" );
assert.strictEqual( `foo$${1+0}$${2+0}$$`, "foo$1$2$$" );
assert.strictEqual( `foo$${1+0}$${2+0}$`, "foo$1$2$" );

// raw
String.raw( `foo$${1+0}$${2+0}$`, `foo$${1+0}$${2+0}$` );

// errors
assert.throws( function() { eval( "`foo${`" ) } );
