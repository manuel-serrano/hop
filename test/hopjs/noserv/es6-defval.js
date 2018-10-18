/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/noserv/es6-defval.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last change :  Tue Feb  6 09:07:02 2018 (serrano)                */
/*    Copyright   :  2015-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 default parameter values                  */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

function d1( x = "toto" ) {
   return x;
}

assert.strictEqual( d1(), "toto" );
assert.strictEqual( d1( undefined ), "toto" );
assert.strictEqual( d1( "tutu" ), "tutu" );

function d2( x = "_", y = "toto" ) {
   return y;
}

assert.strictEqual( d2(), "toto" );
assert.strictEqual( d2( undefined ), "toto" );
assert.strictEqual( d2( "tata", "tutu" ), "tutu" );

assert.strictEqual( d2( "tata" ), "toto" );
assert.strictEqual( d2( "tata", undefined ), "toto" );
assert.strictEqual( d2( "tata", "tutu" ), "tutu" );

var d3a = ((x = 2) => x);
var d3b = ((x = 5, y = 3) => { return x-y; } );

assert.strictEqual( d3a(), 2 );
assert.strictEqual( d3a( 4 ), 4 );
assert.strictEqual( d3b(), 2 );
assert.strictEqual( d3b( 6 ), 3 );
assert.strictEqual( d3b( undefined, 3 ), 2 );
assert.strictEqual( d3b( 10, 2 ), 8 );

