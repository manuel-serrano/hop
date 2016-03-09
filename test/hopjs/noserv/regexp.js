/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/regexp.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Tue Feb  9 14:56:53 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing REGEXP matching                                          */
/*=====================================================================*/
var assert = require( "assert" );

assert.strictEqual( "toto\\tutu".match( /toto\\tutu/ )[ 0 ], "toto\\tutu" );
assert.ok( /[\uD800-\uDBFF]/.test( 'foo' ) ? true: true );
