/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/string.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Tue Dec 22 08:25:06 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing strings                                                  */
/*=====================================================================*/
var assert = require( "assert" );

var s1 = "foo";
var s2 = "bar"

var s3 = s1 + s2;
var s4 = s3 + "gee";

assert.equal( s3, "foobar" );
assert.equal( s4, "foobargee" );
assert.equal( "gee" + s3, "geefoobar" );
assert.equal( s3 + s3, "foobarfoobar" );
assert.equal( s4 + "hux", "foobargeehux" );
assert.equal( "hux" + s4, "huxfoobargee" );

/*---------------------------------------------------------------------*/
/*    Unicode strings                                                  */
/*---------------------------------------------------------------------*/
var s5 = 'A\uD835\uDC68C';

assert.equal( s5.length, 4 );

assert.equal( s5[ 0 ], 'A' );
assert.equal( s5[ 1 ].length, 1 );
assert.equal( s5[ 2 ].length, 1 );
assert.equal( s5[ 3 ], 'C' );

assert.equal( s5.charAt( 0 ), 'A' );
assert.equal( s5.charAt( 1 ).length, 1 );
assert.equal( s5.charAt( 2 ).length, 1 );
assert.equal( s5.charAt( 3 ), 'C' );

assert.equal( s5.charCodeAt( 0 ), 65 );
assert.equal( s5.charCodeAt( 1 ), 55349 );
assert.equal( s5.charCodeAt( 2 ), 56424 );
assert.equal( s5.charCodeAt( 3 ), 67 );

