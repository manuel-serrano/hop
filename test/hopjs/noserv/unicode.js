/*=====================================================================*/
/*    serrano/prgm/project/hop/3.4.x/test/hopjs/noserv/unicode.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:20:00 2014                          */
/*    Last change :  Fri Apr 16 14:55:20 2021 (serrano)                */
/*    Copyright   :  2014-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Unicode testing                                                  */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    UCS-2/UTF-16 representation                                      */
/*---------------------------------------------------------------------*/
var s = '\uD83D\uDC4D';
var s1 = s.substring( 0, 1 );
var s2 = s.substring( 1, 2 );

// \uD83D is not a UCS-2 character as it is a prefix for a 2x16 bits long
// character. In Hop, it is replaced with a 3 bytes sequence encoding
// the code point. This test checks that this encoding is correctly handled
// by char reference, substring, and string concatenation.

assert.strictEqual( s.length, 2, "length" );

assert.strictEqual( s.charCodeAt( 0 ), 0xD83D );
assert.strictEqual( s.charCodeAt( 1 ), 0xDC4D );

assert.strictEqual( "\uD83D".charCodeAt( 0 ), 0xD83D );
assert.strictEqual( "\uDC4D".charCodeAt( 0 ), 0xDC4D );

assert.strictEqual( s.charAt( 0 ), "\uD83D" );
assert.strictEqual( s.charAt( 1 ), "\uDC4D" );
assert.strictEqual( s.charAt( 0 ), s1.charAt( 0 ) );

assert.strictEqual( s1 + s2, s );

assert.strictEqual( "étèpasglop".indexOf( "é" ), 0 );
assert.strictEqual( "étèpasglop".indexOf( "è" ), 2 );
assert.strictEqual( "étèpasêsglop".lastIndexOf( "è", 8 ), 2 );
assert.strictEqual( "user:password@[3ffe:2a00:100:7031::1".lastIndexOf( "@" ), 13 );

assert.deepEqual( "Ã©tÃ©.Ã©tÃ¨.fr".split( "." ),
		  [ "Ã©tÃ©", "Ã©tÃ¨", "fr" ] );
assert.deepEqual( "Ã©tÃ¨.pas.glop.Ã©tÃ¨.fr".split( "." ),
		  [ "Ã©tÃ¨", "pas", "glop", "Ã©tÃ¨", "fr" ] );
assert.deepEqual( "Ã©tÃ¨.pas.glop.Ã©tÃ¨.fr.com".split( "." ),
		  [ "Ã©tÃ¨", "pas", "glop", "Ã©tÃ¨", "fr", "com" ] );
assert.deepEqual( "Ã©tÃ¨.pas.glop.Ã©tÃ¨".split( "." ),
		  [ "Ã©tÃ¨", "pas", "glop", "Ã©tÃ¨" ] );
