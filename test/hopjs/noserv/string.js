/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/string.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Sun Oct 31 06:57:17 2021 (serrano)                */
/*    Copyright   :  2014-21 Manuel Serrano                            */
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
var s5a = 'A\uD835';
var s5b = '\uDC68C';
var s5c = s5a + (s5b.length === 2 ? s5b : "");

assert.equal( s5.length, 4, "utf16 string length" );
assert.equal( s5a.length, 2, "utf16 string length" );
assert.equal( s5b.length, 2, "utf16 string length" );
assert.equal( s5c.length, 4, "utf16 string-append length" );

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

var s6 = "\ud801\udc37";

assert.equal( s6.length, 2 );

var s7a = "\ud801";
var s7b = "\udc37";
var s7 = s7a + s7b;

assert.equal( s7.length, 2 );
assert.equal( s7a.length, 1 );
assert.equal( s7b.length, 1 );
assert.equal( s7.charCodeAt( 0 ), 0xd801 );
assert.equal( s7a.charCodeAt( 0 ), 0xd801 );
assert.equal( s7.charCodeAt( 1 ), 0xdc37 );
assert.equal( s7b.charCodeAt( 0 ), 0xdc37 );

const s8 = "\u{1f0a1}\u{1f0a2}a\uD835\uDC68C";

assert.equal( s8.length, 8 );
assert.equal( s8.charCodeAt( 0 ), 55356 );
assert.equal( s8.charCodeAt( 1 ), 56481 );
assert.equal( s8.charCodeAt( 2 ), 55356 );
assert.equal( s8.codePointAt( 0 ), 0x1f0a1 );
assert.equal( s8.codePointAt( 2 ), 0x1f0a2 );
assert.equal( s8.codePointAt( 4 ), 97 );
assert.equal( s8.codePointAt( 5 ), 119912 );
assert.equal( s8.codePointAt( 7 ), 67 );

/*---------------------------------------------------------------------*/
/*    generic                                                          */
/*---------------------------------------------------------------------*/
function generic( method, raise ) {
   var cnt = 0;
   
   method.call( new String( "toto" ) ); cnt++;
   method.call( "toto" ); cnt++;

   if( raise ) {
      try {
	 method.call( 20 );
      } catch( e ) {
	 cnt++;
      }
   } else {
      method.call( 20 ); cnt++;
   }

   return cnt == 3;
}

assert.ok( generic( String.prototype.toString, true ), "toString" );
assert.ok( generic( String.prototype.valueOf, true ), "valueOf" );
assert.ok( generic( String.prototype.charAt, false ), "charAt" );

/*---------------------------------------------------------------------*/
/*    concat                                                           */
/*---------------------------------------------------------------------*/
function bigStr() {
   var n = 500000;

   var s = '';
   for( var i = 0; i < n; i++ ) {
      s += 'c';
   }
   assert.ok( s.length == n, "length" );
}

bigStr();

/*---------------------------------------------------------------------*/
/*    typing                                                           */
/*---------------------------------------------------------------------*/
function typestr() {
   let c1 = this.charCodeAt( 0 );
   let s = this.toString();
   
   return s.length;
}

assert.equal( typestr.call( "bar", undefined ), 3, "typestr" );

function typestr2() {
   let f1 = this.charCodeAt;
   let f2 = this.charCodeAt;

   return f1 === f2;
}

assert.ok( typestr2.call( "bar", undefined ), "typestr2" );

/*---------------------------------------------------------------------*/
/*    encoding                                                         */
/*---------------------------------------------------------------------*/
var su = unescape( "%C3%A0" );
var se = escape( su );

assert.ok( su.length === 2, "unescape1.length" );
assert.ok( su.charCodeAt( 0 ) === 0xc3 , "unescape1.charCodeAt( 0 )" );
assert.ok( su.charCodeAt( 1 ) === 0xa0 , "unescape1.charCodeAt( 1 )" );
assert.ok( se == "%C3%A0", "escape1" );

var su2 = unescape( "foo%C3%A0" );
var se2 = escape( su2 );

assert.ok( su2.length === 5, "unescape2.length" );
assert.ok( su2.charCodeAt( 3 ) === 0xc3 , "unescape2.charCodeAt( 0 )" );
assert.ok( su2.charCodeAt( 4 ) === 0xa0 , "unescape2.charCodeAt( 1 )" );
assert.ok( se2 == "foo%C3%A0", "escape2" );

var su3 = unescape( "bar%u5555foo" );
var se3 = escape( su3 );

assert.ok( su3.length === 7, "unescape3.length" );
assert.ok( su3.charCodeAt( 3 ) === 21845 , "unescape3.charCodeAt( 3 )" );
assert.ok( su3.charCodeAt( 4 ) === 102 , "unescape3.charCodeAt( 4 )" );
assert.ok( se3 == "bar%u5555foo", "escape3" );

var su4 = "foo" + su;
var se4 = escape( su4 );

assert.ok( su4.length === 5, "unescape4.length" );
assert.ok( su4.charCodeAt( 3 ) === 0xc3 , "unescape4.charCodeAt( 0 )" );
assert.ok( su4.charCodeAt( 4 ) === 0xa0 , "unescape4.charCodeAt( 1 )" );
assert.ok( se4 == "foo%C3%A0", "escape4" );

var se5 = '\0162';
assert.ok( se5.length === 2, "unescape5.length" );
assert.ok( se5.charCodeAt( 0 ) === 14, "unescape5.charCodeAt( 0 )" );
assert.ok( se5.charCodeAt( 1 ) === 50, "unescape5.charCodeAt( 1 )" );

var se6 = '\0162abc';
assert.ok( se6.length === 5, "unescape6.length" );
assert.ok( se6.charCodeAt( 0 ) === 14, "unescape6.charCodeAt( 0 )" );
assert.ok( se6.charCodeAt( 1 ) === 50, "unescape6.charCodeAt( 1 )" );

var se7 = '\016a';
assert.ok( se7.length === 2, "unescape7.length" );
assert.ok( se7.charCodeAt( 0 ) === 14, "unescape7.charCodeAt( 0 )" );
assert.ok( se7.charCodeAt( 1 ) === 97, "unescape7.charCodeAt( 1 )" );


/*---------------------------------------------------------------------*/
/*    replace                                                          */
/*---------------------------------------------------------------------*/
var str1 = "foobargeebar";
var re1 = /bar/;
var re2 = /bar/g;

assert.equal( str1.replace( "bar", "" ), "foogeebar" );
assert.equal( str1.replace( "bar", "----" ), "foo----geebar" );
assert.equal( str1.replace( /bar/, "----" ), "foo----geebar" );

assert.equal( str1.replace( /bar/g, "----" ), "foo----gee----" );

assert.equal( str1.replace( "bar", function( s ) { return "!" + s + "!" } ), "foo!bar!geebar" );
assert.equal( str1.replace( /bar/, function( s ) { return "!" + s + "!" } ), "foo!bar!geebar" );
assert.equal( str1.replace( /bar/g, function( s ) { return "!" + s + "!" } ), "foo!bar!gee!bar!" );

assert.equal( str1.replace( re1, function( s ) { return "!" + s + "!" } ), "foo!bar!geebar" );
assert.equal( re1.lastIndex, 0, "re1.lastIndex" );


assert.equal( str1.match( re2 )[ 0 ], "bar" );
assert.equal( re2.lastIndex, 0 );

assert.equal( re2.exec( str1 ).index, 3 );
assert.equal( re2.lastIndex, 6 );

assert.equal( str1.replace( re2, function( s ) { return "!" + s + "!" } ), "foo!bar!gee!bar!" );
assert.equal( re2.lastIndex, 0 );

assert.equal( "foo".indexOf( "" ), 0, "indexOf with empty string" );

/*---------------------------------------------------------------------*/
/*    replaceAll                                                       */
/*---------------------------------------------------------------------*/
function mdnReplaceAll() {
   const p = 'The quick brown for jumps over the lazy dog. If the dog reacted, was it really lazy?';
   const ra = p.replaceAll('dog', 'monkey');
   assert.equal(ra, 'The quick brown for jumps over the lazy monkey. If the monkey reacted, was it really lazy?');
   
   const regex = /Dog/ig;
   const rb = p.replaceAll(regex, 'ferret');
   assert.equal(rb, 'The quick brown for jumps over the lazy ferret. If the ferret reacted, was it really lazy?');
}

mdnReplaceAll();

/*---------------------------------------------------------------------*/
/*    keys                                                             */
/*---------------------------------------------------------------------*/
assert.ok( Object.keys( "foobar" ).length === 6, "Object.keys" );
