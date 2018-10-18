/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/string.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Sat Sep 30 09:35:05 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
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

assert.equal( s5.length, 4, "utf16 string length" );

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

