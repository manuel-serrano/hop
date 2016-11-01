/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/number.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:25 2014                          */
/*    Last change :  Mon Oct 31 19:22:37 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing JS numbers                                               */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    associativity                                                    */
/*---------------------------------------------------------------------*/
assert.equal( 2+3*4, 2+(3*4) );
assert.equal( 2*3+4, (2*3)+4 );

/*---------------------------------------------------------------------*/
/*    toFixed                                                          */
/*---------------------------------------------------------------------*/
var a = -0.14445826138806367; 
assert.equal( a.toFixed( 2 ), "-0.14" );

var b = 0.14445826138806367; 
assert.equal( b.toFixed( 2 ), "0.14" );

var c = -1.14445826138806367; 
assert.equal( c.toFixed( 2 ), "-1.14" );

var d = 1.14445826138806367; 
assert.equal( d.toFixed( 2 ), "1.14" );

var e = -145564.14445826138806367; 
assert.equal( e.toFixed( 2 ), "-145564.14" );

var f = 6541.14445826138806367; 
assert.equal( f.toFixed( 2 ), "6541.14" );

var g = 0.07;
assert.equal( g.toFixed( 3 ), "0.070" );

var h = -0.07;
assert.equal( h.toFixed( 3 ), "-0.070" );

var i = 0.07;
assert.equal( i.toFixed( 1 ), "0.1" );

var j = -0.07;
assert.equal( h.toFixed( 1 ), "-0.1" );

var k = 0.07;
assert.equal( k.toFixed( 2 ), "0.07" );

var l = -0.07;
assert.equal( l.toFixed( 2 ), "-0.07" );

var m = 0.07;
assert.equal( m.toFixed( 4 ), "0.0700" );

var n = -0.07;
assert.equal( n.toFixed( 4 ), "-0.0700" );

/*---------------------------------------------------------------------*/
/*    integer limits                                                   */
/*---------------------------------------------------------------------*/
var x = 4294967295;

assert.equal( x, +x );
assert.equal( x, Math.ceil( x ) );

assert.ok( Math.floor( 2147483647.0 ) > 0 );

var b = [ 0xff, 0xff, 0xff, 0xff ];

function foo( b ) {
  return ((b[0]) | (b[1] << 8) | (b[2] << 16)) + (b[3] * 0x1000000);
}

assert.ok( foo( b, 0 ) === 0xFFFFFFFF, "maxint" );
