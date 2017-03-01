/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/number.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:25 2014                          */
/*    Last change :  Tue Feb 14 20:34:57 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing JS numbers                                               */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    associativity                                                    */
/*---------------------------------------------------------------------*/
assert.equal( 2+3*4, 2+(3*4), "priority.1" );
assert.equal( 2*3+4, (2*3)+4, "priority.2" );

/*---------------------------------------------------------------------*/
/*    toFixed                                                          */
/*---------------------------------------------------------------------*/
var a = -0.14445826138806367; 
assert.equal( a.toFixed( 2 ), "-0.14", "toFixed1" );

var b = 0.14445826138806367; 
assert.equal( b.toFixed( 2 ), "0.14", "toFixed2" );

var c = -1.14445826138806367; 
assert.equal( c.toFixed( 2 ), "-1.14", "toFixed3" );

var d = 1.14445826138806367; 
assert.equal( d.toFixed( 2 ), "1.14", "toFixed4" );

var e = -145564.14445826138806367; 
assert.equal( e.toFixed( 2 ), "-145564.14", "toFixed5" );

var f = 6541.14445826138806367; 
assert.equal( f.toFixed( 2 ), "6541.14", "toFixed6" );

var g = 0.07;
assert.equal( g.toFixed( 3 ), "0.070", "toFixed7" );

var h = -0.07;
assert.equal( h.toFixed( 3 ), "-0.070", "toFixed8" );

var i = 0.07;
assert.equal( i.toFixed( 1 ), "0.1", "toFixed9" );

var j = -0.07;
assert.equal( h.toFixed( 1 ), "-0.1", "toFixed10" );

var k = 0.07;
assert.equal( k.toFixed( 2 ), "0.07", "toFixed11" );

var l = -0.07;
assert.equal( l.toFixed( 2 ), "-0.07", "toFixed12" );

var m = 0.07;
assert.equal( m.toFixed( 4 ), "0.0700", "toFixed13" );

var n = -0.07;
assert.equal( n.toFixed( 4 ), "-0.0700", "toFixed14" );

var o = 0.9;
assert.ok( 1.0 === o +  0.1, "float.eq" );
assert.ok( 1 === o + 0.1, "float-int.eq" );

/*---------------------------------------------------------------------*/
/*    integer limits                                                   */
/*---------------------------------------------------------------------*/
var x = 4294967295;

assert.equal( x, +x, "+x" );
assert.equal( x, Math.ceil( x ), "ceil" );

assert.ok( Math.floor( 2147483647.0 ) > 0, "floor" );

var b = [ 0xff, 0xff, 0xff, 0xff ];

function foo( b ) {
  return ((b[0]) | (b[1] << 8) | (b[2] << 16)) + (b[3] * 0x1000000);
}

assert.ok( foo( b ) === 0xFFFFFFFF, "maxint" );
