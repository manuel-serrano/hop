/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/bufferrw.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Thu Feb 11 11:05:22 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Extra Nodejs buffer Testing                                      */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    stringify                                                        */
/*---------------------------------------------------------------------*/
var buf = new Buffer( 8, 0 );

buf.writeUInt8( 4, 3 );
assert.equal( buf.readUInt8( 3 ), 4 );

var buf = new Buffer( 6 );
buf.fill( 0 );

buf.writeFloatLE( 3.4, 2 );
assert.equal( Math.round( buf.readFloatLE( 2 ) ), 3.0 );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0x9a, 0x99, 0x59, 0x40 ] ) );

buf.writeFloatBE( 14.85, 2 );
assert.equal( Math.round( buf.readFloatBE( 2 )) , 15.0 );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0x41, 0x6d, 0x99, 0x9a ] ) );

var buf = new Buffer( 10 );
buf.fill( 0 );
buf.writeDoubleLE( 1233.46, 2 );
assert.equal( Math.round( buf.readDoubleLE( 2  )), 1233.0 );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0xa4, 0x70, 0x3d, 0x0a, 0xd7, 0x45, 0x93, 0x40] ) );

buf.writeDoubleBE( 1233.46, 2 );
assert.equal( Math.round( buf.readDoubleBE( 2 )), 1233.0 );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0x40, 0x93, 0x45, 0xd7, 0x0a, 0x3d, 0x70, 0xa4 ] ) );
   

/*---------------------------------------------------------------------*/
/*    Encoding                                                         */
/*---------------------------------------------------------------------*/
var buf = new Buffer( 2 );
buf[ 0 ] = 195;
buf[ 1 ] = 169;

var s1 = buf.toString( "utf8" );

assert.strictEqual( s1.charCodeAt( 0 ), 233 );

buf = new Buffer( 1 );
buf[ 0 ] = 255;

var s1 = buf.toString( "utf8" );

assert.strictEqual( s1.length, 1 );
assert.strictEqual( s1.charCodeAt( 0 ), 65533 );

var b2 = new Buffer( s1 );
assert.strictEqual( b2.length, 3, "b2.length" );
assert.strictEqual( b2[ 0 ], 0xef );
assert.strictEqual( b2[ 1 ], 0xbf );
assert.strictEqual( b2[ 2 ], 0xbd );
