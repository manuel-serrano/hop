/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/bufferrw.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Thu Feb 24 12:55:04 2022 (serrano)                */
/*    Copyright   :  2014-22 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Extra Nodejs buffer Testing                                      */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    stringify                                                        */
/*---------------------------------------------------------------------*/
var buf = new Buffer( 8, 0 );

buf.writeUInt8( 4, 3 );
assert.equal( buf.readUInt8( 3 ), 4, "writeUint8" );

var buf = new Buffer( 6 );
buf.fill( 0 );

buf.writeFloatLE( 3.4, 2 );
assert.equal( Math.round( buf.readFloatLE( 2 ) ), 3.0, "writeFloatLE" );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0x9a, 0x99, 0x59, 0x40 ] ), "buffer.1" );

buf.writeFloatBE( 14.85, 2 );
assert.equal( Math.round( buf.readFloatBE( 2 )) , 15.0, "writeFloatBE" );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0x41, 0x6d, 0x99, 0x9a ] ), "buffer.2" );

var buf = new Buffer( 10 );
buf.fill( 0 );
buf.writeDoubleLE( 1233.46, 2 );
assert.equal( Math.round( buf.readDoubleLE( 2  )), 1233.0, "writeDoubleLE" );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0xa4, 0x70, 0x3d, 0x0a, 0xd7, 0x45, 0x93, 0x40] ),
   "buffer.3" );

buf.writeDoubleBE( 1233.46, 2 );
assert.equal( Math.round( buf.readDoubleBE( 2 )), 1233.0, "writeDoubleBE" );

assert.deepEqual(
   buf, new Buffer( [ 0, 0, 0x40, 0x93, 0x45, 0xd7, 0x0a, 0x3d, 0x70, 0xa4 ] ),
   "buffer.4" );
   
/*---------------------------------------------------------------------*/
/*    Encoding                                                         */
/*---------------------------------------------------------------------*/
var buf = new Buffer( 2 );
buf[ 0 ] = 195;
buf[ 1 ] = 169;

var s1 = buf.toString( "utf8" );

assert.strictEqual( s1.charCodeAt( 0 ), 233, "utf8" );

buf = new Buffer( 1 );
buf[ 0 ] = 255;

var s1 = buf.toString( "utf8" );

assert.strictEqual( s1.length, 1, "utf8.length" );
assert.strictEqual( s1.charCodeAt( 0 ), 65533, "utf8" );

var b2 = new Buffer( s1 );
assert.strictEqual( b2.length, 3, "b2.length" );
assert.strictEqual( b2[ 0 ], 0xef );
assert.strictEqual( b2[ 1 ], 0xbf );
assert.strictEqual( b2[ 2 ], 0xbd );

/*---------------------------------------------------------------------*/
/*    from                                                             */
/*---------------------------------------------------------------------*/
const bf1 = Buffer.from([0x62, 0x75, 0x66, 0x66, 0x65, 0x72]);
assert.equal(bf1.toString(), "buffer");

/* const arr = new Uint16Array(2);                                     */
/*                                                                     */
/* arr[0] = 5000;                                                      */
/* arr[1] = 4000;                                                      */
/*                                                                     */
/* // Shares memory with `arr`.                                        */
/* const buf = Buffer.from(arr.buffer);                                */
/*                                                                     */
/* console.log(buf);                                                   */
/* // Prints: <Buffer 88 13 a0 0f>                                     */
/*                                                                     */
/* // Changing the original Uint16Array changes the Buffer also.       */
/* arr[1] = 6000;                                                      */
/*                                                                     */
/* console.log(buf);                                                   */
// Prints: <Buffer 88 13 70 17>

/* const ab = new ArrayBuffer(10);                                     */
/* const buf = Buffer.from(ab, 0, 2);                                  */
/*                                                                     */
/* console.log(buf.length);                                            */
/* // Prints: 2                                                        */


/* const arrA = Uint8Array.from([0x63, 0x64, 0x65, 0x66]); // 4 elements */
/* const arrB = new Uint8Array(arrA.buffer, 1, 2); // 2 elements       */
/* console.log(arrA.buffer === arrB.buffer); // true                   */
/*                                                                     */
/* const buf = Buffer.from(arrB.buffer);                               */
/* console.log(buf);                                                   */
/* // Prints: <Buffer 63 64 65 66>                                     */


/* const buf1 = Buffer.from('buffer');                                 */
/* const buf2 = Buffer.from(buf1);                                     */
/*                                                                     */
/* buf1[0] = 0x61;                                                     */
/*                                                                     */
/* console.log(buf1.toString());                                       */
/* // Prints: auffer                                                   */
/* console.log(buf2.toString());                                       */
/* // Prints: buffer                                                   */
