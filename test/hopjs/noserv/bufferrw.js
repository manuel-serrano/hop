/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/bufferrw.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Thu Feb 24 17:04:05 2022 (serrano)                */
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
function bufferFromArray() {
   const bf1 = Buffer.from([0x62, 0x75, 0x66, 0x66, 0x65, 0x72]);
   assert.equal(bf1.toString(), "buffer");
}

function bufferFromArrayBuffer() {
   const arr = new Uint16Array(2);

   arr[0] = 5000;
   arr[1] = 4000;

   // Shares memory with `arr`.
   const bf2 = Buffer.from(arr.buffer);
   assert.ok(bf2[0] === 0x88 && bf2[1] === 0x13 && bf2[2] === 0xa0 && bf2[3] === 0xf);

   // Changing the original Uint16Array changes the Buffer also.
   arr[1] = 6000;

   assert.ok(bf2[0] === 0x88 && bf2[1] === 0x13 && bf2[2] === 0x70 && bf2[3] === 0x17);

   const ab = new ArrayBuffer(10);
   const bf3 = Buffer.from(ab, 0, 2);

   assert.equal(bf3.length, 2);

   const arrA = Uint8Array.from([0x63, 0x64, 0x65, 0x66]); // 4 elements
   const arrB = new Uint8Array(arrA.buffer, 1, 2); // 2 elements
   console.log(arrA.buffer === arrB.buffer); // true

   const bf4 = Buffer.from(arrB.buffer);
   assert.ok(bf4[0] === 0x63 && bf4[1] === 0x64 && bf4[2] === 0x65 && bf4[3] === 0x66); 
}

function bufferFromBuffer() {
   const buf1 = Buffer.from('buffer');
   const buf2 = Buffer.from(buf1);

   buf1[0] = 0x61;

   assert.equal(buf1.toString(), "auffer");
   assert.equal(buf2.toString(), "buffer");
}

function bufferFromObject() {
   class Foo {
      [Symbol.toPrimitive]() {
         return 'this is a test';
      }
   }
   const buf = Buffer.from(new String('this is a test'));
   const buf2 = Buffer.from(new Foo(), 'utf8');
   assert.equal(buf.toString(), "this is a test");
   assert.equal(buf2.toString(), "this is a test");
}

function bufferFromString() {
   const buf1 = Buffer.from('this is a tést');
   const buf2 = Buffer.from('7468697320697320612074c3a97374', 'hex');

   console.log(buf1.toString());
   // Prints: this is a tést
   console.log(buf2.toString());
   // Prints: this is a tést
   console.log(buf1.toString('latin1'));
   // Prints: this is a tÃ©st}
}

bufferFromArray();
bufferFromArrayBuffer();
bufferFromBuffer();
bufferFromString();
