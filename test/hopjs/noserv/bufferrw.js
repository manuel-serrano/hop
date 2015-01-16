/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/buffloat.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Sun Sep 28 19:45:55 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
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
   


