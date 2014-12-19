/*=====================================================================*/
/*    .../project/hop/3.0.x/examples/arraybuffer/arraybuffer.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jun 20 18:38:17 2014                          */
/*    Last change :  Wed Dec 17 16:43:50 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    ArrayBuffer example                                              */
/*    see http://www.html5rocks.com/en/tutorials/webgl/typed_arrays/   */
/*    -------------------------------------------------------------    */
/*    run: hopc -v -g arraybuffer.js && ./a.out                        */
/*=====================================================================*/
var b = new ArrayBuffer( 16 );
var b32 = new Int32Array( b, 0, 2 );
b32[ 0 ] = 0x777777;

console.log( "1 ===================================" );
console.log( "b32.length=", b32.length );

console.log( "b32[ 0 ]=", b32[ 0 ] );
console.log( "b=", b[ 0 ], b[ 1 ], b[ 2 ], b[ 3 ] );
console.log( "2 ===================================" );
console.log( "b32=", b32 );

console.log( "3 ===================================" );
var b8 = new Int8Array( 10 );

b8.set( b32, 2 );

console.log( b8 );

console.log( "4 ===================================" );
var b8b = b8.subarray( 2, 6 );
b8b[ 3 ] = 33;

console.log( b8b );
console.log( "%d/%d", b8[ 5 ], b8.get( 5 ) );

console.log( "5 ===================================" );
var b16 = new Int16Array( [ 1,2,3,4,0xffff,0x10000 ] );

console.log( b16 );

console.log( "6 ===================================" );
b16.set( [12,13,14], 1 );
console.log( b16 );

console.log( "7 ===================================" );
var f32 = new Float32Array( 2 );

f32[ 1 ] = 3.14;
console.log( f32 );

console.log( "8 ===================================" );
var dv = new DataView( b );

var ia = new Int8Array( b, 0, 8 );
ia.set( [1, 2, 3, 4, 5, 6, 7, 8 ] );
console.log( ia );

console.log( "0=%s 1=%s", dv.getInt16( 0 ), dv.getInt16( 1 ) );
console.log( "0=%s 1=%s", dv.getInt16( 0, true ), dv.getInt16( 1, true ) );
console.log( "0=%s 1=%s", dv.getInt16( 0, false ), dv.getInt16( 1, false ) );

dv.setInt16( 1, 0x1234 );
console.log( "1a=%s", dv.getInt16( 1, true ) );
console.log( "1b=%s", dv.getInt16( 1, false ) );

dv.setInt16( 1, 0x1234, true );
console.log( "1c=%s", dv.getInt16( 1, true ) );
console.log( "1d=%s", dv.getInt16( 1, false ) );

dv.setInt16( 1, 0x1234, false );
console.log( "1e=%s", dv.getInt16( 1, true ) );
console.log( "1f=%s", dv.getInt16( 1, false ) );

dv.setInt16( 1, -0x1234, true );
console.log( "1g=%s", dv.getInt16( 1, true ) );
console.log( "1h=%s", dv.getInt16( 1, false ) );

dv.setInt16( 1, -0x1234, false );
console.log( "1i=%s", dv.getInt16( 1, true ) );
console.log( "1j=%s", dv.getInt16( 1, false ) );

console.log( "9 ===================================" );

console.log( "0=%s 1=%s", dv.getInt32( 0 ), dv.getInt32( 4 ) );
console.log( "0=%s 1=%s", dv.getInt32( 0, true ), dv.getInt32( 4, true ) );
console.log( "0=%s 1=%s", dv.getInt32( 0, false ), dv.getInt32( 4, false ) );

dv.setInt32( 4, 0x1234 );
console.log( "1a=%s", dv.getInt32( 4, true ) );
console.log( "1b=%s", dv.getInt32( 4, false ) );

dv.setInt32( 4, 0x1234, true );
console.log( "1c=%s", dv.getInt32( 4, true ) );
console.log( "1d=%s", dv.getInt32( 4, false ) );

dv.setInt32( 4, 0x1234, false );
console.log( "1e=%s", dv.getInt32( 4, true ) );
console.log( "1f=%s", dv.getInt32( 4, false ) );

dv.setInt32( 4, -0x1234, true );
console.log( "1g=%s", dv.getInt32( 4, true ) );
console.log( "1h=%s", dv.getInt32( 4, false ) );

dv.setInt32( 4, -0x1234, false );
console.log( "1i=%s", dv.getInt32( 4, true ) );
console.log( "1j=%s", dv.getInt32( 4, false ) );

console.log( "10 ===================================" );

ia.set( [1, 2, 3, 4, 5, 6, 7, 8 ] );

console.log( "0.i=%s 1=%s", dv.getFloat32( 0 ), dv.getFloat32( 4 ) );
console.log( "0.ii=%s 1=%s", dv.getFloat32( 0, true ), dv.getFloat32( 4, true ) );
console.log( "0.iii=%s 1=%s", dv.getFloat32( 0, false ), dv.getFloat32( 4, false ) );

dv.setFloat32( 4, 0x1234 );
console.log( "1a=%s", dv.getFloat32( 4, true ) );
console.log( "1b=%s", dv.getFloat32( 4, false ) );

dv.setFloat32( 4, 0x1234, true );
console.log( "1c=%s", dv.getFloat32( 4, true ) );
console.log( "1d=%s", dv.getFloat32( 4, false ) );

dv.setFloat32( 4, 0x1234, false );
console.log( "1e=%s", dv.getFloat32( 4, true ) );
console.log( "1f=%s", dv.getFloat32( 4, false ) );

dv.setFloat32( 4, -0x1234, true );
console.log( "1g=%s", dv.getFloat32( 4, true ) );
console.log( "1h=%s", dv.getFloat32( 4, false ) );

dv.setFloat32( 4, -0x1234, false );
console.log( "1i=%s", dv.getFloat32( 4, true ) );
console.log( "1j=%s", dv.getFloat32( 4, false ) );

console.log( "11 ===================================" );

console.log( "0=%s 1=%s", dv.getFloat64( 0 ), dv.getFloat64( 1 ) );
console.log( "0=%s 1=%s", dv.getFloat64( 0, true ), dv.getFloat64( 1, true ) );
console.log( "0=%s 1=%s", dv.getFloat64( 0, false ), dv.getFloat64( 1, false ) );

dv.setFloat64( 1, 0x123456789ab );
console.log( b );
dv.setFloat64( 1, 0x123456789ab, true );
console.log( b );
console.log( "1a=%s", dv.getFloat64( 1, true ) );
console.log( "1b=%s", dv.getFloat64( 1, false ) );

dv.setFloat64( 1, 0x1234, true );
console.log( "1c=%s", dv.getFloat64( 1, true ) );
console.log( "1d=%s", dv.getFloat64( 1, false ) );

dv.setFloat64( 1, 0x1234, false );
console.log( "1e=%s", dv.getFloat64( 1, true ) );
console.log( "1f=%s", dv.getFloat64( 1, false ) );

dv.setFloat64( 1, -0x1234, true );
console.log( "1g=%s", dv.getFloat64( 1, true ) );
console.log( "1h=%s", dv.getFloat64( 1, false ) );

dv.setFloat64( 1, -0x1234, false );
console.log( "1i=%s", dv.getFloat64( 1, true ) );
console.log( "1j=%s", dv.getFloat64( 1, false ) );

service arraybuffer() {
   return <HTML> {
      <BODY> {
	 "Console only example"
      }
   }
}
