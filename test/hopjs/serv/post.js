var assert = require( "assert" );
var fs = require( "fs" );

var res = 0;
var content = "toto\nn'est\npas\ncontent";
var boundary = '-----------------------------71500674829540217534185294';

var fakeupload = '--' + boundary + '\r\n'
    + 'Content-Disposition: form-data; name="file"; filename="exfile"\r\n'
    + 'Content-Type: application/octet-stream\r\n\r\n'
    + content
    + '\r\n--' + boundary + '--\r\n';

/* server */ 		
service serv1( o ) {
   var x = (o && "x" in o) ? o.x : 10;
   var y = (o && "y" in o) ? o.y : 100;
   
   var x1 = Number( x ), y1 = Number( y );

   assert.ok( x1 > 0 );
   assert.ok( y1 > x1 );
   res++;
   
   return x1 + y1;
}

service upload( o ) {
   var file = (o && "file" in o) ? o.file : "no file";
   var stats = fs.statSync( file );
   var chars = fs.readFileSync( file );

   assert.ok( stats.isFile() && (Date.now() - stats.ctime.getTime() < 2000) );
   assert.equal( content, chars );
   res++;
   
   return 'OK' ;
}

service serv2( a, b ) {
   if( typeof a === "string" ) {
      return a.length == b.val.length + 1;
   } else {
      return a.val.length == b.length - 1;
   }
}

service serv3() {
   var o = {
      name: "foo",
      age: 34,
      birth: new Date(),
      re: /[ab]*c/,
      bo: new Boolean( false ),
      bo2: true,
      arr: [ { x: 10, y: 24}, { x: 14, z: 33.33 } ],
      i8: new Int8Array( [1,2,3,4,5,-6] ),
      u8: new Uint8Array( [1,2,3,4,5,-6] ),
      i16: new Int16Array( [256, 257, -258] ),
      u16: new Uint16Array( [257, -257] ),
      f32: new Float32Array( [1.0, 1.1, 1.2] ),
      f64: new Float64Array( [10001.0, 10001.1, 10001.2] ),
      dv: new DataView( new Int8Array( [1,2,3,4,5,-6] ).buffer ),
      buf: new Buffer( [-3,-2,-1,0,1,2,3,127 ] ),
      el: <DIV> { style: "border: 2px solid green", id: "bar", "toto" }
   };
   return o;
}
   
/* client */
var querystring = require( 'querystring' );
var http = require( 'http' );

function test() {
   var postData = querystring.stringify( {
      'x' : 1,
      'y' : 2,
   } );

   var req = http.request( {
      hostname: hop.hostname,
      port: hop.port,
      path: '/hop/serv1',
      method: 'POST',
      headers: {
	 'Content-Type': 'application/x-www-form-urlencoded',
	 'Content-Length': postData.length
      }
   }, function(result) {
      result.on( 'data', function( chunk ) {
	 assert.equal( chunk.toString(), "3" );
      } );
   } );

   req.write( postData );
   req.end();

   var req = http.request( {
      hostname: hop.hostname,
      port: hop.port,
      path: '/hop/upload',
      method: 'POST',
      headers: {
	 'Content-Type': 'multipart/form-data; boundary=' + boundary,
	 'Content-Length': fakeupload.length
      }
   } );

   req.on( 'response', function( result ) {
      assert.ok( result.statusCode == 200 );
      result.on( 'data', function ( chunk ) {
	 assert.ok( chunk.toString() == "OK" );
      });
   } );

   req.write( fakeupload );
   req.end();

   serv2( "foobar+", { x: 0, val: "foobar" } )
      .post( function( v ) { assert.ok( v ) } );
   serv2( { x: 0, val: "foobar" }, "foobar+" )
      .post( function( v ) { assert.ok( v ) } );

   serv3()
      .post( function( v ) {
	 assert.ok( v.name === "foo" );
	 assert.ok( v.age === 34 );
	 assert.ok( v.birth instanceof Date );
	 assert.ok( v.re instanceof RegExp );
	 assert.ok( v.bo instanceof Boolean );
	 assert.ok( v.bo2 );
	 assert.ok( v.arr[ 0 ].x == (v.arr[ 1 ].x - 4) );
	 assert.ok( v.arr.length == 2 );
	 assert.ok( v.i8[ 5 ] == -6 );
	 assert.ok( v.i8[ 4 ] == 5 );
	 assert.ok( v.u8[ 4 ] == 5 );
	 assert.ok( v.i16[ 1 ] == 257 );
	 assert.ok( v.f32[ 0 ] < 1.1 );
	 assert.ok( v.f32[ 0 ] > 0.9 );
	 assert.ok( v.buf instanceof Buffer );
	 assert.ok( v.el.id === "bar" );
	 res++;
      } );
}

setTimeout( function() {
   try {
      assert.ok( res === 3 );
   } finally {
      process.exit( res === 3 ? 0 : 1 );
   }
}, 500 );

test();


