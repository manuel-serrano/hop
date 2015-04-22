var hop = require( "hop" );
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
service serv1( { x: 10, y: 100 } ) {
   var x1 = Number( x ), y1 = Number( y );

   assert.ok( x1 > 0 );
   assert.ok( y1 > x1 );
   res++;
   
   return x1 + y1;
}

service upload( { file: 'no file' } ) {
   var stats = fs.statSync( file );
   var chars = fs.readFileSync( file );

   assert.ok( stats.isFile() && (Date.now() - stats.ctime.getTime() < 2000) );
   assert.equal( content, chars );
   res++;
   
   return 'OK' ;
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
}

setTimeout( function() {
   assert.ok( res === 2 );
   process.exit( res === 2 ? 0 : 1 );
}, 500 );

test();


