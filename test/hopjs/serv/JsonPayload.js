"use hopscript";

var path = require( 'path' );
var fs = require( 'fs' );
var assert = require( 'assert' );
var util = require( 'util' );


var smallFile =  module.filename;
var largeFile =  path.dirname( module.filename ) + '/../../../bin/hop';

var refData = {};
refData[ smallFile ] = fs.readFileSync( smallFile );
refData[ largeFile ] = fs.readFileSync( largeFile );


service getPayload( filename ) {
   console.log( "getPayload ", filename ); 
   return hop.HTTPResponseJson( { filename: filename, payloadType: 'Buffer', payload: refData[ filename ] } );
}

service getPayloadWebService( o ) {
   var filename = (o && "filename" in o) ? o.filename : smallFile;
   return hop.HTTPResponseJson( { filename: filename, payloadType: 'Buffer', payload: refData[ filename ]});
}

service getPayloadWSBase64( o ) {
   var filename = (o && "filename" in o) ? o.filename : smallFile;
   return hop.HTTPResponseJson( { filename: filename, payloadType: 'String', payloadEncoding: 'base64', payload: refData[ filename ].toString( 'base64' )});
}

var url = util.format( 'http://%s:%s/hop/getPayloadWebService', hop.hostname, hop.port );
var webService = hop.webService( url );

var webServiceBase64 = hop.webService(
   util.format( 'http://%s:%s/hop/getPayloadWSBase64',
		hop.hostname, hop.port ));

// currently JSON.stringify serialises Buffer objects like
// arrays. This is inconsistent with Node.js. We just check that the
// array and the original Buffer ocntain the same values.

function processJsonResult( arg ) {
   return function( result ) {
      var filename = result.filename;
      assert.equal( filename, arg );
      var copy =  result.payload;
      if (result.payloadType == 'String' ) {
	 copy = new Buffer( copy, result.payloadEncoding )
      };
      if (Array.isArray( copy )) {
	 console.log( 'Warning: Array. A buffer was expected!' );
	 copy = new Buffer( copy );
      };
      if (Buffer.isBuffer( copy )) {
	 console.log( 'filename', filename );
	 console.log( 'payload size:', copy.length );
	 assert.equal( copy.length, refData[ arg ].length );
	 for (var i = 0; i < copy.length; i++) {
	    assert.equal( copy[ i ], refData[ arg ][i] );
	 };
      } else fail();
      pass();
   }
}

var passed = 0
var nextTest = 0;

function next() {
   var testFunction = testSuite[ nextTest ];
   console.log( 'running test', nextTest );
   nextTest ++;
   testFunction();
}

function pass() {
   passed++;
   if ( passed == testSuite.length ) {
      console.log( 'All tests passed' );
      process.exit( 0 );
   } else {
      next();
   };
}

function fail( err = undefined ) {
   console.log( 'Test failed', err );
   process.exit( 1 );
}



var testSuite = [
   function() {
      getPayload( smallFile ).post( processJsonResult( smallFile ), fail );
   },
   function() {
      getPayload( largeFile ).post( processJsonResult( largeFile ), fail );
   },
   function() {
      getPayloadWebService( { filename: smallFile } ).post( processJsonResult( smallFile), fail );
   },
   function() {
      getPayloadWebService( { filename: largeFile } ).post( processJsonResult( largeFile), fail );
   },
   function() {
      webService( { filename: smallFile } ).post( processJsonResult( smallFile), fail );
   },
   function() {
      webService( { filename: largeFile } ).post( processJsonResult( largeFile), fail );
   },
   function() {
      webServiceBase64( { filename: smallFile } ).post( processJsonResult( smallFile), fail );
   },
   function() {
      webServiceBase64( { filename: largeFile } ).post( processJsonResult( largeFile), fail );
   },
];

next();







