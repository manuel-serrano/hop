"use hopscript";

const path = require( 'path' );
const fs = require( 'fs' );
const assert = require( 'assert' );
const util = require( 'util' );


const smallFile =  module.filename;
const largeFile =  path.dirname( module.filename ) + '/../../../bin/hop';

const refData = {};
refData[ smallFile ] = fs.readFileSync( smallFile );
refData[ largeFile ] = fs.readFileSync( largeFile );


service getPayload( filename ) {
   console.log( "getPayload ", filename );
   const x = { filename: filename, payloadType: 'Buffer', payload: refData[ filename ] }
   return hop.HTTPResponseJson( x );
}

service getPayloadWebService( o ) {
   const filename = (o && "filename" in o) ? o.filename : smallFile;
   return hop.HTTPResponseJson( { filename: filename, payloadType: 'Buffer', payload: refData[ filename ]});
}

service getPayloadWSBase64( o ) {
   const filename = (o && "filename" in o) ? o.filename : smallFile;
   return hop.HTTPResponseJson( { filename: filename, payloadType: 'String', payloadEncoding: 'base64', payload: refData[ filename ].toString( 'base64' )});
}

const url = util.format( 'http://%s:%s/hop/getPayloadWebService', 'localhost', hop.port );

const webService = hop.webService( url );


const webServiceBase64 = hop.webService(
   util.format( 'http://%s:%s/hop/getPayloadWSBase64',
		'localhost', hop.port ));

// currently JSON.stringify serialises Buffer objects like
// arrays. This is inconsistent with Node.js. We just check that the
// array and the original Buffer ocntain the same values.

function processJsonResult( arg ) {
   return function( result ) {
      const filename = result.filename;
      assert.equal( filename, arg );
      let copy = result.payload;
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
	 for (let i = 0; i < copy.length; i++) {
	    assert.equal( copy[ i ], refData[ arg ][i] );
	 };
      } else fail();
      pass();
   }
}

let passed = 0
let nextTest = 0;

function next() {
   const testFunction = testSuite[ nextTest ];
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



const testSuite = [
   function() {
      getPayload( smallFile ).post( processJsonResult( smallFile ), fail );
   },
   function() {
      getPayload( largeFile ).post( processJsonResult( largeFile ), fail );
   },
/*    function() {                                                     */
/*       getPayloadWebService( { filename: smallFile } ).post( processJsonResult( smallFile), fail ); */
/*    },                                                               */
/*    function() {                                                     */
/*       getPayloadWebService( { filename: largeFile } ).post( processJsonResult( largeFile), fail ); */
/*    },                                                               */
/*    function() {                                                     */
/*       webService( { filename: smallFile } ).post( processJsonResult( smallFile), fail ); */
/*    },                                                               */
/*    function() {                                                     */
/*       webService( { filename: largeFile } ).post( processJsonResult( largeFile), fail ); */
/*    },                                                               */
/*    function() {                                                     */
/*       webServiceBase64( { filename: smallFile } ).post( processJsonResult( smallFile), fail ); */
/*    },                                                               */
/*    function() {                                                     */
/*       webServiceBase64( { filename: largeFile } ).post( processJsonResult( largeFile), fail ); */
/*    },                                                               */
];

next();







