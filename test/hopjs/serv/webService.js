/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/webService.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  11 14:00:00 2015                         */
/*    Last change :  Thu Nov 26 07:56:27 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing WebService                                               */
/*=====================================================================*/
"use hopscript";

var assert = require( 'assert' );

function test3rdPartyWS() {
   var mymemory = hop.webService( "http://mymemory.translated.net/api/get" );
   mymemory( {q: 'My tailor is rich.', langpair: 'en|fr' } ).post( function( result ) {
      console.log( result.responseData );
      if (!result.responseData.translatedText ) {
	 process.exit();
      } else {
	 console.log( 'memory test passed' );
	 finish();
      }
   }, { fail: function( error ) {
      console.log( 'mymemory test fail' );
      process.exit( 1 );
   }});
}

service myNamedArgsService( o ) {
   var arg = (o && "arg" in o) ? o.arg : "default";
   console.log( 'server side: arg = %s, (%s)', arg, typeof( arg ));
   return arg ;
}

function testLocalWS() {

   var myWebService =
       hop.webService( 'http://' + hop.hostname + ':' + hop.port + '/hop/myNamedArgsService' );

   function test( count = undefined ) {
      var expected;
      var positive;
      var args;
      var frame;
      var next;
      if (count == testValues.length) {
	 console.log( 'local tests completed' );
	 //	 test3rdPartyWS();
	 process.exit( 0 );
	 return;
      };
      if ( count == undefined ) { // call with no arguments
	 positive = true;
	 args = undefined;
	 expected = 'default';
	 frame = myWebService();
	 next = 0;
      } else {
	 positive = testValues[ count ].positive;
	 args = testValues[ count ].args;
	 expected = args.arg || 'default';
	 frame = myWebService( args );
	 next = count + 1;
      };
      console.log( "frame=", frame.toString() );
      frame.post( function( result ) {
	 if (positive) {
	    console.log( expected, result );
	    console.log( typeof( expected), typeof( result ));
	    assert.equal( expected, result );
	    console.log( 'test %s passed', count );
	    test( next );
	 } else {
	    console.log( 'test %s fail', count, testValues[ count ] );
	    process.exit( 1 );
	 }
      }, function( error ) {
	 if (positive) {
	    console.log( 'test %s fail', count, testValues[ count ] );
	    process.exit( 1 );
	 } else {
	    console.log( 'test %s passed', count );
	    test( next );
	 }
      });
      console.log( 'test %s running', count );

   }

   var testValues = [
      { positive: true, args: {} },
      { positive: true, args: { arg: 'my string' }},
      { positive: true, args: { arg: 'used' }},
      { positive: true, args: { arg: 2015 }},
   ];

   test();
}

function finish() {
   console.log( 'webService tests passed' );
   process.exit( 0 );
}

testLocalWS();

setTimeout( function() {
   console.log( 'timeout: test failed' );
   process.exit( 1 );
}, 2000 );

