/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/serv/serviceArguments.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  25 11:43:00 2015                         */
/*    Last change :  Sat Sep 26 18:13:36 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Test service constructor and arguments                           */
/*=====================================================================*/

var assert = require( 'assert' );

var svc = service() {
   return arguments.length;
};

var svc1 = service() {
   return arguments[ 0 ];
};

var svc2 = service() {
   return arguments[ arguments.length - 1 ];
};

var svcnew = new Service( function() {
   return arguments.length;
});

var svcnew1 = new Service( function() {
   return arguments[ 0 ];
});

var svcnew2 = new Service( function() {
   return arguments[ arguments.length - 1 ];
});

var svcF = service( a, b ) {
   assert.equal( a, arguments[ 0 ] );
   assert.equal( b, arguments [ 1 ] );
   return b;
};

var svcN = service( { a: 1, b: 'foo' } ) {
   return b;
};

var svcNN = service svcName( {a: 1, b: 'foo' } ) {
   return a;
};

/* I'd expect svcNN to have this path:
assert.equal( svcNN.path, '/hop/svcName' );
*/

var svcnew3 = new Service( function() {
   return true;
}, 'path' );

assert.equal( svcnew3.path, '/hop/path' );

var svcnew4 = new Service( function( a , b ) {
   return b;
}, 'servName', { a: 1, b: 'foo' } );

var svcCount= 0;

var testSuite = [
   function() {
      svc().post( function( result ) {
	 assert.equal( result, 0 );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc( 'foo' ).post( function( result ) {
	 assert.equal( result, 1 );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 3 );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc1().post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc1( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc1( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc2().post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc2( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svc2( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'gee' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew().post( function( result ) {
	 assert.equal( result, 0 );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew( 'foo' ).post( function( result ) {
	 assert.equal( result, 1 );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 3 );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew1().post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew1( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew1( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew2().post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew2( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew2( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'gee' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcF().post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcF( 'foo' ).post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcF( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcN().post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcN( {} ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcN( { a : 2, b : 'bar' } ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcN( { b: 'bar', a: 2 } ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcN( { a: 2 } ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcN( { c: 0 } ).post( fail, { fail: pass });
   },
   function() {
      svcnew4( {} ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew4().post( function( result ) {
    	 assert.equal( result, 'foo' );
    	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew4( { a : 2, b : 'bar' } ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew4( { b: 'bar', a: 2 } ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew4( { a: 2 } ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, { fail: fail });
   },
   function() {
      svcnew4( { c: 0 } ).post( fail, { fail: pass });
   },
];

var passed = 0

function pass() {
   if ( passed == testSuite.length ) {
      console.log( 'All tests passed' );
      process.exit( 0 );
   } else {
      var next = passed;
      passed++;
      console.log( 'running test', next );
      testSuite[ next ]();
   };
}

function fail() {
   console.log( 'service invocation failed' );
   process.exit( 1 );
}



setTimeout( function() {
   console.log( 'timeout' );
   process.exit( 1 );
}, 1000 );

console.log( 'Test vector length', testSuite.length );
pass();

