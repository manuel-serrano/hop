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

var hop = require( 'hop' );
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

assert.equal( svcnew4.path, '/hop/servName' );

var svcCount= 0;

function fn10( fName, fAge ) {
   return hop.HTTPResponseAsync( function( sendResponse) {
      sendResponse( { name: fName, age: fAge } );
   }, this );
}

var svc10 = new Service( fn10 );

var svc10bis = new Service( fn10, { name: 'anonymous', age: 100 } );

function fn11() {
   return arguments.length;
}

var svc11 = new Service( fn11 );
var svc11bis = new Service( fn11, { a: 'a1', b: 'a2', c: 'a3' } );

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
   // function() {
   //    console.log( 'this one fails' );
   //    svcnew4( { c: 0 } ).post( fail, pass );
   // },
   function() {
      svc10().post( function( result ) {
	 assert.equal( result.name, undefined );
	 assert.equal( result.age, undefined );
	 pass();
      }, fail );
   },
   function() {
      svc10( 'Alice', 101 ).post( function( result ) {
	 assert.equal( result.name, 'Alice' );
	 assert.equal( result.age, 101 );
	 pass();
      }, fail );
   },
   function() {
      svc10bis().post( function( result ) {
	 assert.equal( result.name, 'anonymous' );
	 assert.equal( result.age, 100 );
	 pass();
      }, fail );
   },
   function() {
      svc10bis( {} ).post( function( result ) {
	 assert.equal( result.name, 'anonymous' );
	 assert.equal( result.age, 100 );
	 pass();
      }, fail );
   },
   function() {
      svc10bis( { age: 101, name: 'Alice' } ).post( function( result ) {
	 assert.equal( result.name, 'Alice' );
	 assert.equal( result.age, 101 );
	 pass();
      }, fail );
   },
   function() {
      svc11().post( function( result ) {
	 assert.equal( result, 0 );
	 pass();
      }, fail );
   },
   function() {
      svc11( 'c1', 'c2', 'c3', 'c4' ).post( function( result ) {
	 assert.equal( result, 4 );
	 pass();
      }, fail );
   },
   function() {
      svc11bis().post( function( result ) {
	 assert.equal( result, 3 );
	 pass();
      }, fail );
   },
   function() {
      svc11bis( { c: 'b1' } ).post( function( result ) {
	 assert.equal( result, 3 );
	 pass();
      }, fail );
   },
   // function() {
   // should fail: d is not the name of an argument.
   //    svc11bis( { d: 1 } ).post( fail, pass );
   // },
   // function() {
   // should fail: direct arguments, whereas the service is defined with
   // named arguments.
   //    svc11bis( 1, 2, 3, 4, 5 ).post( fail, pass );
   // },
   // function() {
   // should fail: direct arguments.
   //    svc11bis( 1, 2 ).post( fail, pass );
   // },
];

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

function fail() {
   console.log( 'Test failed' );
   process.exit( 1 );
}



setTimeout( function() {
   console.log( 'timeout' );
   process.exit( 1 );
}, 1000 );

console.log( 'Test vector length', testSuite.length );
next();

