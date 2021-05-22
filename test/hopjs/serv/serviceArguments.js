/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/serv/serviceArguments.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  25 11:43:00 2015                         */
/*    Last change :  Sat May 22 07:01:44 2021 (serrano)                */
/*    Copyright   :  2015-21 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Test service constructor and arguments                           */
/*=====================================================================*/
"use hopscript";

const hop = require( 'hop' );
const assert = require( 'assert' );

const svc = service() {
   return arguments.length;
};

const svc1 = service() {
   return arguments[ 0 ];
};

const svc2 = service() {
   return arguments[ arguments.length - 1 ];
};

const svcnew = new Service( function( ... arguments ) {
   return arguments.length;
});

const svcnew1 = new Service( function( ... arguments ) {
   return arguments[ 0 ];
});

const svcnew2 = new Service( function( ... arguments ) {
   return arguments[ arguments.length - 1 ];
});

const svcF = service( a, b ) {
   assert.equal( a, arguments[ 0 ], "svcF.a" );
   assert.equal( b, arguments [ 1 ], "svcF.b" );
   return b;
};

const svcN = service( o ) {
   const a = (o && "a" in o) ? o.a : 1;
   const b = (o && "b" in o) ? o.b : 'foo';
   return b;
};

const svcNN = service svcName( o ) {
   const a = ("a" in o) ? o.a : 1;
   const b = ("b" in o) ? o.b : 'foo';
   return a;
};

assert.equal( svcNN.path, '/hop/svcName' );

const svcnew3 = new Service( function() {
   return true;
}, 'path' );

assert.equal( svcnew3.path, '/hop/path' );

const svcnew4 = new Service( function( o ) {
   const a = (o && "a" in o) ? o.a : 1;
   const b = (o && "b" in o) ? o.b : 'foo';
   return b;
}, 'servName' );

assert.equal( svcnew4.path, '/hop/servName' );

let svcCount= 0;

function fn10( fName, fAge ) {
   return hop.HTTPResponseAsync( function( sendResponse) {
      sendResponse( { name: fName, age: fAge } );
   }, this );
}

function fn10o( o ) {
   const fName = (o && "name" in o) ? o.name : "anonymous";
   const fAge = (o && "age" in o) ? o.age : 100;
   return fn10.apply( this, [fName, fAge] );
}

const svc10 = new Service( fn10 );

const svc10bis = new Service( fn10o );

function fn11( ... arguments ) {
   return arguments.length;
}

const svc11 = new Service( fn11 );

const testSuite = [
   function() {
      svc().post( function( result ) {
	 assert.equal( result, 0, "svc" );
	 pass();
      }, fail );
   },
   function() {
      svc( 'foo' ).post( function( result ) {
	 assert.equal( result, 1, 'svc("foo")' );
	 pass();
      }, fail );
   },
   function() {
      svc( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 3 );
	 pass();
      }, fail );
   },
   function() {
      svc1().post( function( result ) {
	 assert.equal( result, undefined, "svc1" );
	 pass();
      }, fail );
   },
   function() {
      svc1( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svc1( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svc2().post( function( result ) {
	 assert.equal( result, undefined, "svc2" );
	 pass();
      }, fail );
   },
   function() {
      svc2( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svc2( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'gee' );
	 pass();
      }, fail );
   },
   function() {
      svcnew().post( function( result ) {
	 assert.equal( result, 0 );
	 pass();
      }, fail );
   },
   function() {
      svcnew( 'foo' ).post( function( result ) {
	 assert.equal( result, 1 );
	 pass();
      }, fail );
   },
   function() {
      svcnew( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 3 );
	 pass();
      }, fail );
   },
   function() {
      svcnew1().post( function( result ) {
	 assert.equal( result, undefined, "svcnew1" );
	 pass();
      }, fail );
   },
   function() {
      svcnew1( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svcnew1( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svcnew2().post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, fail );
   },
   function() {
      svcnew2( 'foo' ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svcnew2( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'gee' );
	 pass();
      }, fail );
   },
   function() {
      svcF().post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, fail );
   },
   function() {
      svcF( 'foo' ).post( function( result ) {
	 assert.equal( result, undefined );
	 pass();
      }, fail );
   },
   function() {
      svcF( 'foo', 'bar', 'gee' ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, fail );
   },
   function() {
      svcN().post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svcN( {} ).post( function( result ) {
	 assert.equal( result, 'foo' );
	 pass();
      }, fail );
   },
   function() {
      svcN( { a : 2, b : 'bar' } ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, fail );
   },
   function() {
      svcN( { b: 'bar', a: 2 } ).post( function( result ) {
	 assert.equal( result, 'bar' );
	 pass();
      }, fail );
   },
   function() {
      svcN( { a: 2 } ).post( function( result ) {
	 assert.equal( result, 'foo', "svcN" );
	 pass();
      }, fail );
   },
/*    function() {                                                     */
/*       svcN( { c: 0 } ).post( fail, { fail: pass });                 */
/*    },                                                               */
   function() {
      svcnew4( {} ).post( function( result ) {
	 assert.equal( result, 'foo', "svcnew4.1" );
	 pass();
      }, fail );
   },
   function() {
      svcnew4().post( function( result ) {
    	 assert.equal( result, 'foo', "svcnew4.2" );
    	 pass();
      }, fail );
   },
   function() {
      svcnew4( { a : 2, b : 'bar' } ).post( function( result ) {
	 assert.equal( result, 'bar', "svcnew4.3" );
	 pass();
      }, fail );
   },
   function() {
      svcnew4( { b: 'bar', a: 2 } ).post( function( result ) {
	 assert.equal( result, 'bar', "svcnew4.4" );
	 pass();
      }, fail );
   },
   function() {
      svcnew4( { a: 2 } ).post( function( result ) {
	 assert.equal( result, 'foo', "svcnew4.5" );
	 pass();
      }, fail );
   },
/*    function() {                                                     */
/*        console.log( 'this one fails' );                             */
/*        svcnew4( { c: 0 } ).post( fail, pass );                      */
/*    },                                                               */
   function() {
      svc10().post( function( result ) {
	 assert.equal( result.name, undefined, "svc10.1" );
	 assert.equal( result.age, undefined, "svc10.2" );
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
];

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

