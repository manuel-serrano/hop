/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/noserv/es6-promise.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 19 11:16:33 2015                          */
/*    Last change :  Tue Oct 11 06:28:32 2016 (serrano)                */
/*    Copyright   :  2015-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ES6 promises.                                            */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );
var ok = 0;
var expected = 0;

assert.check = function( val, name ) {
   expected++;
   assert.ok( val, name );
   ok++;
}

// simple tests
console.log( "basic" );

var p = new Promise( function( resolve, reject ) { resolve( 3 ) } );
p.then( function( val ) { assert.equal( val, 3 ) } );

var p = new Promise( function( resolve, reject ) { resolve( 4 ) } )
    .then( function( val ) { assert.equal( val, 4 ); return 5 } )
    .then( function( val ) { assert.equal( val, 5 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .then( undefined, function( val ) { assert.equal( val, -3 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .then( undefined, function( val ) { assert.equal( val, -3 ) } )
    .then( undefined, function( val ) { assert.equal( val, -3 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .catch( function( val ) { assert.equal( val, -3 ) } );

var p = new Promise( function( resolve, reject ) { reject( -3 ) } )
    .catch( function( val ) { assert.equal( val, -3 ) } )
    .catch( function( val ) { assert.equal( val, -3 ) } );

var p = new Promise( function( resolve, reject ) {
   resolve( "Success!" );
} );

p.then( function( value) {
   return value + 1;
} ).then( function( value ) {
   assert.equal( value, "Success!1" );
} );

var p = new Promise( function( resolve, reject ) {
   setTimeout( resolve, 10, 4 );
} );


p.then( function( o ) {
   return new Promise( function( resolve, reject ) {
      setTimeout( resolve, 10, o * 3 );
   } )
} ).then( function( o2 ) {
   assert.check( o2,12 );
} )

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
console.log( "mdn" );

function mdnResolve() {
   Promise.resolve( "Success" ).then( function( value ) {
      assert.check( value === "Success" );
   }, function(value) {
      assert.check( false, "resolve: not called" );
   } );

   var p = Promise.resolve([ 1,2,3] );
   p.then( function( v ) {
      assert.check( v[ 0 ] === 1 )
   } );

   var original = Promise.resolve( true );
   var cast = Promise.resolve( original );
   cast.then( function( v ) {
      assert.check( v === true );
   } );

   assert.check( original === cast );
}

function mdnResolveThen() {
   // Resolving a thenable object
   var p1 = Promise.resolve( {
      then: function( onFulfill, onReject ) { onFulfill( "fulfilled!" ); }
   } );

   assert.check( p1 instanceof Promise );

   p1.then( function( v ) {
      assert.check( v === "fulfilled!", "then" );
   }, function( e ) {
      assert.check( false, "Promise.resolve, not called" );
   });

   // Thenable throws before callback
   // Promise rejects
   var thenable = { then: function( resolve ) {
      throw new TypeError( "Throwing" );
      resolve( "Resolving" );
   }};

   var p2 = Promise.resolve( thenable );
   p2.then( function( v ) {
      assert.check( false, "not called" );
   }, function( e ) {
      assert.check( e instanceof TypeError );
   });

   // Thenable throws after callback
   // Promise resolves
   var thenable = { then: function( resolve ) {
      resolve( "Resolving" );
      throw new TypeError( "Throwing" );
   }};

   var p3 = Promise.resolve( thenable );
   p3.then( function( v ) {
      assert.check( v === "Resolving" );
   }, function( e ) {
      assert.check( false, "not called" );
   });
}

function mdnReject() {
   Promise.reject( "Testing static reject" ).then( function( reason ) {
      assert.check( false, "reject: not called" );
   }, function( reason ) {
      assert.check( reason, "Testing static reject" );
   });

   Promise.reject( new Error( "fail" ) ).then( function( error ) {
      assert.check( false, "reject: not called" );
   }, function( error ) {
      assert.check( error instanceof Error );
   });
}

function mdnAll() {
   var p1 = Promise.resolve( 3 );
   var p2 = 1337;
   var p3 = new Promise( function( resolve, reject ) {
      setTimeout( resolve, 100, "foo" );
   } );

   Promise.all( [p1, p2, p3]).then( function( values ) {
      assert.deepEqual( values, [3, 1337, "foo" ] );
   }) ;

   var p1 = new Promise((resolve, reject) => {
      setTimeout(resolve, 1000, "one");
   });
   var p2 = new Promise((resolve, reject) => {
      setTimeout(resolve, 2000, "two");
   });
   var p3 = new Promise((resolve, reject) => {
      setTimeout(resolve, 3000, "three");
   });
   var p4 = new Promise((resolve, reject) => {
      setTimeout(resolve, 4000, "four");
   });
   var p5 = new Promise((resolve, reject) => {
      reject("reject");
   });

   Promise.all([p1, p2, p3, p4, p5]).then(value => {
      assert.ok( false, "should reject" );
   }, reason => {
      assert.check( reason == "reject" );
   });
}

function mdnAllFail() {
   var p1 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 1000, "one");
   });
   var p2 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 2000, "two");
   });
   var p3 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 3000, "three");
   });
   var p4 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 4000, "four");
   });
   var p5 = new Promise(function(resolve, reject) {
      reject("reject");
   });

   Promise.all([p1, p2, p3, p4, p5]).then(function(value) {
      assert.check( false, "all: not called" );
   }, function(reason) {
      assert.check( reason === "reject" );
   });
}

function mdnAllSuccess() {
   var p1 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 1000, "one");
   });
   var p2 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 2000, "two");
   });
   var p3 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 3000, "three");
   });
   var p4 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 4000, "four");
   });

   Promise.all([p1, p2, p3, p4]).then(function(value) {
      assert.check( value[ 0 ] == "one" && value[ 1 ] == "two"
		    && value[ 2 ] == "three" && value[ 3 ] == "four" );
   }, function(reason) {
      assert.check( false, "all: not called" );
   });
}

function mdnAllSuccess() {
   var p1 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 1000, "one");
   });
   var p2 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 2000, "two");
   });
   var p3 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 3000, "three");
   });
   var p4 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 4000, "four");
   });

   Promise.all([p1, p2, p3, p4]).then(function(value) {
      assert.deepEqual( value, ["one", "two", "three", "four" ] );
   }, function(reason) {
      assert.ok( false, "all: not called" );
   });
}

function mdnRace() {
   var p1 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 500, "one");
   });
   var p2 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 100, "two");
   });

   Promise.race([p1, p2]).then(function(value) {
      assert.check( value == "two", "reject because p2 is faster" );
      // Both resolve, but p2 is faster
   });

   var p3 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 100, "three");
   });
   var p4 = new Promise(function(resolve, reject) {
      setTimeout(reject, 500, "four");
   });

   Promise.race([p3, p4]).then(function(value) {
      assert.check( value == "three", "resolve because p3 is faster" );
      // p3 is faster, so it resolves
   }, function(reason) {
      console.log( "GLOP=", reason );
      assert.ok( false, "all: not called" );
   });

   var p5 = new Promise(function(resolve, reject) {
      setTimeout(resolve, 500, "five");
   });
   var p6 = new Promise(function(resolve, reject) {
      setTimeout(reject, 100, "six");
   });

   Promise.race([p5, p6]).then(function(value) {
      // Not called
   }, function(reason) {
      assert.check( reason = "six", "reject because p6 is faster" );
      // p6 is faster, so it rejects
   });
}

console.log( "   mdnResolve()");
mdnResolve();

console.log( "   mdnResolveThenReject()");
mdnResolveThen();

console.log( "   mdnReject()");
mdnReject();

console.log( "   mdnAll()");
mdnAll();

console.log( "   mdnRace()");
mdnRace();

console.log( "   mdnAllSuccess()");
mdnAllSuccess();

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   var p1 = new Promise( function( resolve, reject ) { resolve( "foo" ); });
   var p2 = new Promise(function( resolve, reject ) { reject( "quux" ); });
   var score = 0;

   function thenFn( result ) { score += (result === "foo");  check(); }
   function catchFn( result ) { score += (result === "quux"); check(); }
   function shouldNotRun( result ) { score = -Infinity; }

   p1.then( thenFn, shouldNotRun );
   p2.then( shouldNotRun, catchFn );
   p1.catch( shouldNotRun) ;
   p2.catch( catchFn );

   p1.then(function() {
      // Promise.prototype.then() should return a new Promise
      score += p1.then() !== p1;
      check();
   });

   function check() {
      assert.check( score, 4 );
   }
}

function kangaxb() {
   new Promise( function(){} );
   try {
      Promise( function(){} );
      return false;
   } catch(e) {
      return true;
   }
}

function kangaxc() {
   var n = 0;
   var fulfills = Promise.all( [
      new Promise( function( resolve ) { setTimeout( resolve, 200, "foo" ); }),
      new Promise( function( resolve ) { setTimeout( resolve, 100, "bar" ); }),
   ]);
   var rejects = Promise.all( [
      new Promise( function( _, reject ) { setTimeout( reject, 200,"baz" ); }),
      new Promise( function( _, reject ) { setTimeout( reject, 100,"qux" ); }),
   ]);
   var score = 0;
   fulfills.then( function( result ) { score += (result + "" === "foo,bar"); check(); });
   rejects.catch( function( result ) { score += (result === "qux"); check(); });

   function check() {
      assert.check( score === ++n );
   }
}

function kangaxd() {
   var n = 0;
   var fulfills = Promise.all( __createIterableObject( [
      new Promise( function( resolve ) { setTimeout( resolve, 200, "foo" ); }),
      new Promise( function( resolve ) { setTimeout(resolve, 100, "bar" ); }),
   ]));
   var rejects = Promise.all( __createIterableObject( [
      new Promise( function( _, reject ) { setTimeout( reject, 200,"baz" ); }),
      new Promise( function( _, reject ) { setTimeout( reject, 100,"qux" ); }),
   ]));
   var score = 0;
   fulfills.then( function( result ) { score += (result + "" === "foo,bar"); check(); });
   rejects.catch( function( result ) { score += (result === "qux"); check(); });

   function check() {
      assert.check( score === ++n );
   }
}

function kangaxe() {
   var n = 0;
   var fulfills = Promise.race( [
      new Promise( function( resolve ) { setTimeout( resolve, 200, "foo" ); }),
      new Promise( function( _, reject ) { setTimeout( reject, 300, "bar" ); }),
   ]);
   var rejects = Promise.race( [
      new Promise( function( _, reject ) { setTimeout( reject, 200, "baz"); }),
      new Promise( function( resolve ) { setTimeout( resolve, 300, "qux"); }),
   ]);
   var score = 0;
   fulfills.then( function( result ) { score += (result === "foo"); check(); });
   rejects.catch( function( result ) { score += (result === "baz"); check(); });

   function check() {
      assert.check( score === ++n );
   }
}

function kangaxf() {
   var n = 0;
   var fulfills = Promise.race( __createIterableObject( [
      new Promise( function( resolve ) { setTimeout( resolve, 200, "foo" ); }),
      new Promise( function( _, reject ) { setTimeout( reject, 300,"bar" ); }),
   ]));
   var rejects = Promise.race( __createIterableObject( [
      new Promise( function( _, reject ) { setTimeout( reject, 200, "baz" ); }),
      new Promise( function( resolve ) { setTimeout( resolve, 300, "qux" ); }),
   ]));
   var score = 0;
   fulfills.then( function( result ) { score += (result === "foo"); check(); });
   rejects.catch( function( result ) { score += (result === "baz"); check(); });

   function check() {
      assert.check( score === ++n );
   }
}

function kangaxg() {
   var prop = Object.getOwnPropertyDescriptor( Promise, Symbol.species );
   assert.check( 'get' in prop && Promise[ Symbol.species ] === Promise );
}

function __createIterableObject(arr, methods) {
   methods = methods || {};
   if( typeof Symbol !== 'function' || !Symbol.iterator ) {
      return {};
   }
   arr.length++;
   var iterator = {
      next: function() {
         return { value: arr.shift(), done: arr.length <= 0 };
      },
      'return': methods[ 'return' ],
      'throw': methods[ 'throw' ]
   };
   var iterable = {};
   iterable[ Symbol.iterator ] = function(){ return iterator; }
   return iterable;
}

console.log( "kangax" );
console.log( "   kangaxa()");
kangaxa();

console.log( "   kangaxb()");
kangaxb();

console.log( "   kangaxc()");
kangaxc();

console.log( "   kangaxd()");
kangaxd();

console.log( "   kangaxe()");
kangaxe();

console.log( "   kangaxf()");
kangaxf();

console.log( "   kangaxg()");
kangaxg();

setTimeout( function() {
   process.exit( ok === expected ? 0 : 1 )
}, 1000 );

