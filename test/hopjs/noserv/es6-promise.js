/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/noserv/es6-promise.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 19 11:16:33 2015                          */
/*    Last change :  Tue Jan  5 09:02:58 2016 (serrano)                */
/*    Copyright   :  2015-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ES6 promises.                                            */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

// simple tests
console.log( "basic" );

var p = new Promise( function( resolve, reject ) { resolve( 3 ) } );
p.then( function( val ) { assert.equal( val, 3 ) } );

var p = new Promise( function( resolve, reject ) { resolve( 3 ) } )
    .then( function( val ) { assert.equal( val, 3 ) } )
    .then( function( val ) { assert.equal( val, 3 ) } );

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

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
console.log( "mdn" );

function mdnResolve() {
   Promise.resolve( "Success" ).then( function( value ) {
      assert.ok( value === "Success" );
   }, function(value) {
      assert.ok( false, "resolve: not called" );
   } );

   var p = Promise.resolve([ 1,2,3] );
   p.then( function( v ) {
      assert.ok( v[ 0 ] === 1 )
   } );

   var original = Promise.resolve( true );
   var cast = Promise.resolve( original );
   cast.then( function( v ) {
      assert.ok( v === true );
   } );

   assert.ok( original === cast );
}

function mdnResolveThen() {
   // Resolving a thenable object
   var p1 = Promise.resolve( { 
      then: function( onFulfill, onReject ) { onFulfill( "fulfilled!" ); }
   } );

   assert.ok( p1 instanceof Promise );

   p1.then( function( v ) {
      assert.ok( v === "fulfilled!", "then" );
   }, function( e ) {
      assert.ok( false, "Promise.resolve, not called" );
   });

   // Thenable throws before callback
   // Promise rejects
   var thenable = { then: function( resolve ) {
      throw new TypeError( "Throwing" );
      resolve( "Resolving" );
   }};

   var p2 = Promise.resolve( thenable );
   p2.then( function( v ) {
      assert.ok( false, "not called" );
   }, function( e ) {
      assert.ok( e instanceof TypeError );
   });

   // Thenable throws after callback
   // Promise resolves
   var thenable = { then: function( resolve ) {
      resolve( "Resolving" );
      throw new TypeError( "Throwing" );
   }};

   var p3 = Promise.resolve( thenable );
   p3.then( function( v ) {
      assert.ok( v === "Resolving" );
   }, function( e ) {
      assert.ok( false, "not called" );
   });
}

function mdnReject() {
   Promise.reject( "Testing static reject" ).then(function( reason ) {
      assert.ok( false, "reject: not called" );
   }, function( reason ) {
      assert.ok( reason, "Testing static reject" );
   });

   Promise.reject(new Error("fail")).then(function(error) {
      // not called
   }, function(error) {
      console.log(error); // Stacktrace
   });
}

function mdnAll() {
   var p1 = Promise.resolve( 3 );
   var p2 = 1337;
   var p3 = new Promise( function( resolve, reject ) {
      setTimeout( resolve, 100, "foo" );
   } ); 

   Promise.all( [p1, p2, p3]).then( function( values ) {
      assert.strictEqual( values, [3, 37, "foo" ] );
   }) ;
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
      assert.ok( false, "all: not called" );
   }, function(reason) {
      assert.ok( reason === "reject" );
   });
}

console.log( "   mdnResolve()");
mdnResolve();

console.log( "   mdnResolveThenReject()");
mdnResolveThen();

console.log( "   mdnReject()");
mdnResolveThen();

console.log( "   mdnAll()");
mdnAll();

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
      assert.ok( score, 4 );
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
      assert.ok( score === 2 );
   }
}

function kangaxd() {
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
      assert.ok( score === 2 );
   }
}

function kangaxe() {
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
      assert.ok( score === 2 );
   }
}

function kangaxf() {
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
      assert.ok( score === 2 );
   }
}

function kangaxg() {
   var prop = Object.getOwnPropertyDescriptor( Promise, Symbol.species );
   return 'get' in prop && Promise[ Symbol.species ] === Promise;
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
