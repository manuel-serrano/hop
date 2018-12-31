/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-array.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 30 17:54:07 2015                          */
/*    Last change :  Mon Dec 31 06:24:02 2018 (serrano)                */
/*    Copyright   :  2015-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 arrays                                    */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    copyWithin ...                                                   */
/*---------------------------------------------------------------------*/
function copywithinmdna() {
   var array1 = [1, 2, 3, 4, 5];
   var array2 = array1.copyWithin(0, 3, 4);

   if( array2.join() !== "4,2,3,4,5" ) return false;
   return array1.copyWithin(1, 3).join() === "4,4,5,4,5";
}

function copywithinkangaxa() {
   return typeof Array.prototype.copyWithin === 'function';
}

console.log( "copywithin" );
console.log( "   copywithinmdna()"); assert.ok( copywithinmdna(), true );
console.log( "   copywithinkangaxa()"); assert.ok( copywithinkangaxa(), true );

/*---------------------------------------------------------------------*/
/*    entries ...                                                      */
/*---------------------------------------------------------------------*/
function entriesmdna() {
   var array1 = ['a', 'b', 'c'];
   var iterator1 = array1.entries();

   if( iterator1.next().value.join() !== "0,a" ) return false;
   return iterator1.next().value.join() === "1,b";
}

function entrieskangaxa() {
   return typeof Array.prototype.entries === 'function';
}

console.log( "entries" );
console.log( "   entriesmdna()"); assert.ok( entriesmdna() );
console.log( "   entrieskangaxa()"); assert.ok( entrieskangaxa() );

/*---------------------------------------------------------------------*/
/*    fill ...                                                         */
/*---------------------------------------------------------------------*/
function filla() {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a[ 5 ] = 2;

   return a[ 0 ] === 1 && a.indexOf( 2 ) === 5;
}

function fillb() {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a[ 0 ] = 2;
   a[ 1 ] = 2;

   return a.indexOf( 1 ) === 2 && a[ 9 ] === 1;
}

function fillc( start ) {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a.fill( 2, start );

   return a.indexOf( 2 ) === start && a[ 9 ] == 2;
}

function filld( start, end ) {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a.fill( 2, start, end );

   return a.indexOf( 2 ) === start && a[ end - 1 ] === 2 && a[ end ] === 1;
}

function fille() {
   let a = new Array( 10 );
   
   a.fill( 1 );
   delete a[ 5 ];
   a.fill( 2 );

   return a.lastIndexOf( 2 ) === 9 && a[ 5 ] === 2 && a[ 0 ] === 2;
}

function fillkangaxa() {
   // https://kangax.github.io/compat-table/es6/
   return typeof Array.prototype.fill === "function";
}

console.log( "fill" );
console.log( "   filla()"); assert.ok( filla() );
console.log( "   fillb()"); assert.ok( fillb() );
console.log( "   fillc()"); assert.ok( fillc( 3 ) );
console.log( "   filld()"); assert.ok( filld( 2, 7 ) );
console.log( "   fille()"); assert.ok( fille() );
console.log( "   fillkangaxa()"); assert.ok( fillkangaxa(), true );

/*---------------------------------------------------------------------*/
/*    includes                                                         */
/*---------------------------------------------------------------------*/
function includesa() {
   return [1, 2, 3].includes(1)
      && ![1, 2, 3].includes(4)
      && ![1, 2, 3].includes(1, 1)
      && [NaN].includes(NaN)
      && Array(1).includes();
}

function includesb() {
   var arr = ['a', 'b', 'c'];

   return !arr.includes('c', 3)
      && !arr.includes('c', 100);
}

function includesc() {
   var arr = ['a', 'b', 'c'];

   return arr.includes('a', -100)
      && arr.includes('b', -100)
      && arr.includes('c', -100)
      && ! arr.includes('a', -2);
}

function includesd() {
   return (function() {
      return ([].includes.call(arguments, 'a'))
  	 && !([].includes.call(arguments, 'd'));
   })('a','b','c');
}

function includese() {
   var passed = 0;
   return [].includes.call(
      { get "0"() { passed = NaN; return 'foo'; },
        get "11"() { passed += 1; return 0; },
        get "19"() { passed += 1; return 'foo'; },
        get "21"() { passed = NaN; return 'foo'; },
        get length() { passed += 1; return 24; }
      }, 'foo', 6) === true && passed === 3;
}

function includesf() {
   return [Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array,
	   Int32Array, Uint32Array, Float32Array, Float64Array]
      .every(function(TypedArray){
	 return new TypedArray([1, 2, 3]).includes(1)
	    && !new TypedArray([1, 2, 3]).includes(4)
		&& !new TypedArray([1, 2, 3]).includes(1, 1);
      });
}

console.log( "include" );
console.log( "   includesa" ); assert.ok( includesa(), "includesa" );
console.log( "   includesb" ); assert.ok( includesb(), "includesb" );
console.log( "   includesc" ); assert.ok( includesc(), "includesc" );
console.log( "   includesd" ); assert.ok( includesd(), "includesd" );
console.log( "   includese" ); assert.ok( includese(), "includese" );
console.log( "   includesf" ); assert.ok( includesf(), "includesf" );

/*---------------------------------------------------------------------*/
/*    iterator ...                                                     */
/*---------------------------------------------------------------------*/
function iteratorkangaxa() {
   // Iterator instance
   var iterator = [][Symbol.iterator]();
   // %ArrayIteratorPrototype%
   var proto1 = Object.getPrototypeOf(iterator);
   // %IteratorPrototype%
   var proto2 = Object.getPrototypeOf(proto1);

   return proto2.hasOwnProperty(Symbol.iterator) &&
      !proto1.hasOwnProperty(Symbol.iterator) &&
      !iterator.hasOwnProperty(Symbol.iterator) &&
      iterator[Symbol.iterator]() === iterator;
}

function iteratorkangaxb() {
   return typeof Array.prototype[Symbol.iterator] === 'function';
}

console.log( "iterator" );
console.log( "   iteratorkangaxa()"); assert.ok( iteratorkangaxa() );
console.log( "   iteratorkangaxb()"); assert.ok( iteratorkangaxb() );

/*---------------------------------------------------------------------*/
/*    of                                                               */
/*---------------------------------------------------------------------*/
function ofmdna() {
   return (Array.of(7).length === 1) && (Array.of(1, 2, 3).length === 3);
}

function ofkangaxa() {
   return (typeof Array.of === 'function') && Array.of(2)[0] === 2;
}

console.log( "of" );
console.log( "   ofmdna()"); assert.ok( ofmdna() );
console.log( "   ofkangaxa()"); assert.ok( ofkangaxa() );

/*---------------------------------------------------------------------*/
/*    species ...                                                      */
/*---------------------------------------------------------------------*/
function specieskangaxa() {
   var prop = Object.getOwnPropertyDescriptor(Array, Symbol.species);
   return 'get' in prop && Array[Symbol.species] === Array;
}

console.log( "species" );
console.log( "   specieskangaxa()"); assert.ok( specieskangaxa() );

/*---------------------------------------------------------------------*/
/*    unscopables                                                      */
/*---------------------------------------------------------------------*/
function unscopableskangaxa() {
   var unscopables = Array.prototype[Symbol.unscopables];
   if (!unscopables) {
      return false;
   }
   var ns = "find,findIndex,fill,copyWithin,entries,keys,values".split(",");
   for (var i = 0; i < ns.length; i++) {
      if (Array.prototype[ns[i]] && !unscopables[ns[i]]) return false;
   }
   return true;
}

console.log( "unscopables" );
console.log( "   unscopableskangaxa" ); 
assert.ok( unscopableskangaxa(), "unscopableskangaxa" );
