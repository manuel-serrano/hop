/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/es6-array.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 30 17:54:07 2015                          */
/*    Last change :  Wed Jun 21 14:08:19 2023 (serrano)                */
/*    Copyright   :  2015-23 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 arrays                                    */
/*=====================================================================*/
"use strict";

var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    copyWithin ...                                                   */
/*---------------------------------------------------------------------*/
function copywithinmdna() {
   var array1 = [1, 2, 3, 4, 5];
   var array2 = array1.copyWithin(0, 3, 4);

   if (array2.join() !== "4,2,3,4,5") return false;
   return array1.copyWithin(1, 3).join() === "4,4,5,4,5";
}

function copywithinkangaxa() {
   return typeof Array.prototype.copyWithin === 'function';
}

console.log("copywithin");
console.log("   copywithinmdna()"); assert.ok(copywithinmdna(), true);
console.log("   copywithinkangaxa()"); assert.ok(copywithinkangaxa(), true);

/*---------------------------------------------------------------------*/
/*    entries ...                                                      */
/*---------------------------------------------------------------------*/
function entriesmdna() {
   var array1 = ['a', 'b', 'c'];
   var iterator1 = array1.entries();

   if (iterator1.next().value.join() !== "0,a") return false;
   return iterator1.next().value.join() === "1,b";
}

function entrieskangaxa() {
   return typeof Array.prototype.entries === 'function';
}

console.log("entries");
console.log("   entriesmdna()"); assert.ok(entriesmdna());
console.log("   entrieskangaxa()"); assert.ok(entrieskangaxa());

/*---------------------------------------------------------------------*/
/*    fill ...                                                         */
/*---------------------------------------------------------------------*/
function filla() {
   let a = new Array(10);
   
   a.fill(1);
   a[ 5 ] = 2;

   return a[ 0 ] === 1 && a.indexOf(2) === 5;
}

function fillb() {
   let a = new Array(10);
   
   a.fill(1);
   a[ 0 ] = 2;
   a[ 1 ] = 2;

   return a.indexOf(1) === 2 && a[ 9 ] === 1;
}

function fillc(start) {
   let a = new Array(10);
   
   a.fill(1);
   a.fill(2, start);

   return a.indexOf(2) === start && a[ 9 ] == 2;
}

function filld(start, end) {
   let a = new Array(10);
   
   a.fill(1);
   a.fill(2, start, end);

   return a.indexOf(2) === start && a[ end - 1 ] === 2 && a[ end ] === 1;
}

function fille() {
   let a = new Array(10);
   
   a.fill(1);
   delete a[ 5 ];
   a.fill(2);

   return a.lastIndexOf(2) === 9 && a[ 5 ] === 2 && a[ 0 ] === 2;
}

function fillkangaxa() {
   // https://kangax.github.io/compat-table/es6/
   return typeof Array.prototype.fill === "function";
}

console.log("fill");
console.log("   filla()"); assert.ok(filla());
console.log("   fillb()"); assert.ok(fillb());
console.log("   fillc()"); assert.ok(fillc(3));
console.log("   filld()"); assert.ok(filld(2, 7));
console.log("   fille()"); assert.ok(fille());
console.log("   fillkangaxa()"); assert.ok(fillkangaxa(), true);

/*---------------------------------------------------------------------*/
/*    includes                                                         */
/*---------------------------------------------------------------------*/
function includesa() {
   console.log(Array(1).includes());
   console.log(Array(1).includes());
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

console.log("include");
console.log("   includesa()"); assert.ok(includesa(), "includesa");
console.log("   includesb()"); assert.ok(includesb(), "includesb");
console.log("   includesc()"); assert.ok(includesc(), "includesc");
console.log("   includesd()"); assert.ok(includesd(), "includesd");
console.log("   includese()"); assert.ok(includese(), "includese");
console.log("   includesf()"); assert.ok(includesf(), "includesf");

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

console.log("iterator");
console.log("   iteratorkangaxa()"); assert.ok(iteratorkangaxa());
console.log("   iteratorkangaxb()"); assert.ok(iteratorkangaxb());

/*---------------------------------------------------------------------*/
/*    of                                                               */
/*---------------------------------------------------------------------*/
function ofmdna() {
   return (Array.of(7).length === 1) && (Array.of(1, 2, 3).length === 3);
}

function ofkangaxa() {
   return (typeof Array.of === 'function') && Array.of(2)[0] === 2;
}

console.log("of");
console.log("   ofmdna()"); assert.ok(ofmdna());
console.log("   ofkangaxa()"); assert.ok(ofkangaxa());

/*---------------------------------------------------------------------*/
/*    species ...                                                      */
/*---------------------------------------------------------------------*/
function specieskangaxa() {
   var prop = Object.getOwnPropertyDescriptor(Array, Symbol.species);
   return 'get' in prop && Array[Symbol.species] === Array;
}

console.log("species");
console.log("   specieskangaxa()"); assert.ok(specieskangaxa());

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

console.log("unscopables");
console.log("   unscopableskangaxa"); 
assert.ok(unscopableskangaxa(), "unscopableskangaxa");

/*---------------------------------------------------------------------*/
/*    flatmap                                                          */
/*---------------------------------------------------------------------*/
function arrayEqual(x, y) {
   if (x === y) return true;
   if (x.length !== y.length) return false;
   
   for (let i = x.length - 1; i >= 0; i--) {
      if (!arrayEqual(x[i], y[i])) return false;
   }
   
   return true;
}

function flatmapmdna() {
   var arr = [1, 2, 3, 4];

   return arrayEqual(
      arr.flatMap(x => [x, x * 2]),
      arr.reduce((acc, x) => acc.concat([x, x * 2]), []));
}

function flatmapmdnb() {
   let arr1 = [1, 2, 3, 4];

   return arrayEqual(arr1.flatMap(x => [x * 2]), [2, 4, 6, 8]);
}

function flatmapmdnc() {
   let arr1 = [1, 2, 3, 4];

   return arrayEqual(arr1.flatMap(x => [[x * 2]]), [[2], [4], [6], [8]]);
}

function flatmapmdnd() {
   let arr1 = ["it's Sunny in", "", "California"];
   return arrayEqual(arr1.flatMap(x => x.split(" ")),
      ["it's","Sunny","in", "", "California"]);
}

function flatmapmdne() {
   let a = [5, 4, -3, 20, 17, -33, -4, 18];
   const fa = a.flatMap((n) => (n < 0) ? [] : (n % 2 == 0) ? [n] : [n-1, 1]);
   
   return arrayEqual(fa, [4, 1, 4, 20, 16, 1, 18]);
}

function flatmapholeya() {
   const v = [10,,30];
   
   return arrayEqual([1,2,3].flatMap(e => v), [ 10, 30, 10, 30, 10, 30 ]);
}

function flatmapholeyb() {
   const v = [10,,30];
   
   return arrayEqual(v.flatMap(e => [e]), [ 10, 30 ]);
}

function flatmapholeyc() {
   const v = [10,,30];
   
   return arrayEqual(v.flatMap(e => v), [ 10, 30, 10, 30 ]);
}

function flatmapval() {
   const v = [10,20];
   
   return arrayEqual(v.flatMap(e => v > 20 ? [v] : v), [ 10, 20, 10, 20 ]);
}

console.log("flatMap");
console.log("   flatmapmdna"); assert.ok(flatmapmdna(), "flatmapmdna");
console.log("   flatmapmdnb"); assert.ok(flatmapmdnb(), "flatmapmdnb");
console.log("   flatmapmdnc"); assert.ok(flatmapmdnc(), "flatmapmdnc");
console.log("   flatmapmdnd"); assert.ok(flatmapmdnd(), "flatmapmdnd");
console.log("   flatmapmdne"); assert.ok(flatmapmdne(), "flatmapmdne");
console.log("   flatmapholeya"); assert.ok(flatmapholeya(), "flatmapholeya");
console.log("   flatmapholeyb"); assert.ok(flatmapholeyb(), "flatmapholeyb");
console.log("   flatmapholeyc"); assert.ok(flatmapholeyc(), "flatmapholeyc");
console.log("   flatmapval"); assert.ok(flatmapval(), "flatmapval");

/*---------------------------------------------------------------------*/
/*    flat                                                             */
/*---------------------------------------------------------------------*/
function flatmdna() {
   const arr1 = [1, 2, 3, [3, 4]];
   return arrayEqual(arr1.flat(), [0, 1, 2, 3, 4]);
}

function flatmdnb() {
   const arr2 = [1, 2, 3, [[[3, 4]]]];
   return arrayEqual(arr2.flat(2), [0, 1, 2, [3, 4]]);
}

function flatdepth() {
   const arr = [1, 2, 3, [4, [5, [6, 7], 8]], [[[10, 11, 12]]]];
   
   return arrayEqual(arr.flat(0), arr)
      && arrayEqual(arr.flat(1), [1, 2, 3, 4, [5, [6, 7], 8], [[10, 11, 12]]])
      && arrayEqual(arr.flat(2), [1, 2, 3, 4, 5, [6, 7], 8, [10, 11, 12]])
      && arrayEqual(arr.flat(3), [1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12])
      && arrayEqual(arr.flat(4), [1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12]);
}
   
function flatsparse() {
   const arr = [1, , 3, [4, ,6]];

   return arrayEqual(arr.flat(), [1, 3, 4, 6]);
}
   
function flatnovec() {
   const arr = [1, {length: 2, "0": 4, "1": 6}];
   const obj = {length: 3, "0": 4, "2": 6};

   return arr.flat().length === 2
      && arrayEqual(Array.prototype.flat.call(obj), [4, 6]);
}
   
console.log("flat");
console.log("   flatmdna"); assert.ok(flatmdna(), "flatmda");
console.log("   flatmdnb"); assert.ok(flatmdnb(), "flatmdb");
console.log("   flatdepth"); assert.ok(flatdepth(), "flatdepth");
console.log("   flatsparse"); assert.ok(flatsparse(), "flatsparse");
console.log("   flatnovec"); assert.ok(flatnovec(), "flatnovec");
