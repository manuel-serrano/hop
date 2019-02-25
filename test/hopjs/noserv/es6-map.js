/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/es6-map.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 25 11:33:29 2019                          */
/*    Last change :  Mon Feb 25 18:39:49 2019 (serrano)                */
/*    Copyright   :  2019 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 MAP object.                              */
/*=====================================================================*/
"use strict";
"use hopscript";

const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   const m = new Map( [[1,2], [4,5]] );
   const k = m.keys();
   
   const k0 = k.next().value;
   const k1 = k.next().value;
   return (k0 === 1 && k1 === 4);
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );

/*---------------------------------------------------------------------*/
/*    kangax ...                                                       */
/*---------------------------------------------------------------------*/
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

function kangaxa() {
   var key = {};
   var map = new Map();

   map.set(key, 123);

   return map.has(key) && map.get(key) === 123
}

function kangaxb() {
   var key1 = {};
   var key2 = {};
   var map = new Map([[key1, 123], [key2, 456]]);

   return map.has(key1) && map.get(key1) === 123 &&
      map.has(key2) && map.get(key2) === 456;
}

function kangaxc() {
   new Map();

   try {
     Map();
     return false;
   } catch(e) {
      return true;
   }
}

function kangaxd() {
   new Map(null);
   return true;
}

function kangaxe() {
   var passed = false;
   var _set = Map.prototype.set;

   Map.prototype.set = function(k, v) {
      passed = true;
   };

   new Map([ [1, 2] ]);
   Map.prototype.set = _set;

   return passed;
}

function kangaxf() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   try {
      new Map(iter);
   } catch(e){}
   return closed;
}

function kangaxg() {
   var map = new Map();
   return map.set(0, 0) === map;
}

function kangaxh() {
   var map = new Map();
   map.set(-0, "foo");
   var k;
   map.forEach(function (value, key) {
      k = 1 / key;
   });
   return k === Infinity && map.get(+0) == "foo";
}

function kangaxi() {
   var key = {};
   var map = new Map();

   map.set(key, 123);

   return map.size === 1;
}

function kangaxj() {
   return typeof Map.prototype.delete === "function";
}

function kangaxk() {
   return typeof Map.prototype.clear === "function";
}

function kangaxl() {
   return typeof Map.prototype.forEach === "function";
}

function kangaxm() {
   return typeof Map.prototype.keys === "function";
}

function kangaxn() {
   return typeof Map.prototype.values === "function";
}

function kangaxo() {
   return typeof Map.prototype.entries === "function";
}

function kangaxp() {
   return typeof Map.prototype[Symbol.iterator] === "function";
}

function kangaxq() {
   new Map();
   var obj = {};
   try {
      Map.prototype.has(obj);
   }
   catch(e) {
      return true;
   }
}

function kangaxr() {
   // Iterator instance
   var iterator = new Map()[Symbol.iterator]();
   // %MapIteratorPrototype%
   var proto1 = Object.getPrototypeOf(iterator);
   // %IteratorPrototype%
   var proto2 = Object.getPrototypeOf(proto1);

   return proto2.hasOwnProperty(Symbol.iterator) &&
      !proto1    .hasOwnProperty(Symbol.iterator) &&
      !iterator  .hasOwnProperty(Symbol.iterator) &&
      iterator[Symbol.iterator]() === iterator;
}

function kangaxs() {
   var prop = Object.getOwnPropertyDescriptor(Map, Symbol.species);
   return 'get' in prop && Map[Symbol.species] === Map;
}

console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );
console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );
console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );
console.log( "   kangaxd()"); assert.ok( kangaxd(), "kangaxd" );
console.log( "   kangaxe()"); assert.ok( kangaxe(), "kangaxe" );
console.log( "   kangaxf()"); assert.ok( kangaxf(), "kangaxf" );
console.log( "   kangaxg()"); assert.ok( kangaxg(), "kangaxg" );
console.log( "   kangaxh()"); assert.ok( kangaxh(), "kangaxh" );
console.log( "   kangaxi()"); assert.ok( kangaxi(), "kangaxi" );
console.log( "   kangaxj()"); assert.ok( kangaxj(), "kangaxj" );
console.log( "   kangaxk()"); assert.ok( kangaxk(), "kangaxk" );
console.log( "   kangaxl()"); assert.ok( kangaxl(), "kangaxl" );
console.log( "   kangaxm()"); assert.ok( kangaxm(), "kangaxm" );
console.log( "   kangaxn()"); assert.ok( kangaxn(), "kangaxn" );
console.log( "   kangaxo()"); assert.ok( kangaxo(), "kangaxo" );
console.log( "   kangaxp()"); assert.ok( kangaxp(), "kangaxp" );
console.log( "   kangaxq()"); assert.ok( kangaxq(), "kangaxq" );
console.log( "   kangaxr()"); assert.ok( kangaxr(), "kangaxr" );
console.log( "   kangaxs()"); assert.ok( kangaxs(), "kangaxs" );



