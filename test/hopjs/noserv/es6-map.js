/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/es6-map.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 25 11:33:29 2019                          */
/*    Last change :  Tue Feb 26 08:16:36 2019 (serrano)                */
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

function miscb() {
   const m = new Map( [[1,2], [11, 12], [4,5]] );
   
   m.delete( 11 );
   const k = m.keys();
   
   const k0 = k.next().value;
   const k1 = k.next().value;

   return (k0 === 1 && k1 === 4);
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function mdna() {
   var map1 = new Map();

   map1.set('bar', 'baz');
   map1.set(1, 'foo');

   const osize = map1.size;
   map1.clear();
   return map1.size === 0 && osize === 2;
}

function mdnb() {
   var map1 = new Map();
   map1.set('bar', 'foo');
   let r = map1.delete('bar');

   return r && !map1.has('bar');
}

function mdnc() {
   var map1 = new Map();
   
   map1.set('0', 'foo');
   map1.set(1, 'bar');

   var iterator1 = map1.entries();
   const [k0, v0] = iterator1.next().value;
   const [k1, v1] = iterator1.next().value;
   
   return k0 === "0" && v0 === "foo" && k1 === 1 && v1 === "bar";
}

function mdnd() {
   let out = "";
   
   function logMapElements(value, key, map) {
      out += `m[${key}] = ${value}`;
   }

   new Map([['foo', 3], ['bar', {}], ['baz', undefined]])
      .forEach(logMapElements);
   
   return out === "m[foo] = 3m[bar] = [object Object]m[baz] = undefined";
}
   
function mdne() {
   var map1 = new Map();
   map1.set('bar', 'foo');

   return map1.get('bar') === "foo" && map1.get('baz') === undefined;
}

function mdnf() {
   var map1 = new Map();
   map1.set('bar', 'foo');

   return map1.has('bar') && !map1.get('baz');
}

function mdng() {
   var map1 = new Map();

   map1.set('0', 'foo');
   map1.set(1, 'bar');

   var iterator1 = map1.keys();
   var k0 = iterator1.next().value;
   var k1 = iterator1.next().value;
   
   return k0 === "0" && k1 === 1;
}

function mdnh() {
   var map1 = new Map();

   map1.set('bar', 'foo');

   return map1.get('bar') === "foo" && map1.get('baz') === undefined;
}

function mdni() {
   var map1 = new Map();

   map1.set('0', 'foo');
   map1.set(1, 'bar');

   var iterator1 = map1.values();

   var k0 = iterator1.next().value;
   var k1 = iterator1.next().value;
   
   return k0 === "foo" && k1 === "bar";
}

function mdnj() {
   let out = "";
   var map1 = new Map();

   map1.set('0', 'foo');
   map1.set(1, 'bar');

   var iterator1 = map1[Symbol.iterator]();

   for (let item of iterator1) {
      const [k,v] = item;
      out += `${k}:${v}`;
   }
   
   return out === "0:foo1:bar";
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );
console.log( "   mdnd()"); assert.ok( mdnd(), "mdnd" );
console.log( "   mdne()"); assert.ok( mdne(), "mdne" );
console.log( "   mdnf()"); assert.ok( mdnf(), "mdnf" );
console.log( "   mdng()"); assert.ok( mdng(), "mdng" );
console.log( "   mdnh()"); assert.ok( mdnh(), "mdnh" );
console.log( "   mdni()"); assert.ok( mdni(), "mdni" );
console.log( "   mdnj()"); assert.ok( mdnj(), "mdnj" );

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



