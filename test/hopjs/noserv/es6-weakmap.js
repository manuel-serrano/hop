/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/noserv/es6-weakmap.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 25 11:33:29 2019                          */
/*    Last change :  Thu May  6 16:21:18 2021 (serrano)                */
/*    Copyright   :  2019-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 WEAKMAP object.                          */
/*=====================================================================*/
"use strict";

const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   return true;
}

function miscb() {
   return true;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function mdna() {
   var wm1 = new WeakMap(), wm2 = new WeakMap(), wm3 = new WeakMap();
   var o1 = {}
   var o2 = function() {};
   var o3 = hop;

   wm1.set(o1, 37);
   wm1.set(o2, 'azerty');
   wm2.set(o1, o2); // a value can be anything, including an object or a function
   wm2.set(o3, undefined);
   wm2.set(wm1, wm2); // keys and values can be any objects. Even WeakMaps!

   // "azerty"
   if( wm1.get(o2) !== "azerty" ) return false; 
   
    // undefined, because there is no key for o2 on wm2
   if( wm2.get(o2) !== undefined ) return false;
   
   // undefined, because that is the set value
   if( wm2.get(o3) !== undefined ) return false; 
   
   
   if( !wm1.has(o2) ) return false; // true
   if( wm2.has(o2) ) return false; // false
   if( !wm2.has(o3) ) return false // true (even if the value itself is 'undefined')

   wm3.set(o1, 37);
   wm3.get(o1); // 37

   if( !wm1.has(o1) ) return false; // true
   wm1.delete(o1);
   if( wm1.has(o1) ) return false;
   
   return true;
}

function mdnb() {
   const weakmap1 = new WeakMap();
   const object1 = {};

   weakmap1.set(object1, 42);

   if(!weakmap1.delete(object1)) return false;
   return !weakmap1.has(object1);
}

function mdnc() {
   const weakmap1 = new WeakMap();
   const object1 = {};
   const object2 = {};

   weakmap1.set(object1, 42);

   if(weakmap1.get(object1) !== 42) return false;
   return weakmap1.get(object2) === undefined;
}

function mdnd() {
   const weakmap1 = new WeakMap();
   const object1 = {};
   const object2 = {};

   weakmap1.set(object1, 'foo');

   if(!weakmap1.has(object1)) return false;

   return !weakmap1.has(object2);
}

function mdne() {
   const weakmap1 = new WeakMap();
   const object1 = {};
   const object2 = {};

   weakmap1.set(object1, 'foo');
   weakmap1.set(object2, 'bar');

   if(weakmap1.get(object1) !== "foo") return false;
   return weakmap1.get(object2) === "bar";
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );
console.log( "   mdnd()"); assert.ok( mdnd(), "mdnd" );
console.log( "   mdne()"); assert.ok( mdne(), "mdne" );

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
   var weakmap = new WeakMap();

   weakmap.set(key, 123);

   return weakmap.has(key) && weakmap.get(key) === 123;
}

function kangaxb() {
   var key1 = {};
   var key2 = {};
   var weakmap = new WeakMap([[key1, 123], [key2, 456]]);

   return weakmap.has(key1) && weakmap.get(key1) === 123 &&
      weakmap.has(key2) && weakmap.get(key2) === 456;
}

function kangaxc() {
   new WeakMap();
   try {
      WeakMap();
      return false;
   } catch(e) {
      return true;
   }
}

function kangaxd() {
   new WeakMap(null);
   return true;
}

function kangaxe() {
   var passed = false;
   var _set = WeakMap.prototype.set;

   WeakMap.prototype.set = function(k, v) {
      passed = true;
   };

   new WeakMap([ [{ }, 42] ]);
   WeakMap.prototype.set = _set;

   return passed;
}


function kangaxf() {
   var f = Object.freeze({});
   var m = new WeakMap;
   m.set(f, 42);
   return m.get(f) === 42;
}

function kangaxg() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   try {
      new WeakMap(iter);
   } catch(e){}
   return closed;
}

function kangaxh() {
   var weakmap = new WeakMap();
   var key = {};
   return weakmap.set(key, 0) === weakmap;
}


function kangaxi() {
   return typeof WeakMap.prototype.delete === "function";
}

function kangaxj() {
   if (!("clear" in WeakMap.prototype)) {
      return true;
   }
   var m = new WeakMap();
   var key = {};
   m.set(key, 2);
   m.clear();
   return m.has(key);
}

function kangaxk() {
   var m = new WeakMap;
   return m.has(1) === false
      && m.get(1) === undefined
      && m.delete(1) === false;
}

function kangaxl() {
   new WeakMap();
   var obj = {};
   try {
      WeakMap.prototype.has(obj);
   }
   catch(e) {
      return true;
   }
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


