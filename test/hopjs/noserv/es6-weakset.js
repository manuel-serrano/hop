/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/noserv/es6-weakset.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 25 11:33:29 2019                          */
/*    Last change :  Thu May  6 16:21:22 2021 (serrano)                */
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
   var ws = new WeakSet();
   var foo = {};
   var bar = {};

   ws.add(foo);
   ws.add(bar);
   
   if(!ws.has(foo)) return false;    // true
   if(!ws.has(bar)) return false;    // true

   ws.delete(foo); // removes foo from the set
   return !ws.has(foo);
}

function mdnb() {
   const weakset1 = new WeakSet();
   const object1 = {};

   weakset1.add(object1);
   if(!weakset1.has(object1)) return false;

   try {
      weakset1.add(1); // raise an error because not an object
      return false;
   } catch(error) {
      return true;
   }
}

function mdnc() {
   const weakset1 = new WeakSet();
   const object1 = {};

   weakset1.add(object1);
   
   if(!weakset1.has(object1)) return false;

   weakset1.delete(object1);

   return !weakset1.has(object1);
}
		   
function mdnd() {
   const weakset1 = new WeakSet();
   const object1 = {};
   const object2 = {};

   weakset1.add(object1);

   if(!weakset1.has(object1)) return false;
   return !weakset1.has(object2);
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );
console.log( "   mdnd()"); assert.ok( mdnd(), "mdnd" );

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
   var obj1 = {};
   var weakset = new WeakSet();

   weakset.add(obj1);
   weakset.add(obj1);

   return weakset.has(obj1);
}

function kangaxb() {
   var obj1 = {}, obj2 = {};
   var weakset = new WeakSet([obj1, obj2]);

   return weakset.has(obj1) && weakset.has(obj2);
}

function kangaxc() {
   new WeakSet();
   try {
      WeakSet();
      return false;
   } catch(e) {
      return true;
   }
}

function kangaxd() {
   new WeakSet(null);
   return true;
}

function kangaxe() {
   var passed = false;
   var _add = WeakSet.prototype.add;

   WeakSet.prototype.add = function(v) {
      passed = true;
   };

   new WeakSet([ { } ]);
   WeakSet.prototype.add = _add;

   return passed;
}

function kangaxf() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   try {
      new WeakSet(iter);
   } catch(e){}
   return closed;
}

function kangaxg() {
   var weakset = new WeakSet();
   var obj = {};
   return weakset.add(obj) === weakset;
}

function kangaxh() {
   return typeof WeakSet.prototype.delete === "function";
}

function kangaxi() {
   if (!("clear" in WeakSet.prototype)) {
      return true;
   }
   var s = new WeakSet();
   var key = {};
   s.add(key);
   s.clear();
   return s.has(key);
}

function kangaxj() {
   var s = new WeakSet;
   return s.has(1) === false
      && s.delete(1) === false;
}

function kangaxk() {
   new WeakSet();
   var obj = {};
   try {
      WeakSet.prototype.has(obj);
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


