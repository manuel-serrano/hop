/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-forof.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Fri Jan 26 10:07:16 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing for ... of                                               */
/*=====================================================================*/
"use strict";
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   var s = new String( "foo" );
   
   s[ Symbol.iterator ] = function () {
      return {
	 next: function() {
	    if( this._first ) {
	       this._first = false;
	       return { value: "bye", done: false };
	    } else {
	       return {done: true };
	    }
	 },
	 _first: true
      }
   }

   let res, count = 0;
   for( let o of s ) { res = o; count++ };

   return count == 1 && res === "bye";
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
      
/*---------------------------------------------------------------------*/
/*    mdn ...                                                          */
/*---------------------------------------------------------------------*/
function* fooa(){
   yield 1;
   yield 2;
}

function mdna() {
   let res=[];
   for (let o of fooa()) {
      res.push(o)
      // console.log(o)
      // expected output: 1

      break; // closes iterator, triggers return
   }

   return assert.deepEqual(res, [1]);
}

function mdnb() {
   let iterable = [10, 20, 30];
   let res = [];
   
   for (let value of iterable) {
      value += 1;
      res.push( value );
      // console.log(value);
   }

   return assert.deepEqual( res, [11,21,31] );
}

function mdnc() {
   let iterable = [10, 20, 30];
   let res = [];
   
   for (const value of iterable) {
      res.push( value );
      // console.log(value);
   }

   return assert.deepEqual( res, [10,20,30] );
}

function mdnd() {
   let iterable = new Uint8Array([0x00, 0xff]);
   let res = [];
   
   for (let value of iterable) {
      res.push( value );
      //console.log(value);
   }
   
   return assert.deepEqual( res, [0,255] );
}

function mdne() {
   let res = [];
   (function() {
      for (let argument of arguments) {
	 res.push( argument );
	 // console.log(argument);
      }
   })(1, 2, 3);
   return assert.deepEqual( res, [1, 2, 3 ] );
}

function mdnf() {
   function* fibonacci() { // a generator function
      let prev = 0, curr = 1;
      while (true) {
	 let oprev = prev;
	 prev = curr, curr = oprev + curr;
	 yield curr;
      }
   }

   let res = 0;

   for (let n of fibonacci()) {
      res += n;
      //console.log(n, res);
      // truncate the sequence at 1000
      if (n >= 1000) {
	 break;
      }
   }

   return assert.ok( res == 4179 );
}

console.log( "mdn" );
console.log( "   mdna()"); mdna();
console.log( "   mdnb()"); mdnb();
console.log( "   mdnc()"); mdnc();
console.log( "   mdnd()"); mdnd();
console.log( "   mdne()"); mdne();
console.log( "   mdnf()"); mdnf();

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
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
   var arr = [5];
   for (var item of arr)
      return item === 5;
}

function kangaxb() {
   var arr = [,,];
   var count = 0;
   for (var item of arr)
      count += (item === undefined);
   return count === 2;
}

function kangaxc() {
   var str = "";
   for (var item of "foo")
      str += item;
   return str === "foo";
}

function kangaxd() {
   var str = "";
   for (var item of "𠮷𠮶")
      str += item + " ";
   
   return str === "𠮷 𠮶 ";
}

function kangaxe() {
   var result = "";
   var iterable = (function*(){ yield 1; yield 2; yield 3; }());
   for (var item of iterable) {
      result += item;
   }
   return result === "123";
}

function kangaxf() {
   var result = "";
   var iterable = __createIterableObject([1, 2, 3]);
   for (var item of iterable) {
      result += item;
   }
   return result === "123";
}

function kangaxg() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   for (var it of iter) break;
   return closed;
}

function kangaxh() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   try {
      for (var it of iter) throw 0;
   } catch(e){}
   return closed;
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

