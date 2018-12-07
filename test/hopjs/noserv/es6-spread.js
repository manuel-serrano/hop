/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-spread.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Thu Dec  6 19:04:25 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Spread syntax                            */
/*=====================================================================*/
"use strict";
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc ...                                                         */
/*---------------------------------------------------------------------*/
function misca() {
   function sum(x, y, z) {
      return x + y + z;
   }
   
   return sum(...[1, 2, 3]) === 6;
}

function miscb() {
   function Foo( a, b, c ) {
      return a + b + c;
   }
   
   Foo.apply = function( self, args ) {
      return 24;
   }
   
   return Foo.apply( null, [ 1, 2, 3, 4 ] ) === 24;
}

function miscc() {
   function Foo( a, b, c ) {
      return this.x + a + b + c;
   }
   const o = { x: 30, f: Foo };

   return o.f( ...[1,2,3] ) === 36;
}

function miscd() {
   function sum(x, y, z) {
      return x + y + z;
   }

   let num = 0;
   if( num < 10 ) num = [1, 2, 3];
   return sum(...num) === 6;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );
console.log( "   miscc()"); assert.ok( miscc(), "miscc" );
console.log( "   miscd()"); assert.ok( miscd(), "miscd" );
	    
/*---------------------------------------------------------------------*/
/*    mdn ...                                                          */
/*---------------------------------------------------------------------*/
function mdna() {
   function sum(x, y, z) {
      return x + y + z;
   }
   const numbers = [1, 2, 3];
   
   return sum(...numbers) === 6;
}

function mdnb() {
   function myFunction(v, w, x, y, z) { return v+w+x+y+z; }
   var args = [0, 1];
   return myFunction(-1, ...args, 2, ...[3]) === -1+0+1+2+3;
}

function mdnc() {
   var dateFields = [1970, 0, 1];  // 1 Jan 1970
   return new Date(...dateFields).getUTCSeconds() === 0;
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );

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
   return Math.max(...[1, 2, 3]) === 3;
}

function kangaxb() {
   return [...[1, 2, 3]][2] === 3;
}

function kangaxc() {
   var a = Array(...[,,]);
   return "0" in a && "1" in a && '' + a[0] + a[1] === "undefinedundefined";
}

function kangaxd() {
   var a = [...[,,]];
   return "0" in a && "1" in a && '' + a[0] + a[1] === "undefinedundefined";
}

function kangaxe() {
   return Math.max(..."1234") === 4;
}

function kangaxf() {
   return ["a", ..."bcd", "e"][3] === "d";
}

function kangaxg() {
   const src = decodeURIComponent( "%F0%A0%AE%B7%F0%A0%AE%B6" );
   const tgt = decodeURIComponent( "%F0%A0%AE%B7" );
   return Array(...src)[0] === tgt;
}

function kangaxh() {
   const src = decodeURIComponent( "%F0%A0%AE%B7%F0%A0%AE%B6" );
   const tgt = decodeURIComponent( "%F0%A0%AE%B7" );
   return [...src][0] === tgt;
}

function kangaxi() {
   var iterable = (function*(){ yield 1; yield 2; yield 3; }());
   return Math.max(...iterable) === 3;
}

function kangaxj() {
   var iterable = (function*(){ yield 1; yield 2; yield 3; }());
   return ["a", ...iterable, "e"][3] === "d";
}

function kangaxk() {
   var iterable = __createIterableObject([1, 2, 3]);
   return Math.max(...iterable) === 3;
}

function kangaxl() {
   var iterable = __createIterableObject(["b", "c", "d"]);
   return ["a", ...iterable, "e"][3] === "d";
}

function kangaxm() {
   var iterable = __createIterableObject([1, 2, 3]);
   return Math.max(...Object.create(iterable)) === 3;
}

function kangaxn() {
   var iterable = __createIterableObject(["b", "c", "d"]);
   return ["a", ...Object.create(iterable), "e"][3] === "d";
}

function kangaxo() {
   try {
      Math.max(...2);
   } catch(e) {
      return Math.max(...[1, 2, 3]) === 3;
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
console.log( "   kangaxm()"); assert.ok( kangaxm(), "kangaxm" );
console.log( "   kangaxn()"); assert.ok( kangaxn(), "kangaxn" );
console.log( "   kangaxo()"); assert.ok( kangaxo(), "kangaxo" );

