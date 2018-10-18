/*=====================================================================*/
/*    .../hop/3.2.x/test/hopjs/noserv/es6-destructing-param.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb  7 09:48:34 2018                          */
/*    Last change :  Mon Jul  9 08:53:37 2018 (serrano)                */
/*    Copyright   :  2018 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2015+ destructuring function parameters       */
/*=====================================================================*/

"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    hop                                                              */
/*---------------------------------------------------------------------*/
const hopa = ( {a, b}, res ) => {
   return a - b == res;
};

const hopb = ( {a: A, b: B}, res ) => {
   return A - B == res;
};

function hopc() {
   var [[a], b] = [[5], 6 ];
   return a + b;
}

function hopd() {
   var { a, c:e = 20 } = {a: 10, b:20 };
   return a + e;
}

function hope() {
   const obj = { a: [{ foo: 123, bar: 'abc' }, {}], b: true };
   const { a: [{foo: f}] } = obj;
   return f;
}

function hopf() {
   const {length: len} = 'abc'; // len = 3
   const {toString: s} = 123;

   return len === 3 && s === Number.prototype.toString;
}

console.log( "hop" );
console.log( "   hopa()"); assert.ok( hopa( {a: 1, b: 2}, -1 ), "hopa" );
console.log( "   hopa()"); assert.ok( hopa( {b: 2, a: 1}, -1 ), "hopa" );
console.log( "   hopb()"); assert.ok( hopb( {a: 1, b: 2}, -1 ), "hopb" );
console.log( "   hopb()"); assert.ok( hopb( {b: 2, a: 1}, -1 ), "hopb" );
console.log( "   hopc()"); assert.ok( hopc() === 11, "hopc" );
console.log( "   hopd()"); assert.ok( hopd() === 30, "hopd" );
console.log( "   hope()"); assert.ok( hope() === 123, "hope" );
console.log( "   hopf()"); assert.ok( hopf(), "hopf" );

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
   var [a, , [b], c] = [5, null, [6]];
   return a === 5 && b === 6 && c === undefined
}

function kangaxb() {
   var [a, , b] = [,,,];
   return a === undefined && b === undefined;
}

function kangaxc() {
   var [a, b, c] = "ab";
   return a === "a" && b === "b" && c === undefined;
}

function kangaxd() {
   var [c] = decodeURI( "%F0%A0%AE%B7%F0%A0%AE%B6" );
   return c === decodeURI( "%F0%A0%AE%B7" );
}

function kangaxe() {
   var [a, b, c] = (function*(){ yield 1; yield 2; }());
   return a === 1 && b === 2 && c === undefined;
}

function kangaxf() {
   var [a, b, c] = __createIterableObject([1, 2], {});
   return a === 1 && b === 2 && c === undefined;
}

function kangaxg() {
   var [a, b, c] = Object.create(__createIterableObject([1, 2], {}));
   return a === 1 && b === 2 && c === undefined;
}

function kangaxh() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   var [a, b] = iter;
   return closed;
}

function kangaxi() {
   var [a,] = [1];
   return a === 1;
}

function kangaxj() {
   return function( {c, x: d, e} ) {
      return c === 7 && d === 8 && e === undefined;
   }( {c:7, x:8} );
}

function kangaxk() {
   var {toFixed} = 2;
   var {slice} = '';
   return toFixed === Number.prototype.toFixed
      && slice === String.prototype.slice;
}

function kangaxl() {
   var {a,} = {a:1};
   return a === 1;
}

function kangaxm() {
   try {
      var {a} = null;
      return false;
   } catch(e) {
      if (!(e instanceof TypeError))
	 return false;
   }
   try {
      var {b} = undefined;
      return false;
   } catch(e) {
      if (!(e instanceof TypeError))
	 return false;
   }
   return true;
}

function kangaxn() {
   var qux = "corge";
   var { [qux]: grault } = { corge: "garply" };
   return grault === "garply";  
}

function kangaxo() {
   var [a,b] = [5,6], {c,d} = {c:7,d:8};
   return a === 5 && b === 6 && c === 7 && d === 8;
}

function kangaxp() {
   var [e, {x:f, g}] = [9, {x:10}];
   var {h, x:[i]} = {h:11, x:[12]};
   return e === 9 && f === 10 && g === undefined
      && h === 11 && i === 12;   
}

function kangaxq() {
   for(var [i, j, k] in { qux: 1 }) {
      return i === "q" && j === "u" && k === "x";
   }
}

function kangaxr() {
   for(var [i, j, k] of [[1,2,3]]) {
      return i === 1 && j === 2 && k === 3;
   }
}

function kangaxs() {
   try {
      throw [1,2];
   } catch([i,j]) {
      try {
	 throw { k: 3, l: 4 };
      } catch({k, l}) {
	 return i === 1 && j === 2 && k === 3 && l === 4;
      }
   }
}

function kangaxt() {
   var [a, ...b] = [3, 4, 5];
   var [c, ...d] = [6];
   return a === 3 && b instanceof Array && (b + "") === "4,5" &&
      c === 6 && d instanceof Array && d.length === 0;
}

function kangaxu() {
   var {a = 1, b = 0, z:c = 3} = {b:2, z:undefined};
   var [d = 0, e = 5, f = 6] = [4,,undefined];
   return a === 1 && b === 2 && c === 3
      && d === 4 && e === 5 && f === 6;
}

function kangaxv() {
   var {a, b = 2} = {a:1};
   try {
      eval("let {c = c} = {};");
      return false;
   } catch(e){}
   try {
      eval("let {c = d, d} = {d:1};");
      return false;
   } catch(e){}
   return a === 1 && b === 2;
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
console.log( "   kangaxt()"); assert.ok( kangaxt(), "kangaxt" );
console.log( "   kangaxu()"); assert.ok( kangaxu(), "kangaxu" );
console.log( "   kangaxv()"); assert.ok( kangaxv(), "kangaxv" );
       
