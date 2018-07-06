/*=====================================================================*/
/*    serrano/trashcan/es6-destructing-assig.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb  7 09:48:34 2018                          */
/*    Last change :  Fri Jul  6 08:29:10 2018 (serrano)                */
/*    Copyright   :  2018 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2015+ destructuring function parameters       */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    hop                                                              */
/*---------------------------------------------------------------------*/
function hopa() {
   const obj = {};
   ({ foo: obj.prop } = { foo: 123 });
   return obj.prop;
}

function hopb() {
   var [a, ...[b,c]] = [1,2,3];

   return a*100 + b*10 + c;
}

console.log( "hop" );
console.log( "   hopa()"); assert.ok( hopa(), 123 );
console.log( "   hopb()"); assert.ok( hopb(), 123 )

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
   var a,b,c;
   [a, , [b], c] = [5, null, [6]];
   return a === 5 && b === 6 && c === undefined;
}

function kangaxb() {
   var a, b;
   [a, , b] = [,,,];
   return a === undefined && b === undefined;
}

function kangaxc() {
   var a,b,c;
   [a, b, c] = "ab";
   return a === "a" && b === "b" && c === undefined;
}

function kangaxd() {
   var c;
   [c] = "𠮷𠮶";
   return c === "𠮷";
}

function kangaxe() {
   var a,b,c;
   [a, b, c] = (function*(){ yield 1; yield 2; }());
   return a === 1 && b === 2 && c === undefined;
}

function kangaxf() {
   var a,b,c;
   [a, b, c] = __createIterableObject([1, 2], {});
   return a === 1 && b === 2 && c === undefined;
}

function kangaxg() {
   var closed = false;
   var iter = __createIterableObject([1, 2, 3], {
      'return': function(){ closed = true; return {}; }
   });
   var a,b;
   [a, b] = iter;
   return closed;
}

function kangaxh() {
   var a, b, iterable = [1,2];
   return ([a, b] = iterable) === iterable;
}

function kangaxi() {
   var a,b,c,d;
   [a,b] = [c,d] = [1,2];
   return a === 1 && b === 2 && c === 1 && d === 2;
}

function kangaxj() {
   var a;
   [a,] = [1];
   return a === 1;
}

function kangaxk() {
   var c,d,e;
   ({c, x:d, e} = {c:7, x:8});
   return c === 7 && d === 8 && e === undefined;
}

function kangaxl() {
   var toFixed, slice;
   ({toFixed} = 2);
   ({slice} = '');
   return toFixed === Number.prototype.toFixed
      && slice === String.prototype.slice;
}

function kangaxm() {
   var a;
   ({a,} = {a:1});
   return a === 1;
}

function kangaxn() {
   var a, b;
   ({a,b} = {a:1,b:2});
   try {
      eval("({a,b}) = {a:3,b:4};");
   }
   catch(e) {
      return a === 1 && b === 2;
   }
}

function kangaxo() {
   var a,b,c,d;
   ({a,b} = {c,d} = {a:1,b:2,c:3,d:4});
   return a === 1 && b === 2 && c === 3 && d === 4;
}

function kangaxp() {
   var a,b;
   try {
      ({a} = null);
      return false;
   } catch(e) {
      if (!(e instanceof TypeError))
         return false;
   }
   try {
      ({b} = undefined);
      return false;
   } catch(e) {
      if (!(e instanceof TypeError))
         return false;
   }
   return true;
}

function kangaxq() {
   var grault, qux = "corge";
   ({ [qux]: grault } = { corge: "garply" });
   return grault === "garply";
}

function kangaxr() {
   var e,f,g,h,i;
   [e, {x:f, g}] = [9, {x:10}];
   ({h, x:[i]} = {h:11, x:[12]});
   return e === 9 && f === 10 && g === undefined
      && h === 11 && i === 12;
}

function kangaxs() {
   var a,b,c,d;
   [a, ...b] = [3, 4, 5];
   [c, ...d] = [6];
   return a === 3 && b instanceof Array && (b + "") === "4,5" &&
      c === 6 && d instanceof Array && d.length === 0;
}

function kangaxt() {
   var a = [1, 2, 3], first, last;
   [first, ...[a[2], last]] = a;
   return first === 1 && last === 3 && (a + "") === "1,2,2";
}

function kangaxu() {
   [] = [1,2];
   ({} = {a:1,b:2});
   return true;
}

function kangaxv() {
   var a,b,c,d,e,f;
   ({a = 1, b = 0, z:c = 3} = {b:2, z:undefined});
   [d = 0, e = 5, f = 6] = [4,,undefined];
   return a === 1 && b === 2 && c === 3
      && d === 4 && e === 5 && f === 6;
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
       
