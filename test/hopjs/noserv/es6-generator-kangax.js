/*=====================================================================*/
/*    .../hop/3.1.x/test/hopjs/noserv/es6-generator-kangax.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Feb  4 09:23:13 2017                          */
/*    Last change :  Sat Feb  4 09:23:29 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Kangax generator tests                                           */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*    -------------------------------------------------------------    */
/*    http://kangax.github.io/compat-table/es6/                        */
/*---------------------------------------------------------------------*/
function kangaxa() {
   function *generator() {
      yield 5; yield 6;
   };
   
   var iterator = generator();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxb() {
   var generator = function * (){
      yield 5; yield 6;
   };
   var iterator = generator();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxc() {
   function * generator(){
      yield this.x; yield this.y;
   };
   var iterator = { g: generator, x: 5, y: 6 }.g();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxd() {
   function * generator(){
      yield this.x; yield this.y;
   };
   try {
      (new generator()).next();
   }
   catch (e) {
      return true;
   }
}

function kangaxe() {
   var sent;
   function * generator(){
      sent = [yield 5, yield 6];
   };
   var iterator = generator();
   iterator.next();
   iterator.next("foo");
   iterator.next("bar");
   return sent[0] === "foo" && sent[1] === "bar";
}

function kangaxf() {
   function * generatorFn(){}
   var ownProto = Object.getPrototypeOf(generatorFn());
   var passed = ownProto === generatorFn.prototype;

   var sharedProto = Object.getPrototypeOf(ownProto);
   passed &= sharedProto !== Object.prototype &&
      sharedProto === Object.getPrototypeOf(function*(){}.prototype) &&
      sharedProto.hasOwnProperty('next');

   return passed;
}

function kangaxg() {
   function * g (){}
   var iterator = new g.constructor("a","b","c","yield a; yield b; yield c;")(5,6,7);
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 7 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxh() {
   var passed = false;
   function * generator(){
      try {
	 yield 5; yield 6;
      } catch(e) {
	 passed = (e === "foo");
      }
   };
   var iterator = generator();
   iterator.next();
   iterator.throw("foo");
   return passed;   
}

function kangaxi() {
   function * generator(){
      yield 5; yield 6;
   };
   var iterator = generator();
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.return("quxquux");
   passed &= item.value === "quxquux" && item.done === true;
   item = iterator.next();
   passed &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxj() {
   var passed;
   function * generator(){
      passed = yield 0 ? true : false;
   };
   var iterator = generator();
   iterator.next();
   iterator.next(true);
   return passed;   
}

function kangaxk() {
   var iterator = (function * generator() {
      yield * [5, 6];
   }());
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;   
}

function kangaxl() {
   var iterator = (function * generator() {
      yield * [,,];
   }());
   var item = iterator.next();
   var passed = item.value === undefined && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxm() {
   var iterator = (function * generator() {
      yield * "56";
   }());
   var item = iterator.next();
   var passed = item.value === "5" && item.done === false;
   item = iterator.next();
   passed    &= item.value === "6" && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;
}

function kangaxn() {
   var iterator = (function * generator() {
      yield * "𠮷𠮶";
   }());
   var item = iterator.next();
   var passed = item.value === "𠮷" && item.done === false;
   item = iterator.next();
   passed    &= item.value === "𠮶" && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;   
}

function kangaxo() {
   var iterator = (function * generator() {
      yield * (function*(){ yield 5; yield 6; yield 7; }());
   }());
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 6 && item.done === false;
   item = iterator.next();
   passed    &= item.value === 7 && item.done === false;
   item = iterator.next();
   passed    &= item.value === undefined && item.done === true;
   return passed;   
}

function kangaxr() {
   var iterator = (function * generator() {
      yield * [5];
   }());
   var item = iterator.next();
   var passed = item.value === 5 && item.done === false;
   iterator = (function * generator() {
      yield * 5;
   }());
   try {
      iterator.next();
   } catch (e) {
      return passed;
   }
}

/* function kangaxu() {                                                */
/*    var o = {                                                        */
/* 	 * generator() {                                               */
/* 	    yield 5; yield 6;                                          */
/* 	 },                                                            */
/*    };                                                               */
/*    var iterator = o.generator();                                    */
/*    var item = iterator.next();                                      */
/*    var passed = item.value === 5 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === 6 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === undefined && item.done === true;     */
/*    return passed;                                                   */
/* }                                                                   */
/*                                                                     */
/* function kangaxv() {                                                */
/*    var o = {                                                        */
/* 	 * "foo bar"() {                                               */
/* 	    yield 5; yield 6;                                          */
/* 	 },                                                            */
/*    };                                                               */
/*    var iterator = o["foo bar"]();                                   */
/*    var item = iterator.next();                                      */
/*    var passed = item.value === 5 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === 6 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === undefined && item.done === true;     */
/*    return passed;                                                   */
/* }                                                                   */
/*                                                                     */
/* function kangaxw() {                                                */
/*    var garply = "generator";                                        */
/*    var o = {                                                        */
/* 	 * [garply] () {                                               */
/* 	    yield 5; yield 6;                                          */
/* 	 },                                                            */
/*    };                                                               */
/*    var iterator = o.generator();                                    */
/*    var item = iterator.next();                                      */
/*    var passed = item.value === 5 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === 6 && item.done === false;            */
/*    item = iterator.next();                                          */
/*    passed    &= item.value === undefined && item.done === true;     */
/*    return passed;                                                   */
/* }                                                                   */

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true );

console.log( "   kangaxb()");
assert.equal( kangaxb(), true );

console.log( "   kangaxc()");
assert.equal( kangaxc(), true );

console.log( "   kangaxd()");
assert.equal( kangaxd(), undefined );

console.log( "   kangaxe()");
assert.equal( kangaxe(), true );

console.log( "   kangaxf()");
assert.equal( kangaxf(), true );

console.log( "   kangaxg()");
assert.equal( kangaxg(), true );

console.log( "   kangaxh()");
assert.equal( kangaxh(), true );

console.log( "   kangaxi()");
assert.equal( kangaxi(), true );

console.log( "   kangaxj()");
assert.equal( kangaxj(), true );

console.log( "   kangaxk()");
assert.equal( kangaxk(), true );

console.log( "   kangaxl()");
assert.equal( kangaxl(), true );

console.log( "   kangaxm()");
assert.equal( kangaxm(), true );

console.log( "   kangaxn()");
assert.equal( kangaxn(), true );

console.log( "   kangaxo()");
assert.equal( kangaxo(), true );

console.log( "   kangaxr()");
assert.equal( kangaxr(), true );

/* console.log( "   kangaxu()");                                       */
/* assert.equal( kangaxu(), true );                                    */
/*                                                                     */
/* console.log( "   kangaxv()");                                       */
/* assert.equal( kangaxv(), true );                                    */
/*                                                                     */
/* console.log( "   kangaxw()");                                       */
/* assert.equal( kangaxw(), true );                                    */
/*                                                                     */



