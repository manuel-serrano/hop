/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-reflect.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Fri Dec 21 18:39:46 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Reflect object                           */
/*=====================================================================*/
"use strict";
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   return true;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );

/*---------------------------------------------------------------------*/
/*    mdn ...                                                          */
/*---------------------------------------------------------------------*/
function mdna() {
   return Reflect.apply(Math.floor, undefined, [1.75]) === 1;
}

function mdnb() {
   return Reflect.apply(String.fromCharCode, undefined, [104, 101, 108, 108, 111]) === "hello";
}

function mdnc() {
   return Reflect.apply(RegExp.prototype.exec, /ab/, ['confabulationo']).index;
}

function mdnd() {
   return Reflect.apply(''.charAt, 'ponies', [3]) === "i";
}
   
function mdne() {
   function func1(a, b, c) {
      this.sum = a + b +  c;
   }
   
   const args = [1, 2, 3];
   const object1 = new func1(...args);
   const object2 = Reflect.construct(func1, args);
   
   return object2.sum === 6 && object1.sum === 6;
}

function mdnf() {
   var passed = undefined;
   const object1 = {};
   
   if( Reflect.defineProperty(object1, 'property1', {value:42})) {
      passed = true;
   } else {
      passed = false;
   }
   return passed && object1.property1 === 42;
}

function mdng() {
   var passed = false;
   const object1 = { property1: 42 };
   
   Reflect.deleteProperty(object1, 'property1');
   passed = (object1.property1 === undefined);
   
   var array1 = [1,2,3,4,5];
   Reflect.deleteProperty(array1, '3');
   return passed && array1.join() === "1,2,3,,5";   
}

function mdnh() {
   var obj = { x: 1, y: 2 };
   return Reflect.get(obj, 'x') === 1;
}

function mdni() {
   console.log( Reflect.get(['zero', 'one'], 1) );
   return Reflect.get(['zero', 'one'], 1) === "one";
}

function mdnj() {
   var x = {p: 1};
   var obj = new Proxy(x, {
      get(t, k, r) { return k + 'bar'; }
   });
   return Reflect.get(obj, 'foo') === "foobar";
}

function mdnk() {
   const object1 = {
      property1: 42
   };

   return Reflect.getOwnPropertyDescriptor(object1, 'property1').value === 42
      && Reflect.getOwnPropertyDescriptor(object1, 'property2') === undefined
      && Reflect.getOwnPropertyDescriptor(object1, 'property1').writable === true;
}

function mdnl() {
   const object1 = {
      property1: 42
   }
   
   const proto1 = Reflect.getPrototypeOf(object1);
   return Reflect.getPrototypeOf(proto1) === null;
}

function mdnm() {
   const object1 = {
      property1: 42
   };
   
   return Reflect.has(object1, 'property1') === true
      && Reflect.has(object1, 'property2') === false
      && Reflect.has(object1, 'toString') === true;
}

function mdnn() {
   const object1 = {};
   if( Reflect.isExtensible(object1) !== true ) return false;
   		
   Reflect.preventExtensions(object1);
		
   if( Reflect.isExtensible(object1) ) return false;
		
   const object2 = Object.seal({});
   return !Reflect.isExtensible(object2);
}

function mdno() {
   const object1 = {
      property1: 42,
      property2: 13
   };
   
   var array1 = [];
   
   return Reflect.ownKeys(object1).join() === "property1,property2"
      && Reflect.ownKeys(array1).join() === "length";
}

function mdnp() {
   var object1 = {};
   
   if( Reflect.isExtensible(object1) !== true ) return false;
   
   Reflect.preventExtensions(object1);
   
   return Reflect.isExtensible(object1) === false;
}

function mdnq() {
   var object1 = {};
   
   Reflect.set(object1, 'property1', 42);
   
   if( object1.property1 !== 42 ) return false;
   
   const array1 = ['duck', 'duck', 'duck'];
   Reflect.set(array1, 2, 'goose');
   return array1[2] === "goose";
}

function mdnr() {
   var object1 = {};
   
   if( Reflect.setPrototypeOf(object1, Object.prototype) !== true )
      return false;
   
   if( Reflect.setPrototypeOf(object1, null) !== true )
      return false;
   
   const object2 = {};
   
   return Reflect.setPrototypeOf(Object.freeze(object2), null) === false;
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
console.log( "   mdnk()"); assert.ok( mdnk(), "mdnk" );
console.log( "   mdnl()"); assert.ok( mdnl(), "mdnl" );
console.log( "   mdnm()"); assert.ok( mdnm(), "mdnm" );
console.log( "   mdnn()"); assert.ok( mdnn(), "mdnn" );
console.log( "   mdno()"); assert.ok( mdno(), "mdno" );
console.log( "   mdnp()"); assert.ok( mdnp(), "mdnp" );
console.log( "   mdnq()"); assert.ok( mdnq(), "mdnq" );
console.log( "   mdnr()"); assert.ok( mdnr(), "mdnr" );

/*---------------------------------------------------------------------*/
/*    kangax ...                                                       */
/*---------------------------------------------------------------------*/
function kangaxa() {
   return Reflect.get({ qux: 987 }, "qux") === 987;
}

function kangaxb() {
   var obj = {};
   Reflect.set(obj, "quux", 654);
   return obj.quux === 654;
}

function kangaxc() {
   return Reflect.has({ qux: 987 }, "qux");
}

function kangaxd() {
   var obj = { bar: 456 };
   Reflect.deleteProperty(obj, "bar");
   return !("bar" in obj);
}

function kangaxe() {
   var obj = { baz: 789 };
   var desc = Reflect.getOwnPropertyDescriptor(obj, "baz");
   return desc.value === 789 &&
      desc.configurable && desc.writable && desc.enumerable;
}

function kangaxf() {
   var obj = {};
   Reflect.defineProperty(obj, "foo", { value: 123 });
   return obj.foo === 123 &&
      Reflect.defineProperty(Object.freeze({}), "foo", { value: 123 }) === false;
}

function kangaxg() {
   return Reflect.getPrototypeOf([]) === Array.prototype;
}

function kangaxh() {
   var obj = {};
   Reflect.setPrototypeOf(obj, Array.prototype);
   return obj instanceof Array;
}

function kangaxi() {
   return Reflect.isExtensible({}) &&
      !Reflect.isExtensible(Object.preventExtensions({}));
}

function kangaxj() {
   var obj = {};
   Reflect.preventExtensions(obj);
   return !Object.isExtensible(obj);
}

function kangaxk() {
   var obj = Object.create({ C: true });
   obj.A = true;
   Object.defineProperty(obj, 'B', { value: true, enumerable: false });

   return Reflect.ownKeys(obj).sort() + '' === "A,B";
}

function kangaxl() {
   var s1 = Symbol(), s2 = Symbol(), s3 = Symbol();
   var proto = {};
   proto[s1] = true;
   var obj = Object.create(proto);
   obj[s2] = true;
   Object.defineProperty(obj, s3, { value: true, enumerable: false });

   var keys = Reflect.ownKeys(obj);
   return keys.indexOf(s2) >-1 && keys.indexOf(s3) >-1 && keys.length === 2;
}

function kangaxm() {
   return Reflect.apply(Array.prototype.push, [1,2], [3,4,5]) === 5;
}

function kangaxn() {
   return Reflect.construct(function(a, b, c) {
      this.qux = a + b + c;
   }, ["foo", "bar", "baz"]).qux === "foobarbaz";
}

function kangaxo() {
   return Reflect.construct(function(a, b, c) {
      if (new.target === Object) {
    	 this.qux = a + b + c;
      }
   }, ["foo", "bar", "baz"], Object).qux === "foobarbaz";
}

function kangaxp() {
   function F(){}
   var obj = Reflect.construct(function(){ this.y = 1; }, [], F);
   return obj.y === 1 && obj instanceof F;
}

function kangaxq() {
   function F(){}
   var obj = Reflect.construct(Array, [], F);
   obj[2] = 'foo';
   return obj.length === 3 && obj instanceof F;
}

function kangaxr() {
   function F(){}
   var obj = Reflect.construct(RegExp, ["baz","g"], F);
   return RegExp.prototype.exec.call(obj, "foobarbaz")[0] === "baz"
  						      && obj.lastIndex === 9 && obj instanceof F;
}

function kangaxs() {
   function F(){}
   var obj = Reflect.construct(Function, ["return 2"], F);
   return obj() === 2 && obj instanceof F;
}

/* function kangaxt() {                                                */
/*    function F(){}                                                   */
/*    var p1 = Reflect.construct(Promise,[function(resolve, reject) { resolve("foo"); }], F); */
/*    var p2 = Reflect.construct(Promise,[function(resolve, reject) { reject("quux"); }], F); */
/*    var score = +(p1 instanceof F && p2 instanceof F);               */
/*                                                                     */
/*    function thenFn(result)  { score += (result === "foo");  check(); } */
/*    function catchFn(result) { score += (result === "quux"); check(); } */
/*    function shouldNotRun(result)  { score = -Infinity;   }          */
/*                                                                     */
/*    p1.then = p2.then = Promise.prototype.then;                      */
/*    p1.catch = p2.catch = Promise.prototype.catch;                   */
/*                                                                     */
/* p1.then(thenFn, shouldNotRun);                                      */
/* p2.then(shouldNotRun, catchFn);                                     */
/* p1.catch(shouldNotRun);                                             */
/*    p2.catch(catchFn);                                               */
/*                                                                     */
/*       function check() {                                            */
/* 	 assert.ok( score === 4, "kangaxt" );                          */
/*       }                                                             */
/*                                                                     */
/*       return true;                                                  */
/* }                                                                   */

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
/* console.log( "   kangaxt()"); assert.ok( kangaxt(), "kangaxt" );    */

