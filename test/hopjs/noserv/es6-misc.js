/*=====================================================================*/
/*    serrano/trashcan/es6-misc.js                                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Nov 25 06:02:02 2017                          */
/*    Last change :  Sun Nov 26 10:22:18 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ES6 misc features                                        */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*    -------------------------------------------------------------    */
/*    http://kangax.github.io/compat-table/es6/                        */
/*---------------------------------------------------------------------*/
function kangaxa() {
   var result = '';
   var target = {};

   "012349 DBACEFGHIJKLMNOPQRST".split('').concat(-1).forEach(function(key){
      Object.defineProperty(target, key, {
	 set: function(){
	    result += key;
	 }
      })
   });

   var obj = {2: 2, 0: 0, 1: 1, ' ': ' ', 9: 9, D: 'D', B: 'B', '-1': '-1'};
   Object.defineProperty(obj, 'A', {value: 'A',  enumerable: true});
   Object.defineProperty(obj, '3', {value: '3',  enumerable: true});
   Object.defineProperty(obj, 'C', {value: 'C',  enumerable: true});
   Object.defineProperty(obj, '4', {value: '4',  enumerable: true});
   delete obj[2];
   obj[2] = true;

   "EFGHIJKLMNOPQRST".split('').forEach(function(key){
      obj[key] = key;
   });

   Object.assign(target, obj);

   return result === "012349 DB-1ACEFGHIJKLMNOPQRST";
}

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true, "kangaxa" );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*    -------------------------------------------------------------    */
/*    https://developer.mozilla.org/en-US/docs/Web/                    */
/*       JavaScript/Reference/Global_Objects/Object/assign             */
/*---------------------------------------------------------------------*/
function mdnAssign1() {
   var o1 = { a: 1 };
   var o2 = { [Symbol('foo')]: 2 };

   var obj = Object.assign({}, o1, o2);

   return obj.a === 1
      && Object.getOwnPropertySymbols(obj)[0].toString() == Symbol('foo').toString();
}

function mdnAssign2() {
   var obj = Object.create({ foo: 1 }, { // foo is on obj's prototype chain.
      bar: {
	 value: 2  // bar is a non-enumerable property.
      },
      baz: {
	 value: 3,
	 enumerable: true  // baz is an own enumerable property.
      }
   });

   var copy = Object.assign({}, obj);
   return copy.baz === 3;
}

function mdnAssign3() {
   var v1 = 'abc';
   var v2 = true;
   var v3 = 10;
   var v4 = Symbol('foo');

   var obj = Object.assign({}, v1, null, v2, undefined, v3, v4); 
   // Primitives will be wrapped, null and undefined will be ignored.
   // Note, only string wrappers can have own enumerable properties.

   return obj[ 0 ] == "a" && obj[ 1 ] == "b" && obj[ 2 ] == "c";
}

function mdnAssign4() {
   var target = Object.defineProperty({}, 'foo', {
      value: 1,
      writable: false
   }); // target.foo is a read-only property
   var exn = false

   try {
      Object.assign(target, { bar: 2 }, { foo2: 3, foo: 3, foo3: 3 }, { baz: 4 });
      // TypeError: "foo" is read-only
      // The Exception is thrown when assigning target.foo
   } catch( e ) {
      exn = true;
   }
      
   return target.bar === 2
      && target.foo2 === 3
      && exn
      && target.foo3 === undefined
      && target.baz === undefined;
}

function mdnAssign5() {
   var obj = {
      foo: 1,
      get bar() {
	 return 2;
      }
   };

   var copy = Object.assign({}, obj);
   // { foo: 1, bar: 2 }, the value of copy.bar is obj.bar's getter's return value.
   if( copy.foo !== 1 || copy.bar !== 2 ) return false;

   // This is an assign function that copies full descriptors
   function completeAssign(target, ...sources) {
      sources.forEach(source => {
	 let descriptors = Object.keys(source).reduce((descriptors, key) => {
	    descriptors[key] = Object.getOwnPropertyDescriptor(source, key);
	    return descriptors;
	 }, {});
	 // by default, Object.assign copies enumerable Symbols too
	 Object.getOwnPropertySymbols(source).forEach(sym => {
	    let descriptor = Object.getOwnPropertyDescriptor(source, sym);
	    if (descriptor.enumerable) {
               descriptors[sym] = descriptor;
	    }
	 });
	 Object.defineProperties(target, descriptors);
      });
      return target;
   }

   var copy = completeAssign({}, obj);

   // { foo:1, get bar() { return 2 } }
   return copy.foo === 1 && copy.bar === 2;
}

console.log( "mdnAssign" );

console.log( "   mdnAssign1()");
assert.equal( mdnAssign1(), true, "mdnAssign1" );

console.log( "   mdnAssign2()");
assert.equal( mdnAssign2(), true, "mdnAssign2" );

console.log( "   mdnAssign3()");
assert.equal( mdnAssign3(), true, "mdnAssign3" );

console.log( "   mdnAssign4()");
assert.equal( mdnAssign4(), true, "mdnAssign4" );

console.log( "   mdnAssign5()");
assert.equal( mdnAssign5(), true, "mdnAssign5" );

