/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/ecma8.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Tue Jan 15 08:36:28 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing basic ECMA 262, 8 features                               */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   var o = {};
   
   o.b = 34;
   o[ Symbol.iterator ] = 39;
   Object.defineProperty( o, "foo", {value:12, enumerable: false } );
   
   const names = Object.getOwnPropertyNames( o );
   const syms = Object.getOwnPropertySymbols( o );
   const descs = Object.getOwnPropertyDescriptors( o );
   
   return names.length === 2 
      && syms.length === 1
      && descs.b.value === 34
      && descs.foo.value === 12
      && descs[ Symbol.iterator ].value === 39;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function mdna() {
   var object1 = { property1: 42 };   
   const descriptors1 = Object.getOwnPropertyDescriptors(object1);
   return descriptors1.property1.writable === true
      && descriptors1.property1.value === 42;
}

function mdnb() {
   const obj = { a: 1 };
   Object.defineProperty( obj, "b", {value: 2, enumerable: false} );
   
   const clone = Object.create( 
      Object.getPrototypeOf(obj),
      Object.getOwnPropertyDescriptors(obj) );
   
   return clone.a === 1 && clone.b === 2;
}
   
console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
}

function kangaxb() {
}

function kangaxc() {
}

function kangaxd() {
}
 
/* console.log( "kangax" );                                            */
/* console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );    */
/* console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );    */
/* console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );    */
/* console.log( "   kangaxd()"); assert.ok( kangaxd(), "kangaxd" );    */

