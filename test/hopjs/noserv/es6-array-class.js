/*=====================================================================*/
/*    .../project/hop/3.2.x/test/hopjs/noserv/es6-array-class.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 25 11:33:29 2019                          */
/*    Last change :  Wed Feb 27 08:31:03 2019 (serrano)                */
/*    Copyright   :  2019 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Array subclassing.                       */
/*=====================================================================*/
"use strict";
"use hopscript";

const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    kangax ...                                                       */
/*---------------------------------------------------------------------*/
function kangaxa() {
   class C extends Array {}
   var c = new C();
   var len1 = c.length;
   c[2] = 'foo';
   var len2 = c.length;
   return len1 === 0 && len2 === 3;
}

function kangaxb() {
   class C extends Array {}
   var c = new C();
   c[2] = 'foo';
   c.length = 1;
   return c.length === 1 && !(2 in c);
}

function kangaxc() {
   class C extends Array {}
   var c = new C();
   return c instanceof C && c instanceof Array && Object.getPrototypeOf(C) === Array;
}

function kangaxd() {
   class C extends Array {}
   return Array.isArray(new C());
}

function kangaxe() {
   class C extends Array {}
   var c = new C();
   return c.concat(1) instanceof C;
}

function kangaxf() {
   class C extends Array {}
   var c = new C();
   return c.filter(Boolean) instanceof C;
}

function kangaxg() {
   class C extends Array {}
   var c = new C();
   return c.map(Boolean) instanceof C;
}

function kangaxh() {
   class C extends Array {}
   var c = new C();
   c.push(2,4,6);
   return c.slice(1,2) instanceof C;
}

function kangaxi() {
   class C extends Array {}
   var c = new C();
   c.push(2,4,6);
   return c.splice(1,2) instanceof C;
}

function kangaxj() {
   class C extends Array {}
   return C.from({ length: 0 }) instanceof C;
}

function kangaxk() {
   class C extends Array {}
   return C.of(0) instanceof C;
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

