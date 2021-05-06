/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/noserv/es6-misc-class.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 25 11:33:29 2019                          */
/*    Last change :  Thu May  6 16:20:32 2021 (serrano)                */
/*    Copyright   :  2019-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Array subclassing.                       */
/*=====================================================================*/
"use strict";

const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    kangax ...                                                       */
/*---------------------------------------------------------------------*/
function kangaxa() {
   class C extends Boolean {}
   var c = new C(true);
   return c instanceof Boolean
      && c instanceof C
      && c == true;
}

function kangaxb() {
   class C extends Number {}
   var c = new C(6);
   return c instanceof Number
      && c instanceof C
      && +c === 6;
}

function kangaxc() {
   class C extends String {}
   var c = new C("golly");
   return c instanceof String
       && c instanceof C
       && c + '' === "golly"
       && c[0] === "g"
       && c.length === 5;
}

function _kangaxd() {
   class C extends Error {}
   var c = new C();
   return c instanceof Error
       && c instanceof C
       && Object.prototype.toString.call(c) === "[object Error]";
}

function kangaxd() {
   var e = _kangaxd();
   console.error( new.target );
   return e;
}

function kangaxe() {
   var key = {};
   class M extends Map {}
   var map = new M();

   map.set(key, 123);

   return map instanceof M && map.has(key) && map.get(key) === 123;   
}

function kangaxf() {
   var obj = {};
   class S extends Set {}
   var set = new S();

   set.add(123);
   set.add(123);

   return set instanceof S && set.has(123);
}

console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );
console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );
console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );
console.log( "   kangaxd()"); assert.ok( kangaxd(), "kangaxd" );
console.log( "   kangaxe()"); assert.ok( kangaxe(), "kangaxe" );
console.log( "   kangaxf()"); assert.ok( kangaxf(), "kangaxf" );
