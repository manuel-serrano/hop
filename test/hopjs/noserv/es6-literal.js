/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-literal.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Feb  6 09:07:26 2018                          */
/*    Last change :  Wed Feb  7 17:42:40 2018 (serrano)                */
/*    Copyright   :  2018 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2015+ object literal syntaxes                 */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    hop                                                              */
/*---------------------------------------------------------------------*/
function hopa() {
   var foo = 44;
   var o = {
      foo() { return foo; }
   };

   return o.foo();
}

function hopb() {
   var i = 10;
   var a = {
      i
   }
   return a.i == 10;
}

function hopc() {
   var o = {
      get() { return this.a },
      a: 3
   }

   return o.get() == 3;
}

console.log( "hop" );
console.log( "   hopa()"); assert.ok( hopa(), "hopa" );
console.log( "   hopb()"); assert.ok( hopb(), "hopb" );
console.log( "   hopc()"); assert.ok( hopc(), "hopc" );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function mdna() {
   // Shorthand property names (ES2015)
   var a = 'foo', b = 42, c = {};
   var o = {a, b, c};

   return o.a == 'foo' && o.b == 42 && o.c instanceof Object && o.a == {a}.a;
}

function mdnb() {
   // Shorthand method names (ES2015)
   var o = {
      property(/*[parameters]*/) {}
   };

   return o.property instanceof Function;
}

function mdnc() {
   // Computed property names (ES2015)
   var prop = 'foo';
   var o = {
      [prop]: 'hey',
      ['b' + 'ar']: 'there'
   };

   return o.foo == 'hey' && o.bar == 'there';

}

function mdnd() {
   // Computed property names (ES2015)
   var i = 0;
   var a = {
      ['foo' + ++i]: i,
      ['foo' + ++i]: i,
      ['foo' + ++i]: i
   };

   return a.foo1 == 1 && a.foo2 == 2 && a.foo3 == 3;
}

function mdne() {
   // Computed property names (ES2015)
   var param = 'size';
   var config = {
      [param]: 12,
      ['mobile' + param.charAt(0).toUpperCase() + param.slice(1)]: 4
   };

   return config.size == 12 && config.mobileSize == 4;
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );
console.log( "   mdnd()"); assert.ok( mdnd(), "mdnd" );
console.log( "   mdne()"); assert.ok( mdne(), "mdne" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   var x = 'y';
   return ({ [x]: 1 }).y === 1;
}

function kangaxb() {
   var a = 7, b = 8, c = {a,b};
   return c.a === 7 && c.b === 8;
}

function kangaxc() {
   return ({ y() { return 2; } }).y() === 2;
}

function kangaxd() {
   return ({ "foo bar"() { return 4; } })["foo bar"]() === 4;
}

function kangaxe() {
   var x = 'y';
   return ({ [x](){ return 1 } }).y() === 1;
}

function kangaxf() {
   var x = 'y',
       valueSet,
       obj = {
	  get [x] () { return 1 },
	  set [x] (value) { valueSet = value }
       };
   obj.y = 'foo';
   console.log( "obj=", obj.y );
   return obj.y === 1 && valueSet == 'foo';
}

console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );
console.log( "   kangaxb()"); assert.ok( kangaxb(), "kangaxb" );
console.log( "   kangaxc()"); assert.ok( kangaxc(), "kangaxc" );
console.log( "   kangaxd()"); assert.ok( kangaxd(), "kangaxd" );
console.log( "   kangaxe()"); assert.ok( kangaxe(), "kangaxe" );
console.log( "   kangaxf()"); assert.ok( kangaxf(), "kangaxf" );

