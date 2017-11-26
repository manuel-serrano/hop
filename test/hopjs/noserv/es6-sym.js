/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/es6-sym.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 12 08:24:17 2015                          */
/*    Last change :  Sat Nov 25 20:20:55 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ES6 symbol support.                                      */
/*      http://www.ecma-international.org/ecma-262/6.0/#19.4           */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
assert.strictEqual( typeof( Symbol( "foo" ) ), "symbol", "typeof" );
assert.ok( Symbol( "foo" ) !== Symbol( "foo" ) );
assert.ok( !(Symbol( "foo" ) instanceof Symbol), "instanceof" );
assert.strictEqual( Symbol.prototype.__proto__, Object.prototype, "prototype" );

assert.strictEqual( Symbol( "foo" ).toString(), "Symbol(foo)" );
assert.strictEqual( Symbol.iterator.toString(), "Symbol(Symbol.iterator)" );

assert.equal( JSON.stringify( Symbol( "foo" ) ), undefined, "JSON.stringify" );

assert.throws( function() { new Symbol( "foo" ) } );

var s = Symbol.for( "bar" );

assert.ok( s !== Symbol( "bar" ) );

var s2 = Symbol.for( "bar" );
assert.strictEqual( s, s2, "for" );

assert.ok( Symbol.keyFor( s2 ), "keyFor" );
assert.ok( !Symbol.keyFor( Symbol( "foo" ) ) );

assert.equal( typeof( Symbol.unscopables ), "symbol" );
assert.equal( typeof( Symbol.toPrimitive ), "symbol" );
assert.equal( typeof( Symbol.iterator ), "symbol" );
assert.equal( Symbol.keyFor( Symbol.iterator ), undefined );

assert.throws( function() { new Symbol( "foo" ) + 0; } );

var o = {};
o[ Symbol.iterator ] = 5;

assert.equal( Object.keys( o ).length, 0, "keys.length.1" );
assert.deepEqual( Object.getOwnPropertySymbols( o ), [ Symbol.iterator ], "gops.1" );

var o = {};
o[ s ] = 5;

assert.equal( Object.keys( o ).length, 0, "keys.length.2" );
assert.deepEqual( Object.getOwnPropertySymbols( o ), [ s ], "gops.2" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   var object = {};
   var symbol = Symbol();
   var value = {};
   object[symbol] = value;
   return object[symbol] === value;
}

function kangaxb() {
   return typeof Symbol() === "symbol";
}

function kangaxc() {
   var object = {};
   var symbol = Symbol();
   object[symbol] = 1;

   for (var x in object){}
   var passed = !x;

   if (Object.keys && Object.getOwnPropertyNames) {
      passed &= Object.keys(object).length === 0
	 && Object.getOwnPropertyNames(object).length === 0;
   }

   return passed;
}

function kangaxd() {
   var object = {};
   var symbol = Symbol();
   var value = {};

   if (Object.defineProperty) {
      Object.defineProperty(object, symbol, { value: value });
      return object[symbol] === value;
   }

   return passed;
}

function kangaxe() {
   var symbol = Symbol();
   var passed = symbol.foo === undefined;

   Symbol.prototype.foo = 2;
   passed &= symbol.foo === 2;

   delete Symbol.prototype.foo;
   return passed;
}

function kangaxf() {
   var symbol = Symbol();

   try {
      symbol + "";
      return false;
   }
   catch(e) {}

   try {
      symbol + 0;
      return false;
   } catch(e) {}

   return true;
}

function kangaxg() {
   return String(Symbol("foo")) === "Symbol(foo)";
}

function kangaxh() {
   var symbol = Symbol();
   try {
      new Symbol();
   } catch(e) {
      return true;
   }
}

function kangaxi() {
   var symbol = Symbol();
   var symbolObject = Object(symbol);

   return typeof symbolObject === "object" &&
      symbolObject instanceof Symbol &&
      symbolObject == symbol &&
      symbolObject !== symbol &&
      symbolObject.valueOf() === symbol;
}

function kangaxj() {
   var object = { foo: Symbol() };
   object[Symbol()] = 1;
   var array = [Symbol()];
   return JSON.stringify(object) === '{}' && JSON.stringify(array) === '[null]' && JSON.stringify(Symbol()) === undefined;
}

function kangaxk() {
   var testSymbolObject = function (sym) {
      var object = { foo: sym };
      try {
	 // some browsers throw a TypeError when setting symbol object keys.
	 // this isn't part of this test, so, ignore it if so.
	 object[sym] = 1;
      } catch (e) {} // some browsers throw a TypeError when setting symbol object keys.
      var array = [sym];
      return JSON.stringify(object) === '{"foo":{}}' && JSON.stringify(array) === '[{}]' && JSON.stringify(sym) === '{}';
   };
   var objSym = Object(Symbol());
   var symNoToJSON = Object(Symbol());
   Object.defineProperty(symNoToJSON, 'toJSON', { enumerable: false, value: null }); // ensure it overrides the prototype, but is not callable
   return testSymbolObject(objSym) && testSymbolObject(symNoToJSON);
}

function kangaxl() {
   var symbol = Symbol.for('foo');
   return Symbol.for('foo') === symbol &&
      Symbol.keyFor(symbol) === 'foo';
}

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true, "kangaxa" );

console.log( "   kangaxb()");
assert.equal( kangaxb(), true, "kangaxb" );

console.log( "   kangaxc()");
assert.equal( kangaxc(), true, "kangaxc" );

console.log( "   kangaxd()");
assert.equal( kangaxd(), true, "kangaxd" );

console.log( "   kangaxe()");
assert.equal( kangaxe(), true, "kangaxe" );

console.log( "   kangaxf()");
assert.equal( kangaxf(), true, "kangaxf" );

console.log( "   kangaxg()");
assert.equal( kangaxg(), true, "kangaxg" );

console.log( "   kangaxh()");
assert.equal( kangaxh(), true, "kangaxh" );

console.log( "   kangaxi()");
assert.equal( kangaxi(), true, "kangaxi" );

console.log( "   kangaxj()");
assert.equal( kangaxj(), true, "kangaxj" );

console.log( "   kangaxk()");
assert.equal( kangaxk(), true, "kangaxk" );

console.log( "   kangaxl()");
assert.equal( kangaxl(), true, "kangaxl" );

/*---------------------------------------------------------------------*/
/*    Kangax Well Known Symbols                                        */
/*---------------------------------------------------------------------*/
function kangaxSyma() {
   var passed = false;
   var obj = { foo: true };
   var C = function(){};
   console.log( Symbol.hasInstance );
   Object.defineProperty(C, Symbol.hasInstance, {
      value: function(inst) { passed = inst.foo; return false; }
   });
   obj instanceof C;
   return passed;
}

console.log( "kangax well known symbols" );

/* console.log( "   kangaxSyma()");                                    */
/* assert.equal( kangaxSyma(), true, "kangaxSyma" );                   */

