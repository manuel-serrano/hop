/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-class.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep  2 01:49:55 2017                          */
/*    Last change :  Sat Sep  9 12:00:48 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 classes                                   */
/*=====================================================================*/
"use strict";

const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    basic                                                            */
/*---------------------------------------------------------------------*/
function basica() {
   class A {
      constructor() { this.x = 1; }
   }
   class B extends A {
      constructor() { this.y = 2; }
   }
   try {
      new B();
      return false;
   } catch( e ) {
      return e instanceof ReferenceError;
   }
}

function basicb() {
   class A {
      constructor() { this.x = 1; }
   }
   class B extends A {
      constructor() {}
   }
   try {
      new B();
      return false;
   } catch( e ) {
      return e instanceof ReferenceError;
   }
}

function basicc() {
   class A {
      constructor() { this.x = 1; }
   }
   class B extends A {
      constructor() { super(); }
   }
   try {
      return new B().x == 1;
   } catch( e ) {
      return false;
   }
}

function basicd() {
   var o = {
      foo() {
	 var f = Object.getOwnPropertyDescriptor( this, "foo" );
	 var b = Object.getOwnPropertyDescriptor( this, "bar" );
	 return f.enumerable && f.writable && f.configurable
	    && b.enumerable && b.writable && b.configurable;
      },
      bar: function() { return 2; }
   }
   return o.foo();
}

function basice() {
   var o = {
      foo() {
	 return foo = 4;
      }
   }
   try {
      o.foo();
      return false;
   } catch( e ) {
      return e instanceof ReferenceError;
   }
}

function basicf() {
   var p = {
      foo() {
	 super.gee();
      }
   }

   try {
      p.foo();
   } catch( e ) {
      return true;
   }
}

function basicg() {
   var proto = {
      gee() { console.log( "gee" ); }
   }

   var p = {
      foo() {
	 super.gee();
      }
   }

   p.__proto__ = proto;
   
   try {
      p.foo();
      return true;
   } catch( e ) {
      return false;
   }
}

console.log( "basic" );
console.log( "   basica()" );
assert.ok( basica(), "basica" );

console.log( "   basicb()" );
assert.ok( basicb(), "basicb" );

console.log( "   basicc()" );
assert.ok( basicc(), "basicc" );

console.log( "   basicd()" );
assert.ok( basicd(), "basicd" );

console.log( "   basice()" );
assert.ok( basice(), "basice" );

console.log( "   basicf()" );
assert.ok( basicf(), "basicf" );

console.log( "   basicg()" );
assert.ok( basicg(), "basicg" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   return typeof class {} === "function";
}

function kangaxb() {
   class C {
      constructor() { this.x = 1; }
   }
   return C.prototype.constructor === C
      && new C().x === 1;
}

function kangaxc() {
   class C {
      method() { return 2; }
   }
   return typeof C.prototype.method ==="function"
      && new C().method() === 2;   
}

function kangaxd() {
   class C {
      "foo bar"() { return 2; }
   }
   return typeof C.prototype["foo bar"] === "function"
      && new C()["foo bar"]() === 2;
}

function kangaxe() {
   var foo = "method";
   class C {
      [foo]() { return 2; }
   }
   return typeof C.prototype.method === "function"
      && new C().method() === 2;
}

function kangaxf() {
   class C {
      ;
      method() { return 2; };
      method2() { return 2; }
      method3() { return 2; };
   }
   return typeof C.prototype.method === "function"
      && typeof C.prototype.method2 === "function"
      && typeof C.prototype.method3 === "function";
}

function kangaxg() {
   class C {
      static method() { return 3; }
   }
   return typeof C.method === "function"
      && C.method() === 3;
}

function kangaxh() {
   var foo = "method";
   class C {
      static [foo]() { return 3; }
   }
   return typeof C.method === "function"
      && C.method() === 3;
}

function kangaxi() {
   var baz = false;
   class C {
      get foo() { return "foo"; }
      set bar(x) { baz = x; }
   }
   new C().bar = true;
   return new C().foo === "foo" && baz;
}

function kangaxj() {
   var garply = "foo", grault = "bar", baz = false;
   class C {
      get [garply]() { return "foo"; }
      set [grault](x) { baz = x; }
   }
   new C().bar = true;
   return new C().foo === "foo" && baz;
}

function kangaxk() {
   var baz = false;
   class C {
      static get foo() { return "foo"; }
      static set bar(x) { baz = x; }
   }
   C.bar = true;
   return C.foo === "foo" && baz;
}

function kangaxl() {
   var garply = "foo", grault = "bar", baz = false;
   class C {
      static get [garply]() { return "foo"; }
      static set [grault](x) { baz = x; }
   }
   C.bar = true;
   return C.foo === "foo" && baz;
}

function kangaxm() {
   class C {
      method() { return typeof C === "function"; }
   }
   var M = C.prototype.method;
   C = undefined;
   return C === undefined && M();
}

function kangaxn() {
   try {
      var B = class C {
	 [C](){}
      }
   } catch(e) {
      return true;
   }
}

function kangaxo() {
   class C {
      foo() { return 1; }
      static bar() {}
   }
   return !C.prototype.propertyIsEnumerable("foo")
      && !C.propertyIsEnumerable("bar");
}

function kangaxp() {
   class C {
      static method() { return this === undefined; }
   }
   return (0,C.method)();
}

function kangaxq() {
   class C {}
   try {
      C();
   }
   catch(e) {
      return true;
   }
}

function kangaxr() {
   class B {}
   class C extends B {}
   return new C() instanceof B
      && B.isPrototypeOf(C);
}

function kangaxs() {
   var B;
   class C extends (B = class {}) {}
   return new C() instanceof B
      && B.isPrototypeOf(C);
}

function kangaxt() {
   class C extends null {
      constructor() { return Object.create(null); }
   }
   return Function.prototype.isPrototypeOf(C)
      && Object.getPrototypeOf(C.prototype) === null;
}

/* function kangaxu() {                                                */
/*    var passed = false;                                              */
/*    new function f() {                                               */
/*       passed = new.target === f;                                    */
/*    }();                                                             */
/*                                                                     */
/*    class A {                                                        */
/*       constructor() {                                               */
/* 	 passed &= new.target === B;                                   */
/*       }                                                             */
/*    }                                                                */
/*    class B extends A {}                                             */
/*    new B();                                                         */
/*    return passed;                                                   */
/* }                                                                   */

function kangaxv() {
   var passed = false;
   class B {
      constructor(a) { passed = (a === "barbaz"); }
   }
   class C extends B {
      constructor(a) { super("bar" + a); }
   }
   new C("baz");
   return passed;
}

function kangaxw() {
   class B {
      constructor(a) { return ["foo" + a]; }
   }
   class C extends B {
      constructor(a) { return super("bar" + a); }
   }
   return new C("baz")[0] === "foobarbaz";
}

function kangaxx() {
   class B {}
   B.prototype.qux = "foo";
   B.prototype.corge = "baz";
   class C extends B {
      quux(a) { return super.qux + a + super["corge"]; }
   }
   C.prototype.qux = "garply";
   return new C().quux("bar") === "foobarbaz";
}

function kangaxy() {
   class B {
      qux(a) { return "foo" + a; }
   }
   class C extends B {
      qux(a) { return super.qux("bar" + a); }
   }
   return new C().qux("baz") === "foobarbaz";
}

function kangaxz() {
   class B {
      qux(a) { return this.foo + a; }
   }
   class C extends B {
      qux(a) { return super.qux("bar" + a); }
   }
   var obj = new C();
   obj.foo = "foo";
   return obj.qux("baz") === "foobarbaz";
}

/* function kangaxA() {                                                */
/*    var passed;                                                      */
/*    class B {                                                        */
/*       constructor() { passed = (new.target === C); }                */
/*    }                                                                */
/*    class C extends B {                                              */
/*       constructor() { super(); }                                    */
/*    }                                                                */
/*    new C();                                                         */
/*    return passed;                                                   */
/* }                                                                   */

function kangaxB() {
   class B {
      qux() { return "bar"; }
   }
   class C extends B {
      qux() { return super.qux() + this.corge; }
   }
   var obj = {
      qux: C.prototype.qux,
      corge: "ley"
   };
   return obj.qux() === "barley";
}

function kangaxC() {
   var passed;
   class B {
      constructor() {
         passed = true;
      }
   };
   B.prototype.constructor = function () {
      passed = false;
   };
   class C extends B { };
   new C;
   return passed;
}

console.log( "kangax" );
console.log( "   kangaxa()" );
assert.ok( kangaxa(), "kangaxa" );

console.log( "   kangaxb()" );
assert.ok( kangaxb(), "kangaxb" );

console.log( "   kangaxc()" );
assert.ok( kangaxc(), "kangaxc" );

console.log( "   kangaxd()" );
assert.ok( kangaxd(), "kangaxd" );

console.log( "   kangaxe()" );
assert.ok( kangaxe(), "kangaxe" );

console.log( "   kangaxf()" );
assert.ok( kangaxf(), "kangaxf" );

console.log( "   kangaxg()" );
assert.ok( kangaxg(), "kangaxg" );

console.log( "   kangaxh()" );
assert.ok( kangaxh(), "kangaxh" );

console.log( "   kangaxi()" );
assert.ok( kangaxi(), "kangaxi" );

console.log( "   kangaxj()" );
assert.ok( kangaxj(), "kangaxj" );

console.log( "   kangaxk()" );
assert.ok( kangaxk(), "kangaxk" );

console.log( "   kangaxl()" );
assert.ok( kangaxl(), "kangaxl" );

console.log( "   kangaxm()" );
assert.ok( kangaxm(), "kangaxm" );

console.log( "   kangaxn()" );
assert.ok( kangaxn(), "kangaxn" );

console.log( "   kangaxo()" );
assert.ok( kangaxo(), "kangaxo" );

console.log( "   kangaxp()" );
assert.ok( kangaxp(), "kangaxp" );

console.log( "   kangaxq()" );
assert.ok( kangaxq(), "kangaxq" );

console.log( "   kangaxr()" );
assert.ok( kangaxr(), "kangaxr" );

console.log( "   kangaxs()" );
assert.ok( kangaxs(), "kangaxs" );

console.log( "   kangaxt()" );
assert.ok( kangaxt(), "kangaxt" );

/* console.log( "   kangaxu()" );                                      */
/* assert.ok( kangaxu(), "kangaxu" );                                  */

console.log( "   kangaxv()" );
assert.ok( kangaxv(), "kangaxv" );

console.log( "   kangaxw()" );
assert.ok( kangaxw(), "kangaxw" );

console.log( "   kangaxx()" );
assert.ok( kangaxx(), "kangaxx" );

console.log( "   kangaxy()" );
assert.ok( kangaxy(), "kangaxy" );

console.log( "   kangaxz()" );
assert.ok( kangaxz(), "kangaxz" );

/* console.log( "   kangaxA()" );                                      */
/* assert.ok( kangaxA(), "kangaxA" );                                  */

console.log( "   kangaxB()" );
assert.ok( kangaxB(), "kangaxB" );

console.log( "   kangaxC()" );
assert.ok( kangaxC(), "kangaxC" );
