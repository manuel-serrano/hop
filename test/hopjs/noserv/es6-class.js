/*=====================================================================*/
/*    serrano/trashcan/es6-class.js                                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep  2 01:49:55 2017                          */
/*    Last change :  Sat Sep 18 06:50:39 2021 (serrano)                */
/*    Copyright   :  2017-21 Manuel Serrano                            */
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
	 return foo;
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
      gee() { "gee"; }
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

function basich() {
   class kla {
      constructor() {
	 return false;
      }
   }
   
   try {
      return kla();
   } catch( e ) {
      return new kla() instanceof kla;
   }
}

function basici() {
   class kla {
   }
   
   try {
      return kla();
   } catch( e ) {
      return new kla() instanceof kla;
   }
}

function basicj() {
   class Animal {
      constructor(name) {
    	 this.name = name;
      }

      get() {
    	 return this.name;
      }

      set() {
    	 this.name = "john";
      }
   }
   
   let animal = new Animal("Jean");
   
   if( animal.name === "Jean" && animal.get() === "Jean" ) {
      animal.set();
      return animal.name === "john";
   } else {
      return false;
   }
}

function basick() {
   class basick {
      constructor() {
      	 this._x = 0; this._y = 0;
      }
      
      getX() {
      	 return this._x;
      }

      get x() {
      	 return this._x + 100;
      }
   }

   class basicl extends basick {
      constructor() {
      	 super();
      	 this._z = 0;
      }
      
      getX() {
      	 return super.getX() + 666;
      }

      get x() {
      	 return super.getX() + 555;
      }
   }

   const p = new basicl();
   return p.x === 555;
}

function basicl() {
   class Point {
      x = 44;
      y;
   }

   class Point3d extends Point {
      z = 55;
   }
   
   const o = new Point3d();
   return o.hasOwnProperty("x") 
      && o.hasOwnProperty("y") 
      && o.hasOwnProperty("z");
}

function basicm() {
   class C {
      f = 55;

      constructor(n) {
      }
   }
   
   const o = new C(20);
   o.f = 66;
   const o2= new C(11);
   
   return o.f === 66
      && Object.getOwnPropertyDescriptor(o, "f")
      && !o.__proto__.f
      && !Object.getOwnPropertyDescriptor(o.__proto__, "f")
      && o2.f === 55
      && Object.getOwnPropertyDescriptor(o2, "f")
      && !o2.__proto__.f
      && !Object.getOwnPropertyDescriptor( o2.__proto__, "f" );
}

function basicn() {
   class C {
      f() { return  55 };

      constructor(n) {
      }
   }
   
   const o = new C(20);
   o.f = 66;
   const o2= new C(11);
   
   return o.f === 66
      && Object.getOwnPropertyDescriptor(o, "f")
      && o.__proto__.f
      && Object.getOwnPropertyDescriptor(o.__proto__, "f")
      && !Object.getOwnPropertyDescriptor(o2, "f")
      && o2.__proto__.f
      && Object.getOwnPropertyDescriptor( o2.__proto__, "f" );
}

function basico() {
   class Point {
      constructor(n) {
      	 this.y = n;
      }
      x = 44;
      y;
      typeof() { return "Point" };
   }
   const o = new Point(10);
   return o.x === 44 && o.y === 10;
}

function basicp() {
   class Point {
      x = 44;
      y;
      typeof() { return "Point" };
   }
   const o = new Point(10);
   return o.x === 44 && o.y === undefined;
}

function basicq() {
   class Point {
      x = 44;
      y;
      typeof() { return "Point" };
   }
   class Point3d extends Point {
   }
   const o = new Point3d(10);
   return o.x === 44 && o.y === undefined;
}

function basicr() {
   class Point {
      x = 44;
      y;
      typeof() { return "Point" };
   }
   class Point3d extends Point {
      z = 55;
   }
   const o = new Point3d(10);
   return o.x === 44 && o.y === undefined && o.z === 55;
}

function basics() {
   class Point {
      constructor(n) {
	 this.y = n;
      }
      x = 44;
      y;
      typeof() { return "Point" };
   }
   class Point3d extends Point {
      constructor(n) {
	 super(n);
      }
      z = 55;
   }
   const o = new Point3d(10);
   return o.x === 44 && o.y === 10 && o.z === 55;
}

function basict() {
   class Point {
      constructor(n) {
	 this.y = new.target;
      }
      x = 44;
      y;
      typeof() { return "Point" };
   }
   class Point3d extends Point {
      constructor(n) {
	 super(n);
      }
      z = 55;
   }
   const o = new Point3d(10);
   return o.x === 44 && o.y !== undefined && o.z === 55;
}

function basicu() {
   class Point {
      constructor(x,y) {
      	 this.x = x;
      	 this.y = y;
      }
   }
   
   class Point3d extends Point {
      z = 66;
   }

   return new Point3d(10).x === 10;
}

function basicv() {
   class Point {
      constructor(n) {
      	 this.y = new.target;
      }
      x = 44;
      y;
      typeof() { return "Point" };
   }
   const o = new Point(10);
   return o.x === 44 && o.y !== undefined;
}

function basicw() {
   class A {
      constructor(a) { 
	 this.a = a;
      }
   }
   class B extends A {
      b = 23;
   }
   class C extends B {
   }
   return (new A(1).a + new B(4).a + new C(20).a + new C(10).b)=== 48;
}

function basicx() {
   class AAAA {
      constructor(a) { 
      	 this.a = a;
      }
   }

   AAAA = 4;
   try {
      const o = new AAAA();
      return false;
   } catch(e) {
      return AAAA === 4;
   }
}

console.log( "basic" );
console.log( "   basica()" ); assert.ok( basica(), "basica" );
console.log( "   basicb()" ); assert.ok( basicb(), "basicb" );
console.log( "   basicc()" ); assert.ok( basicc(), "basicc" );
console.log( "   basicd()" ); assert.ok( basicd(), "basicd" );
console.log( "   basice()" ); assert.ok( basice(), "basice" );
console.log( "   basicf()" ); assert.ok( basicf(), "basicf" );
console.log( "   basicg()" ); assert.ok( basicg(), "basicg" );
console.log( "   basich()" ); assert.ok( basich(), "basich" );
console.log( "   basici()" ); assert.ok( basici(), "basici" );
console.log( "   basicj()" ); assert.ok( basicj(), "basicj" );
console.log( "   basick()" ); assert.ok( basick(), "basick" );
console.log( "   basicl()" ); assert.ok( basicl(), "basicl" );
console.log( "   basicm()" ); assert.ok( basicm(), "basicm" );
console.log( "   basicn()" ); assert.ok( basicn(), "basicn" );
console.log( "   basico()" ); assert.ok( basico(), "basico" );
console.log( "   basicp()" ); assert.ok( basicp(), "basicp" );
console.log( "   basicq()" ); assert.ok( basicq(), "basicq" );
console.log( "   basicr()" ); assert.ok( basicr(), "basicr" );
console.log( "   basics()" ); assert.ok( basics(), "basics" );
console.log( "   basict()" ); assert.ok( basict(), "basict" );
console.log( "   basicu()" ); assert.ok( basicu(), "basicu" );
console.log( "   basicv()" ); assert.ok( basicv(), "basicv" );
console.log( "   basicw()" ); assert.ok( basicw(), "basicw" );
console.log( "   basicx()" ); assert.ok( basicx(), "basicx" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   let passed = false;
   
   function FArray( a ) {
      passed = (a === 20);
   }

   class C extends FArray {}
   
   var c = new C( 20 );
   
   return passed;
}

function misca2() {
   let passed = false;
   
   function FArray( a ) {
      passed = (a === 20) && new.target !== undefined;
   }

   class C extends FArray {}
   
   var c = new C( 20 );
   
   return passed;
}

function misca3() {
   let passed = false;
   
   function FArray( a ) {
      passed = (a === 20) && new.target !== undefined;
   }

   class C extends { f: FArray }.f {}
   
   var c = new C( 20 );
   
   return passed;
}

function misca4() {
   let passed = false;
   
   class Carray {
      constructor(a) {
      	 passed = (a === 20) && new.target !== undefined;
      }
   }

   class C extends { c: Carray }.c {}
   
   var c = new C( 20 );
   
   return passed;
}

function miscb() {
   let passed = false;
   
   function FArray( a, b ) {
      passed = ((a === 20) && (b === undefined));
   }

   class C extends FArray {}
   
   var c = new C( 20 );
   
   return passed;
}

function miscb2() {
   let passed = false;
   
   function FArray( a, b ) {
      passed = ((a === 20) && (b === undefined) && new.target !== undefined);
   }

   class C extends FArray {}
   
   var c = new C( 20 );
   
   return passed;
}

function miscb3() {
   let passed = false;
   
   function FArray( a, b ) {
      passed = ((a === 20) && (b === undefined) && new.target !== undefined);
   }

   class C extends { f: FArray }.f {}
   
   var c = new C( 20 );
   
   return passed;
}

function miscb4() {
   let passed = false;
   
   class CArray {
      constructur( a, b ) {
      	 passed = ((a === 20) && (b === undefined) && new.target !== undefined);
      }
   }

   class C extends { c: CArray }.c {}
   
   var c = new C( 20 );
   
   return passed;
}

function miscc() {
   let passed = false;
   
   function FArray( a, b, c ) {
      passed = ((a === 20) && (b === 30) && (c == 40));
   }

   class C extends FArray {}
   
   var c = new C( 20, 30, 40, 50 );
   
   return passed;
}

function miscc2() {
   let passed = false;
   
   function FArray( a, b, c ) {
      passed = ((a === 20) && (b === 30) && (c == 40) && new.target !== undefined);
   }

   class C extends FArray {}
   
   var c = new C( 20, 30, 40, 50 );
   
   return passed;
}

function miscc3() {
   let passed = false;
   
   function FArray( a, b, c ) {
      passed = ((a === 20) && (b === 30) && (c == 40) && new.target !== undefined);
   }

   class C extends { f: FArray }.f {}
   
   var c = new C( 20, 30, 40, 50 );
   
   return passed;
}

function miscc4() {
   let passed = false;
   
   class CArray {
      constructor( a, b, c ) {
      	 passed = ((a === 20) && (b === 30) && (c == 40) && new.target !== undefined);
      }
   }

   class C extends { c: CArray }.c {}
   
   var c = new C( 20, 30, 40, 50 );
   
   return passed;
}

console.log( "misc" );
console.log( "   misca()" ); assert.ok( misca(), "misca" );
console.log( "   misca2()" ); assert.ok( misca2(), "misca2" );
console.log( "   misca3()" ); assert.ok( misca3(), "misca3" );
console.log( "   miscb()" ); assert.ok( miscb(), "miscb" );
console.log( "   miscb2()" ); assert.ok( miscb2(), "miscb2" );
console.log( "   miscb3()" ); assert.ok( miscb3(), "miscb3" );
console.log( "   miscc()" ); assert.ok( miscc(), "miscc" );
console.log( "   miscc2()" ); assert.ok( miscc2(), "miscc2" );
console.log( "   miscc3()" ); assert.ok( miscc3(), "miscc3" );

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

function kangaxu() {
   var passed = false;
   new function f() {
      passed = new.target === f;
   }();

   class A {
      constructor() {
	 passed &= new.target === B;
      }
   }
   class B extends A {}
   new B();
   return passed;
}

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

function kangaxA() {
   var passed;
   class B {
      constructor() { passed = (new.target === C); }
   }
   class C extends B {
      constructor() { super(); }
   }
   new C();
   return passed;
}

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

console.log( "   kangaxu()" );
assert.ok( kangaxu(), "kangaxu" );

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

console.log( "   kangaxA()" );
assert.ok( kangaxA(), "kangaxA" );

console.log( "   kangaxB()" );
assert.ok( kangaxB(), "kangaxB" );

console.log( "   kangaxC()" );
assert.ok( kangaxC(), "kangaxC" );
