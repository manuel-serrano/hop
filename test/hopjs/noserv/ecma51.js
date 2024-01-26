/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/ecma51.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Fri Jan 26 11:35:27 2024 (serrano)                */
/*    Copyright   :  2014-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing basic ECMA 262, 5.1 features                             */
/*=====================================================================*/
var assert = require("assert");

/*---------------------------------------------------------------------*/
/*    for                                                              */
/*---------------------------------------------------------------------*/
console.log("for...");

function ForGee() {
   this.gee_a = 10;
   this.gee_b = 20;
   this.foo_a = 30;
   this.foo_nonenum = 40;
}

function ForFoo() {
   this.foo_a = 1;
   this.foo_b = 2;
}

ForFoo.prototype = new ForGee();

var o = new ForFoo();

Object.defineProperty(o, "foo_nonenum", { value: 50, enumerable: false });

var count = 0;

var no = {};

for (var k in o) {
   count++;
   no[ k ] = o[ k ];
}

assert.equal(count, 4);
assert.deepEqual(no, { foo_a: o.foo_a, 
			foo_b: o.foo_b,
			gee_a: o.gee_a,
			gee_b: o.gee_b });

function forlbl() {
   // test262/test/suite/ch07/7.9/S7.9_A1.js
   label1: for (var i = 0; i <= 0; i++) {
      for (var j = 0; j <= 1; j++) {
	 if (j === 0) {
	    continue label1;
	 } else {
	    throw('#1: Check continue statement for automatic semicolon insertion');
	 }
      }
    }
	   
    return i === 1 && j === 0;
}

assert.ok("forlbl", "forlbl");

/*---------------------------------------------------------------------*/
/*    value of an assignment                                           */
/*---------------------------------------------------------------------*/
console.log("assignment value...");

var val = (function() {
   var _ = function(a){ return 4;};

   var aaa = _.aaa = _.c = function(o, i, c) {};
   
   return aaa;
}());

assert.strictEqual(val instanceof Function, true, "instanceof");


/*---------------------------------------------------------------------*/
/*    function assignments                                             */
/*---------------------------------------------------------------------*/
console.log("function assignments...");

function Ffoo1() { Ffoo1 = 5; }

Ffoo1();
assert.strictEqual(Ffoo1, 5, "mutable function.1");

var Ffoo2 = function Ffoo2() { Ffoo2 = 4; }
var bck = Ffoo2;

Ffoo2();
assert.strictEqual(Ffoo2, bck, "mutable function.2");

function Ffoo3() {
   Ffoo3 = 3;
   assert.strictEqual(Ffoo3, 3, "inner mutable function.1");

}

Ffoo3();
assert.strictEqual(Ffoo3, 3);

var Ffoo4 = function Ffoo4() {
   Ffoo4 = 3;
   assert.ok(Ffoo4 instanceof Function, "inner mutable function.2");
}

Ffoo4();
assert.ok(Ffoo4 instanceof Function, "mutable function.3");

var Ffoo5_aux = 0;
var Ffoo5 = function Ffoo5(n) {
   if (n == 1) {
      Ffoo5(0, (Ffoo5_aux = 6));
   }

}
Ffoo5(1);
assert.strictEqual(Ffoo5_aux, 6, "mutable function.4");

/*---------------------------------------------------------------------*/
/*    switches                                                         */
/*---------------------------------------------------------------------*/
console.log("switches...");

function swfoo1(xxx) {
   switch(xxx) {
      case 1:
      case 2:
      case 3: return 1;
      default: return 2;
   }
}

function swfoo2(yyy) {
   switch(yyy) {
      case 1:
      case 2: return 3;
      case 3: return 1;
      default: return 2;
   }
}

assert.strictEqual(swfoo1(2), 1, "swfoo1");
assert.strictEqual(swfoo1(4), 2, "swfoo1");
assert.strictEqual(swfoo2(1), 3, "swfoo2");
assert.strictEqual(swfoo2(2), 3, "swfoo2");
assert.strictEqual(swfoo2(3), 1, "swfoo2");
assert.strictEqual(swfoo2(false), 2, "swfoo2");

/*---------------------------------------------------------------------*/
/*    variables and parameters                                         */
/*---------------------------------------------------------------------*/
console.log("variables and parameters...");

function foo1(zzz) {
   "use strict";
   zzz = 45;
   var zzz = 55;
   return zzz;
}

function foo2(uuu) {
   "use strict";
   uuu = 45;
   var uuu = 55;
   return arguments[ 0 ];
}

function bar1(ttt) {
   ttt = 45;
   var ttt = 55;
   return ttt;
}

function bar2(www) {
   www = 45;
   var www = 55;
   return arguments[ 0 ];
}

function bar3(a, b, c) {
   arguments.length = 1;
   return arguments.length == 1 && !Object.hasOwnProperty(arguments, 2);
}

function bar4(buf) {
   var buf = 3;
   return arguments[ 0 ];
}

function bar5() {
   var LENGTH = "length";
   var argObj = (function () { return arguments })();
   argObj[LENGTH] = 1001;
   
   return argObj.length === 1001;
}

function bar6() {
   function setProto2() {
      Object.defineProperty(arguments.__proto__, "2",
      	 { get: function(el) { this[ 1 ] = 23; return 0;} });
   }
      
   function bar(a, b, c) {
      return b;
   }
   
   setProto2();
   
   return arguments[ 2 ] + bar.apply(this, arguments);
}

var xxxx = 4;
if (xxxx > 10) {
   var yyyy = 3;
}

function bar7() {
   function setProto100() {
      Object.defineProperty(arguments.__proto__, "100",
			    { get: function(el) { return 4;} });
   }

   const n = arguments[1];
   const p = arguments[100];
   setProto100();
   const q = arguments[100];
   return p === undefined && q + n === 5;
}

function createUTCDate(y) {
   var date, args;
   if (y < 100 && y >= 0) {
      args = Array.prototype.slice.call(arguments);
      date = new Date(Date.UTC.apply(null, args));
   } else {
      date = new Date(Date.UTC.apply(null, arguments));
   }
   
   return date;
}

assert.equal(foo1(10), 55, "foo1");
assert.equal(foo2(10), 10, "foo2");
assert.equal(bar1(10), 55, "bar1");
assert.equal(bar2(10), 55, "bar2");
assert.strictEqual(bar3(1, 2, 3, 4, 5, 6), true, "arguments.length");
assert.strictEqual(bar4(10), 3, "arguments overriding");
assert.strictEqual(bar5(), true, "arguments length");
assert.strictEqual(bar6(1, 2), 23, "arguments.__proto__");
assert.ok(bar7(1, 2), "arguments.__proto__ (2)");
assert.equal(yyyy, undefined, "global lifting");
assert.ok(createUTCDate(98) instanceof  Date);
assert.ok(createUTCDate(198) instanceof  Date);
assert.ok(({f: createUTCDate}.f(30)) instanceof  Date);
assert.ok(({f: createUTCDate}.f(2000)) instanceof  Date);

/*---------------------------------------------------------------------*/
/*    undefined                                                        */
/*---------------------------------------------------------------------*/
console.log("undefined...");
var _undefined = undefined;
var undefined = 3;

assert.equal(typeof undefined, "number");
assert.equal(typeof this.undefined, "undefined");

assert.equal((function() { var undefined = 3; return undefined; })(), 3);

function typeofs() {
   const p = [ new Array(1), new RegExp(""), null, { a: 1 }, Object(true) ];
   const n = [ function(x) { return 1 }, x => 1, undefined, "foo", 1, true ];
   
   for (let i = 0; i < p.length; i++) {
      console.log("p=", i, typeof(p[ i ]));
      if (typeof(p[ i ]) !== "object") return false;
   } 
   for (let i = 0; i < n.length; i++) {
      console.log("n=", i, typeof(n[ i ]), n[ i ]);
      if (typeof(n[ i ]) === "object") return false;
   } 
   return true;
}
   
assert.ok(typeofs(), "typeofs"); 

undefined = _undefined;

/*---------------------------------------------------------------------*/
/*    arity                                                            */
/*---------------------------------------------------------------------*/
console.log("arity...");

function foo6(a,b,c,d,e,f) { return a; };
function foo7(a,b,c,d,e,f,g) { return a; };
function foo8(a,b,c,d,e,f,g,h) { return a; };
function foo9(a,b,c,d,e,f,g,h,i) { return a; };
function foo11(a,b,c,d,e,f,g,h,i,j,k) { return a; };

console.log("foo11");
assert.equal(foo11(1), 1);
assert.equal(foo11(1, 2, 3, 4, 5, 6), 1);
assert.equal(foo11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1);
assert.equal(foo11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 1);
assert.equal(foo11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1);

console.log("foo11.apply");
assert.equal(foo11.apply(this, [1]), 1);
assert.equal(foo11.apply(this, [1, 2, 3, 4, 5, 6]), 1);
assert.equal(foo11.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1);
assert.equal(foo11.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1);
assert.equal(foo11.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1);

console.log("foo8.apply");
assert.equal(foo8.apply(this, [1]), 1);
assert.equal(foo8.apply(this, [1, 2, 3, 4, 5, 6]), 1);
assert.equal(foo8.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1);
assert.equal(foo8.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1);
assert.equal(foo8.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1);

console.log("foo7");
assert.equal(foo7(1, 2, 3, 4, 5, 6), 1);
assert.equal(foo7(1, 2, 3, 4, 5, 6, 7), 1);
assert.equal(foo7(1, 2, 3, 4, 5, 6, 7, 8), 1);
assert.equal(foo7(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1);

console.log("foo8");
assert.equal(foo8(1, 2, 3, 4, 5, 6, 7), 1);
assert.equal(foo8(1, 2, 3, 4, 5, 6, 7, 8), 1);
assert.equal(foo8(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1);

console.log("foo9");
assert.equal(foo9(1, 2, 3, 4, 5, 6, 7), 1);
assert.equal(foo9(1, 2, 3, 4, 5, 6, 7, 8), 1);
assert.equal(foo9(1, 2, 3, 4, 5, 6, 7, 8, 9), 1);
assert.equal(foo9(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1);

console.log("foo6.apply");
assert.equal(foo6.apply(this, [1]), 1);
assert.equal(foo6.apply(this, [1, 2, 3, 4, 5, 6]), 1);
assert.equal(foo6.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1);
assert.equal(foo6.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1);
assert.equal(foo6.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1);

function bar11(a,b,c,d,e,f,g,h,i,j,k) { if (arguments.length >= 0) return a; };
function bar8(a,b,c,d,e,f,g,h) { if (arguments.length >= 0) return a; };
function bar7(a,b,c,d,e,f) { if (arguments.length >= 0) return a; };

console.log("bar11");
assert.equal(bar11(1), 1, "arguments.length");
assert.equal(bar11(1, 2, 3, 4, 5, 6), 1);
assert.equal(bar11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1);
assert.equal(bar11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 1);
assert.equal(bar11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), 1);

console.log("bar11.apply");
assert.equal(bar11.apply(this, [1]), 1);
assert.equal(bar11.apply(this, [1, 2, 3, 4, 5, 6]), 1);
assert.equal(bar11.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1);
assert.equal(bar11.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1);
assert.equal(bar11.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1);

console.log("bar8.apply");
assert.equal(bar8.apply(this, [1]), 1);
assert.equal(bar8.apply(this, [1, 2, 3, 4, 5, 6]), 1);
assert.equal(bar8.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1);
assert.equal(bar8.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1);
assert.equal(bar8.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1);

console.log("bar7.apply");
assert.equal(bar7.apply(this, [1]), 1);
assert.equal(bar7.apply(this, [1, 2, 3, 4, 5, 6]), 1);
assert.equal(bar7.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1);
assert.equal(bar7.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1);
assert.equal(bar7.apply(this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1);

/*---------------------------------------------------------------------*/
/*    function call                                                    */
/*---------------------------------------------------------------------*/
console.log("call...");

var o = {name: "toto", f: function() { return this.name }}

assert.ok(o.f() === "toto");
assert.ok((o["f"])() === "toto");
assert.ok((o).f() === "toto");
assert.ok((1,o).f() === "toto");
assert.ok((2>1?o:undefined).f() === "toto");
assert.ok(((function () { return o })()).f() === "toto");

/*---------------------------------------------------------------------*/
/*    typing                                                           */
/*---------------------------------------------------------------------*/
console.log("typing...");

function typing(z, data)  {
   if (z > 3) {
      data = true;
   }

   if (data) {
      return 1;
   } else {
      return 2;
   }
}

assert.ok(typing(2) === 2, "typing");
assert.ok(typing(20) === 1, "typing");

/*---------------------------------------------------------------------*/
/*    access                                                           */
/*---------------------------------------------------------------------*/
console.log("access...");

var t = [ 'a' ];
var i = '0';

assert.ok(t[ 0 ] === 'a', "int access");
assert.ok(t[ '0' ] === 'a', "string access");
assert.ok(t[ i ] === 'a', "string access");

/*---------------------------------------------------------------------*/
/*    constuctor                                                       */
/*---------------------------------------------------------------------*/
console.log("ctor...");

function ctor() {
   this.a = 1; this.b = 2; this.c = 3; this.d = 4; this.e = 5; this.f = this.a;
}

var o = new ctor();

assert.ok(o.a === 1, "ctor");
assert.ok(o.e === 5, "ctor");
assert.ok(o.f === o.a, "ctor");

/*---------------------------------------------------------------------*/
/*    constructor ...                                                  */
/*---------------------------------------------------------------------*/
console.log("CTOR...");

function CTOR(x) {
   this.x = x;
   this.y = x;
   this.z = x;
}

const oo = new CTOR(1);
const ctotor = oo.constructor;

function ctotorcall(n) {
   try {
      ctotor.call(3, n);
      return false;
   } catch(e) {
      return e instanceof TypeError;
   }
}

assert.ok(!ctotorcall(2), "ctotor sans object");

/*---------------------------------------------------------------------*/
/*    Function properties                                              */
/*---------------------------------------------------------------------*/
var p1 = Object.getOwnPropertyDescriptor(ctotor, "length");
var p2 = Object.getOwnPropertyDescriptor(ctotor, "name");

assert.ok(!p2.writable && !p2.enumerable && p2.configurable);

/*---------------------------------------------------------------------*/
/*    assignop                                                         */
/*---------------------------------------------------------------------*/
console.log("assignop...");

var x = 0;
var a = [1,2,3,4,5];
a[ x++ ] += 3;;

assert.ok(x === 1, "increment in assignment");

var y = "foo";
y++;

assert.ok(isNaN(y));

var y = "foo";
y += 1;

assert.equal(y, "foo1");

function postfixTypes() {
   var x = "3";
   var y = x++;
   return x === 4 && y === 3 
      && (typeof x === "number") 
      && (typeof y === "number");
}
   
assert.ok(postfixTypes(), "postfix types");

/*---------------------------------------------------------------------*/
/*    literal with prototype                                           */
/*---------------------------------------------------------------------*/
console.log("literal...");

function protoLit(CNT, m) {
   function fun(i) {
      return this.x + i;
   }

   const proto = { f: fun };
   let os = [ { x: 12345, y : 2, __proto__: proto } ];
   let o = os [ 0 ];
   
   return o.f(0);
}

assert.ok(protoLit(20000, 1) === 12345, "literal with __proto__");

/*---------------------------------------------------------------------*/
/*    Compilation failure                                              */
/*---------------------------------------------------------------------*/
console.log("compilation failure...");

(function() {
   function bar() {
      return "ok";
   }
   
   function foo() {
      return bar();
   }
   
   foo();
   var myVar = "foo";
})();

/*---------------------------------------------------------------------*/
/*    binding                                                          */
/*---------------------------------------------------------------------*/
console.log("binding...");

var Reference = exports.Reference = function Reference() {
   return Reference;
}

assert.ok(Reference);

/*---------------------------------------------------------------------*/
/*    method call                                                      */
/*---------------------------------------------------------------------*/
console.log("method call...");

function illmet() {
   let o = new Object();
   let b = o.bar;
   try {
      let c = b.toString();
      return false;
   } catch(e) {
      return true;
   }
}
assert.ok(illmet(), "illegal method");

/*---------------------------------------------------------------------*/
/*    apply                                                            */
/*---------------------------------------------------------------------*/
console.log("apply...");

function Foo(a, b, c) {
   return a + b + c;
}

Foo.apply = function(self, args) {
   return 24;
}

assert.ok(Foo.apply(null, [ 1, 2, 3, 4 ]) === 24, "apply");

/*---------------------------------------------------------------------*/
/*    seal and freeze                                                  */
/*---------------------------------------------------------------------*/
console.log("seal and freeze...");

function testSeal() {
   var o1 = { a: 10 };
   
   Object.seal(o1);
   o1.a = 100;
   
   var o2 = { __proto__: o1 };
   const o2a = o2.a;
   
   o2.a = 50;

   return o2a + o2.a === 150;
}

function testSealReadOnly() {
   var o1 = {};
   
   Object.defineProperty(o1, "a", { value: 100, writable: false });
   
   Object.seal(o1);
   
   var o2 = { __proto__: o1 };
   const o2a = o2.a;
   
   o2.a = 50;

   return o2a + o2.a === 200;
}

function testReadOnly() {
   var o1 = {};
   
   Object.defineProperty(o1, "a", { value: 100, writable: false });
   
   var o2 = { __proto__: o1 };
   const o2a = o2.a;
   
   o2.a = 50;

   return o2a + o2.a === 200;
}

function testSealReadOnlyStrict() {
   "use strict";
   var o1 = {};
   
   Object.defineProperty(o1, "a", { value: 100, writable: false });
   
   Object.seal(o1);
   
   var o2 = { __proto__: o1 };
   const o2a = o2.a;
   
   o2.a = 50;

   return o2a + o2.a === 200;
}

function testFreeze() {
   var o1 = { a: 10 };
   
   Object.freeze(o1);
   o1.a = 100;
   
   var o2 = { __proto__: o1 };
   const o2a = o2.a;
   
   o2.a = 50;
   
   return o2a + o2.a === 20;
}

assert.ok(testSeal(), "seal");
assert.ok(testReadOnly(), "readOnly");
assert.ok(testSealReadOnly(), "sealReadOnly");
assert.throws(testSealReadOnlyStrict, "sealReadOnlyStrict");
assert.ok(testFreeze(), "freeze");

/*---------------------------------------------------------------------*/
/*    for-in-of                                                        */
/*---------------------------------------------------------------------*/
console.log("for-in-of...");

function forinarr(a) {
   let r = 0;

   for (let k in a) {
      r += a[ k ];
   }
   
   return r;
}

function forofarr(a) {
   let r = 0;

   for (let v of a) {
      r += v;
   }
   
   return r;
}

function testforinof() { 
   let a = [2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2];
   
   return forinarr(a) === forofarr(a);
}
	  
assert.ok(testforinof(), "for in/of");

/*---------------------------------------------------------------------*/
/*    forin                                                            */
/*---------------------------------------------------------------------*/
console.log("forin...");

function testforin() {
   var a = [1, 2];
   var p = { "1": 20, __proto__: Array };
   var cnt = 0;

   a.__proto__ = p;
   
   for (let k in a) { 
      cnt++;
   }
   
   return cnt === 2;
}

assert.ok(testforin(), "for in");

/*---------------------------------------------------------------------*/
/*    Global this                                                      */
/*---------------------------------------------------------------------*/
console.log("global this...");

assert.ok(function() {
   return function(_global_) {
      return typeof(_global_) === "object";
   }(this);
}(), "non-strict global object");

assert.ok(function() {
   "use strict"
   return function(_global_) {
      return typeof(_global_) === "undefined";
   }(this);
}(), "strict global object");

/*---------------------------------------------------------------------*/
/*    exceptions                                                       */
/*---------------------------------------------------------------------*/
console.log("exceptions...");

function fin1() {
   let tmp = 0;
   try {
      tmp = 6666;
      throw 8888;
   } finally {
      return 33 + tmp;
   }
}

function fin2() {
   let tmp = 0;
   try {
      tmp = 6666;
      throw new Error(8888);
   } finally {
      tmp++;
   }
}

function fin3() {
   let tmp = 0;
   try {
      tmp = 6666;
      throw 8888;
   } catch(e) {
      return 8;
   } finally {
      return 9;
   }
}

assert.ok(fin1() === 6699, "fin.1");
assert.throws(function() { fin2() }, Error, "fin.2");
assert.ok(fin3() === 9, "fin.3");
