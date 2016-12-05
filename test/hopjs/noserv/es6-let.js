/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/noserv/es6-let.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 30 17:54:33 2015                          */
/*    Last commit :  2015-12-27 [bffbda2] (Manuel Serrano)             */
/*    Copyright   :  2015-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 let construct                             */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

var acc = ""

acc += "111 ";
var _ZZZ = (acc += "222 ",  123);
const ZZZ = (acc += "333", _ZZZ);

function simple( z, a ) {
   let xxx = 1;
   if( z > 0 ) {
      let f = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + ZZZ;
   }
}

function multiple( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); },
	  g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
}

function multipleReturn( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); },
	  g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
   // after the returns
   let ____ = 35;
}

function split( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); };
      let g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
}

function split2( z, a ) {
   let xxx = 1, tttt = 2;
   if( z > 0 ) {
      let f = function( v ) { return g( v ); };
      if( xxx > tttt ) tttt = g;
      let g = function( v ) { return v - a; };

      return f( z );
   } else {
      return xxx + tttt;
   }
}

function errForward() {
   let f = function( x ) { return h( x + 1 ); };
   f( 34 );
   let h = function( y ) { return y + 10; };
   
   return h( 45 );
}

function errForward2() {
   let f = function( x ) { return h( x + 1 ); };
   let h = function( y ) { return i( y ); };
   f( 34 );
   let i = function( y ) { return y + 10; };
   
   return h( 45 );
}

function arity() {
   var x = 0;
   
   let f = function( x, a, b ) { return x; }

   f( 0, x++, x++ );

   return x;
}

function forLabel() {
   label1: for( let i = 0; i <= 0; i++ ) {
      for( let j = 0; j <= 1; j++ ) {
	 if( j === 0 ) {
	    continue label1;
	 } else {
	    return false;
	 }
      }  
   }
   return true;
}

function optim( x ) {
   var y = x;

   let aaaa = ++x;
   
   return aaaa - y;
}

function optim2() {
   var x = 3;
   let y = 5;
   var x = 8;

   return x;
}
   
function optim3() {
   var x = 3;
   var y = z;
   var z = 8;

   return y;
}

function optim4() {
   var _a = { x: 1, y: 2 }, b = _a.x, c = _a.y;

   return b + c;
}

function optim5( f ) {
   if (typeof f !== 'string') {
      return i;
   }

   var i = 1;
   var len = 23;

   return i;
}

function letFun() { 
   let gee = function () { return my_var };
   let my_var = 'foo';
   
   return gee();
}

function letFun2() {
   let my_var = 'foo';

   return bar();

   function bar() {
      return my_var;
   }
}   

function letFor() {
   for( let i = 0; i < 5; i++ );
   return 5;
}

function letFor2( files ) {
   for( let i = 0; i < files.length; i++ ) {
      var file = files[ i ];

      return file;
   }
}

console.log( "basic" );
assert.strictEqual( acc, "111 222 333" );

console.log( "   simple()" );
assert.strictEqual( simple( 10, 20 ), -10 );
assert.strictEqual( simple( -10, 20 ), 1 + ZZZ );

console.log( "   multiple()" );
assert.strictEqual( multiple( 10, 20 ), -10 );
assert.strictEqual( multiple( -10, 20 ), 3 );

console.log( "   multipleReturn()" );
assert.strictEqual( multipleReturn( 10, 20 ), -10 );
assert.strictEqual( multipleReturn( -10, 20 ), 3 );

console.log( "   split()" );
assert.strictEqual( split( 10, 20 ), -10 );
assert.strictEqual( split( -10, 20 ), 3 );

console.log( "   split2()" );
assert.strictEqual( split2( 10, 20 ), -10 );
assert.strictEqual( split2( -10, 20 ), 3 );

console.log( "   throws()" );
assert.throws( errForward );
assert.throws( errForward2 );

console.log( "   for/label()" );
assert.ok( forLabel() );

console.log( "   optim()" );
assert.ok( optim( 10 ) == 1 );

console.log( "   optim2()" );
assert.ok( optim2() == 8 );

console.log( "   optim3()" );
assert.ok( optim3() == undefined );

console.log( "   optim4()" );
assert.ok( optim4() == 3 );

console.log( "   optim5()" );
assert.ok( optim5( "foo" ) == 1 );

console.log( "   arity()" );
assert.ok( arity() == 2 );

console.log( "   letFun()" );
assert.ok( letFun() == "foo" );

console.log( "   letFun2()" );
assert.ok( letFun2() == "foo" );

console.log( "   letFor()" );
assert.ok( letFor() == 5 );

console.log( "   letFor2()" );
assert.ok( letFor2( ["foo"] ) == "foo" );

/*---------------------------------------------------------------------*/
/*    compiler bugs                                                    */
/*---------------------------------------------------------------------*/
function compBug( x ) {
   if( x > 10 ) {
      var LEFT = 0;
   }

   var y = 20;

   return LEFT + y;
}

assert.ok( compBug( 15 ), 35 );

/*---------------------------------------------------------------------*/
/*    Kangax                                                           */
/*    -------------------------------------------------------------    */
/*    https://kangax.github.io/compat-table/es6/                       */
/*---------------------------------------------------------------------*/
function kangaxa() {
   const foo = 123;
   return (foo === 123);
}

function kangaxb() {
   const bar = 123;
   { const bar = 456; }
   return bar === 123;
}

function kangaxc() {
   const baz = 1;
   try {
      Function("const foo = 1; foo = 2;")();
   } catch(e) {
      return true;
   }
}

function kangaxd() {
   var passed = (function(){ try { qux; } catch(e) { return true; }}());
   function fn() { passed &= qux === 456; }
   const qux = 456;
   fn();
   return passed;
}

function kangaxe() {
   "use strict";
   const foo = 123;
   return (foo === 123);
}

function kangaxf() {
   'use strict';
   const bar = 123;
   { const bar = 456; }
   return bar === 123;
}

function kangaxg() {
   'use strict';
   const baz = 1;
   try {
      Function("'use strict'; const foo = 1; foo = 2;")();
   } catch(e) {
      return true;
   }
}

function kangaxh() {
   'use strict';
   var passed = (function(){ try { qux; } catch(e) { return true; }}());
   function fn() { passed &= qux === 456; }
   const qux = 456;
   fn();
   return passed;
}

function kangaxi() {
   let foo = 123;
   return (foo === 123);
}

function kangaxj() {
   let bar = 123;
   { let bar = 456; }
   return bar === 123;
}

function kangaxk() {
   let baz = 1;
   for(let baz = 0; false; false) {}
   return baz === 1;
}

function kangaxl() {
   var passed = (function(){ try {  qux; } catch(e) { return true; }}());
   function fn() { passed &= qux === 456; }
   let qux = 456;
   fn();
   return passed;
}

function kangaxm() {
   let scopes = [];
   for(let i = 0; i < 2; i++) {
      scopes.push(function(){ return i; });
   }
   let passed = (scopes[0]() === 0 && scopes[1]() === 1);

   scopes = [];
   for(let i in { a:1, b:1 }) {
      scopes.push(function(){ return i; });
   }
   passed &= (scopes[0]() === "a" && scopes[1]() === "b");
   return passed;
}

function kangaxn() {
   'use strict';
   let foo = 123;
   return (foo === 123);
}

function kangaxo() {
   'use strict';
   let bar = 123;
   { let bar = 456; }
   return bar === 123;
}

function kangaxp() {
   'use strict';
   let baz = 1;
   for(let baz = 0; false; false) {}
   return baz === 1;
}

function kangaxq() {
   'use strict';
   var passed = (function(){ try {  qux; } catch(e) { return true; }}());
   function fn() { passed &= qux === 456; }
   let qux = 456;
   fn();
   return passed;
}

function kangaxr() {
   'use strict';
   let scopes = [];
   for(let i = 0; i < 2; i++) {
      scopes.push(function(){ return i; });
   }
   let passed = (scopes[0]() === 0 && scopes[1]() === 1);

   scopes = [];
   for(let i in { a:1, b:1 }) {
      scopes.push(function(){ return i; });
   }
   passed &= (scopes[0]() === "a" && scopes[1]() === "b");
   return passed;
}

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true );

console.log( "   kangaxb()");
assert.equal( kangaxb(), true );

console.log( "   kangaxc()");
assert.equal( kangaxc(), true );

console.log( "   kangaxd()");
assert.equal( kangaxd(), true );

console.log( "   kangaxe()");
assert.equal( kangaxe(), true );

console.log( "   kangaxf()");
assert.equal( kangaxf(), true );

console.log( "   kangaxg()");
assert.equal( kangaxg(), true );

console.log( "   kangaxh()");
assert.equal( kangaxh(), true );

console.log( "   kangaxi()");
assert.equal( kangaxi(), true );

console.log( "   kangaxj()");
assert.equal( kangaxj(), true );

console.log( "   kangaxk()");
assert.equal( kangaxk(), true );

console.log( "   kangaxl()");
assert.equal( kangaxl(), true );

console.log( "   kangaxm()");
assert.equal( kangaxm(), true );

console.log( "   kangaxn()");
assert.equal( kangaxn(), true );

console.log( "   kangaxo()");
assert.equal( kangaxo(), true );

console.log( "   kangaxp()");
assert.equal( kangaxp(), true );

console.log( "   kangaxq()");
assert.equal( kangaxq(), true );

console.log( "   kangaxr()");
assert.equal( kangaxr(), true );

/*---------------------------------------------------------------------*/
/*    mdn                                                              */
/*---------------------------------------------------------------------*/
function varTest() {
   var res = 0;
   var x = 31;
   
  if (true) {
     var x = 71;  // same variable!
     res += x;
  }

   return res + x;
}

function letTest() {
   var res = 0;
   let x = 31;
   if (true) {
      let x = 71;  // different variable
      res += x;  // 71
   }
   return res + x;
}

function rebind( x ) {
   try {
      if (x) {
	 eval( "let foo; let foo; " );
	 return false;
      }
   } catch( _ ) {
      return true;
   }
}

function deadzone() {
   try {
      console.log(foo); // ReferenceError
      let foo = 2;
      return false;
   } catch( _ ) {
      return true;
   }
}
   
console.log( "mdn" );

console.log( "   scope()");
assert.equal( varTest(), 71 * 2 );
assert.equal( letTest(), 71 + 31 );

console.log( "   rebind()");
assert.ok( rebind( true ) );
assert.ok( deadzone() );

/*---------------------------------------------------------------------*/
/*    optimization                                                     */
/*---------------------------------------------------------------------*/
function narrow() {
   var x = {a: 1, b:2 };
   Math.abs( x.a );
   var x = {a: 3, b:4 };
   var t = x;
   return t.a;
}

console.log( "   narrow()" );
assert.ok( narrow() == 3, "narrow" );
