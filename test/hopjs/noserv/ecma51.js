/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/ecma51.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Sat Mar 17 10:48:38 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing basic ECMA 262, 5.1 features                             */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    for                                                              */
/*---------------------------------------------------------------------*/
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

Object.defineProperty( o, "foo_nonenum", { value: 50, enumerable: false } );

var count = 0;

var no = {};

for( var k in o ) {
   count++;
   no[ k ] = o[ k ];
}

assert.equal( count, 5 );
assert.deepEqual( no, { foo_a: o.foo_a,
			foo_b: o.foo_b,
			foo_nonenum: o.foo_nonenum,
			gee_a: o.gee_a,
			gee_b: o.gee_b } );
assert.equal( no.foo_nonenum, 50, "defineProperty" );

/*---------------------------------------------------------------------*/
/*    value of an assignment                                           */
/*---------------------------------------------------------------------*/
var val = (function() {
   var _ = function(a){ return 4;};

   var aaa = _.aaa = _.c = function( o, i, c ) {};
   
   return aaa;
}());

assert.strictEqual( val instanceof Function, true, "instanceof" );


/*---------------------------------------------------------------------*/
/*    function assignments                                             */
/*---------------------------------------------------------------------*/
function Ffoo1() { Ffoo1 = 5; }

Ffoo1();
assert.strictEqual( Ffoo1, 5, "mutable function.1" );

var Ffoo2 = function Ffoo2() { Ffoo2 = 4; }
var bck = Ffoo2;

Ffoo2();
assert.strictEqual( Ffoo2, bck, "mutable function.2" );

function Ffoo3() {
   Ffoo3 = 3;
   assert.strictEqual( Ffoo3, 3, "inner mutable function.1" );

}

Ffoo3();
assert.strictEqual( Ffoo3, 3 );

var Ffoo4 = function Ffoo4() {
   Ffoo4 = 3;
   assert.ok( Ffoo4 instanceof Function, "inner mutable function.2" );
}

Ffoo4();
assert.ok( Ffoo4 instanceof Function, "mutable function.3" );

var Ffoo5_aux = 0;
var Ffoo5 = function Ffoo5( n ) {
   if( n == 1 ) {
      Ffoo5( 0, (Ffoo5_aux = 6) );
   }

}
Ffoo5( 1 );
assert.strictEqual( Ffoo5_aux, 6, "mutable function.4" );

/*---------------------------------------------------------------------*/
/*    switches                                                         */
/*---------------------------------------------------------------------*/
function swfoo1( xxx ) {
   switch( xxx ) {
      case 1:
      case 2:
      case 3: return 1;
      default: return 2;
   }
}

function swfoo2( yyy ) {
   switch( yyy ) {
      case 1:
      case 2: return 3;
      case 3: return 1;
      default: return 2;
   }
}

assert.strictEqual( swfoo1( 2 ), 1, "swfoo1" );
assert.strictEqual( swfoo1( 4 ), 2, "swfoo1" );
assert.strictEqual( swfoo2( 1 ), 3, "swfoo2" );
assert.strictEqual( swfoo2( 2 ), 3, "swfoo2" );
assert.strictEqual( swfoo2( 3 ), 1, "swfoo2" );
assert.strictEqual( swfoo2( false ), 2, "swfoo2" );

/*---------------------------------------------------------------------*/
/*    variables and parameters                                         */
/*---------------------------------------------------------------------*/
function foo1( zzz ) {
   "use strict";
   zzz = 45;
   var zzz = 55;
   return zzz;
}

function foo2( uuu ) {
   "use strict";
   uuu = 45;
   var uuu = 55;
   return arguments[ 0 ];
}

function bar1( ttt ) {
   ttt = 45;
   var ttt = 55;
   return ttt;
}

function bar2( www ) {
   www = 45;
   var www = 55;
   return arguments[ 0 ];
}

function bar3( a, b, c ) {
   arguments.length = 1;
   return arguments.length == 1 && !Object.hasOwnProperty( arguments, 2 );
}

function bar4( buf ) {
   var buf = 3;
   return arguments[ 0 ];
}

assert.equal( foo1( 10 ), 55, "foo1" );
assert.equal( foo2( 10 ), 10, "foo2" );
assert.equal( bar1( 10 ), 55, "bar1" );
assert.equal( bar2( 10 ), 55, "bar2" );
assert.strictEqual( bar3( 1, 2, 3, 4, 5, 6 ), true, "arguments.length" );
assert.strictEqual( bar4( 10 ), 3, "arguments overriding" );

/*---------------------------------------------------------------------*/
/*    undefined                                                        */
/*---------------------------------------------------------------------*/
var undefined = 3;

assert.equal( typeof undefined, "number" );
assert.equal( typeof this.undefined, "undefined" );

assert.equal( (function() { var undefined = 3; return undefined; })(), 3 );

/*---------------------------------------------------------------------*/
/*    arity                                                            */
/*---------------------------------------------------------------------*/
function foo6(a,b,c,d,e,f) { return a; };
function foo7(a,b,c,d,e,f,g) { return a; };
function foo8(a,b,c,d,e,f,g,h) { return a; };
function foo9(a,b,c,d,e,f,g,h,i) { return a; };
function foo11(a,b,c,d,e,f,g,h,i,j,k) { return a; };

assert.equal( foo11( 1 ), 1 );
assert.equal( foo11( 1, 2, 3, 4, 5, 6 ), 1 );
assert.equal( foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ), 1 );
assert.equal( foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ), 1 );
assert.equal( foo11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ), 1 );

assert.equal( foo11.apply( this, [1]), 1 );
assert.equal( foo11.apply( this, [1, 2, 3, 4, 5, 6]), 1 );
assert.equal( foo11.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1 );
assert.equal( foo11.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1 );
assert.equal( foo11.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1 );

assert.equal( foo8.apply( this, [1]), 1 );
assert.equal( foo8.apply( this, [1, 2, 3, 4, 5, 6]), 1 );
assert.equal( foo8.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1 );
assert.equal( foo8.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1 );
assert.equal( foo8.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1 );

assert.equal( foo7( 1, 2, 3, 4, 5, 6), 1 );
assert.equal( foo7( 1, 2, 3, 4, 5, 6, 7), 1 );
assert.equal( foo7( 1, 2, 3, 4, 5, 6, 7, 8), 1 );
assert.equal( foo7( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1 );

assert.equal( foo8( 1, 2, 3, 4, 5, 6, 7), 1 );
assert.equal( foo8( 1, 2, 3, 4, 5, 6, 7, 8), 1 );
assert.equal( foo8( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1 );

assert.equal( foo9( 1, 2, 3, 4, 5, 6, 7), 1 );
assert.equal( foo9( 1, 2, 3, 4, 5, 6, 7, 8), 1 );
assert.equal( foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9), 1 );
assert.equal( foo9( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1 );

assert.equal( foo6.apply( this, [1]), 1 );
assert.equal( foo6.apply( this, [1, 2, 3, 4, 5, 6]), 1 );
assert.equal( foo6.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1 );
assert.equal( foo6.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1 );
assert.equal( foo6.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1 );

function bar11(a,b,c,d,e,f,g,h,i,j,k) { if( arguments.length >= 0 ) return a; };
function bar8(a,b,c,d,e,f,g,h) { if( arguments.length >= 0 ) return a; };
function bar6(a,b,c,d,e,f) { if( arguments.length >= 0 ) return a; };

assert.equal( bar11( 1 ), 1, "arguments.length" );
assert.equal( bar11( 1, 2, 3, 4, 5, 6 ), 1 );
assert.equal( bar11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ), 1 );
assert.equal( bar11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ), 1 );
assert.equal( bar11( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ), 1 );

assert.equal( bar11.apply( this, [1]), 1 );
assert.equal( bar11.apply( this, [1, 2, 3, 4, 5, 6]), 1 );
assert.equal( bar11.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1 );
assert.equal( bar11.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1 );
assert.equal( bar11.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1 );

assert.equal( bar8.apply( this, [1]), 1 );
assert.equal( bar8.apply( this, [1, 2, 3, 4, 5, 6]), 1 );
assert.equal( bar8.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1 );
assert.equal( bar8.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1 );
assert.equal( bar8.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1 );

assert.equal( bar6.apply( this, [1]), 1 );
assert.equal( bar6.apply( this, [1, 2, 3, 4, 5, 6]), 1 );
assert.equal( bar6.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), 1 );
assert.equal( bar6.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]), 1 );
assert.equal( bar6.apply( this, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]), 1 );

/*---------------------------------------------------------------------*/
/*    function call                                                    */
/*---------------------------------------------------------------------*/
var o = {name: "toto", f: function() { return this.name }}

assert.ok( o.f() === "toto" );
assert.ok( (o["f"])() === "toto" );
assert.ok( (o).f() === "toto" );
assert.ok( (1,o).f() === "toto" );
assert.ok( (2>1?o:undefined).f() === "toto" );
assert.ok( ((function () { return o })()).f() === "toto" );

/*---------------------------------------------------------------------*/
/*    typing                                                           */
/*---------------------------------------------------------------------*/
function typing( z, data )  {
   if( z > 3 ) {
      data = true;
   }

   if( data ) {
      return 1;
   } else {
      return 2;
   }
}

assert.ok( typing( 2 ) === 2, "typing" );
assert.ok( typing( 20 ) === 1, "typing" );

/*---------------------------------------------------------------------*/
/*    access                                                           */
/*---------------------------------------------------------------------*/
var t = [ 'a' ];
var i = '0';

assert.ok( t[ 0 ] === 'a', "int access" );
assert.ok( t[ '0' ] === 'a', "string access" );
assert.ok( t[ i ] === 'a', "string access" );

/*---------------------------------------------------------------------*/
/*    constuctor                                                       */
/*---------------------------------------------------------------------*/
function ctor() {
   this.a = 1; this.b = 2; this.c = 3; this.d = 4; this.e = 5; this.f = this.a;
}

var o = new ctor();

assert.ok( o.a === 1, "ctor" );
assert.ok( o.e === 5, "ctor" );
assert.ok( o.f === o.a, "ctor" );

/*---------------------------------------------------------------------*/
/*    assignop                                                         */
/*---------------------------------------------------------------------*/
var x = 0;
var a = [1,2,3,4,5];
a[ x++ ] += 3;;

assert.ok( x === 1, "increment in assignment" );

var y = "foo";
y++;

assert.ok( isNaN( y ) );

var y = "foo";
y += 1;

assert.equal( y, "foo1" );


/*---------------------------------------------------------------------*/
/*    literal with prototype                                           */
/*---------------------------------------------------------------------*/
function protoLit( CNT, m ) {
   function fun( i ) {
      return this.x + i;
   }

   const proto = { f: fun };
   let os = [ { x: 12345, y : 2, __proto__: proto } ];
   let o = os [ 0 ];
   
   return o.f( 0 );
}

assert.ok( protoLit( 20000, 1 ) === 12345, "literal with __proto__" );

/*---------------------------------------------------------------------*/
/*    Compilation failure                                              */
/*---------------------------------------------------------------------*/
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
/*    Function properties                                              */
/*---------------------------------------------------------------------*/
var p1 = Object.getOwnPropertyDescriptor( ctor, "length" );
var p2 = Object.getOwnPropertyDescriptor( ctor, "name" );

assert.ok( !p2.writable && !p2.enumerable && !p2.configurable );

/*---------------------------------------------------------------------*/
/*    binding                                                          */
/*---------------------------------------------------------------------*/
var Reference = exports.Reference = function Reference() {
   return Reference;
}

assert.ok( Reference );
