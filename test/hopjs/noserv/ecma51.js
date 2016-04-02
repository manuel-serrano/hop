/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/ecma51.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Tue Mar 22 17:30:42 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
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
assert.equal( no.foo_nonenum, 50 );

/*---------------------------------------------------------------------*/
/*    value of an assignment                                           */
/*---------------------------------------------------------------------*/
var val = (function() {
   var _ = function(a){ return 4;};

   var aaa = _.aaa = _.c = function( o, i, c ) {};
   
   return aaa;
}());

assert.strictEqual( val instanceof Function, true );


/*---------------------------------------------------------------------*/
/*    function assignments                                             */
/*---------------------------------------------------------------------*/
function Ffoo1() { Ffoo1 = 5; }

Ffoo1();
assert.strictEqual( Ffoo1, 5 );

var Ffoo2 = function Ffoo2() { Ffoo2 = 4; }
var bck = Ffoo2;

Ffoo2();
assert.strictEqual( Ffoo2, bck );

function Ffoo3() {
   Ffoo3 = 3;
   assert.strictEqual( Ffoo3, 3 );

}

Ffoo3();
assert.strictEqual( Ffoo3, 3 );

var Ffoo4 = function Ffoo4() {
   Ffoo4 = 3;
   assert.ok( Ffoo4 instanceof Function );
}

Ffoo4();
assert.ok( Ffoo4 instanceof Function );

var Ffoo5_aux = 0;
var Ffoo5 = function Ffoo5( n ) {
   if( n == 1 ) {
      Ffoo5( 0, (Ffoo5_aux = 6) );
   }

}
Ffoo5( 1 );
assert.strictEqual( Ffoo5_aux, 6 );

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

assert.equal( foo1( 10 ), 55 );
assert.equal( foo2( 10 ), 10 );
assert.equal( bar1( 10 ), 55 );
assert.equal( bar2( 10 ), 55 );

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

assert.equal( bar11( 1 ), 1 );
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
