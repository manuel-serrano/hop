/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/ecma51.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Wed Nov 26 09:51:05 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
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
function Ffoo1() { Ffoo1 = 4; }

Ffoo1();

assert.strictEqual( Ffoo1, 4 );

var Ffoo2 = function Ffoo2() { Ffoo2 = 4; }
var bck = Ffoo2;

Ffoo2();
assert.strictEqual( Ffoo2, bck );

/*---------------------------------------------------------------------*/
/*    switches                                                         */
/*---------------------------------------------------------------------*/
function swfoo1( x ) {
   switch( x ) {
      case 1:
      case 2:
      case 3: return 1;
      default: return 2;
   }
}

function swfoo2( x ) {
   switch( x ) {
      case 1:
      case 2: return 3;
      case 3: return 1;
      default: return 2;
   }
}

assert.strictEqual( swfoo1( 2 ), 1 );
assert.strictEqual( swfoo1( 4 ), 2 );
assert.strictEqual( swfoo2( 1 ), 3 );
assert.strictEqual( swfoo2( 2 ), 3 );
assert.strictEqual( swfoo2( 3 ), 1 );
assert.strictEqual( swfoo2( false ), 2 );

/*---------------------------------------------------------------------*/
/*    variables and parameters                                         */
/*---------------------------------------------------------------------*/
function foo1( x ) {
   "use strict";
   x = 45;
   var x = 55;
   return x;
}

function foo2( x ) {
   "use strict";
   x = 45;
   var x = 55;
   return arguments[ 0 ];
}

function bar1( x ) {
   x = 45;
   var x = 55;
   return x;
}

function bar2( x ) {
   x = 45;
   var x = 55;
   return arguments[ 0 ];
}


assert.equal( foo1( 10 ), 55 );
assert.equal( foo2( 10 ), 10 );
assert.equal( bar1( 10 ), 55 );
assert.equal( bar2( 10 ), 55 );

