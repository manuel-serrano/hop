/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/property.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 05:40:26 2014                          */
/*    Last change :  Thu Jul 25 06:21:50 2019 (serrano)                */
/*    Copyright   :  2014-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Property access (get/set) tests.                                 */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    Constructors used in many following tests                        */
/*---------------------------------------------------------------------*/
function Obj( type ) {
   this.type = type;
}

Obj.prototype = { version: 1.0 };

function Cons( a, d ) {
   this.car = a;
   this.cdr = d;
}

Cons.prototype = new Obj( "cons" );

/*---------------------------------------------------------------------*/
/*    Direct accesses                                                  */
/*---------------------------------------------------------------------*/
var o = new Cons( 1, 2 );

assert.strictEqual( o.car, 1 );
assert.strictEqual( o.type, "cons" );
assert.equal( o.version, 1.0 );
assert.equal( o.hasOwnProperty( "type" ), false );

o.car = 3;
o.cer = 4;
o.type = "econs";
o.version = 1.1;

assert.strictEqual( o.car, 3 );
assert.strictEqual( o.type, "econs" );
assert.equal( o.version, 1.1 );
assert.equal( o.hasOwnProperty( "type" ), true );

// check that o assignment has not modified the prototype objects
var o2 = new Cons( 4, 5 );

assert.strictEqual( o2.car, 4 );
assert.strictEqual( o2.type, "cons" );
assert.equal( o2.version, 1.0 );

/*---------------------------------------------------------------------*/
/*    Accessor properties                                              */
/*---------------------------------------------------------------------*/
var o = new Cons( 1, 2 );

Object.defineProperty( o, "sum",
   { get: function() { return this.car + this.cdr; },
     set: function( v ) { this.car = v/2; this.cdr = v/2; } } );

assert.strictEqual( o.sum, 3 );

o.sum = 4;
assert.strictEqual( o.car, 2 );
assert.strictEqual( o.cdr, 2 );

Object.defineProperty( Cons.prototype, "sum2",
   { get: function() { return this.car + this.cdr; },
     set: function( v ) { this.car = v/2; this.cdr = v/2; } } );

assert.strictEqual( o.sum2, 4 );

o.sum2 = 8;
assert.strictEqual( o.car, 4 );
assert.strictEqual( o.cdr, 4 );

Object.defineProperty( Cons.prototype, "sum3",
   { enumerable: true,
     get: function() { return this.car + this.cdr; },
     set: function( v ) { this.car = v/2; this.cdr = v/2; } } );

assert.strictEqual( o.sum3, 8 );

o.sum3 = 16;
assert.strictEqual( o.car, 8 );
assert.strictEqual( o.cdr, 8 );

assert.deepEqual( Object.keys( o ), [ "car", "cdr" ] );
assert.deepEqual( Object.keys( o.__proto__ ), [ "type", "sum3" ] );

Object.defineProperty( Cons.prototype, "sum4",
   { enumerable: true,
     get: () => { return this.car + this.cdr; },
     set: ( v ) => { this.car = v/2; this.cdr = v/2; } } );

o.sum4 = 16;
assert.strictEqual( o.car, 8 );
assert.strictEqual( o.cdr, 8 );

assert.deepEqual( Object.keys( o ), [ "car", "cdr" ] );
assert.deepEqual( Object.keys( o.__proto__ ), [ "type", "sum3", "sum4" ] );

/*---------------------------------------------------------------------*/
/*    Accessor properties with array prototypes                        */
/*---------------------------------------------------------------------*/
Cons.prototype = [];

Cons.prototype.type = "acons";
Cons.prototype[ 0 ] = 10;

var o = new Cons( 1, 2 );

assert.strictEqual( o.type, "acons" );
assert.strictEqual( o[ 0 ], 10, "Cons prototype" );

Object.defineProperty( o, "sum",
   { get: function() { return this.car + this.cdr; },
     set: function( v ) { this.car = v/2; this.cdr = v/2; } } );

assert.strictEqual( o.sum, 3 );

o.sum = 4;
assert.strictEqual( o.car, 2 );
assert.strictEqual( o.cdr, 2 );

Object.defineProperty( Cons.prototype, "sum2",
   { get: function() { return this.car + this.cdr; },
     set: function( v ) { this.car = v/2; this.cdr = v/2; } } );

assert.strictEqual( o.sum2, 4 );

o.sum2 = 8;
assert.strictEqual( o.car, 4 );
assert.strictEqual( o.cdr, 4 );

Object.defineProperty( Cons.prototype, "sum3",
   { enumerable: true,
     get: function() { return this.car + this.cdr; },
     set: function( v ) { this.car = v/2; this.cdr = v/2; } } );

assert.strictEqual( o.sum3, 8 );

o.sum3 = 16;
assert.strictEqual( o.car, 8 );
assert.strictEqual( o.cdr, 8 );

assert.deepEqual( Object.keys( o ), [ "car", "cdr" ] );
assert.deepEqual( Object.keys( o.__proto__ ), [ "0", "type", "sum3" ] );


/*---------------------------------------------------------------------*/
/*    Array specific tests                                             */
/*---------------------------------------------------------------------*/
var o = [1, 2, 3];

assert.strictEqual( "2" in o, true );

o.length = 2;

assert.strictEqual( "2" in o, false );


/*---------------------------------------------------------------------*/
/*    Accessors                                                        */
/*---------------------------------------------------------------------*/
var o = { get readwrite() { return 24; },set readwrite( val ) {} };

assert.strictEqual( o.readwrite, 24 );

function getfun() {
   var foo = 55;
   var o = { get foo() { return foo; } };

   return o.foo;
}

assert.strictEqual( getfun(), 55, "getter" );

/*---------------------------------------------------------------------*/
/*    Prototypes                                                       */
/*---------------------------------------------------------------------*/
var p0 = { a: 456, x: 11 };
var o1 = {};
o1.__proto__ = p0;

var o2 = { __proto__: o1.__proto__ };

assert.strictEqual( o1.a, 456, "o1.prototype" );
assert.strictEqual( o2.a, 456, "o2.prototype" );

var o3 = {get __proto__() { return { a: 1 } } };
assert.strictEqual( o3.a, undefined );

var p1 = { x: 4 };
var p2 = { x: 5 };
var p3 = { __proto__: p1, x: 6 };
var p4 = { __proto__: p1 };
var p5 =  { __proto__: p3, x: 38 }

var o = { __proto__: p1 };
var o4 = { __proto__: p5, x: 49 };

function getP( o ) { return o.x; }

var v1 = getP( o );
assert.strictEqual( v1, p1.x, "proto eq" );
assert.strictEqual( v1, getP( o ), "proto eq.2" );

o.__proto__ = p2;

assert.ok( getP( o ) == p2.x, "proto mutated.1" );
assert.ok( getP( o ) != p1.x, "proto mutated.2" );

o.__proto__ = p3;

assert.ok( getP( o ) != p1.x );
assert.ok( getP( o ) != p2.x );
assert.ok( getP( o ) == p3.x );

o.__proto__ = p4;

assert.ok( getP( o ) == p1.x );
assert.ok( getP( o ) != p2.x );
assert.ok( getP( o ) != p3.x );
assert.ok( getP( o ) == p4.x );

assert.strictEqual( getP( o4 ), 49, "o4 prototypes" );

delete o4.x;
assert.strictEqual( getP( o4 ), p5.x, "delete o2.x" );

delete p5.x;
assert.strictEqual( getP( o4 ), p3.x, "delete p5.x" );

p5.__proto__ = p2;
assert.strictEqual( getP( o4 ), p2.x, "p5.__proto__" );


/*---------------------------------------------------------------------*/
/*    Setters                                                          */
/*---------------------------------------------------------------------*/
function setX( o, v ) {
   var r = o.x = ++v;
   return r;
}

var o1 = {};
var o2 = {};

assert.ok( setX( o1, 4 ) == 5 );
assert.ok( setX( o1, 10 ) == 11 );
assert.ok( setX( o2, 41 ) == 42 );

/*---------------------------------------------------------------------*/
/*    Constructor                                                      */
/*---------------------------------------------------------------------*/
function Ctor( a, b, c, d ) {
   this.a = a;
   this.b = b;
   this.c = c;
   this.d = d;
}

assert( new Ctor( 1, 2, 3, 4 ).d == 4, "plain constructor" );

Ctor.prototype = {
   set d(v) { return 1 }, get d() { return 222; }
};

assert( new Ctor( 1, 2, 3, 4 ).d == 222, "prototype constructor" );

Ctor.prototype = {
   d: 333,
   __proto__: { set d(v) { return 1 }, get d() { return 444; } }
};

assert( new Ctor( 1, 2, 3, 4 ).d == 4, "prototype.__proto__ constructor" );

Ctor.prototype = {
   __proto__: { set d(v) { return 1 }, get d() { return 444; } }
};

assert( new Ctor( 1, 2, 3, 4 ).d == 444, "prototype.__proto__ constructor bis" );

function Ctor2( a, b, c, d, e, f ) {
   this.a = a;
   this.b = b;
   this.c = c;
   this.d = d;
   this.e = e;
   this.f = f;
}

Ctor2.prototype = {
   __proto__: {
      set e( v ) { },
      get e() { return 34;}
   }
}

assert( new Ctor2( 1, 2, 3, 4, 5, 6 ).e === 34, "ctor2 with proto setter" );
   
function Ctor3( a, b, c, d, e, f ) {
   this.a = a;
   this.b = b;
   this.c = c;
   this.d = d;
   this.e = e;
   this.f = f;
}

var Ctor3__proto__ = {};

Ctor3.prototype = Object.create( Ctor3__proto__ );

assert( new Ctor3( 1, 2, 3, 4, 5, 6 ).e === 5, "ctor3 with proto setter" );
assert( new Ctor3( 1, 2, 3, 4, 50, 6 ).e === 50, "ctor3 with proto setter" );
assert( new Ctor3( 1, 2, 3, 4, 500, 6 ).e === 500, "ctor3 with proto setter" );

Object.defineProperty( Ctor3__proto__, "e", {
   set( v ) { },
   get() { return 34;}
} );

assert( new Ctor3( 1, 2, 3, 4, 5, 6 ).e === 34, "ctor3 with proto setter" );

function SETX( o, v ) {
   o.xxx = v;
   return o.xxx;
}

function GETX( o ) {
   return o.xxx;
}

var p = {};
var o = { __proto__: p };

var eq = assert.strictEqual;

eq( SETX( o, 3 ), 3, "plain set" );

eq( SETX( o, 4 ), 4, "plain set" );

o = { __proto__: p };
p.__proto__ = { get xxx() { return 18 }, set xxx( v ) {} };
eq( SETX( o, 5 ), 18, "proto set" );
p.__proto__ = {};
eq( SETX( o, 5 ), 5, "proto unset" );

var i = { get xxx() { return 19 }, set xxx( v ) {} };
p = { __proto__: i, get xxx() { return 18 }, set xxx( v ) {} };
p.__proto__ = i;

eq( i.xxx, 19, "p.xxx " + i.xxx + "/19" );
eq( p.xxx, 18, "p.xxx " + p.xxx + "/18" );

var i2 = { get xxx() { return 19 }, set xxx( v ) {} };
p2 = { __proto__: i, get xxx() { return 18 }, set xxx( v ) {} };
p2.__proto__ = i2;

eq( i2.xxx, 19, "p.xxx " + i2.xxx + "/19" );
eq( p2.xxx, 18, "p.xxx " + p2.xxx + "/18" );

var pxxx = GETX( p );
eq( pxxx, 18, "p.xxx " + pxxx + "/18" );
pxxx = GETX( p );
eq( pxxx, 18, "p.xxx " + pxxx + "/18" );

o = { __proto__: p };

eq( o.xxx, 18, "proto with proto " + o.xxx + "/18" );

eq( SETX( o, 3 ), 18, "proto set" );
eq( SETX( o, 3 ), 18, "proto set2" );
delete p.xxx;
eq( SETX( o, 3 ), 19, "after delete" );
   
/*---------------------------------------------------------------------*/
/*    Caches                                                           */
/*---------------------------------------------------------------------*/
function setCache( o ) {
   o.z = 10;
}

function Root() {
   this.a = 1;
   this.b = 1;
   this.c = 1;
   this.d = 1;
}

function bugPutCache() {
   var o1 = new Root();
   var o2 = new Root();

   setCache( o1 );
   setCache( o2 );

   return o1.z == o2.z;
}

assert.ok( bugPutCache, "put cache" );

/*---------------------------------------------------------------------*/
/*    Caches & prototypes                                              */
/*---------------------------------------------------------------------*/
function getZ( o ) {
   return o.z;
}

var pz0 = { z : 45 };
var pz = { __proto__: pz0 };
var o = { __proto__: pz };

eq( getZ( o ), 45, "first cache" );
pz.z = 55;
eq( getZ( o ), 55, "second cache" );

var tz0 = { a: 10, z: 45 };
var tz = { __proto__: tz0 };
var o2 = { __proto__: tz };
var f = function() { return 0 };

eq( getZ( o2 ), 45, "first cache (bis)" );
tz.z = f;
eq( getZ( o2 ), f, "second cache (bis)" );

/*---------------------------------------------------------------------*/
/*    Cmap property bug                                                */
/*---------------------------------------------------------------------*/
function Foo() {};
function Bar() {};

Object.defineProperty( Foo.prototype, "prop", {
   get: function() {}, set: function(v) {}, enumerable: true
} );

Object.defineProperty( Bar.prototype, "prop", {
   value: 10, enumerable: false
} );


assert.strictEqual( Foo.prototype.propertyIsEnumerable( "prop" ), true );
assert.strictEqual( Bar.prototype.propertyIsEnumerable( "prop" ), false );

/*---------------------------------------------------------------------*/
/*    Call bug                                                         */
/*---------------------------------------------------------------------*/
function caller( f, a, b ) {
   return f.call( undefined, a, b );
}

function test( caller ) {
   // log: 6283f5308940c164e74a6a70be89053826ea8bbc
   // all the built closures share the same cmap but still 
   // call cannot be cached
   function mkClo( x ) {
      return function( a, b ) { return x + a + b }
   }
   
   return caller( mkClo( 1 ), 2, 3 ) === 6
      && caller( mkClo( 2 ), 4, 5 ) === 11
      && caller( mkClo( 3 ), 4, 5 ) === 12;
}

assert.ok( caller, "caller" );
