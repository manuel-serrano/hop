/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/property.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 05:40:26 2014                          */
/*    Last change :  Wed Oct  1 11:56:41 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
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

/*---------------------------------------------------------------------*/
/*    Accessor properties with array prototypes                        */
/*---------------------------------------------------------------------*/
Cons.prototype = [];

Cons.prototype.type = "acons";
Cons.prototype[ 0 ] = 10;

var o = new Cons( 1, 2 );

assert.strictEqual( o.type, "acons" );
assert.strictEqual( o[ 0 ], 10 );

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
