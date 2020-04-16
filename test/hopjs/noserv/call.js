/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/call.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Apr  8 15:44:36 2020                          */
/*    Last change :  Thu Apr 16 06:57:19 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing function calls                                           */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    F0 ...                                                           */
/*---------------------------------------------------------------------*/
function F0() {
   return 0;
}

console.log( "testing arity 0 (fixed)" );
console.log( "F0.direct.1...")
assert.ok( F0() === 0, "F0.direct.1" );
console.log( "F0.direct.2...");
assert.ok( F0( 1 ) === 0, "F0.direct.2" );
console.log( "F0.funcall.1..." );
assert.ok( ({ fun: F0 }).fun() === 0, "F0.funcall.1" );
console.log( "F0.funcall.2..." );
assert.ok( ({ fun: F0 }).fun( 1 ) === 0, "F0.funcall.2" );
console.log( "F0.apply.1..." );
assert.ok( F0.apply( undefined, [] ) === 0, "F0.apply.1" );
console.log( "F0.apply.2..." );
assert.ok( F0.apply( undefined, [ 1 ] ) === 0, "F0.apply.2" );
console.log( "F0.call..." );
assert.ok( F0.call( undefined, undefined ) === 0, "F0.call" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F0args ...                                                       */
/*---------------------------------------------------------------------*/
function F0args() {
   return arguments[ 0 ];
}

console.log( "testing arity 0 (arguments)" );
console.log( "F0args.direct.1..." );
assert.ok( F0args() === undefined, "F0args.direct.1" );
console.log( "F0args.direct.2..." );
assert.ok( F0args( 1 ) === 1, "F0args.direct.2" );
console.log( "F0args.funcall.1..." );
assert.ok( ({ fun: F0args }).fun() === undefined, "F0args.funcall.1" );
console.log( "F0args.funcall.2..." );
assert.ok( ({ fun: F0args }).fun( 1 ) === 1, "F0args.funcall.2" );
console.log( "F0args.apply.1..." );
assert.ok( F0args.apply( undefined, [] ) === undefined, "F0args.apply.1" );
console.log( "F0args.apply.2..." );
assert.ok( F0args.apply( undefined, [ 1 ] ) === 1, "F0args.apply.2" );
console.log( "F0args.call..." );
assert.ok( F0args.call( undefined, undefined ) === undefined, "F0args.call" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F0rest ...                                                       */
/*---------------------------------------------------------------------*/
function F0rest( ... rest ) {
   return rest[ 0 ];
}

console.log( "testing arity 0 (rest)" );
console.log( "F0rest.direct.1..." );
assert.ok( F0rest() === undefined, "F0rest.direct.1" );
console.log( "F0rest.direct.2..." );
assert.ok( F0rest( 1 ) === 1, "F0rest.direct.2" );
console.log( "F0rest.funcall.1..." );
assert.ok( ({ fun: F0rest }).fun() === undefined, "F0rest.funcall.1" );
console.log( "F0rest.funcall.2..." );
assert.ok( ({ fun: F0rest }).fun( 1 ) === 1, "F0rest.funcall.2" );
console.log( "F0rest.apply.1..." );
assert.ok( F0rest.apply( undefined, [] ) === undefined, "F0rest.apply.1" );
console.log( "F0rest.apply.2..." );
assert.ok( F0rest.apply( undefined, [ 1 ] ) === 1, "F0rest.apply.2" );
console.log( "F0rest.call..." );
assert.ok( F0rest.call( undefined, undefined ) === undefined, "F0rest.call" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F1fx ...                                                         */
/*---------------------------------------------------------------------*/
function F1fx( x ) {
   return x;
}

console.log( "testing arity 1 (fixed)" );
console.log( "F1fx.direct.1..." );
assert.ok( F1fx() === undefined, "F1fx.direct.1" );
console.log( "F1fx.direct.2..." );
assert.ok( F1fx( 1 ) === 1, "F1fx.direct.2" );
console.log( "F1fx.direct.3..." );
assert.ok( F1fx( 1, 2 ) === 1, "F1fx.direct.3" );
console.log( "F1fx.funcall.1..." );
assert.ok( ({ fun: F1fx }).fun() === undefined, "F1fx.funcall.1" );
console.log( "F1fx.funcall.2..." );
assert.ok( ({ fun: F1fx }).fun( 1 ) === 1, "F1fx.funcall.2" );
console.log( "F1fx.funcall.3..." );
assert.ok( ({ fun: F1fx }).fun( 1, 2 ) === 1, "F1fx.funcall.3" );
console.log( "F1fx.apply.1..." );
assert.ok( F1fx.apply( undefined, [] ) === undefined, "F1fx.apply.1" );
console.log( "F1fx.apply.2..." );
assert.ok( F1fx.apply( undefined, [ 1 ] ) === 1, "F1fx.apply.2" );
console.log( "F1fx.apply.3..." );
assert.ok( F1fx.apply( undefined, [ 1, 2 ] ) === 1, "F1fx.apply.3" );
console.log( "F1fx.call.1..." );
assert.ok( F1fx.call( undefined, undefined ) === undefined, "F1fx.call.1" );
console.log( "F1fx.call.2..." );
assert.ok( F1fx.call( undefined, 1 ) === 1, "F1fx.call.2" );
console.log( "F1fx.call.3..." );
assert.ok( F1fx.call( undefined, 1, 2 ) === 1, "F1fx.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F1args ...                                                       */
/*---------------------------------------------------------------------*/
function F1args( x ) {
   return arguments[ 0 ];
}

console.log( "testing arity 1 (arguments)" );
console.log( "F1args.direct.1..." );
assert.ok( F1args() === undefined, "F1args.direct.1" );
console.log( "F1args.direct.2..." );
assert.ok( F1args( 1 ) === 1, "F1args.direct.2" );
console.log( "F1args.direct.3..." );
assert.ok( F1args( 1, 2 ) === 1, "F1args.direct.3" );
console.log( "F1args.funcall.2..." );
assert.ok( ({ fun: F1args }).fun( 1 ) === 1, "F1args.funcall.2" );
console.log( "F1args.funcall.3..." );
assert.ok( ({ fun: F1args }).fun( 1, 2 ) === 1, "F1args.funcall.3" );
console.log( "F1args.apply.1..." );
assert.ok( F1args.apply( undefined, [] ) === undefined, "F1args.apply.1" );
console.log( "F1args.apply.2..." );
assert.ok( F1args.apply( undefined, [ 1 ] ) === 1, "F1args.apply.2" );
console.log( "F1args.apply.3..." );
assert.ok( F1args.apply( undefined, [ 1, 2 ] ) === 1, "F1args.apply.3" );
console.log( "F1args.call.1..." );
assert.ok( F1args.call( undefined, undefined ) === undefined, "F1args.call.1" );
console.log( "F1args.call.2..." );
assert.ok( F1args.call( undefined, 1 ) === 1, "F1args.call.2" );
console.log( "F1args.call.3..." );
assert.ok( F1args.call( undefined, 1, 2 ) === 1, "F1args.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F1opt ...                                                        */
/*---------------------------------------------------------------------*/
function F1opt( x = 1 ) {
   return x;
}

console.log( "testing arity 1 (optionals)" );
console.log( "F1opt.direct.1..." );
assert.ok( F1opt() === 1, "F1opt.direct.1" );
console.log( "F1opt.direct.2..." );
assert.ok( F1opt( 1 ) === 1, "F1opt.direct.2" );
console.log( "F1opt.direct.3..." );
assert.ok( F1opt( 1, 2 ) === 1, "F1opt.direct.3" );
console.log( "F1opt.funcall.1..." );
assert.ok( ({ fun: F1opt }).fun() === 1, "F1opt.funcall.1" );
console.log( "F1opt.funcall.2..." );
assert.ok( ({ fun: F1opt }).fun( 1 ) === 1, "F1opt.funcall.2" );
console.log( "F1opt.funcall.3..." );
assert.ok( ({ fun: F1opt }).fun( 1, 2 ) === 1, "F1opt.funcall.3" );
console.log( "F1opt.apply.1..." );
assert.ok( F1opt.apply( undefined, [] ) === 1, "F1opt.apply.1" );
console.log( "F1opt.apply.2..." );
assert.ok( F1opt.apply( undefined, [ 1 ] ) === 1, "F1opt.apply.2" );
console.log( "F1opt.apply.3..." );
assert.ok( F1opt.apply( undefined, [ 1, 2 ] ) === 1, "F1opt.apply.3" );
console.log( "F1opt.call.1..." );
assert.ok( F1opt.call( undefined, undefined ) === 1, "F1opt.call.1" );
console.log( "F1opt.call.2..." );
assert.ok( F1opt.call( undefined, 1 ) === 1, "F1opt.call.2" );
console.log( "F1opt.call.3..." );
assert.ok( F1opt.call( undefined, 1, 2 ) === 1, "F1opt.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F2fx ...                                                         */
/*---------------------------------------------------------------------*/
function F2fx( x, y ) {
   return y;
}

console.log( "testing arity 2 (fixed)" );
console.log( "F2fx.direct.1..." );
assert.ok( F2fx() === undefined, "F2fx.direct.1" );
console.log( "F2fx.direct.2..." );
assert.ok( F2fx( 1 ) === undefined, "F2fx.direct.2" );
console.log( "F2fx.direct.3..." );
assert.ok( F2fx( 1, 2 ) === 2, "F2fx.direct.3" );
console.log( "F2fx.funcall.1..." );
assert.ok( ({ fun: F2fx }).fun() === undefined, "F2fx.funcall.1" );
console.log( "F2fx.funcall.2..." );
assert.ok( ({ fun: F2fx }).fun( 1 ) === undefined, "F2fx.funcall.2" );
console.log( "F2fx.funcall.3..." );
assert.ok( ({ fun: F2fx }).fun( 1, 2 ) === 2, "F2fx.funcall.3" );
console.log( "F2fx.apply.1..." );
assert.ok( F2fx.apply( undefined, [] ) === undefined, "F2fx.apply.1" );
console.log( "F2fx.apply.2..." );
assert.ok( F2fx.apply( undefined, [ 1 ] ) === undefined, "F2fx.apply.2" );
console.log( "F2fx.apply.3..." );
assert.ok( F2fx.apply( undefined, [ 1, 2 ] ) === 2, "F2fx.apply.3" );
console.log( "F2fx.call.1..." );
assert.ok( F2fx.call( undefined, undefined ) === undefined, "F2fx.call.1" );
console.log( "F2fx.call.2..." );
assert.ok( F2fx.call( undefined, 1 ) === undefined, "F2fx.call.2" );
console.log( "F2fx.call.3..." );
assert.ok( F2fx.call( undefined, 1, 2 ) === 2, "F2fx.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F2args ...                                                       */
/*---------------------------------------------------------------------*/
function F2args( x, y ) {
   return arguments[ 1 ];
}

console.log( "testing arity 2 (arguments)" );
console.log( "F2args.direct.1..." );
assert.ok( F2args() === undefined, "F2args.direct.1" );
console.log( "F2args.direct.2..." );
assert.ok( F2args( 1 ) === undefined, "F2args.direct.2" );
console.log( "F2args.direct.3..." );
assert.ok( F2args( 1, 2 ) === 2, "F2args.direct.3" );
console.log( "F2args.funcall.2..." );
assert.ok( ({ fun: F2args }).fun( 1 ) === undefined, "F2args.funcall.2" );
console.log( "F2args.funcall.3..." );
assert.ok( ({ fun: F2args }).fun( 1, 2 ) === 2, "F2args.funcall.3" );
console.log( "F2args.apply.1..." );
assert.ok( F2args.apply( undefined, [] ) === undefined, "F2args.apply.1" );
console.log( "F2args.apply.2..." );
assert.ok( F2args.apply( undefined, [ 1 ] ) === undefined, "F2args.apply.2" );
console.log( "F2args.apply.3..." );
assert.ok( F2args.apply( undefined, [ 1, 2 ] ) === 2, "F2args.apply.3" );
console.log( "F2args.call.1..." );
assert.ok( F2args.call( undefined, undefined ) === undefined, "F2args.call.1" );
console.log( "F2args.call.2..." );
assert.ok( F2args.call( undefined, 1 ) === undefined, "F2args.call.2" );
console.log( "F2args.call.3..." );
assert.ok( F2args.call( undefined, 1, 2 ) === 2, "F2args.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F2opt ...                                                        */
/*---------------------------------------------------------------------*/
function F2opt( x = 1, y = 3 ) {
   return y;
}

console.log( "testing arity 2 (optionals)" );
console.log( "F2opt.direct.1..." );
assert.ok( F2opt() === 3, "F2opt.direct.1" );
console.log( "F2opt.direct.2..." );
assert.ok( F2opt( 1 ) === 3, "F2opt.direct.2" );
console.log( "F2opt.direct.3..." );
assert.ok( F2opt( 1, 2 ) === 2, "F2opt.direct.3" );
console.log( "F2opt.funcall.1..." );
assert.ok( ({ fun: F2opt }).fun() === 3, "F2opt.funcall.1" );
console.log( "F2opt.funcall.2..." );
assert.ok( ({ fun: F2opt }).fun( 1 ) === 3, "F2opt.funcall.2" );
console.log( "F2opt.funcall.3..." );
assert.ok( ({ fun: F2opt }).fun( 1, 2 ) === 2, "F2opt.funcall.3" );
console.log( "F2opt.apply.1..." );
assert.ok( F2opt.apply( undefined, [] ) === 3, "F2opt.apply.1" );
console.log( "F2opt.apply.2..." );
assert.ok( F2opt.apply( undefined, [ 1 ] ) === 3, "F2opt.apply.2" );
console.log( "F2opt.apply.3..." );
assert.ok( F2opt.apply( undefined, [ 1, 2 ] ) === 2, "F2opt.apply.3" );
console.log( "F2opt.call.1..." );
assert.ok( F2opt.call( undefined, undefined ) === 3, "F2opt.call.1" );
console.log( "F2opt.call.2..." );
assert.ok( F2opt.call( undefined, 1 ) === 3, "F2opt.call.2" );
console.log( "F2opt.call.3..." );
assert.ok( F2opt.call( undefined, 1, 2 ) === 2, "F2opt.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F2opt2 ...                                                       */
/*---------------------------------------------------------------------*/
function F2opt2( x, y = 3 ) {
   return y;
}

console.log( "testing arity 2 (optionals)" );
console.log( "F2opt2.direct.1..." );
assert.ok( F2opt2() === 3, "F2opt2.direct.1" );
console.log( "F2opt2.direct.2..." );
assert.ok( F2opt2( 1 ) === 3, "F2opt2.direct.2" );
console.log( "F2opt2.direct.3..." );
assert.ok( F2opt2( 1, 2 ) === 2, "F2opt2.direct.3" );
console.log( "F2opt2.funcall.1..." );
assert.ok( ({ fun: F2opt2 }).fun() === 3, "F2opt2.funcall.1" );
console.log( "F2opt2.funcall.2..." );
assert.ok( ({ fun: F2opt2 }).fun( 1 ) === 3, "F2opt2.funcall.2" );
console.log( "F2opt2.funcall.3..." );
assert.ok( ({ fun: F2opt2 }).fun( 1, 2 ) === 2, "F2opt2.funcall.3" );
console.log( "F2opt2.apply.1..." );
assert.ok( F2opt2.apply( undefined, [] ) === 3, "F2opt2.apply.1" );
console.log( "F2opt2.apply.2..." );
assert.ok( F2opt2.apply( undefined, [ 1 ] ) === 3, "F2opt2.apply.2" );
console.log( "F2opt2.apply.3..." );
assert.ok( F2opt2.apply( undefined, [ 1, 2 ] ) === 2, "F2opt2.apply.3" );
console.log( "F2opt2.call.1..." );
assert.ok( F2opt2.call( undefined, undefined ) === 3, "F2opt2.call.1" );
console.log( "F2opt2.call.2..." );
assert.ok( F2opt2.call( undefined, 1 ) === 3, "F2opt2.call.2" );
console.log( "F2opt2.call.3..." );
assert.ok( F2opt2.call( undefined, 1, 2 ) === 2, "F2opt2.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F3opt ...                                                        */
/*---------------------------------------------------------------------*/
function F3opt( x = 1, y = 3, z = 4 ) {
   return z;
}

console.log( "testing arity 3 (optionals)" );
console.log( "F3opt.direct.1..." );
assert.ok( F3opt() === 4, "F3opt.direct.1" );
console.log( "F3opt.direct.2..." );
assert.ok( F3opt( 1 ) === 4, "F3opt.direct.2" );
console.log( "F3opt.direct.3..." );
assert.ok( F3opt( 1, 2 ) === 4, "F3opt.direct.2" );
console.log( "F3opt.direct.4..." );
assert.ok( F3opt( 1, 2, 3 ) === 3, "F3opt.direct.3" );
console.log( "F3opt.funcall.1..." );
assert.ok( ({ fun: F3opt }).fun() === 4, "F3opt.funcall.1" );
console.log( "F3opt.funcall.2..." );
assert.ok( ({ fun: F3opt }).fun( 1 ) === 4, "F3opt.funcall.2" );
console.log( "F3opt.funcall.3..." );
assert.ok( ({ fun: F3opt }).fun( 1, 2 ) === 4, "F3opt.funcall.3" );
console.log( "F3opt.funcall.4..." );
assert.ok( ({ fun: F3opt }).fun( 1, 2, 3 ) === 3, "F3opt.funcall.3" );
console.log( "F3opt.apply.1..." );
assert.ok( F3opt.apply( undefined, [] ) === 4, "F3opt.apply.1" );
console.log( "F3opt.apply.2..." );
assert.ok( F3opt.apply( undefined, [ 1 ] ) === 4, "F3opt.apply.2" );
console.log( "F3opt.apply.3..." );
assert.ok( F3opt.apply( undefined, [ 1, 2 ] ) === 4, "F3opt.apply.3" );
console.log( "F3opt.apply.4..." );
assert.ok( F3opt.apply( undefined, [ 1, 2, 3 ] ) === 3, "F3opt.apply.3" );
console.log( "F3opt.call.1..." );
assert.ok( F3opt.call( undefined, undefined ) === 4, "F3opt.call.1" );
console.log( "F3opt.call.2..." );
assert.ok( F3opt.call( undefined, 1 ) === 4, "F3opt.call.2" );
console.log( "F3opt.call.3..." );
assert.ok( F3opt.call( undefined, 1, 2 ) === 4, "F3opt.call.3" );
console.log( "F3opt.call.4..." );
assert.ok( F3opt.call( undefined, 1, 2, 3 ) === 3, "F3opt.call.3" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F3opt2 ...                                                       */
/*---------------------------------------------------------------------*/
function F3opt2( x, y = 3, z = 4 ) {
   return z;
}

console.log( "testing arity 3 (optionals)" );
console.log( "F3opt2.direct.1..." );
assert.ok( F3opt2() === 4, "F3opt2.direct.1" );
console.log( "F3opt2.direct.2..." );
assert.ok( F3opt2( 1 ) === 4, "F3opt2.direct.2" );
console.log( "F3opt2.direct.3..." );
assert.ok( F3opt2( 1, 2 ) === 4, "F3opt2.direct.3" );
console.log( "F3opt2.direct.4..." );
assert.ok( F3opt2( 1, 2, 3 ) === 3, "F3opt2.direct.4" );
console.log( "F3opt2.funcall.1..." );
assert.ok( ({ fun: F3opt2 }).fun() === 4, "F3opt2.funcall.1" );
console.log( "F3opt2.funcall.2..." );
assert.ok( ({ fun: F3opt2 }).fun( 1 ) === 4, "F3opt2.funcall.2" );
console.log( "F3opt2.funcall.3..." );
assert.ok( ({ fun: F3opt2 }).fun( 1, 2 ) === 4, "F3opt2.funcall.3" );
console.log( "F3opt2.funcall.4..." );
assert.ok( ({ fun: F3opt2 }).fun( 1, 2, 3 ) === 3, "F3opt2.funcall.4" );
console.log( "F3opt2.apply.1..." );
assert.ok( F3opt2.apply( undefined, [] ) === 4, "F3opt2.apply.1" );
console.log( "F3opt2.apply.2..." );
assert.ok( F3opt2.apply( undefined, [ 1 ] ) === 4, "F3opt2.apply.2" );
console.log( "F3opt2.apply.3..." );
assert.ok( F3opt2.apply( undefined, [ 1, 2 ] ) === 4, "F3opt2.apply.3" );
console.log( "F3opt2.apply.4..." );
assert.ok( F3opt2.apply( undefined, [ 1, 2, 3 ] ) === 3, "F3opt2.apply.4" );
console.log( "F3opt2.call.1..." );
assert.ok( F3opt2.call( undefined, undefined ) === 4, "F3opt2.call.1" );
console.log( "F3opt2.call.2..." );
assert.ok( F3opt2.call( undefined, 1 ) === 4, "F3opt2.call.2" );
console.log( "F3opt2.call.3..." );
assert.ok( F3opt2.call( undefined, 1, 2 ) === 4, "F3opt2.call.3" );
console.log( "F3opt2.call.4..." );
assert.ok( F3opt2.call( undefined, 1, 2, 3 ) === 3, "F3opt2.call.4" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F4opt                                                            */
/*---------------------------------------------------------------------*/
function F4opt( x, y, z, t = 5 ) {
   return t;
}

console.log( "testing arity 4 (optionals)" );
console.log( "F4opt.direct.1..." );
assert.ok( F4opt() === 5, "F4opt.direct.1" );
console.log( "F4opt.direct.2..." );
assert.ok( F4opt( 1 ) === 5, "F4opt.direct.2" );
console.log( "F4opt.direct.3..." );
assert.ok( F4opt( 1, 2 ) === 5, "F4opt.direct.3" );
console.log( "F4opt.direct.4..." );
assert.ok( F4opt( 1, 2, 3 ) === 5, "F4opt.direct.4" );
console.log( "F4opt.direct.5..." );
assert.ok( F4opt( 1, 2, 3, 4 ) === 4, "F4opt.direct.5" );
console.log( "F4opt.funcall.1..." );
assert.ok( ({ fun: F4opt }).fun() === 5, "F4opt.funcall.1" );
console.log( "F4opt.funcall.2..." );
assert.ok( ({ fun: F4opt }).fun( 1 ) === 5, "F4opt.funcall.2" );
console.log( "F4opt.funcall.3..." );
assert.ok( ({ fun: F4opt }).fun( 1, 2 ) === 5, "F4opt.funcall.3" );
console.log( "F4opt.funcall.4..." );
assert.ok( ({ fun: F4opt }).fun( 1, 2, 3 ) === 5, "F4opt.funcall.4" );
console.log( "F4opt.funcall.5..." );
assert.ok( ({ fun: F4opt }).fun( 1, 2, 3, 4 ) === 4, "F4opt.funcall.5" );
console.log( "F4opt.apply.1..." );
assert.ok( F4opt.apply( undefined, [] ) === 5, "F4opt.apply.1" );
console.log( "F4opt.apply.2..." );
assert.ok( F4opt.apply( undefined, [ 1 ] ) === 5, "F4opt.apply.2" );
console.log( "F4opt.apply.3..." );
assert.ok( F4opt.apply( undefined, [ 1, 2 ] ) === 5, "F4opt.apply.3" );
console.log( "F4opt.apply.4..." );
assert.ok( F4opt.apply( undefined, [ 1, 2, 3 ] ) === 5, "F4opt.apply.4" );
console.log( "F4opt.apply.5..." );
assert.ok( F4opt.apply( undefined, [ 1, 2, 3, 4 ] ) === 4, "F4opt.apply.5" );
console.log( "F4opt.call.1..." );
assert.ok( F4opt.call( undefined, undefined ) === 5, "F4opt.call.1" );
console.log( "F4opt.call.2..." );
assert.ok( F4opt.call( undefined, 1 ) === 5, "F4opt.call.2" );
console.log( "F4opt.call.3..." );
assert.ok( F4opt.call( undefined, 1, 2 ) === 5, "F4opt.call.3" );
console.log( "F4opt.call.4..." );
assert.ok( F4opt.call( undefined, 1, 2, 3 ) === 5, "F4opt.call.4" );
console.log( "F4opt.call.5..." );
assert.ok( F4opt.call( undefined, 1, 2, 3, 4 ) === 4, "F4opt.call.5" );
console.log( "" );

/*---------------------------------------------------------------------*/
/*    F4optrest ...                                                    */
/*---------------------------------------------------------------------*/
const F4optrest = ( a = 0, ... b ) => a + b.reduce( (x,y) => x+y, 0 );

console.log( "testing optional + rest " );

console.log( "F4optrest.direct.1..." );
assert.ok( F4optrest() === 0, "F4optrest.direct.1" );
console.log( "F4optrest.direct.2..." );
assert.ok( F4optrest( 1 ) === 1, "F4optrest.direct.2" );
console.log( "F4optrest.direct.3..." );
assert.ok( F4optrest( 1, 2 ) === 3, "F4optrest.direct.3" );
console.log( "F4optrest.direct.4..." );
assert.ok( F4optrest( 1, 2, 3 ) === 6, "F4optrest.direct.4" );
console.log( "F4optrest.funcall.1..." );
assert.ok( ({ fun: F4optrest }).fun() === 0, "F4optrest.funcall.1" );
console.log( "F4optrest.funcall.2..." );
assert.ok( ({ fun: F4optrest }).fun( 1 ) === 1, "F4optrest.funcall.2" );
console.log( "F4optrest.funcall.3..." );
assert.ok( ({ fun: F4optrest }).fun( 1, 2 ) === 3, "F4optrest.funcall.3" );
console.log( "F4optrest.funcall.4..." );
assert.ok( ({ fun: F4optrest }).fun( 1, 2, 3 ) === 6, "F4optrest.funcall.4" );
console.log( "F4optrest.apply.1..." );
assert.ok( F4optrest.apply(undefined) === 0, "F4optrest.apply.1" );
console.log( "F4optrest.apply.2..." );
assert.ok( F4optrest.apply(undefined, [1] ) === 1, "F4optrest.apply.2" );
console.log( "F4optrest.apply.3..." );
assert.ok( F4optrest.apply(undefined, [1, 2] ) === 3, "F4optrest.apply.3" );
console.log( "F4optrest.apply.4..." );
assert.ok( F4optrest.apply( undefined, [1, 2, 3] ) === 6, "F4optrest.apply.4" );
console.log( "F4optrest.call.1..." );
assert.ok( F4optrest.call(undefined) === 0, "F4optrest.call.1" );
console.log( "F4optrest.call.2..." );
assert.ok( F4optrest.call(undefined, 1 ) === 1, "F4optrest.call.2" );
console.log( "F4optrest.call.3..." );
assert.ok( F4optrest.call(undefined, 1, 2 ) === 3, "F4optrest.call.3" );
console.log( "F4optrest.call.4..." );
assert.ok( F4optrest.call( undefined, 1, 2, 3 ) === 6, "F4optrest.call.4" );
