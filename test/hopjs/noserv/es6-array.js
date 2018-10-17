/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/noserv/es6-array.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Oct 30 17:54:07 2015                          */
/*    Last change :  Thu Apr 13 08:20:13 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 arrays                                    */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    fill ...                                                         */
/*---------------------------------------------------------------------*/
function filla() {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a[ 5 ] = 2;

   return a[ 0 ] === 1 && a.indexOf( 2 ) === 5;
}

function fillb() {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a[ 0 ] = 2;
   a[ 1 ] = 2;

   return a.indexOf( 1 ) === 2 && a[ 9 ] === 1;
}

function fillc( start ) {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a.fill( 2, start );

   return a.indexOf( 2 ) === start && a[ 9 ] == 2;
}

function filld( start, end ) {
   let a = new Array( 10 );
   
   a.fill( 1 );
   a.fill( 2, start, end );

   return a.indexOf( 2 ) === start && a[ end - 1 ] === 2 && a[ end ] === 1;
}

function fille() {
   let a = new Array( 10 );
   
   a.fill( 1 );
   delete a[ 5 ];
   a.fill( 2 );

   return a.lastIndexOf( 2 ) === 9 && a[ 5 ] === 2 && a[ 0 ] === 2;
}

function kangaxa() {
   // https://kangax.github.io/compat-table/es6/
   return typeof Array.prototype.fill === "function";
}

console.log( "fill" );

console.log( "   filla()");
assert.ok( filla() );

console.log( "   fillb()");
assert.ok( fillb() );

console.log( "   fillc()");
assert.ok( fillc( 3 ) );

console.log( "   filld()");
assert.ok( filld( 2, 7 ) );

console.log( "   fille()");
assert.ok( fille() );

console.log( "   kangaxfill()");
assert.ok( kangaxa(), true );



