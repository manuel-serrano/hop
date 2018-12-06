/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-spread.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Thu Dec  6 13:57:39 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Spread syntax                            */
/*=====================================================================*/
"use strict";
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc ...                                                         */
/*---------------------------------------------------------------------*/
function misca() {
   function sum(x, y, z) {
      return x + y + z;
   }
   
   return sum(...[1, 2, 3]) === 6;
}

function miscb() {
   function Foo( a, b, c ) {
      return a + b + c;
   }
   
   Foo.apply = function( self, args ) {
      return 24;
   }
   
   return Foo.apply( null, [ 1, 2, 3, 4 ] ) === 24;
}

function miscc() {
   function Foo( a, b, c ) {
      return this.x + a + b + c;
   }
   const o = { x: 30, f: Foo };

   return o.f( ...[1,2,3] ) === 36;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );
console.log( "   miscc()"); assert.ok( miscc(), "miscc" );
	    
/*---------------------------------------------------------------------*/
/*    mdn ...                                                          */
/*---------------------------------------------------------------------*/
function mdna() {
   function sum(x, y, z) {
      return x + y + z;
   }
   const numbers = [1, 2, 3];
   
   return sum(...numbers) === 6;
}

function mdnb() {
   function myFunction(v, w, x, y, z) { return v+w+x+y+z; }
   var args = [0, 1];
   return myFunction(-1, ...args, 2, ...[3]) === -1+0+1+2+3;
}

function mdnc() {
   var dateFields = [1970, 0, 1];  // 1 Jan 1970
   new Date(...dateFields).toString() === "1969-12-31T23:00:00.000Z";
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );
