/*=====================================================================*/
/*    .../hop/3.2.x/test/hopjs/noserv/es6-destructing-param.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb  7 09:48:34 2018                          */
/*    Last change :  Fri Mar 23 13:05:25 2018 (serrano)                */
/*    Copyright   :  2018 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2015+ destructuring function parameters       */
/*=====================================================================*/

"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    hop                                                              */
/*---------------------------------------------------------------------*/
const hopa = ( {a, b}, res ) => {
   return a - b == res;
};

const hopb = ( {a: A, b: B}, res ) => {
   return A - B == res;
};

console.log( "hop" );
console.log( "   hopa()"); assert.ok( hopa( {a: 1, b: 2}, -1 ), "hopa" );
console.log( "   hopa()"); assert.ok( hopa( {b: 2, a: 1}, -1 ), "hopa" );
console.log( "   hopb()"); assert.ok( hopb( {a: 1, b: 2}, -1 ), "hopb" );
console.log( "   hopb()"); assert.ok( hopb( {b: 2, a: 1}, -1 ), "hopb" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxj() {
   return function( {c, x: d, e} ) {
      return c === 7 && d === 8 && e === undefined;
   }( {c:7, x:8} );
}

console.log( "kangax" );
console.log( "   kangaxj()"); assert.ok( kangaxj(), "kangaxj" );
