/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-reflect.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Oct  7 07:34:02 2014                          */
/*    Last change :  Wed Dec  5 22:18:44 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 2016 Reflect objects                          */
/*=====================================================================*/
"use strict";
"use hopscript";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   return true;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );

/*---------------------------------------------------------------------*/
/*    mdn ...                                                          */
/*---------------------------------------------------------------------*/
function mdna() {
   var obj = { x: 1, y: 2 };
   return Reflect.get(obj, 'x') === 1;
}

function mdnb() {
   return Reflect.get(['zero', 'one'], 1) === "one";
}

function mdnc() {
   var x = {p: 1};
   var obj = new Proxy(x, {
      get(t, k, r) { return k + 'bar'; }
   });
   return Reflect.get(obj, 'foo') === "foobar";
}

console.log( "mdn" );
console.log( "   mdna()"); assert.ok( mdna(), "mdna" );
console.log( "   mdnb()"); assert.ok( mdnb(), "mdnb" );
console.log( "   mdnc()"); assert.ok( mdnc(), "mdnc" );

/*---------------------------------------------------------------------*/
/*    kangax ...                                                       */
/*---------------------------------------------------------------------*/
function kangaxa() {
   return true;
}

console.log( "kangax" );
console.log( "   kangaxa()"); assert.ok( kangaxa(), "kangaxa" );

