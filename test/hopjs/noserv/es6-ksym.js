/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/es6-ksym.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Nov 26 20:00:49 2017                          */
/*    Last change :  Sun Nov 26 20:06:24 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ES6 well-known symbols                                   */
/*=====================================================================*/
const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    kangax                                                           */
/*---------------------------------------------------------------------*/
function kangaxa() {
   var passed = false;
   var obj = { foo: true };
   var C = function(){};
   Object.defineProperty(C, Symbol.hasInstance, {
      value: function(inst) { passed = inst.foo; return false; }
   });
   obj instanceof C;
   return passed;
}

function kangaxb() {
   var a = [], b = [];
   b[Symbol.isConcatSpreadable] = false;
   a = a.concat(b);
   return a[0] === b;
}

function kangaxc() {
   return "iterator" in Symbol;
}

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true, "kangaxa(hasInstance)" );

console.log( "   kangaxc()");
assert.equal( kangaxc(), true, "kangaxc(iterator)" );


