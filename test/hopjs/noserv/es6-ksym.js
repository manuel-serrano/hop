/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/es6-ksym.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Nov 26 20:00:49 2017                          */
/*    Last change :  Sun Nov 26 20:01:43 2017 (serrano)                */
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

console.log( "kangax" );

console.log( "   kangaxa()");
assert.equal( kangaxa(), true, "kangaxa(hasInstance)" );


