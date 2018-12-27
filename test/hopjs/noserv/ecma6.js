/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/ecma6.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Thu Dec 27 17:42:05 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing basic ECMA 262, 6 features                               */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    misc                                                             */
/*---------------------------------------------------------------------*/
function misca() {
   return !new.target;
}

function miscb() {
   let res = false;
   
   function ctor() {
      res = new.target;
   }
   
   new ctor();
   
   return res;
}

function miscc() {
   let res = false;
   
   function aux() {
      return !new.target;
   }
   
   function ctor() {
      res = aux();
   }
   
   new ctor();
   
   return res;
}

console.log( "misc" );
console.log( "   misca()"); assert.ok( misca(), "misca" );
console.log( "   miscb()"); assert.ok( miscb(), "miscb" );
console.log( "   miscc()"); assert.ok( miscc(), "miscc" );
