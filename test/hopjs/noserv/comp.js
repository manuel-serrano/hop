/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/noserv/comp.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jun  9 07:48:11 2017                          */
/*    Last change :  Fri Jun  9 07:49:59 2017 (serrano)                */
/*    Copyright   :  2017 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Compilation and optimization tests                               */
/*=====================================================================*/
var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    tyflow ...                                                       */
/*---------------------------------------------------------------------*/
function tyflow( x ) {
   function bar() {
      x = 3;
   }

   if( typeof x == "string" ) {
      bar();
      return (typeof x == "number");
   } else {
      return false;
   }
}

assert.ok( tyflow( "foo" ), "tyflow" );
      
