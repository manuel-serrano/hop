/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/comp.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Jun  9 07:48:11 2017                          */
/*    Last change :  Thu May  3 07:03:44 2018 (serrano)                */
/*    Copyright   :  2017-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Compilation and optimization tests                               */
/*=====================================================================*/
const assert = require( "assert" );

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

/*---------------------------------------------------------------------*/
/*    rettype ...                                                      */
/*---------------------------------------------------------------------*/
function rettype( x ) {

   while( x != null ) {
      if( x.car == 1 ) {
	 return x.car;
      } else {
	 x = x.cdr;
      }
   }

   zzz = x;
   return typeof x != "object";
}

assert.ok( rettype( { car: 10, cdr: undefined } ), "return typing" );
	 
