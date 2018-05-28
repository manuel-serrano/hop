/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/optim.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Mon May 28 16:53:47 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Optimization testing (i.e., optimizations that were wrong).      */
/*=====================================================================*/
"use strict";

const assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    bool ...                                                         */
/*---------------------------------------------------------------------*/
function bool( o ) {
   return o || false;
}

assert.ok( bool( undefined ) === false );
