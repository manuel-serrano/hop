/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/optim.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Mon Jun  4 18:55:56 2018 (serrano)                */
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

/*---------------------------------------------------------------------*/
/*    assignment expressions                                           */
/*---------------------------------------------------------------------*/
var oscTypeCodes = { 'f': { rep : 'float' } };

function bug( code ) {
   var ref;

   return (ref = oscTypeCodes[code]) != null ? ref.rep : void 0;
}

assert.equal( bug( "f" ), "float" );
