/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/noserv/optim.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Thu Jul 25 06:39:08 2019 (serrano)                */
/*    Copyright   :  2014-20 Manuel Serrano                            */
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

/*---------------------------------------------------------------------*/
/*    Unboxed CTOR                                                     */
/*    -------------------------------------------------------------    */
/*    git: 907392f7964b6985cab146f98f5e0a0ab320702f                    */
/*    CTOR uses unboxed arguments.                                     */
/*---------------------------------------------------------------------*/
function unboxedCTOR() {
   
   function CTOR( a, b, c, d ) {
      this.a = a;
      this.b = b;
      this.c = c;
      this.d = d;
      this.a2 = a;
      this.b2 = b;
      this.c2 = c;
      this.d2 = d;
   }
   
   for( let i = 0; i < 10; i++ ) {
      let III = i;
      o = new CTOR( III, 20, 0, 1 );
   }
   
   return o.a === 9 && o.d === 1;
}

assert.ok( unboxedCTOR, "constructors with unboxed arguments" );
   
/*---------------------------------------------------------------------*/
/*    glop prop on double assignments                                  */
/*    up to commit: f2a84ad64a872426c1ddd955b769863ae96978fe           */
/*---------------------------------------------------------------------*/
function moment() {
   return hookCallback.apply(null, arguments);
}

var momentProperties = (moment.momentProperties = []);

assert.ok( momentProperties.length === 0, "globprop" );
