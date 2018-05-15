/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-async.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May 15 11:33:27 2018                          */
/*    Last change :  Tue May 15 11:35:03 2018 (serrano)                */
/*    Copyright   :  2018 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing ECMAScript 1.6 async functions                           */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

/*---------------------------------------------------------------------*/
/*    async keyword                                                    */
/*---------------------------------------------------------------------*/
function bg() {
   return new Promise( function( resolve, reject ) {
      setTimeout( function() { resolve( 10 ) }, 0 );
   } );
}

async function async() {
   var async = await bg();

   return async == 10;
}

assert.ok( async(), "async" );
