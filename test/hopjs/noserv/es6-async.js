/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-async.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May 15 11:33:27 2018                          */
/*    Last change :  Fri Aug 17 07:51:41 2018 (serrano)                */
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

/*---------------------------------------------------------------------*/
/*    await                                                            */
/*---------------------------------------------------------------------*/
function resolveAfter2Seconds( x ) {
   return new Promise( resolve => {
      setTimeout( () => { resolve( x ) }, 200 );
   } );
}

async function asyncAwait( x ) {
   var a = resolveAfter2Seconds( 20 );
   return await a + 20;
}

async function asyncYield( x ) {
   var a = resolveAfter2Seconds( 20 );
   return yield a + 20;
}

asyncAwait( 10 ).then( v => { assert.ok( v, 40, "await parsing" ) } );
asyncYield( 10 ).then( v => { assert.ok( typeof v, "string", "yield parsing" ) } );

