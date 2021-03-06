/*=====================================================================*/
/*    .../prgm/project/hop/3.2.x/test/hopjs/noserv/es6-async.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue May 15 11:33:27 2018                          */
/*    Last change :  Fri Aug 24 05:48:11 2018 (serrano)                */
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

/*---------------------------------------------------------------------*/
/*    this                                                             */
/*---------------------------------------------------------------------*/
function asyncCtor() {
   this.myvar = "hello";
}

asyncCtor.prototype.test = async function() {
   assert.ok( this.myvar === "hello", "async this" );
   return this.myvar;
} 

new asyncCtor().test();

function asyncCtorReturn( x ) {
   if( x > 10 ) return;
   this.myvar = "hello";
}

asyncCtorReturn.prototype.test = async function() {
   assert.ok( this.myvar === "hello", "async this with return" );
   return this.myvar;
} 

new asyncCtorReturn().test();
