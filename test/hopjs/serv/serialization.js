/*=====================================================================*/
/*    .../prgm/project/hop/hop/test/hopjs/serv/serialization.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jan 11 18:14:33 2015                          */
/*    Last change :  Thu May  6 16:21:47 2021 (serrano)                */
/*    Copyright   :  2015-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing server-to-server serialization                           */
/*=====================================================================*/
"use hopscript";

const assert = require( "assert" );
let res = 0;

function doBufTest( buf, md5 ) {
   assert.ok( hop.md5sum( buf.toString() ) === md5 );
   res++;
}

function doTest( val ) {
   console.log( "doTest ------ " );
   val.forEach( function( el, idx=undefined, arr=undefined ) {
      assert.ok( (el instanceof Array) && el.length == 2 );
      assert.equal( el[ 0 ].valueOf(), el[ 1 ] );

      res++;
   } );
}

service serv( val ) {
   doTest( val );
   return val;
}

service servbuf( buf, md5 ) {
   doBufTest( buf, md5 );
   return buf;
}

function test() {
   const dt = new Date();
   const re = new RegExp( "[az]" );
   const buf = new Buffer( "toto n'est pas content" );
   const md5 = hop.md5sum( buf.toString() );

   serv( [
      [ new Number( 0 ), 0 ],
      [ new Number( 4.5 ), 4.5 ],
      [ new Boolean( true ), true ],
      [ new Boolean( false ), false ],
      [ new String( "foobar" ), "foobar" ],
      [ dt, dt.valueOf() ],
      [ re, re.toString() ]
   ] ).post( doTest );

   servbuf( buf, md5 ).post( function(v) { doBufTest( v, md5 ) } );
}

setTimeout( function() {
   try { 
      assert.ok( res === 16, "not all tests executed" );
   } finally {
      process.exit( 0 );
   }
}, 100 );

test();


