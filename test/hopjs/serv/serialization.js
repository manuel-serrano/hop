/*=====================================================================*/
/*    .../prgm/project/hop/3.1.x/test/hopjs/serv/serialization.js      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jan 11 18:14:33 2015                          */
/*    Last change :  Sat Feb  4 10:23:58 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing server-to-server serialization                           */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );
var res = 0;

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
   var dt = new Date();
   var re = new RegExp( "[az]" );
   var buf = new Buffer( "toto n'est pas content" );
   var md5 = hop.md5sum( buf.toString() );

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


