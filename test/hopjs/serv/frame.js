/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/serv/frame.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 09:36:15 2015                          */
/*    Last change :  Tue Jun  5 09:48:37 2018 (serrano)                */
/*    Copyright   :  2015-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing HopFrames                                                */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );
var srv = new hop.Server();
var res = 0;

service foo( o ) {
   return o.a + o.b;
}

var o = { a: 1, b: 2 };

var f = foo.call( srv, o );

f.post( function( v ) {
   assert.ok( v == 3, "post.1" );
   res++;

   if( ++res === 2 ) process.exit( 0 );
   
   o.a = 4;
   f.post( function( v ) {
      assert.ok( v == 6, "post.2" );
      if( ++res === 2 ) process.exit( 0 );
   } )
} );
   
setTimeout( function() {
   try {
      if( hop.compilerDriver.pending > 0 ) {
	 hop.compilerDriver.addEventListener( "all", function( e ) {
	    assert.ok( res === 2, "res (after compile)" );
	 } );
      } else {
	 assert.ok( res === 2, "res" );
      }
   } finally {
      process.exit( res === 2 ? 0 : 1 );
   }
}, 1500 );
  





