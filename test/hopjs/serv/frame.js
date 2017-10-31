/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/serv/frame.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 09:36:15 2015                          */
/*    Last change :  Mon Oct 30 17:25:13 2017 (serrano)                */
/*    Copyright   :  2015-17 Manuel Serrano                            */
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

// testing frame arguments evaluation time
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
   if( hop.compilerDriver.pending > 0 ) {
      hop.compilerDriver.addEventListener( "all", function( e ) {
	 assert.ok( res === 2, "res" );
	 process.exit( 0 );
      } );
   } else {
      assert.ok( res === 2, "res" );
      process.exit( res === 2 ? 0 : 1 );
   }
}, 1000 );
  





