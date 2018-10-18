/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/test/hopjs/serv/frame.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 09:36:15 2015                          */
/*    Last change :  Sun Jul  9 20:07:32 2017 (serrano)                */
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
   
   o.a = 4;
   f.post( function( v ) {
      assert.ok( v == 6, "post.2" );
      res++;
   } )
} );
   
setTimeout( function() {
   try {
      assert.ok( res === 2, "res" );
   } finally {
      process.exit( res === 2 ? 0 : 1 );
   }
}, 500 );
  





