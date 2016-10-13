/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/frame.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 09:36:15 2015                          */
/*    Last change :  Thu Oct 13 08:06:05 2016 (serrano)                */
/*    Copyright   :  2015-16 Manuel Serrano                            */
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
   assert.ok( v == 3 );
   res++;
   
   o.a = 4;
   f.post( function( v ) {
      assert.ok( v == 6 );
      res++;
   } )
} );
   
setTimeout( function() {
   try {
      assert.ok( res === 2 );
   } finally {
      process.exit( res === 2 ? 0 : 1 );
   }
}, 500 );
  





