/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serv/frame.js          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Nov 26 09:36:15 2015                          */
/*    Last change :  Thu Nov 26 09:49:40 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing HopFrames                                                */
/*=====================================================================*/
"use hopscript";

var assert = require( "assert" );

service foo( o ) {
   return o.a + o.b;
}

var o = { a: 1, b: 2 };

// testing frame arguments evaluation time
var f = foo( o );

v.post( function( v ) {
   assert.ok( v == 3 );
   o.a = 4;
   v.post( function( v ) {
      assert.ok( v == 6 );
   } )
} );
   
	   
   





