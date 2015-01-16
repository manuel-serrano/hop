/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/regexp.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Mon Dec 22 06:15:09 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing REGEXP matching                                          */
/*=====================================================================*/
var assert = require( "assert" );

assert.strictEqual( "toto\\tutu".match( /toto\\tutu/ )[ 0 ], "toto\\tutu" );
