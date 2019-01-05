/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/regexp.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Sat Jan  5 06:27:57 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing REGEXP matching                                          */
/*=====================================================================*/
var assert = require( "assert" );

assert.strictEqual( "toto\\tutu".match( /toto\\tutu/ )[ 0 ], "toto\\tutu" );
assert.ok( /[\uD800-\uDBFF]/.test( 'foo' ) ? true: true );

/*---------------------------------------------------------------------*/
/*    exec properties                                                  */
/*---------------------------------------------------------------------*/
function execProps() {
   const r = /a(b)c/.exec( "abc" );
		  
   for( let k in r ) {
      if( k !== "length" ) {
      	 const p = Object.getOwnPropertyDescriptor( r, k );
		
      	 if( !(p.writable && p.enumerable && p.configurable) ) return false;
      }
   }
		  
   return true;
}

assert.ok( execProps(), "exec result properties" );
