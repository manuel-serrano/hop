/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/test/hopjs/serialization.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jan 11 18:14:33 2015                          */
/*    Last change :  Sun Jan 11 20:57:56 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing server-to-server serialization                           */
/*=====================================================================*/
var assert = require( "assert" );
var res = 0;

function doTest( val ) {
   console.log( "doTest ------ " );
   val.forEach( function( el ) {
      console.log( "el=", el );
      assert.ok( (el instanceof Array) && el.length == 2 );
      assert.equal( el[ 0 ].valueOf(), el[ 1 ] );
      
      res += (el[ 0 ].valueOf() === el[ 1 ]) ? 1 : 0;
   } );
}
 		
service serv( val ) {
   doTest( val );
   return val;
}

function test() {
   var dt = new Date();
   var re = new RegExp( "[az]" );
   
   serv( [ [ new Number( 0 ), 0 ],
	      [ new Boolean( true ), true ],
	      [ new Boolean( false ), false ],
	      [ new String( "foobar" ), "foobar" ],
	      [ dt, dt.valueOf() ],
	      [ re, re.valueOf() ] ] ).post( doTest );
}

setTimeout( function() {
   assert.ok( res === 12 );
   process.exit( res === 12 );
}, 100 );

test();


