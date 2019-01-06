/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/test/hopjs/noserv/regexp.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Sep 27 10:27:29 2014                          */
/*    Last change :  Sat Jan  5 10:37:36 2019 (serrano)                */
/*    Copyright   :  2014-19 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing REGEXP matching                                          */
/*=====================================================================*/
var assert = require( "assert" );

assert.strictEqual( "toto\\tutu".match( /toto\\tutu/ )[ 0 ], "toto\\tutu" );
assert.ok( /[\uD800-\uDBFF]/.test( 'foo' ) ? true: true );

/*---------------------------------------------------------------------*/
/*    test                                                             */
/*---------------------------------------------------------------------*/
function rxTest( rx ) {
   return rx.test( "foobar" );
}

assert.ok( rxTest( /[bc]/ ), "rxText literal" );
assert.ok( rxTest( new RegExp( "[bc]" ), "rxText dynamic" ) );

/*---------------------------------------------------------------------*/
/*    exec ...                                                         */
/*---------------------------------------------------------------------*/
function rxExecMDN() {
   var res = "";
   var myRe = /ab*/g;
   var str = 'abbcdefabh';
   var myArray;
   while ((myArray = myRe.exec(str)) !== null) {
      var msg = 'Found ' + myArray[0] + '. ';
      msg += 'Next match starts at ' + myRe.lastIndex;
      res += msg;
   }
   
   return res === 
	     "Found abb. Next match starts at 3Found ab. Next match starts at 9";
}

function rxExecProps() {
   const r = /a(b)c/.exec( "abc" );
		  
   for( let k in r ) {
      if( k !== "length" ) {
      	 const p = Object.getOwnPropertyDescriptor( r, k );
		
      	 if( !(p.writable && p.enumerable && p.configurable) ) return false;
      }
   }
		  
   return true;
}

assert.ok( rxExecMDN(), "exec" );
assert.ok( rxExecProps(), "exec result properties" );

/*---------------------------------------------------------------------*/
/*    properties                                                       */
/*---------------------------------------------------------------------*/
function rxProperties() {
   const rx = /foo|bar/;
   
   return !Object.getOwnPropertyDescriptor( rx, "global" ) 
      && !Object.getOwnPropertyDescriptor( rx, "source" )
      && !Object.getOwnPropertyDescriptor( rx, "multiline" )
      && !Object.getOwnPropertyDescriptor( rx, "ignoreCase" )
      && Object.getOwnPropertyDescriptor( rx, "lastIndex" );
}

assert.ok( rxProperties(), "regexp properties" );


