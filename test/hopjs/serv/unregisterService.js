/*=====================================================================*/
/*    unregisterService.js                                             */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  4 18:43:00 2015                          */
/*    Last change :  Fri Sep  4 18:43:00 2015                          */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing unregister service                                       */
/*=====================================================================*/

var assert = require( 'assert' );

service foo (){
   return true;
}

foo().post( function( result ) {
   // console.log( 'ok, service is running' );
   assert.ok( result );
   foo.unregister();
   foo().post( function( result) {
      // console.log( 'still active');
      process.exit( 1 );
   },
	       function( error ) {
		  // console.log( 'ok, no service' );
		  process.exit( 0 );
	       }
	     );
},
	    function( error ){
	       // console.log( 'service not running');
	       process.exit( 1 );
	    }
	  );
	    
