/*=====================================================================*/
/*    .../project/hop/3.0.x/test/hopjs/serv/unregisterService.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  4 18:43:00 2015                          */
/*    Last change :  Fri Nov 27 08:14:05 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing unregister service                                       */
/*=====================================================================*/

var assert = require( 'assert' );

service testSvc() {
   return true;
}

testSvc().post( function( result ) {
   // console.log( 'ok, service is running' );
   assert.ok( result );
   testSvc.unregister();
   testSvc().post( function( result ) {
      console.log( 'still active', result );
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
	    
