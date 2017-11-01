/*=====================================================================*/
/*    .../project/hop/3.2.x/test/hopjs/serv/unregisterService.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  4 18:43:00 2015                          */
/*    Last change :  Wed Nov  1 07:49:58 2017 (serrano)                */
/*    Copyright   :  2015-17 Inria                                     */
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
      console.log( 'error, service still active: ', result );
      process.exit( 1 );
   },
		   function( error ) {
		      // console.log( 'ok, no service' );
		      process.exit( 0 );
		   } );
},
	    function( error ){
	       console.log( 'error, service not running: ', error );
	       process.exit( 2 );
	    }
	  );
	    
