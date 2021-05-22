/*=====================================================================*/
/*    .../project/hop/hop/test/hopjs/serv/unregisterService.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Fri Sep  4 18:43:00 2015                          */
/*    Last change :  Sat May 22 07:08:23 2021 (serrano)                */
/*    Copyright   :  2015-21 Inria                                     */
/*    -------------------------------------------------------------    */
/*    Testing unregister service                                       */
/*=====================================================================*/
"use hopscript";

const assert = require( 'assert' );

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
	    
