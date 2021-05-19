/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopjs/serv/asyncSrvRsp2.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar  1 07:13:49 2016                          */
/*    Last change :  Wed May 19 07:17:13 2021 (serrano)                */
/*    Copyright   :  2016-21 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Testing asynchronous responses                                   */
/*=====================================================================*/
"use hopscript";

const assert = require( "assert" );

service bar( arg ) {
   console.log( ">>> bar ", arg );
   return new Promise( (resolve, reject) => {
      for( let i = 0; i < 1000000; i++ ) {
	 ;
      }
      console.log( "<<< bar resolve ", arg );
      resolve( arg );
   } )
}

function test() {
   let pending = 9;

   function call( i ) {
      bar( i ).post()
	 .then( function( v ) {
	    console.log( "--- bar received ", v );
	    pending -= 1;
	    if( pending == 0 ) process.exit( 0 );
	 } )
	 .catch( function( e ) {
	    console.error( "error=", e );
	    pending -= 1;
	    if( pending == 0 ) process.exit( 0 );
	 } );
   }
   
   for( let i = 0; i <= 5; i++ ) {
      call( i );
   }
   setTimeout( function() {
      for( let i = 6; i <= 9; i++ ) {
	 call( i );
      }
   }, 3000 );
}

test();
