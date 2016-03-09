/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/serv/asyncSrvRsp2.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Mar  1 07:13:49 2016                          */
/*    Last change :  Tue Mar  1 08:09:09 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing asynchronous responses                                   */
/*=====================================================================*/
"use strict";

var assert = require( "assert" );

service bar( arg ) {
   return new Promise( (resolve, reject) => {
      for( let i = 0; i < 1000000; i++ ) {
	 ;
      }
      resolve( arg );
   } )
}

function test() {
   let pending = 9;

   function call( i ) {
      console.log( ">>> i=", i );
      bar( i ).post().then( function( v ) {
	 pending -= 1;
	 console.log( "<<< i=", i, " pending=", pending );
	 if( pending == 0 ) process.exit( 0 );
      } )
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
