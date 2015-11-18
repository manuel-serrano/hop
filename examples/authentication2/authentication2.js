/*=====================================================================*/
/*    .../hop/3.0.x/examples/authentication2/authentication2.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:35 2014                          */
/*    Last change :  Wed Nov 18 10:22:08 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    show how to authenticate server-to-server requests               */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 remote.js --rc-file passwd.js                   */
/*         hop -- authentication2.js 9999                              */
/*=====================================================================*/
import service s_public();
import service s_private();

var port = parseInt( process.argv[ process.argv.length - 1 ] );

var opt = { server: new hop.Server( "localhost", port ) };

function connect() {
   /* accepted connection */
   try {
      console.log( s_public( "no password" ).postSync( opt ) );
   } catch( err ) {
      console.log( "public, no password, connection refused: ", err )
   }

   try {
      console.log( s_public( "password" ).postSync( opt, {
	 user: "foo",
	 password: "foo"
      } ) );
   } catch( err ) {
      console.log( "public, with password, connection refused: ", err );
   }

   /* accepted connection */
   try {
      console.log( s_private( "password" ).postSync( opt, {
	 user: "foo",
	 password: "foo"
      } ) );
   } catch( err ) {
      console.log( "private, with password, connection refused: ", err );
   }
   
   /* refused connection */
   try {
      console.log( s_private( "no password" ).postSync( opt ) );
   } catch( err ) {
      console.log( "private, no password, connection refused: ", err )
   }
}

setTimeout( connect, 1000 );
