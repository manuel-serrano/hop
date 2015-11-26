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
import service publicOnly();
import service publicOrProtected();
import service protectedOnly()

var port = parseInt( process.argv[ process.argv.length - 1 ] );

var server = new hop.Server( "localhost", port );
var auth = { user: "foo", password: "bar" };

server.publicOnly = publicOnly;
server.publicOrProtected = publicOrProtected;
server.protectedOnly = protectedOnly;

//  result handler for asynchronous service calls
function resultHandler( result ) {
   console.log( 'Asynchronous call', result );
}

//  error handler for asynchrounous service calls
function errorHandler( message ) {
   return function( error ) {
      console.log( 'asynchronous call: %s, connection refused:', message );
   };
}

function connect() {
   /* accepted connection */
   try {
      console.log( server.publicOnly( "no password" ).postSync() );
   } catch( err ) {
      console.log( "publicOnly, no password, connection refused: ", err )
   }

   /* refused connection */
   try {
      console.log( server.publicOnly( "password" ).setOptions( auth ).postSync() );
   } catch( err ) {
      console.log( "publicOnly, with password, connection refused: ", err );
   }

   /* accepted connection */
   try {
      console.log( server.protectedOnly( "password" ).setOptions( auth ).postSync() );
   } catch( err ) {
      console.log( "protectedOnly, with password, connection refused: ", err );
   }

   /* refused connection */
   try {
      console.log( server.protectedOnly( "no password" ).postSync() );
   } catch( err ) {
      console.log( "protectedOnly, no password, connection refused: ", err )
   }

   /* accepted connection */
   try {
      console.log( server.publicOrProtected( "no password" ).postSync() );
   } catch( err ) {
      console.log( "publicOrProtected, no password, connection refused: ", err )
   }

   /* accepted connection */
   try {
      console.log( server.publicOrProtected( "password" ).setOptions( auth ).postSync() );
   } catch( err ) {
      console.log( "publicOrProtected, with password, connection refused: ", err );
   }

   /* asynchronous calls */
   server.publicOnly( "no password" )
      .post( resultHandler );
   server.publicOnly( "password" )
      .setOptions( auth )
      .post( resultHandler, errorHandler( 'publicOnly with password' ) );
   server.protectedOnly( "no password" )
      .post( resultHandler, errorHandler( 'protectedOnly without password' ) );
   server.protectedOnly( "password" )
      .setOptions( auth )
      .post( resultHandler );
   server.publicOrProtected( "no password" )
      .post( resultHandler );
   server.publicOrProtected( "password" )
      .setOptions( auth )
      .post( resultHandler );
}

setTimeout( connect, 1000 );
