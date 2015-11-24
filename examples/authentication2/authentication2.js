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

var optAnonymous = { server: new hop.Server( "localhost", port ) };
var optAuthenticated = { server: new hop.Server( "localhost", port ),
			 user: "foo",
			 password: "bar" };

//  result handler for asynchronous service calls
function resultHandler( result ) {
   console.log( 'Asynchronous call', result );
}

//  error handler (added to options) for asynchrounous service calls
function addErrorHandler ( options, message ) {
   var opt = {};
   for (var key in options) {
      opt[key] = options[key];
   }
   opt.fail = function( error ) {
      console.log( 'asynchronous call: %s, connection refused:', message );
   };
   return opt;
}

function connect() {

   /* accepted connection */
   try {
      console.log( publicOnly( "no password" ).postSync( optAnonymous ) );
   } catch( err ) {
      console.log( "publicOnly, no password, connection refused: ", err )
   }

   /* refused connection */
   try {
      console.log( publicOnly( "password" ).postSync( optAuthenticated ) );
   } catch( err ) {
      console.log( "publicOnly, with password, connection refused: ", err );
   }

   /* accepted connection */
   try {
      console.log( protectedOnly( "password" ).postSync( optAuthenticated ) );
   } catch( err ) {
      console.log( "protectedOnly, with password, connection refused: ", err );
   }
   
   /* refused connection */
   try {
      console.log( protectedOnly( "no password" ).postSync( optAnonymous ) );
   } catch( err ) {
      console.log( "protectedOnly, no password, connection refused: ", err )
   }

   /* accepted connection */
   try {
      console.log( publicOrProtected( "no password" ).postSync( optAnonymous ) );
   } catch( err ) {
      console.log( "publicOrProtected, no password, connection refused: ", err )
   }

   /* accepted connection */
   try {
      console.log( publicOrProtected( "password" ).postSync( optAuthenticated ) );
   } catch( err ) {
      console.log( "publicOrProtected, with password, connection refused: ", err );
   }

   /* asynchronous calls */
   publicOnly( "no password" ).post( resultHandler, optAnonymous );
   publicOnly( "password" ).post(
      resultHandler,
      addErrorHandler( optAuthenticated, 'publicOnly with password' ) );
   protectedOnly( "no password" ).post(
      resultHandler,
      addErrorHandler( optAnonymous, 'protectedOnly without password' ) );
   protectedOnly( "password" ).post( resultHandler, optAuthenticated );
   publicOrProtected( "no password" ).post( resultHandler, optAnonymous );
   publicOrProtected( "password" ).post( resultHandler, optAuthenticated );
   
}

setTimeout( connect, 1000 );
