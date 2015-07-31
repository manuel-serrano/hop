/*=====================================================================*/
/*    .../hop/3.0.x/examples/authentication2/authentication2.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:35 2014                          */
/*    Last change :  Fri Jul 31 16:12:54 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    show how to authenticate server-to-server requests               */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 remote.js --rc-file passwd.json                 */
/*         hop -- authentication2.js 9999                              */
/*=====================================================================*/
var hop = require( "hop" );

import service s_public();
import service s_private();

var port = parseInt( process.argv[ process.argv.length - 1 ] );

function connect() {
   /* accepted connection */
   s_public( "no password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      port: port,
      fail: function( err ) {
	 console.log( "public, no password, connection refused: ", err )
      } } );

   s_public( "password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      user: "foo",
      password: "foo",
      port: port,
      fail: function( err ) {
	 console.log( "public, with password, connection refused: ", err )
      } } );

   /* accepted connection */
   s_private( "password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      port: port,
      user: "foo",
      password: "foo",
      fail: function( err ) {
	 console.log( "private, with password, connection refused: ", err )
      } } );

   /* refused connection */
   s_private( "no password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      port: port,
      fail: function( err ) {
	 console.log( "private, no password, connection refused: ", err )
      } } );
}

setTimeout( connect, 1000 );
