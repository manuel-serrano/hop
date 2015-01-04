/*=====================================================================*/
/*    .../hop/3.0.x/examples/authentication2/authentication2.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:35 2014                          */
/*    Last change :  Sun Dec 21 11:09:07 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    show how to authenticate server-to-server requests               */
/*    -------------------------------------------------------------    */
/*    run: hop -p 9999 remote.js --rc-file passwd.hop                  */
/*         hop -- authentication2.js 9999                              */
/*=====================================================================*/
var hop = require( "hop" );

import service public(type);
import service private(type);

var port = parseInt( process.execArgv[ process.execArgv.length - 1 ] );

function connect() {
   /* accepted connection */
   public( "no password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      port: port,
      fail: function( err ) {
	 console.log( "public, no password, connection refused: ", err )
      } } );

   public( "password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      user: "foo",
      password: "foo",
      port: port,
      fail: function( err ) {
	 console.log( "public, with password, connection refused: ", err )
      } } );

   /* accepted connection */
   private( "password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      port: port,
      user: "foo",
      password: "foo",
      fail: function( err ) {
	 console.log( "private, with password, connection refused: ", err )
      } } );

   /* refused connection */
   private( "no password" ).post( console.log, {
      asynchronous: false,
      host: "localhost",
      port: port,
      fail: function( err ) {
	 console.log( "private, no password, connection refused: ", err )
      } } );
}

setTimeout( connect, 1000 );
