/*=====================================================================*/
/*    .../prgm/project/hop/3.0.x/test/hopjs/serv/authentication.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  7 15:00:00 2015                          */
/*    Last change :  Tue Sep  8 17:39:16 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing the user module                                          */
/*=====================================================================*/

console.log( 'loading test module authentication.js' );

var hop = require( 'hop' );
var assert = require( 'assert' );

service foo( who ) {
   console.log( 'service foo, user %s', who );
   console.log( 'header', this.header.authorization );
   return who;
}

service bar() {
   console.log( 'service bar' );
   return true;
}

function launchTest() {
   foo( 'Bob' ).post( function( result ) {
      assert.equal( result, 'Bob' );
      console.log( 'PASS: Bob is authenticated' );
      proceedWithBadPassword();
   }, { fail: function( error ) {
      console.log( 'FAIL: Bob should have got access to the foo service' );
      process.exit( 1 );
   }, user: 'Bob', password: 'secret' } );
}

function proceedWithBadPassword() {
   foo( 'Bob' ).post( function( result ) {
      console.log( 'FAIL: Bob is given access to foo with a bad password' );
      process.exit( 1 );
   }, { fail: function( error ) {
      console.log( 'PASS: (Bob, wrong) is blocked access to the foo service' );
      proceedWithBadService()
   }, user: 'Bob', password: 'wrong' } );
}

function proceedWithBadService() {
   bar( 'Bob' ).post( function( result ) {
      console.log( 'FAIL: Bob should not get access to the bar service' );
      process.exit( 1 );
   }, { fail: function( error ) {
      console.log( 'PASS: Bob is not allowed to access the bar service' );
      proceedWithAlice();
   }, user: 'Bob', password: 'secret' } );
}

function proceedWithAlice() {
   foo( 'Alice' ).post( function( result ) {
      console.log( 'FAIL: Alice should not authenticate to service foo' );
      process.exit( 1 );
   }, { fail: function( error ) {
      console.log( 'PASS: Alice request is rejected' );
      try {
	 user.add( {name: 'Alice', password: '+fa5bf62b74f948ccb631233c9d30e192', services: '*', directories: '*' } );
      }
      catch( e ) {
	 console.log( 'PASS: not authorized to register user once the server is running', e );
      };
      foo( 'Alice' ).post( function( result ) {
	 console.log( 'FAIL: late registration should not be honored' );
	 process.exit( 1 );
      }, { fail: function( error ) {
	 console.log( 'PASS: late registration ignored' );
	 process.exit( 0 );
      }, user: 'Alice', password: 'Alice' });
   }, user: 'Alice', password: 'Alice' } );
}

launchTest();

console.log( 'authentication.js loaded' );
