/*=====================================================================*/
/*    authentication.js                                                */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Mon Sep  7 15:00:00 2015                          */
/*    Last change :  Mon Sep  7 15:00:00 2015                          */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Testing the user module                                          */
/*=====================================================================*/

console.log( 'loading test module authentication.js' );

var hop = require( 'hop' );
var assert = require( 'assert' );

var user = require( hop.user );

// Bob is declared on the server
user.add( {name: 'Bob', password: '+8cb2e8c7c9ea3edaad2ba50487eeec30', services: '*', directories: '*' } );
// Alice is not declared on the server
//user.add( {name: 'Alice', password: '+fa5bf62b74f948ccb631233c9d30e192', services: '*', directories: '*' } );
// anonymous must be declared to handle authentication errors, otherwise we get a server side error while handling Alice request.
user.add( { name: 'anonymous', services: [], directories: [] } );

service foo( who ) {
   console.log( 'service foo, user %s', who );
   console.log( 'header', this.header.authorization );
   return who;
}


foo( 'Bob' ).post( function( result ) {
   assert.equal( result, 'Bob' );
   console.log( 'PASS: Bob is authenticated' );
}, { fail: function( error ) {
   console.log( 'FAIL: Bob should have got access to the foo service' );
   process.exit( 1 );
}, user: 'Bob', password: 'secret' } );

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
      process.exit( 0 );
   };
   foo( 'Alice' ).post( function( result ) {
      console.log( 'FAIL: late registration should not be honored' );
      process.exit( 1 );
   }, { fail: function( error ) {
      console.log( 'PASS: late registration ignored' );
      process.exit( 0 );
   }, user: 'Alice', password: 'Alice' });
}, user: 'Alice', password: 'Alice' } );


console.log( 'authentication.js loaded' );
