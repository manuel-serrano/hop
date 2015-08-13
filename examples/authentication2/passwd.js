var hop = require( "hop" );
var user = require( hop.user );
var config = require( hop.config );

user.add( {
   name: "anonymous",
   serivces: ["s_public"]
} );

user.add( {
   name: "foo",
   password: "+45fae6f23632b1d2ffe7e7aeab32c333",
   services: ["foo", "bar"],
   directories: ["tmp"]
} )
   
