var hop = require( 'hop' );
var user = require( hop.user );

// Bob is declared on the server
user.add( {name: 'Bob', password: ( user.encryptPassword( 'Bob', 'secret') ), services: [ 'foo' ], directories: '*' } );
// Alice is not declared on the server
//user.add( {name: 'Alice', password: ( user.encryptPassword( 'Alice', 'Alice' ), services: '*', directories: '*' } );
// anonymous must be declared to handle authentication errors, otherwise we get a server side error while handling Alice request.
user.add( { name: 'anonymous', services: [], directories: [] } );

