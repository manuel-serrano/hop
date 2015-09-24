var hop = require( 'hop' );
var user = require( hop.user );
var config = require( hop.config );
var path = require( 'path' );
user.add( { name: 'anonymous', services: '*', directories: [] } );

console.log( 'key:', config.HTTPSPrivateKey );
console.log( 'cert:', config.HTTPSCertificate );
console.log( 'HTTPSPort', config.HTTPSPort );
console.log( 'HTTPPort', config.HTTPPort );
