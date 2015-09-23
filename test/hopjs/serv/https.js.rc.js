var hop = require( 'hop' );
var user = require( hop.user );
var config = require( hop.config );
var path = require( 'path' );
user.add( { name: 'anonymous', services: '*', directories: [] } );
config.HTTPSPrivateKey = path.dirname( module.filename ) + '/https/key.pem';
config.HTTPSCertificate = path.dirname( module.filename ) + '/https/cert.pem';
config.HTTPSPort = 8001;

console.log( config.HTTPSPrivateKey );
console.log( 'the rc file is loaded before the server starts ...' );
