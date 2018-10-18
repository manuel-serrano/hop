var hop = require( 'hop' );
var config = require( hop.config );

console.log( 'http: %s, https: %s', config.HTTPPort, config.HTTPSPort );
require( './http.js' );

