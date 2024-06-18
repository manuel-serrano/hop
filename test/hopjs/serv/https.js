import * as config from "@hop/config";

console.log( 'http: %s, https: %s', config.HTTPPort, config.HTTPSPort );
require( './http.js' );

