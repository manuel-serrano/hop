/* TODO :
   fix rc-file / server initialization order.
   then replace this module by require( ./http.js ) */

var hop = require( 'hop' );
var config = require( hop.config );

service foo( arg ) {
   return arg ;
}

foo(1).post( function( result ) {
   console.log( 'ok' );
}, { ssl: true } );

console.log( 'http: %s, https: %s', config.HTTPPort, config.HTTPSPort );
