var mod2 = require( "./mod2.js" );

var s = "";

s += "hello";

exports.hello = function( x ) {
   return mod2.hello( s );
}
   
