/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirew/requirew.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Dec 20 10:00:23 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    URL based require example                                        */
/*    -------------------------------------------------------------    */
/*    run: hop -g requirew.js                                          */
/*    browser: http://localhost:8080/hop/requirew                      */
/*=====================================================================*/
var util = require( "util" );
var hop = require( "hop" );

var url = util.format( "http://%s:%d/hop/providerGetModule?file=foo.js",
		       hop.hostname, hop.port );

var w = new Worker( "./provider.js" );

service requirew() {
   return hop.HTTPResponseAsync(
      function( reply ) {
	 setTimeout( function() {
	    var imp = require( url );
	    reply( imp.hello() );
	 }, 1000 );
      }, this );
}

console.log( "Go to \"http://%s:%d/hop/requirew\"", hop.hostname, hop.port );
