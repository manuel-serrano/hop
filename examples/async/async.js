/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/async/async.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed Dec 17 12:00:02 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Simple example showing asynchronous response                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g async.js                                          */
/*    browser: http://localhost:8080/hop/async                         */
/*=====================================================================*/
var hop = require( "hop" );
var fs = require( "fs" );

service async() {
   return hop.HTTPResponseAsync(
      function( reply ) {
         fs.readFile( async.resource( "async.js" ), "ascii",
                      function( err, data ) {
                         reply( <HTML> { <PRE> { data } } )
                      } );
      },
   this );
}

console.log( "Go to \"http://%s:%d/hop/async\"", hop.hostname, hop.port );
