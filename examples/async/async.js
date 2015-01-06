/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/async/async.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Mon Jan  5 17:35:37 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
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
      function( sendResponse ) {
         fs.readFile( async.resource( "async.js" ), "ascii",
                      function( err, data ) {
                         sendResponse( <HTML> { <PRE> { data } } )
                      } );
      },
   this );
}

console.log( "Go to \"http://%s:%d/hop/async\"", hop.hostname, hop.port );
