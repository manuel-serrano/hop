/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/async/async.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Fri Jul 31 15:36:45 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Simple example showing asynchronous response                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g async.js                                          */
/*    browser: http://localhost:8080/hop/async                         */
/*=====================================================================*/
var hop = require( "hop" );
var fs = require( "fs" );
var fontifier = require( hop.fontifier );

service async() {
   return hop.HTTPResponseAsync(
      function( sendResponse ) {
         fs.readFile( async.resource( "async.js" ), "ascii",
                      function( err, data ) {
                         sendResponse( <html>
				         <head css=${fontifier.css}/>
			                 <pre class="fontifier-prog">
${fontifier.hop( data )}
				         </pre>
				       </html> )
		      } );
      }, this );
}

console.log( 'Go to "http://%s:%d/hop/async"', hop.hostname, hop.port );
