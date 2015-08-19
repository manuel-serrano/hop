/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/async/async.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed Aug 19 13:49:48 2015 (serrano)                */
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
   return new Promise( function( resolve, reject ) {
      fs.readFile( async.resource( "async.js" ), "ascii",
                   function( err, data ) {
                      resolve( <html>
			<head css=${fontifier.css}/>
			<pre class="fontifier-prog">
${fontifier.hopscript( data )}
			</pre>
		      </html> ) } );
      } );
}

console.log( 'Go to "http://%s:%d/hop/async"', hop.hostname, hop.port );
