/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/async/async.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Wed May 17 13:59:22 2017 (serrano)                */
/*    Copyright   :  2014-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Simple example showing asynchronous response                     */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g asvc.js                                           */
/*    browser: http://localhost:8080/hop/asvc                          */
/*=====================================================================*/
var fs = require( "fs" );
var fontifier = require( hop.fontifier );

service asvc() {
   return new Promise( function( resolve, reject ) {
      fs.readFile( asvc.resource( "asvc.js" ), "ascii",
                   function( err, data ) {
                      resolve( <html>
			<head css=${fontifier.css}/>
			<pre class="fontifier-prog">
${fontifier.hopscript( data )}
			</pre>
		      </html> ) } );
      } );
}

console.log( 'Go to "http://%s:%d/hop/asvc"', hop.hostname, hop.port );
