/*=====================================================================*/
/*    .../prgm/project/hop/hop/examples/redirection/redirection.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Mar 20 06:36:35 2021                          */
/*    Last change :                                                    */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    basic example that shows HTTP redirections                       */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g file.js                                           */
/*    browser: http://localhost:8080/hop/redirection                   */
/*=====================================================================*/
service redirection( { x } ) {
   console.log( `${x} http://${this.host}:${this.port}${target( x || 23 )}` );
   return hop.HTTPResponseString( "",
      {
	 startLine: "HTTP/1.0 307 See Other",
	 header: { "location": `http://${this.host}:${this.port}${target( x || 23 )}` }
      } );
}

service target( x ) {
   return <html>
     I am the final destination: ${x}
   </html>
}
					  
console.log( "Go to \"http://%s:%d/hop/redirection?x=23\"", hop.hostname, hop.port );
