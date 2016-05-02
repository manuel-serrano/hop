/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/reactsrv/reactsrv.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Fri Apr 29 19:29:19 2016 (serrano)                */
/*    Copyright   :  2014-16 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Reactive programming with Hop.js                                 */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g reactsrv.js                                       */
/*    browser: http://localhost:8080/hop/reactsrv                      */
/*=====================================================================*/
var clients = [];

service reactsrv() {
   clients.push( clients.length + ": " + Date() );
   hop.broadcast( "clients", clients );
   return <html>
     ~{ var conn = server.reactProxy( "clients", [] ); }
     <div># clients:
       <ol>
	 <react>~{ conn.value.map( function( c ) {
	    return <li>${c}</li>
	 } ) }
	 </react>
       </ol>
     </div>
   </html>
}

console.log( "Go to \"http://%s:%d/hop/reactsrv\"", hop.hostname, hop.port );
