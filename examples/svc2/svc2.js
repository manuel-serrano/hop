/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc2/svc2.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Sat Nov 28 09:03:16 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services declarations.            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc2.js                                           */
/*    browser: http://localhost:8080/hop/svc2                          */
/*=====================================================================*/
require( "./extern.js" );

service svc2() {
   return <html>
      <button onclick=~{
	 ${dummy}( { b: 22 } )
	    .post( function( r ) {
	       document.body.appendChild(
		  <table>
		    ${r.map( function( e ) {
		       return <tr><th>${ e.head }</th><td>${ e.data }</td></tr>
		    } )}
		  </table>
	       ) } ) }>
	 add
       </button>
   </html>;
}

service dummy();

console.log( "Go to \"http://%s:%d/hop/svc2\"", hop.hostname, hop.port );
