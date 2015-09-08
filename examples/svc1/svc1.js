/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc1/svc1.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Colin Vidal                                       */
/*    Creation    :  Tue Sep 08 13:59:31 2015                          */
/*    Last change :  Tue Sep  8 16:48:13 2015 (serrano)                */
/*    Copyright   :  2015 Colin Vidal                                  */
/*    -------------------------------------------------------------    */
/*    Service example                                                  */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc1.js                                           */
/*    browser: http://localhost:8080/hop/svc1                          */
/*=====================================================================*/
var hop = require( "hop" );

function computeFact( n ) {
   if( n <= 1 ) {
      return n;
   } else {
      return computeFact( n - 1 ) * n;
   }
}

service fact( n ) {
   return computeFact( n );
}

service svc1() {
   var input = <input size="5"/>;
   var result = <div/>;

   return <html>
     ${input}
     <button onclick=~{
	var inputVal = ${input}.value;
	${fact}( inputVal )
	   .post( function( res ) {
	      ${result}.innerHTML = "fact(" + inputVal + ") = " + res;
	   })}>
      compute!
     </button>
     ${result}
   </html>
}

console.log( "Go to \"http://%s:%d/hop/svc1\"", hop.hostname, hop.port );
