/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker4/worker4.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Thu Nov  6 14:12:03 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Services in workers example                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker3.js                                        */
/*    browser: http://localhost:8080/hop/worker3                       */
/*=====================================================================*/

var hop = require( "hop" );
var w = new Worker( "./slave.js" );

service worker4() {
   var count = <SPAN> { "-" };
   
   return <HTML> {
      <DIV> {
	 "counter=", count,
      },
      <BUTTON> {
	 onclick: ~{
	    ${counter}()
	       .post( function( v ) { ${count}.innerHTML = v } )
	 },
	 "inc me"
      }
   }
}

service counter() {
   return hop.HTTPResponseAsync(
      function( reply )  {
	 w.postMessage( reply );
      } );
}
