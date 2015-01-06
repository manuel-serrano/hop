/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker4/worker4.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Mon Jan  5 17:36:53 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
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
      <DIV> { "counter=", count },
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
      function( sendResponse )  {
	 w.postMessage( sendResponse );
      }, this );
}

console.log( "Go to \"http://%s:%d/hop/worker4\"", hop.hostname, hop.port );
