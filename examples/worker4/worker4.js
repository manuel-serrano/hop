/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker4/worker4.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 10:09:31 2014                          */
/*    Last change :  Fri Sep  4 12:11:47 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Services in workers example                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g worker4.js                                        */
/*    browser: http://localhost:8080/hop/worker4                       */
/*=====================================================================*/
var hop = require( "hop" );
var w = new Worker( "./slave.js" );

service worker4() {
   var count = <span>-</span>;
   
   return <html>
     <div>counter=${count}</div>
     <button onclick=~{
	${counter}()
	   .post( function( v ) { ${count}.innerHTML = v } )
     }>
        inc me (async)
     </button>
     <button onclick=~{
	${counter2}()
	   .post( function( v ) { ${count}.innerHTML = v } )
     }>
         inc me (promise)
     </button>
   </html>;
}

service counter() {
   return hop.HTTPResponseAsync(
      function( sendResponse )  {
	 w.postMessage( sendResponse );
      }, this );
}

service counter2() {
   return new Promise( function( resolve, reject ) {
      console.log( "resolve=", resolve.toString() );
      w.postMessage( resolve );
   } );
}

console.log( "Go to \"http://%s:%d/hop/worker4\"", hop.hostname, hop.port );
