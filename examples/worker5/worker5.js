/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker5/worker5.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Sat Jun 27 08:10:01 2015                          */
/*    Last change :  Thu Aug 20 09:00:33 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Server side workers and services                                 */
/*=====================================================================*/


var hop = require( 'hop' );

function createWorker( title ) {
   var w = new Worker( "./slave.js" );
   w.onmessage = function( message ) {
      w.svc = message.data;
      console.log( 'main:  service handle received from %s', title );
   }
   w.title = title;
   console.log( 'main: %s created', title );
   return w;
}

var w1 = createWorker( 'left worker' );
var w2 = createWorker( 'right worker' );

service worker5() {
   return new Promise( function( resolve, reject ) {
      setTimeout( function() {
	 resolve( <html>
	   <div>
	     <iframe style="border: 1px solid black" src=${w1.svc( w1.title )}/>
	     <iframe style="border: 1px solid black" src=${w2.svc( w2.title )}/>
	   </div>
	 </html> ) }, 1000 );
   } );
}

console.log( "Go to \"http://%s:%d/hop/worker5\"", hop.hostname, hop.port );

