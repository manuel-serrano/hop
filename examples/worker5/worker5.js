/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker5/worker5.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Vincent Prunet                                    */
/*    Creation    :  Sat Jun 27 08:10:01 2015                          */
/*    Last change :  Fri Jul 17 10:15:53 2015 (serrano)                */
/*    Copyright   :  2015 Inria                                        */
/*    -------------------------------------------------------------    */
/*    Server side workers and services                                 */
/*=====================================================================*/


// Example demonstrating advanced use of JavaScript workers. The main
// task is responsible to initiate two slave twin workers running the
// same code, and create a browser service launch both workers in
// iframes.  When launched, each slave worker defines a dynamic service
// and sends a service handle to the main thread to let the thread build
// the main html page.
// 
// Service handles, just like other hop.js objects, can be sent as worker
// messages.
// 
// Since there is a unique name space for services, all services created
// in slave.js are anonymous, hop.js ensures that unique names are
// generated for each service.
// 
// Note that each slave.js worker operates its private counter. The same
// isolation property would apply to submodules required by slave.js
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
   return hop.HTTPResponseAsync( function( send ) {
      setTimeout( function() {
	 send( <HTML> { <DIV> {
	    <IFRAME> {
	       style: "border: 1px solid black", src: w1.svc( w1.title )
	    },
	    <IFRAME> {
	       style: "border: 1px solid black", src: w2.svc( w2.title )
	    }
	 } } )
      }, 1000 );
   }, this );
}

console.log( 'worker5: service is now defined' );

// Note : the worker5 service cannot be invoked until both slave workers
// are fully initialized, which is not an issue since the service is
// invoked from the client browser only.
// 
// Putting the createWorker calls within the main service would require
// an asynchronous coding style where the service implementation creates
// slave workers and returns only after the worker services are
// created.
