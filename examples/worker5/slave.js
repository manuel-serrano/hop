/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/worker5/slave.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat Jun 27 08:11:22 2015                          */
/*    Last change :  Thu Aug 20 09:02:27 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Server workers and services                                      */
/*=====================================================================*/



console.log( "starting worker-slave..." );

var counter = 0;

var svc = service( title ) {
   console.log( '%s service invoked: building html frame', title );
   var count = <span id="counter">${counter}</span>;
   return <html>
     <h1>${title}</h1>
     <div>counter: ${count}</div>
     <button onclick=~{
	${service () {
	   console.log( '%s counter: set value to %d', title, counter + 1 );
	   return ++counter;
	}}().post( function( v ) { ${count}.innerHTML = v } )
     }>
     inc me
     </button>
   </html>
}

onmessage = function( message ) {
   console.log( "slave worker: message received from main thread: ignoring ",
		message.data );
}

postMessage( svc );

console.log( 'slave worker: service handle sent to main thread' );
