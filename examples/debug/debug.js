/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/debug/debug.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:41:10 2014                          */
/*    Last change :  Thu Dec 17 08:02:18 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Illustrates multitier error reporting                            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g debug.js                                          */
/*    browser: http://localhost:8080/hop/debug                         */
/*=====================================================================*/
service debug() {
   return <html>
     <head module="./bugs.js">~{ var BUGS = require( "./bugs.js" ) }</head>
     <h1>Examples of error reporting</h1>
     <ol>
       <li>
	 <button onclick=~{ BUGS.raise( event.type ) }>
            raise
	 </button>
         onclick throw
       </li>
       <li> 
	 <button onclick=~{ BUGS.unbound() }>
            raise
	 </button>
         onclick -> variable unbound error
       </li>
       <li>
	 <button onclick=~{ BUGS.interval() }>
            raise
	 </button>
         onclick -> interval -> exception
       </li>
       <li>
	 <button id="direct-button"
		 onclick=~{ throw new Error( "direct error" ); }>
            raise
	 </button>
	 direct throw
       </li>
       <li>
	 <button id="srv-button"
		 onclick=~{ ${bug}( { y: 4 } ).post() }>
            raise
	 </button>
	 server side service error
       </li>
       <li>
	 <button id="srv-button"
		 onclick=~{ setTimeout( function() {
		    ${bug}( { y: 4 } ).post() }, 0 ) }>
            raise
	 </button>
	        server side service error within timeout
       </li>
       <li>
	 <button id="srv-button"
		 onclick=~{ setTimeout( function() {}, 0 );
			    ${bug}( { y: 4 } ).post() }>
            raise
	 </button>
         server side service error after timeout
       </li>
     </ol>
   </html>
}

service bug( query ) {
   return query[ "x" ][ "car" ];
}


if( process.features.debug < 1 ) {
   console.log( "This weblet needs debug mode." );
   console.log( "Re-run hop with \"-g\" option." );
   process.exit( 1 );
} else {
   console.log( "Go to \"http:/%s:%d/%s\"",
		hop.hostname, hop.port, debug().toString() );
}

