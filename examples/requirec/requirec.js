/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirec/requirec.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Thu Dec 17 07:33:31 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    multitier require                                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g requirec.js                                       */
/*    browser: http://localhost:8080/hop/requirec                      */
/*=====================================================================*/
service requirec() {
   return <html> 
     <head module= ${[ "./mod1.js", "./mod2.js" ]}>
       ~{
	  var mod1;

	  window.onload = function() {
	     mod1 = require( "./mod1.js" );
	  }
       }
     </head>
     <button onclick=~{ document.body.appendChild( mod1.hello() ) }>
       click me
     </button>
   </html>;
}

console.log( "Go to \"http://%s:%d/hop/requirec\"", hop.hostname, hop.port );
