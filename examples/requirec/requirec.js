/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/examples/requirec/requirec.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Feb  3 09:37:26 2018 (serrano)                */
/*    Copyright   :  2014-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    multitier require                                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g requirec.js                                       */
/*    browser: http://localhost:8080/hop/requirec                      */
/*=====================================================================*/
const mod1 = require( "./mod1.js" );

service requirec() {
   return <html> 
     <head>
       <script src="./mod1.js" module="hopscript"/>
       <script src="./mod2.js" module="hopscript"/>
       <script defer>
	  var mod1 = require( "./mod1.js" );
       </script>
     </head>
     <button onclick=~{ document.body.appendChild( mod1.hello() ) }>
       click me
     </button>
   </html>;
}

console.log( "Go to \"http://%s:%d/hop/requirec\"", hop.hostname, hop.port );
