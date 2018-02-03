/*=====================================================================*/
/*    serrano/prgm/project/hop/3.2.x/examples/requirec/requirec.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Feb  3 09:45:28 2018 (serrano)                */
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
       <script src="./example.json" module="hopscript"/>
       <script src="./mod.html" module="hopscript"/>
       <script defer>
	  var mod1 = require( "./mod1.js" );
	  var ex = require( "./example.json" );
	  var dhtml = require( "./mod.html" );
       </script>
     </head>
     <button onclick=~{ document.body.appendChild( mod1.hello() ) }>
       click me
     </button>
     <button onclick=~{ alert( "desc=" + ex.description ) }>
       json me
     </button>
     <button onclick=~{ alert( "desc=" + dhtml.outerHTML ) }>
       html me
     </button>
   </html>;
}

console.log( "Go to \"http://%s:%d/hop/requirec\"", hop.hostname, hop.port );
