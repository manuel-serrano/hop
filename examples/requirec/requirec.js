/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirec/requirec.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Dec 20 09:45:55 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    multitier require                                                */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g requirec.js                                       */
/*    browser: http://localhost:8080/hop/requirec                      */
/*=====================================================================*/
var hop = require( 'hop' );

service requirec() {
   return <HTML> {
      <HEAD> {
	 require: [ "./mod1.js", "./mod2.js" ],
	 ~{
	    var mod1;

	    window.onload = function() {
	       mod1 = require( "./mod1.js" );
	    }
	 }
      },
      <BUTTON> {
	 onclick: ~{ document.body.appendChild( mod1.hello() ) },
	 "click me"
      }
   }
}

console.log( "Go to \"http://%s:%d/hop/requirec\"", hop.hostname, hop.port );
