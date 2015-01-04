/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/tower/tower.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun  3 16:32:25 2014                          */
/*    Last change :  Sun Dec 21 08:40:21 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Recursive client-side tower                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v tower.js                                             */
/*    browser: http://localhost:8080/hop/tower                         */
/*=====================================================================*/

var hop = require( 'hop' );

service tower() {
   return <HTML> {
      ~{
         function clicked (msg) {
            var but = <BUTTON> {
               onclick: ~{ clicked( ${msg + "+"} ) },
               msg
            };
            
            document.body.appendChild( but );
         }
      },
      <BUTTON> {
         onclick: ~{ clicked( "click me also" ); },
         "click me"
      }
   }
}

console.log( "Go to \"http://%s:%d/hop/tower\"", hop.hostname, hop.port );
