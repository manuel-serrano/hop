/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/tower/tower.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun  3 16:32:25 2014                          */
/*    Last change :  Mon Sep  7 16:12:59 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Recursive client-side tower                                      */
/*    -------------------------------------------------------------    */
/*    run: hop -v tower.js                                             */
/*    browser: http://localhost:8080/hop/tower                         */
/*=====================================================================*/
service tower() {
   return <HTML>
     ~{
        function clicked (msg) {
           var but = <BUTTON onclick=~{ clicked( ${msg + "+"} ) }>${msg}</BUTTON>;
           document.body.appendChild( but );
        }
     }
     <BUTTON onclick=~{ clicked( "click me also" ) }>click me</BUTTON>
   </HTML>
}

console.log( "Go to \"http://%s:%d/hop/tower\"", hop.hostname, hop.port );
