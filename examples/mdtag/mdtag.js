/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/mdtag/mdtag.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Fri Oct 23 11:10:57 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example using the MARKDOWN Html tag                           */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g mdtag.js                                          */
/*    browser: http://localhost:8080/hop/mdtag                         */
/*=====================================================================*/
var md = require( hop.markdown );

service mdtag( o ) {
   var name = o && "name" in o ? o.name : "bar";
   return <html>
     <md.markdown>
toto n'est pas content

  * toto
  * tutu ${name}
  * titi
  
tutu non plus.
   </md.markdown>
</html>;
}

console.log( "Go to \"http://%s:%d/hop/mtag\"", hop.hostname, hop.port );
