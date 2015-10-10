/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/mdtag/mdtag.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Sat Oct 10 11:40:31 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example using the MARKDOWN Html tag                           */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g mdtag.js                                          */
/*    browser: http://localhost:8080/hop/mdtag                         */
/*=====================================================================*/
var hop = require( "hop" );
var md = require( hop.markdown );

service mdtag( {name: "bar"} ) {
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
