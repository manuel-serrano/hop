/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirew/requirew.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Fri Oct 24 13:58:08 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    URL based require example                                        */
/*    -------------------------------------------------------------    */
/*    run: hop -g requirew.js                                          */
/*    run: hop -g -p 9999 provider.js                                  */
/*    browser: http://localhost:8080/hop/requirew                      */
/*=====================================================================*/

var util = require( "util" );
var hop = require( "hop" );

var url = util.format( "http://%s:9999/hop/providerGetModule", hop.hostname );

service requirew() {
   var imp = require( url );
   return imp.hello();
}

console.log( "Go to \"http://%s:%d/hop/requirew\"", hop.hostname, hop.port );
