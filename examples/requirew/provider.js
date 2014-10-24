/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirew/provider.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Fri Oct 24 13:57:08 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    URL based require example                                        */
/*    -------------------------------------------------------------    */
/*    run: hop -g requirew.js                                          */
/*    run: hop -g -p 9999 provider.js                                  */
/*    browser: http://localhost:8080/hop/requirew                      */
/*=====================================================================*/

var hop = require( "hop" );

service providerGetModule() {
   return hop.HTTPResponseFile( "foo.js" );
}

