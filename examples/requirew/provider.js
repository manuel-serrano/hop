/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/requirew/provider.js     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Apr 18 09:42:04 2014                          */
/*    Last change :  Sat Dec 20 09:57:34 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    URL based require example                                        */
/*    -------------------------------------------------------------    */
/*    See requirew.js                                                  */
/*=====================================================================*/

var hop = require( "hop" );

service providerGetModule( { file: "" } ) {
   return hop.HTTPResponseFile( providerGetModule.resource( file ) );
}
