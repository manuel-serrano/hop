/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/custom/custom.js         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Aug 23 08:07:57 2015                          */
/*    Last change :  Thu Nov 26 19:08:22 2015 (serrano)                */
/*    Copyright   :  2015 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    An example using the custom Html tag                             */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g custom.js                                         */
/*    browser: http://localhost:8080/hop/custom                        */
/*=====================================================================*/
"use hopscript";

service custom () {
   return <html>
     <head jscript=${custom.resource( "foo-bar.js" )}
	   css=${custom.resource( "foo-bar.css" )}/>
     <foo-bar>
       <div>toto n est pas content</div>
     </foo-bar>
   </html>;
}

global[ "FOO-BAR" ] = function( attr, ... body ) {
   var el = hop.createElement( "foo-bar", attr, body );
   el.attributes = attr;
   el.children = body;
   return el;
}
