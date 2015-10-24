"use hopscript";

service foo () {
   return <html>
     <head jscript=${foo.resource( "foo-bar.js" )}
	   css=${foo.resource( "foo-bar.css" )}/>
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
