/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/svc4/svc4.js             */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed May 21 07:50:20 2014                          */
/*    Last change :  Thu Jan 15 21:54:14 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Basic example that illustrates services declarations.            */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g svc4.js                                           */
/*    browser: http://localhost:8080/hop/svc4                          */
/*=====================================================================*/
service svc4( o ) {
   var name = o && "name" in o ? o.name : "foo";
   import service svc();
   svc.path = "/hop/" + name;

   return hop.HTTPResponseAsync(
      function( sendResponse ) {
	 svc.apply( undefined, [ "foo", "bar" ] ).post( sendResponse );
      }, this );
}
	    
service foo( a, b ) {
   return <html>
     <h1>foo</h1>
     <div> ${ a } </div>
     <div> ${ b } </div>
   </html>
}

service bar( a, b ) {
   return <html>
     <h1>bar</h1>
     <div> ${ a } </div>
     <div> ${ b } </div>
   </html>
}

console.log( "Go to \"http://%s:%d/hop/svc4?name=foo\"", hop.hostname, hop.port );
console.log( "   or \"http://%s:%d/hop/svc4?name=bar\"", hop.hostname, hop.port );
