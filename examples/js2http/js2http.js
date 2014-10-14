/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/js2http/js2http.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun 25 08:36:20 2014                          */
/*    Last change :  Tue Oct 14 10:36:51 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    This shows how to implement a JavaScript HTTP plugin to the      */
/*    js2scheme compiler                                               */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g -p 8888 js2http.js                                */
/*    run: hopc -v fib.js --js-driver syntax,hopscript-header, \       */
/*          loopexit,bestpractice,symbol,this,read-only,return, \      */
/*          property,http://localhost:8888/hop/js2http,scheme && a.out */
/*=====================================================================*/
var hopc = require( "hopc" );

service js2http( a ) {
   var ast = hopc.intern( a );
   var w = new hopc.HopcAstWalker();

   console.log( JSON.stringify( ast ) );
   
   w.J2SDeclFun = function( node ) {
      console.log( "Function: ", node.id );
      return node;
   }

   w.J2SDeclSvc = function( node ) {
      console.log( "Service: ", node.id );
      return node;
   }

   w.walk( ast );

   return ast;
}
