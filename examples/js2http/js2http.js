/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/js2http/js2http.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun 25 08:36:20 2014                          */
/*    Last change :  Wed Oct 15 08:43:16 2014 (serrano)                */
/*    Copyright   :  2014 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    This shows how to implement a JavaScript HTTP plugin to the      */
/*    js2scheme compiler.                                              */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g -p 8888 js2http.js                                */
/*    run: hopc -v fact.js --js-driver syntax,hopscript-header, \      */
/*          loopexit,bestpractice,symbol,this,read-only,return, \      */
/*          property,http://localhost:8888/hop/js2http,scheme && a.out */
/*=====================================================================*/
var hopc = require( "hopc" );

service js2http( a ) {
   var ast = hopc.intern( a );
   var w = new hopc.HopcAstWalker();

   w.J2SDeclFun = function( node ) {
      console.log( "scanning function ", node.id.__symbol__ );
      w.walk( node );
      return node;
   }

   w.J2SDeclSvc = function( node ) {
      console.log( "scanning service ", node.id.__symbol__ );
      w.walk( node );
      return node;
   }

   w.J2SBinary = function( node ) {
      w.walk( node );
      if( node.op.__symbol__ === "*" ) {
	 console.log( 'transforming operator "*" into "+"' );
	 node.op = { __symbol__: '+' };
      }
      return node;
   }

   w.walk( ast );

   return ast;
}
