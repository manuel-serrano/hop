/*=====================================================================*/
/*    serrano/prgm/project/hop/3.0.x/examples/js2http/js2http.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Jun 25 08:36:20 2014                          */
/*    Last change :  Wed Nov  4 13:57:18 2015 (serrano)                */
/*    Copyright   :  2014-15 Manuel Serrano                            */
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
var ast = require( hopc.ast );

service js2http( a ) {
   var prg = hopc.intern( a );
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

   w.J2SAccess = function( node ) {
      console.log( 'profiling access' );
      
      var con = new ast.J2SUnresolvedRef( node.loc, undefined, false,
					  { __symbol__: "console" } );
      var log = new ast.J2SString( node.loc, undefined, "log", false );
      var access = new ast.J2SAccess( node.loc, undefined, false, con, log )
      var name = new ast.J2SString( node.loc, undefined, accessName( node.obj ), false );
      var call = new ast.J2SCall( node.loc, undefined, access, [ name ] );
      
      var exprs = [ call, w.walk( node ) ];

      return new ast.J2SSequence( node.loc, undefined, exprs );
   }

   w.walk( prg );

   return prg;
}

function accessName( node ) {
   if( node.__node__ == "J2SUnresolvedRef" ) {
      return node.id;
   } else if( node.__node__ == "J2SRef" ) {
      return node.decl.id.__symbol__;
   } else {
      return "???";
   }
}
