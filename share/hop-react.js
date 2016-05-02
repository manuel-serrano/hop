/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hop-react.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 28 11:23:23 2016                          */
/*    Last change :  Fri Apr 29 17:58:32 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Reactive runtime.                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    reactProxy ...                                                   */
/*---------------------------------------------------------------------*/
window.hop.reactProxy = function( val ) {
   var attrs = [];

   function getHandler( target, name ) {
      if( window.hop.reactInCollect &&
	  attrs.indexOf( window.hop.reactInCollect ) == - 1) {
	 attrs.push( window.hop.reactInCollect );
      }

      return target[ name ];
   };
   
   function setHandler( obj, prop, value ) {
      obj[ prop ] = value;
      
      attrs.forEach( function( obj, idx, arr ) {
	 obj();
      } );

      return value;
   }
   
   return new Proxy( val, { get: getHandler, set: setHandler } );
}

/*---------------------------------------------------------------------*/
/*    reactCollectProxy ...                                            */
/*---------------------------------------------------------------------*/
window.hop.reactCollectProxy = function( proc ) {
   function react() {
      return invoke( proc );
   }
      
   function invoke( proc ) {
      var old = window.hop.reactInCollect;
      try {
	 window.hop.reactInCollect = react;
	 return proc();
      } finally {
	 window.hop.reactInCollect = old;
      }
   }

   react();
}

/*---------------------------------------------------------------------*/
/*    window.hop.reactInit ...                                         */
/*---------------------------------------------------------------------*/
window.hop.reactInit = function( parent, sibling, key, proc ) {
   function insertReactNodes() {
      var parentNode = document.getElementById( parent );
      var nodes = invoke( proc );

      if( !parentNode ) {
	 throw "Cannot find react parent " + parent;
      }
      
      if( !(nodes instanceof Array) ) {
	 nodes = [ nodes ];
      }

      nodes.forEach( function( n, idx, arr ) {
	 if( !n ) {
	    n = document.createTextNode( "" );
	    arr[ idx ] = n;
	 } else if( !(n instanceof Node) ) {
	    n = document.createTextNode( n.toString() );
	    arr[ idx ] = n;
	 }
      } );

      if( parentNode[ key ] ) {
	 nodes.forEach( function( n, idx, arr ) {
	    parentNode.insertBefore( n, parentNode[ key ][ 0 ] );
	 } );
      } else if( sibling ) {
	 var s = document.getElementById( sibling );
	 nodes.forEach( function( n, idx, arr ) {
	    parentNode.insertBefore( n, s );
	 } );
      } else {
	 for( var i = nodes.length - 1; i >= 0; i-- ) {
	    parentNode.appendChild( nodes[ i ] );
	 }
      }

      if( parentNode[ key ] ) {
	 parentNode[ key ].forEach( function( n, idx, arr ) {
	    parentNode.removeChild( n );
	 } );
      }

      parentNode[ key ] = nodes;
   }

   function invoke( proc ) {
      var old = window.hop.reactInCollect;
      try {
	 window.hop.reactInCollect = insertReactNodes;
	 return proc();
      } finally {
	 window.hop.reactInCollect = old;
      }
   }
   
   return insertReactNodes();
}
      
  
