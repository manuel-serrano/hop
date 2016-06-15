/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hop-react.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 28 11:23:23 2016                          */
/*    Last change :  Fri Jun 10 08:20:44 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Reactive runtime.                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    window.hop.reactDynKey ...                                       */
/*---------------------------------------------------------------------*/
window.hop.reactDynKey = 0;
window.hop.reactInitThunks = [];
window.hop.reactIntialized = false;

/*---------------------------------------------------------------------*/
/*    window.hop.pushReact ...                                         */
/*---------------------------------------------------------------------*/
window.hop.pushReact = function( thunk ) {
   if( window.hop.reactIntialized ) {
      setTimeout( thunk, 0 );
   } else {
      if( window.hop.reactInitThunks.length == 0 ) {
	 window.addEventListener( "load", function() {
	    // push back after every other things
	    setTimeout( function() {
	       window.hop.reactInitThunks.forEach( function( f ) {
		  f(); } );
	       window.hop.reactInitThunks = [];	    
	    }, 1 );
	 } );
      }

      window.hop.reactInitThunks.push( thunk );
   }
}

/*---------------------------------------------------------------------*/
/*    REACT ...                                                        */
/*---------------------------------------------------------------------*/
/*** META ((export <REACT>)) */
function REACT( _ ) {
   var cnt = 0;
   var id = "react" + window.hop.reactDynKey++;;
   var script = false;

   for( var i = 0; i < arguments.length; i++ ) {
      var el = arguments[ i ];

      if( el instanceof Node ) {
	 script = el;
	 break;
      }
   }

   if( script ) {
      var el = document.createElement( "div" );
      var sc = document.createElement( "script" );
      var loader = "var el = document.getElementById( '" + id + "');"
	  + "var sibling = el.nextSibling;"
	  + "var sid = sibling ? sibing.id : false;"
	  + "var parent = el.parentNode;"
	  + "var pid = parent ? parent.id : false;"
	  + "var proc = function() { return " + script.text + " };"
	  + "parent.removeChild( el );"
	  + "window.hop.reactInit( parent.id, sid, '" + id + "', proc );"

      sc.type = "text/javascript";
      sc.text = loader;

      el.id = id;
      el.appendChild( sc );
      return el;
   } else {
      return null;
   }
}

/*---------------------------------------------------------------------*/
/*    reactProxy ...                                                   */
/*---------------------------------------------------------------------*/
window.hop.reactProxy = function( val ) {
   var attrs = [];

   function getHandler( target, name ) {
      if( window.hop.reactInCollect &&
	  attrs.indexOf( window.hop.reactInCollect ) == -1) {
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
window.hop.reactInit = function( parent, sibling, anchor, key, proc ) {
   
   function getElementByAnchor( nodes, anchor ) {
      for( var i = nodes.length - 1; i >=0; i-- ) {
	 var n = nodes[ i ];
	 
	 if( n.nodeType == 3 ) {
	    if( n.textContent == anchor ) return n;
	 }
      }

      return false;
   }

   function insertReactNodes() {
      var parentNode = document.getElementById( parent );
      var nodes = invoke( proc );

      if( nodes ) {
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
	    if( s ) {
	       nodes.forEach( function( n, idx, arr ) {
		  parentNode.insertBefore( n, s );
	       } );
	    } else {
	       return;
	    }
	 } else if( anchor ) {
	    var s = getElementByAnchor( parentNode.childNodes, anchor );
	    if( s ) {
	       nodes.forEach( function( n, idx, arr ) {
		  parentNode.insertBefore( n, s );
	       } );
	    } else {
	       return;
	    }
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

   if( anchor ) anchor = decodeURIComponent( anchor );
   
   window.hop.pushReact( insertReactNodes );
}
