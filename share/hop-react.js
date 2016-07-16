/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hop-react.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 28 11:23:23 2016                          */
/*    Last change :  Wed Jul 13 15:16:58 2016 (serrano)                */
/*    Copyright   :  2016 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Reactive runtime.                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    React global parameters                                          */
/*---------------------------------------------------------------------*/
window.hop.reactInitThunks = [];
window.hop.reactIntialized = false;
window.hop.reactReactorQueueStamp = 1;
window.hop.reactReactorQueue = [];

/*---------------------------------------------------------------------*/
/*    window.hop.reactDelay ...                                        */
/*    -------------------------------------------------------------    */
/*    Ensure that all REACT expressions are executed after page        */
/*    load and initialization.                                         */
/*---------------------------------------------------------------------*/
window.hop.reactDelay = function( thunk ) {
   if( window.hop.reactIntialized ) {
      setTimeout( thunk, 0 );
   } else {
      if( window.hop.reactInitThunks.length == 0 ) {
	 window.addEventListener( "load", function() {
	    // push back after every other things
	    setTimeout( function() {
	       window.hop.reactInitThunks.forEach( function( f ) { f(); } );
	       window.hop.reactIntialized = true;
	       window.hop.reactInitThunks = [];	    
	    }, 1 );
	 } );
      }

      window.hop.reactInitThunks.push( thunk );
   }
}

/*---------------------------------------------------------------------*/
/*    window.hop.reactReactorQueueFlush ...                            */
/*---------------------------------------------------------------------*/
window.hop.reactReactorQueueFlush = function() {
   var queue = window.hop.reactReactorQueue;
   var stamp = window.hop.reactReactorQueueStamp;
   
   window.hop.reactReactorQueue = [];
   window.hop.reactReactorQueueStamp++;

   queue.forEach( function( reactor, idx, arr ) {
      if( reactor.stamp == stamp ) {
	 reactor.stamp = 0;
	 reactor();
      }
   } );
}
   
/*---------------------------------------------------------------------*/
/*    window.hop.reactReactorQueuePush ...                             */
/*---------------------------------------------------------------------*/
window.hop.reactReactorQueuePush = function( reactor, _idx, _arr ) {
   if( !reactor.stamp ) {
      reactor.stamp = window.hop.reactReactorQueueStamp;
      if( window.hop.reactReactorQueue.length == 0 ) {
	 setTimeout( window.hop.reactReactorQueueFlush, 0 );
      }
      window.hop.reactReactorQueue.push( reactor );
   }
}
    
/*---------------------------------------------------------------------*/
/*    window.hop.reactProxy ...                                        */
/*---------------------------------------------------------------------*/
window.hop.reactProxy = function( val ) {
   var reactors = {};

   function getHandler( target, prop ) {
      
      if( window.hop.reactInCollect ) {
	 if( !(prop in reactors) ) { reactors[ prop ] = [] }
	 if( reactors[ prop ].indexOf( window.hop.reactInCollect ) == -1 ) {
	    reactors[ prop ].push( window.hop.reactInCollect );
	    window.hop.reactInCollect.stamp = 0;
	 }
      }

      return target[ prop ];
   };
   
   function setHandler( obj, prop, value ) {
      obj[ prop ] = value;

      if( prop in reactors ) {
	 reactors[ prop ].forEach( window.hop.reactReactorQueuePush );
      }

      return true;
   }
   
   return new Proxy( val, { get: getHandler, set: setHandler } );
}

/*---------------------------------------------------------------------*/
/*    window.hop.reactInvoke ...                                       */
/*---------------------------------------------------------------------*/
window.hop.reactInvoke = function( proc, react ) {
   var old = window.hop.reactInCollect;
   
   try {
      window.hop.reactInCollect = react;
      return proc();
   } finally {
      window.hop.reactInCollect = old;
   }
}
   
/*---------------------------------------------------------------------*/
/*    window.hop.reactAttribute ...                                    */
/*---------------------------------------------------------------------*/
window.hop.reactAttribute = function( proc ) {
   
   function react() {
      return window.hop.reactInvoke( proc, react );
   }
   
   window.hop.reactDelay( react );
}

/*---------------------------------------------------------------------*/
/*    window.hop.reactNode ...                                         */
/*---------------------------------------------------------------------*/
window.hop.reactNode = function( proc, parent, sibling, anchor, key ) {
   
   function getElementByAnchor( nodes, anchor ) {
      for( var i = nodes.length - 1; i >=0; i-- ) {
	 var n = nodes[ i ];
	 
	 if( n.nodeType == 3 ) {
	    if( n.textContent == anchor ) return n;
	 }
      }

      return false;
   }

   function react() {
      var parentNode = parent ? document.getElementById( parent ) : document.body;
      var nodes = window.hop.reactInvoke( proc, react );

      if( nodes == 0 ) nodes = [ "0" ];

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

   if( anchor ) anchor = decodeURIComponent( anchor );
   
   window.hop.reactDelay( react );
}

/*---------------------------------------------------------------------*/
/*    window.hop.reactDynKey ...                                       */
/*---------------------------------------------------------------------*/
window.hop.reactDynKey = 0;

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
	  + "window.hop.reactNode( proc, parent.id, sid, '" + id + "' );"

      sc.type = "text/javascript";
      sc.text = loader;

      el.id = id;
      el.appendChild( sc );
      return el;
   } else {
      return null;
   }
}
