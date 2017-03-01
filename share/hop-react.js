/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hop-react.js                */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu Apr 28 11:23:23 2016                          */
/*    Last change :  Fri Jan 27 13:07:33 2017 (serrano)                */
/*    Copyright   :  2016-17 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Reactive runtime.                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    React global parameters                                          */
/*---------------------------------------------------------------------*/
hop.reactInitThunks = [];
hop.reactIntialized = false;
hop.reactReactorQueueStamp = 1;
hop.reactReactorQueue = [];

/*---------------------------------------------------------------------*/
/*    hop.reactDelay ...                                               */
/*    -------------------------------------------------------------    */
/*    Ensure that all REACT expressions are executed after page        */
/*    load and initialization.                                         */
/*---------------------------------------------------------------------*/
hop.reactDelay = function( thunk ) {
   if( hop.reactIntialized ) {
      setTimeout( thunk, 0 );
   } else if( document.readyState == "complete" ) {
      hop.reactIntialized = true;
      setTimeout( thunk, 0 );
   } else {
      if( hop.reactInitThunks.length == 0 ) {
	 window.addEventListener( "load", function() {
	    // push back after every other things
	    setTimeout( function() {
	       hop.reactInitThunks.forEach( function( f ) { f(); } );
	       hop.reactIntialized = true;
	       hop.reactInitThunks = [];	    
	    }, 1 );
	 } );
      }

      hop.reactInitThunks.push( thunk );
   }
}

/*---------------------------------------------------------------------*/
/*    hop.reactReactorQueueFlush ...                                   */
/*---------------------------------------------------------------------*/
hop.reactReactorQueueFlush = function() {
   var queue = hop.reactReactorQueue;
   var stamp = hop.reactReactorQueueStamp;
   
   hop.reactReactorQueue = [];
   hop.reactReactorQueueStamp++;

   queue.forEach( function( reactor, idx, arr ) {
      if( reactor.stamp == stamp ) {
	 reactor.stamp = 0;
	 reactor();
      }
   } );
}
   
/*---------------------------------------------------------------------*/
/*    hop.reactReactorQueuePush ...                                    */
/*---------------------------------------------------------------------*/
hop.reactReactorQueuePush = function( reactor, _idx, _arr ) {
   if( !reactor.stamp ) {
      reactor.stamp = hop.reactReactorQueueStamp;
      if( hop.reactReactorQueue.length == 0 ) {
	 setTimeout( hop.reactReactorQueueFlush, 0 );
      }
      hop.reactReactorQueue.push( reactor );
   }
}
    
/*---------------------------------------------------------------------*/
/*    hop.reactProxy ...                                               */
/*---------------------------------------------------------------------*/
hop.reactProxy = function( val ) {
   var reactors = {};

   function getHandler( target, prop ) {
      // name mangling for avoid confusion with existing properties
      let nprop = "%" + prop;
      
      if( hop.reactInCollect ) {
	 if( !(nprop in reactors) ) { reactors[ nprop ] = [] }
	 if( reactors[ nprop ].indexOf( hop.reactInCollect ) == -1 ) {
	    reactors[ nprop ].push( hop.reactInCollect );
	    hop.reactInCollect.stamp = 0;
	 }
      }

      return target[ prop ];
   };
   
   function setHandler( obj, prop, value ) {
      let nprop = "%" + prop;
      
      obj[ prop ] = value;

      if( nprop in reactors ) {
	 reactors[ nprop ].forEach( hop.reactReactorQueuePush );
      }

      return true;
   }
   
   return new Proxy( val, { get: getHandler, set: setHandler } );
}

/*---------------------------------------------------------------------*/
/*    hop.reactInvoke ...                                              */
/*---------------------------------------------------------------------*/
hop.reactInvoke = function( proc, react ) {
   var old = hop.reactInCollect;
   
   try {
      hop.reactInCollect = react;
      return proc();
   } finally {
      hop.reactInCollect = old;
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop.reactAttribute ...                                           */
/*---------------------------------------------------------------------*/
hop.reactAttribute = function( proc ) {
   
   function react() {
      return hop.reactInvoke( proc, react );
   }
   
   hop.reactDelay( react );
}

/*---------------------------------------------------------------------*/
/*    hop.reactNode ...                                                */
/*---------------------------------------------------------------------*/
hop.reactNode = function( proc, parent, sibling, anchor, key ) {
   
   function getElementByAnchor( nodes, anchor ) {
      for( var i = nodes.length - 1; i >=0; i-- ) {
	 var n = nodes[ i ];
	 
	 if( n.nodeType == 3 ) {
	    if( n.textContent == anchor ) return n;
	 }
      }

      return false;
   }
   
   function htmlDecode( input ) {
      var e = document.createElement( 'div' );
      e.innerHTML = input;
      return e.childNodes.length === 0 ? "" : e.childNodes[0].nodeValue;
   }
   
   function react() {
      var parentNode = parent ? document.getElementById( parent ) : document.body;
      var nodes = hop.reactInvoke( proc, react );

      if( nodes == 0 && typeof( nodes ) == "number" ) nodes = [ "0" ];

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
	       for( var i = nodes.length - 1; i >= 0; i-- ) {
		  parentNode.appendChild( nodes[ i ] );
	       }
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
   
   hop.reactDelay( react );
}

/*---------------------------------------------------------------------*/
/*    hop.reactDynKey ...                                              */
/*---------------------------------------------------------------------*/
hop.reactDynKey = 0;

/*---------------------------------------------------------------------*/
/*    REACT ...                                                        */
/*---------------------------------------------------------------------*/
/*** META ((export <REACT>)) */
function REACT( _ ) {
   var cnt = 0;
   var id = "react" + hop.reactDynKey++;;
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
	  + "var sid = sibling ? sibling.id : false;"
	  + "var parent = el.parentNode;"
	  + "var pid = parent ? parent.id : false;"
	  + "var proc = function() { return " + script.text + " };"
          + "var anchor = false; console.log( 'sibling=', sid );"
	  + "parent.removeChild( el );"
	  + "hop.reactNode( proc, parent.id, sid, anchor, '" + id + "' );"

      sc.type = "text/javascript";
      sc.text = loader;

      el.id = id;
      el.appendChild( sc );
      return el;
   } else {
      return null;
   }
}
