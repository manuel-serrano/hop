/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/share/hop-spage.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Nov 20 07:27:15 2020                          */
/*    Last change :  Sun Nov 22 09:08:58 2020 (serrano)                */
/*    Copyright   :  2020 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Client side library for spage (version 2).                       */
/*=====================================================================*/
"use strict"

/*---------------------------------------------------------------------*/
/*    Spage ...                                                        */
/*---------------------------------------------------------------------*/
let { spageInit: BGl_spagezd2initzd2zz__hopzd2spagezd2,
      sptabAddEventListener: BGl_sptabzd2addzd2eventzd2listenerz12zc0zz__hopzd2spagezd2,
      spagePopUpdate: BGl_spagezd2popzd2updatez00zz__hopzd2spagezd2,
      spagePushService: BGl_spagezd2pushzd2servicez00zz__hopzd2spagezd2,
      spagePushUrl: BGl_spagezd2pushzd2urlz00zz__hopzd2spagezd2,
      spagePushNode: BGl_spagezd2pushzd2nodez00zz__hopzd2spagezd2
    } =

/// dummy module   
(function() {
   /// forced intendation
   
   function getElement( el ) {
      return typeof( el ) === "string" ? document.getElementById( el ) : el;
   }
      
   // spageAddEventListener
   function spageAddEventListener( spage, event, proc, capture ) {
      if( event === "onchange" ) {
	 spage.onchg = proc;
      } else {
	 spage.addEventListener( event, proc, capture );
      }
   }
      
   // sptabAddEventListener
   function sptabAddEventListener( sptab, event, proc, capture ) {
      if( event === "onselect" ) {
	 sptab.onselect = proc;
      } else {
	 sptab.addEventListener( event, proc, capture );
      }
   }
      
   // spageUpdate
   function spageUpdate( spage ) {
      spageResize( spage );
      spage.childNodes.forEach( hop_update );
   }
      
   // resize
   function spageResize( spage ) {
      const spbody = spage.spviewport.childNodes[ 0 ];
      const cwidth = spage.clientWidth - frameBorderWidth( spage );
      const cheight = spage.clientHeight - frameBorderHeight( spage );
      
      spage.spwidth = cwidth;
      spage.spbodywidth = cwidth - frameBorderWidth( spbody );
      spage.spbodyheight = spage.parentNode.clientHeight 
         - frameBorderHeight( spbody ) 
         - spage.sphead.offsetHeight;
   }
      
   // spageResetSize
   function spageResetSize( spage ) {
      if( spage.spviewport ) {
	 // proceed only when initialized
	 spageResize( spage );
	 
	 // webkit got the animation wrong if the viewport is just not
	 // larger enough before adding a new tab
	 spage.spscrollwidth = (spage.depth + 2) * spage.spwidth;
	 spage.spoffset = spage.depth * spage.spwidth;
	 
	 // we have to enforce the page size otherwise the browsers
	 // use the viewport width for the containing block width
	 node_style_set( spage.spwindow, "width", spage.spwidth + "px" );
	 node_style_set( spage.spviewport, "width", spage.spscrollwdith + "px" );
	 
	 node_style_set( spage.spviewport.childNodes[ 0 ], "width", spage.spbodywidth + "px" );
      }
   }
      
   // spageAddEventListener
   function spageAddEventListener( spage, event, proc, capture ) {
      if( event === "onchange" ) {
	 spage.onchg = proc;
      } else {
	 hop_add_native_event_listener( spage, event, proc, capture );
      }
   }

   // transitionStyle
   function spageTransitionStyle( spage ) {
      return node_computed_style_get( spage.spstyle, "cursor" );
   }

   // cssTransitionDuration
   function cssTransitionDuration( el ) {
      
      function cssGet( el, key ) {
	 const v = node_computed_style_get( el, key );
	 if( v ) {
	    return parseFloat( v );
	 } else {
	    return false;
	 }
      }
	 
      return cssGet( el, "transition-duration" )
	 || cssGet( el, "-webkit-transition-duration" )
	 || cssGet( el, "-moz-transition-duration" )
	 || cssGet( el, "-o-transition-duration" )
	 0.5;
   }
      
   // push
   function spagePush( spage, tab, tbody ) {
      
      function spagePushNone( spage, spviewport, tbody, otab ) {
	 // mark the transition style (needed on resize)
	 spage.transitionstyle = "none";
	 tbody.setAttribute( "data-transition", "none" );
	 node_style_set( otab, "display", "none" );
      }
      
      function spagePushSlide( spage, spviewport, tbody, otab ) {
	 // mark the transition style (needed on resize)
	 spage.transitionstyle = "slide";
	 tbody.setAttribute( "data-transition", "slide" );
	 node_style_set( spviewport, "left", -spage.spoffset + "px" );
      }
      
      function spagePushFade( spage, spviewport, tbody, otab ) {
	 // mark the transition style (needed on resize)
	 spage.transitionstyle = "fade";
	 tbody.setAttribute( "data-transition", "fade" );
	 node_style_set( tbody, {
	    "-webkit-transition-property": "none",
	    "-moz-transition-property": "none",
	    "-o-transition-property": "none",
	    "transition-property": "none",
	    "opacity": "0",
	    "z-index": spage.depth + "",
	    "top": "0",
	    "left": -spage.spoffset + "px"
	 } );
	 setTimeout( () => {
	       node_style_set( tbody, {
		  "webkit-transition-property": "opacity",
		  "-moz-transition-property": "opacity",
		  "-o-transition-property": "opacity",
		  "transition-property": "opacity",
		  "opacity": 1
	       } );
	       node_style_set( spage.tabs[ spage.tabs.length - 2 ], {
		  "webkit-transition-property": "opacity",
		  "-moz-transition-property": "opacity",
		  "-o-transition-property": "opacity",
		  "transition-property": "opacity",
		  "opacity": 0
	       } );
	    }, 1 );
      }
      
      function spagePushZoom( spage, spviewport, tbody, otab ) {
	 // mark the transition style (needed on resize)
	 spage.transitionstyle = "zoom";
	 tbody.setAttribute( "data-transition", "zoom" );
	 node_style_set( tbody, {
	    "-webkit-transition-property": "none",
	    "-moz-transition-property": "none",
	    "-o-transition-property": "none",
	    "transition-property": "none",
	    "z-index": spage.depth + "",
	    "top": "0",
	    "left": -spage.spoffset + "px"
	 } );
	 node_style_set( tbody, {
	    "transform": "scale( 0.90 )"
	 } );
	 setTimeout( () => {
	       node_style_set( tbody, {
		  "webkit-transition-property": "all",
		  "-moz-transition-property": "all",
		  "-o-transition-property": "all",
		  "transition-property": "all"
	       } );
	       node_style_set( tbody, {
	    	  "transform": "scale( 1 )"
	       } );
	       node_style_set( spage.tabs[ spage.tabs.length - 2 ], {
		  "webkit-transition-property": "all",
		  "-moz-transition-property": "all",
		  "-o-transition-property": "all",
		  "transition-property": "all",
	    	  "transform": "scale( 0.90 )",
		  "opacity": "0"
	       } );
	    }, 1 );
      }
      
      tab = getElement( tab );
      const spviewport = spage.spviewport;
      const otab = spage.tabs[ spage.tabs.length - 1 ];
      
      // adjust the size of the viewport
      spageResize( spage );
      
      // increment the number of pushed elements
      spage.depth++;
      spage.tabs.push( tbody );
      tbody.tab = tab;
      
      // expand the body div when necessary
      // MS 12dec2020: spoffset re-computation
      // spage.spoffset = spage.depth * spage.spwidth;
      spage.spoffset += spage.spwidth;
      spage.spscrollwidth = (spage.depth + 1) * spage.spwidth;
      
      // set the tab and viewport dimensions
      // webkit requires spviewport to larger that the sum of the bodies
      // we provision it with an extra body width
      node_style_set( spviewport, "width", spage.spbodywidth + spage.spscrollwidth + "px" );
      node_style_set( otab, "width", spage.spbodywidth + "px" );
      node_style_set( tbody, "width", spage.spbodywidth + "px" );
      
      // sptab event listener
      sptabInvokeOnselectListener( tab, tbody, "select" );
      
      // add the new tab
      dom_add_child( spviewport, tbody );
      
      // the event listeners
      spageInvokeOnchangeListener( spage, tbody, "push" );
      sptabInvokeOnselectListener( tab, tbody, "push" );
      
      // show the new tbody
      switch( spageTransitionStyle( spage ) ) {
	 case "auto":
	 case "move": spagePushSlide( spage, spviewport, tbody, otab ); break;
	 case "help": spagePushFade( spage, spviewport, tbody, otab ); break;
	 case "wait": spagePushZoom( spage, spviewport, tbody, otab ); break;
	 default: spagePushNode( spage, spviewport, tbody, otab );
      }
   }

   // pop
   function spagePop( spage, kont = undefined ) {
      
      function shrinkViewport( spage, spviewport ) {
	 spage.spscrollwidth -= spage.spoffset;
	 node_style_set( spviewport, "width", spage.spscrollwidth + "px" );
      }
      
      function invokePopListeners( spage, tbody ) {
	 spageInvokeOnchangeListener( spage, tbody, "pop" );
	 if( tbody.tab ) {
	    sptabInvokeOnselectListener( tbody.tab, tbody, "pop" );
	 }
      }
      
      function restoreStaticBody( tab ) {
	 if( tab.staticNode ) {
	    dom_add_child( tab.staticBody, tab.staticNode );
	 }
      }
      
      function spagePopNone( spage, spviewport, tbody, otab ) {
	 spviewport.removeChild( tbody );
	 restoreStaticBody( tbody.tab );
	 shrinkViewport( spage, spviewport );
	 node_style_set( otab, "display", "block" );
	 if( typeof( kont ) === "function" ) kont( spage );
	 spage.inpop = false;
      }
      
      function spagePopFade( spage, spviewport, tbody, otab ) {
	 const d = cssTransitionDuration( tbody );
	 node_style_set( tbody, "opacity", "0" );
	 node_style_set( spage.tabs[ spage.tabs.length - 1 ], "opacity", "1" );
	 
	 setTimeout( () => {
	       spviewport.removeChild( tbody );
	       restoreStaticBody( tbody.tab );
	       node_style_set( spviewport, "width", spage.spscrollwidth + "px" );
	       if( typeof( kont ) === "function" ) kont( spage );
	       spage.inpop = false;
	    }, d * 1500 );
      }
      
      function spagePopSlide( spage, spviewport, tbody, otab ) {
	 const d = cssTransitionDuration( tbody );
	 node_style_set( spviewport, "left", -spage.spoffset + "px" );
	 
	 setTimeout( () => {
	       spviewport.removeChild( tbody );
	       restoreStaticBody( tbody.tab );
	       node_style_set( spviewport, "width", spage.spscrollwidth + "px" );
	       if( typeof( kont ) === "function" ) kont( spage );
	       spage.inpop = false;
	    }, d * 1500 );
      }
      
      function spagePopZoom( spage, spviewport, tbody, otab ) {
	 const d = cssTransitionDuration( tbody );
	 node_style_set( spage.tabs[ spage.tabs.length - 1 ], "transform", "scale(1)" );
	 node_style_set( spage.tabs[ spage.tabs.length - 1 ], "opacity", "1" );
	 
	 setTimeout( () => {
	       spviewport.removeChild( tbody );
	       restoreStaticBody( tbody.tab );
	       node_style_set( spviewport, "width", spage.spscrollwidth + "px" );
	       if( typeof( kont ) === "function" ) kont( spage );
	       spage.inpop = false;
	    }, d * 1500 );
      }
      
      const spviewport = spage.spviewport;
      
      if( spage.tabs.length > 0 ) {
	 const tbody = spage.tabs[ spage.tabs.length - 1 ];
	 const otab = spage.tabs[ spage.tabs.length - 2 ];
	 
	 // mark the tab no longer pushed
	 tbody.tab.pushed = false;
	 
	 // decrement the number of pushed elements
	 spage.depth--;
	 spage.tabs.pop();
	 spage.spoffset -= spage.spwidth;
	 
	 // invoke the listener before removing any node
	 if( spage.tabs.length > 0 ) {
	    invokePopListeners( spage, spage.tabs[ spage.tabs.length - 1 ] );
	 }
	 
	 // pop the element from the gui
	 switch( spageTransitionStyle( spage ) ) {
	    case "auto":
	    case "move": spagePopSlide( spage, spviewport, tbody, otab ); break;
	    case "help": spagePopFade( spage, spviewport, tbody, otab ); break;
	    case "wait": spagePopZoom( spage, spviewport, tbody, otab ); break;
	    default: spagePopNone( spage, spviewport, tbody, otab ); break;
	 }
      }
   }
      
   // spagePopAll
   function spagePopAll( spage ) {
      while( spage.depth > 0 ) {
	 spagePop( spage );
      }
   }

   // findFromTag
   function spageFindFromTag( el, tag ) {
      let parent = el.parentNode; 
      
      while( parent ) {
	 if( parent.getAttribute( "data-hss-tag" ) === tag ) {
	    return parent;
	 } else {
	    parent = parent.parentNode;
	 }
      }
      
      throw new RefenceError( "findTag: cannot find parent spage " + el );
   }
      
   // spagePopUpdate
   function spagePopUpdate( el ) {
      
      function popBodyNode( spage, kont ) {
	 const head = spage.heads.pop();
	 const sphead = spage.sphead;
	 const spheadcontent = sphead.firstChild;
	 const spheadbutton = sphead.lastChild;
	 const tabs = spage.tabs;
	 const tbody = tabs[ tabs.length - 1 ];
	 const tab = tbody.tab;
	 
	 window.scrollTo( 0, head[ 3 ] );
	 hop_innerHTML_set( spheadbutton.firstChild, head[ 2 ] );
	 hop_innerHTML_set( spheadcontent, head[ 1 ] );
	 hop_innerHTML_set( tab.firstChild.firstChild, head[ 0 ] );
	 
	 spagePop( spage, kont );
	 
	 if( spage.depth === 0 ) spheadbutton.className = "";
      }
      
      function spageTabUpdate( spage ) {
	 
	 function update( tab, body ) {
	    const spage = spageFindFromTag( tab, "hop-spage" );
	    const spviewport = spage.spviewport;
	    
	    spageResize( spage );
	    node_style_set( body, "width", spage.spbodywidth + "px" );
	    spage.tabs.pop();
	    spage.tabs.push( body );
	    body.tab = tab;
	    dom_add_child( spviewport, body );
	 }
	 
	 const t = spage.tabs;
	 const tab = t[ t.length - 1 ];
	 
	 if( typeof( tab.svc ) === "string" ) {
	    with_hop( tab.svc, body => update( tab, tab.build( body ) ) );
	 } else {
	    with_hop( tab.svc, body => update( tab, body ) );
	 }
      }
      
      const spage = (el.getAttribute( "data-hss-tag" ) === "hop-spage") 
	 ? el : spageFindFromTag( el, "hop-spage" );
      
      if( !spage.inpop ) {
	 spage.inpop = true;
	 const tabs = spage.tabs;
	 const tbody = tabs[ tabs.length - 1 ];
	 const tparent = tabs.length > 1 ? tabs[ 1 ] : false;
	 const tab = tparent ? tparent.tab : false;
	 
	 if( tbody.tab.pushed ) {
	    if( tab 
		&& (typeof( tab.svc ) === "function" || typeof( tab.svc ) === "string")
		&& tab.getAttribute( "data-hop-svc-direction" ) === "both" ) {
	       popBodyNode( spage, spageTabUpdate );
	    } else {
	       popBodyNode( spage, false );
	    }
	 }
      }
   }

   // tabPushBody
   function tabPushBody( tab, body ) {
      const spage = spageFindFromTag( tab, "hop-spage" );
      const sphead = spage.sphead;
      const spheadcontent = sphead.firstChild;
      const spheadbutton = sphead.lastChild;
      const tabhead = tab.firstChild.firstChild;
      
      // reparent the tab nodes
      const button = Array.prototype.slice.call( spheadbutton.firstChild.childNodes, 0 );
      const content = Array.prototype.slice.call( spheadcontent.childNodes, 0 );
      const headtab = Array.prototype.slice.call( tabhead.childNodes, 0 );
      
      spage.heads.push( [ headtab, content, button, window.pageYOffset ] );
      hop_innerHTML_set( spheadcontent, headtab );
      window.scrollTo( 0, 0 );

      spheadbutton.className = "visible";
      spagePush( spage, tab, body );
   }
      
   // spagePushService
   function spagePushService( tab, svc ) {
      if( !tab.pushed ) {
	 tab.pushed = true;
	 with_hop( svc(),
	    body => { 
	       tab.svc = svc;
	       tab.staticNode = undefined;
	       tabPushBody( tab, body );
	    },
	    xhr => {
	       tab.pushed = false;
	    } );
      }
   }
      
   // spagePushUrl
   function spagePushUrl( tab, svcurl, build ) {
      if( !tab.pushed ) {
	 tab.pushed = true;

	 with_hop( svcurl,
	    body => {
	       tab.svc = svcurl;
	       tab.staticNode = undefined;
	       tab.build = build;
	       tabPushBody( tab, build( body ) );
	    },
	    xhr => {
	       tab.pushed = false;
	    } );
      }
   }
      
   // spagePushNode
   function spagePushNode( tab, node ) {
      // save the static-body that will be restore when popped
      const p = node.parentNode;
      tab.pushed = true;
      tab.staticNode = node;
      tab.staticBody = p;
      p.removeChild( node );
      
      tabPushBody( tab, node );
   }
      
   // spageTabPush
   function spageTabPush( tab ) {
      const el = getElement( tab );
      const svc = el.getAttribute( "data-hop-svc" );
      
      if( typeof( svc ) === "string" ) {
	 spagePushService( el, () => svc );
      } else {
	 spagePushNode( el, el.lastChild.firstChild );
      }
   }
      
   // spageTabUpdate
   function spageTabUpdate( tab ) {
      
      function update( body ) {
	 const spage = findSpageFromTag( tab, "hop-spage" );
	 const spviewport = spage.spviewport;
	 
	 spageResize( spage );
	 node_style_set( body, "width", spage.spbodywidth + "px" );
	 spage.tabs.pop();
	 spage.tabs.push( body );
	 body.tab = tab;
	 dom_add_child( spviewport, body );
      }
      
      tab = getElement( tab );
      
      if( typeof( tab.svc ) === "string" ) {
	 with_hop( tab.svc, body => update( tab.build( body ) ) );
      } else {
	 with_hop( tab.svc, update );
      }
   }
      
   // spageTabPop
   function spageTabPop( tab ) {
      spagePopUpdate( tab );
   }
      
   // frameBorderHeight
   function frameBorderHeight( el ) {
      return parseInt( node_computed_style_get( el, "borderBottomWidth" ) )
	 + parseInt( node_computed_style_get( el, "borderTopWidth" ) );
      }
      
   // frameBorderWidth
   function frameBorderWidth( el ) {
      return parseInt( node_computed_style_get( el, "borderRightWidth" ) )
	 + parseInt( node_computed_style_get( el, "borderLeftWidth" ) );
   }
      
   // invokeOnchangeListener
   function spageInvokeOnchangeListener( spage, tbody, action ) {
      if( typeof( spage.onchg ) === "function" ) {
	 const evt = new HopEvent( "change", spage );
	 evt.target = tbody;
	 evt.action = action;
	 spage.onchg( evt );
      }
   }
      
   // invokeOnselectListener
   function sptabInvokeOnselectListener( sptab, tbody, action ) {
      if( typeof( sptab.onselect ) === "function" ) {
	 const evt = new HopEvent( "select", sptab );
	 evt.target = tbody;
	 evt.action = action;
	 sptab.onselect( evt );
      }
   }
      
   // Spage constructor
   function spageInit( el, onchange ) {
      const spage = getElement( el );
      const childs = spage.childNodes;
      
      spage.sphead = childs[ 0 ];
      spage.spstyle = childs[ 1 ];
      spage.spwindow = childs[ 2 ];
      spage.spviewport = spage.spwindow.firstChild;
      spage.depth = 0;
      spage.tabs = [ spage.spviewport.firstChild ];
      spage.heads = [];
      spage.inpop = false;
      spage.onchg = onchange;
      spage.transitionstyle = "none";
      spage.hop_add_event_listener = spageAddEventListener;
      spage.hop_update = spageUpdate;
      
      spage.pop = function() {
	 spagePopUpdate( this );
      }
      
      // adjust the body size
      spageResetSize( spage );

      // set the transition effet
      setTimeout( () => node_style_set( spage.spviewport, {
	 "-webkit-transition-property": "all",
	 "-moz-transition-property": "all",
	 "-o-transition-property": "all",
	 "transition-property": "all"
      } ), 1 );
   }
      
   return { 
      spageInit: spageInit,
      sptabAddEventListener: sptabAddEventListener,
      spagePopUpdate: spagePopUpdate, 
      spagePushService: spagePushService, 
      spagePushUrl: spagePushUrl, 
      spagePushNode: spagePushNode,
   };
})();
