/*=====================================================================*/
/*    serrano/prgm/project/hop/1.10.x/share/hop-window.js              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Sep 19 14:46:53 2007                          */
/*    Last change :  Mon Nov 10 17:27:24 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP unified window API                                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Dynamic load                                                     */
/*---------------------------------------------------------------------*/
dom_add_head_script( hop_share_directory() + "/hop-fx.js" );

/*---------------------------------------------------------------------*/
/*    HopWindowEvent ...                                               */
/*---------------------------------------------------------------------*/
function HopWindowEvent() {
   this.isStopped = false;
   this.preventDefault = function() { };
   this.stopPropagation = this.preventDefault;
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_invoke_listener ...                                  */
/*---------------------------------------------------------------------*/
function hop_iwindow_invoke_listener( lst, event ) {
   while( sc_isPair( lst ) ) {
      lst.car( event );

      if( event.isStopped ) break;

      lst = lst.cdr;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_add_event_listener ...                               */
/*---------------------------------------------------------------------*/
function hop_iwindow_add_event_listener( event, proc, capture ) {
   if( event === "iconify" )
      return this.oniconify = sc_cons( proc, this.oniconify );

   if( event === "resize" )
      return this.onresize = sc_cons( proc, this.onresize );

   if( event === "maximize" )
      return this.onmaximize = sc_cons( proc, this.onmaximize );

   if( event === "close" )
      return this.onclose = sc_cons( proc, this.onclose );

   if( event === "move" )
      return this.onmove = sc_cons( proc, this.onmove );

   if( event === "raise" )
      return this.onraise = sc_cons( proc, this.onraise );

   return hop_add_native_event_listener( this, event, proc, capture );
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_remove_event_listener ...                            */
/*---------------------------------------------------------------------*/
function hop_iwindow_remove_event_listener( event, proc ) {
   if( event === "iconify" )
      return this.oniconify = sc_deleteBang( proc, this.oniconify );

   if( event === "resize" )
      return this.onresize = sc_deleteBang( proc, this.onresize );

   if( event === "maximize" )
      return this.onmaximize = sc_deleteBang( proc, this.onmaximize );

   if( event === "close" )
      return this.onclose = sc_deleteBang( proc, this.onclose );

   if( event === "move" )
      return this.onmove = sc_deleteBang( proc, this.onmove );

   if( event === "raise" )
      return this.onraise = sc_deleteBang( proc, this.onraise );

   return hop_remove_native_event_listener( this, event, proc, capture );
}

/*---------------------------------------------------------------------*/
/*    hop_get_window ...                                               */
/*---------------------------------------------------------------------*/
function hop_get_window( o ) {
   if( hop_is_html_element( o ) ) {
      return o;
   } else {
      var win = document.getElementById( o );

      if( win ) {
	 return win;
      } else {
	 return window;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_zindex ...                                           */
/*---------------------------------------------------------------------*/
var hop_iwindow_zindex = 0;
var hop_iwindow_count = 0;

/*---------------------------------------------------------------------*/
/*    hop_iwindow_close ...                                            */
/*---------------------------------------------------------------------*/
function hop_iwindow_close( win ) {
   var evt = new HopWindowEvent();

   hop_iwindow_invoke_listener( win.onclose, evt );

   if( !evt.isStopped ) node_style_set( win, "display", "none" );
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_maximize ...                                         */
/*---------------------------------------------------------------------*/
function hop_iwindow_maximize( win ) {
   if( win.resizable ) {
      var evt = new HopWindowEvent();

      hop_iwindow_invoke_listener( win.onmaximize, evt );
      if( evt.isStopped ) return;
      
      if( win.maximized ) {
	 win.maximized = false;

	 node_style_set( win.el_win, "width", win.oldwidth );
	 node_style_set( win.el_win, "height", win.oldheight );
	 node_style_set( win, "top", win.oldtop );
	 node_style_set( win, "left", win.oldleft );
      } else {
	 win.maximized = true;
      
	 win.oldwidth = node_style_get( win.el_win, "width" );
	 win.oldheight = node_style_get( win.el_win, "height" );
	 win.oldtop = node_style_get( win, "top" );
	 win.oldleft = node_style_get( win, "left" );

	 if( win.user_parent ) {
	    var p = win.parentNode;

	    if( win.el_shadow_box != null ) {
	       node_style_set( win.el_win, "width",
			       (p.offsetWidth - win.el_shadow_box.offsetWidth) +
			       "px" );
	       node_style_set( win.el_win, "height",
			       (p.offsetHeight - win.el_shadow_box.offsetHeight) +
			       "px" );
	    } else {
	       node_style_set( win.el_win, "width", p.offsetWidth + "px" );
	       node_style_set( win.el_win, "height", p.offsetHeight + "px" );
	    }

	    node_style_set( win, "top", (hop_element_y( p ) + 1) + "px" );
	    node_style_set( win, "left", (hop_element_x( p ) + 1) + "px" );
	 } else {
	    if( win.el_shadow_box != null ) {
	       node_style_set( win.el_win, "width",
			       window.innerWidth -
			       (win.el_shadow_box.offsetWidth - 2) + "px" );
	       node_style_set( win.el_win, "height",
			       window.innerHeight -
			       (win.el_shadow_box.offsetHeight - 2) + "px" );
	    } else {
	       node_style_set( win.el_win, "width",
			       (window.innerWidth - 2) + "px" );
	       node_style_set( win.el_win, "height",
			       (window.innerHeight - 2) + "px" );
	    }

	    node_style_set( win, "top", 0 );
	    node_style_set( win, "left", 0 );
	 }
      }

      hop_iwindow_invoke_listener( win.onresize, evt );
      if( evt.isStopped ) return;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_iconify ...                                          */
/*---------------------------------------------------------------------*/
function hop_iwindow_iconify( win ) {
   var evt = new HopWindowEvent();

   hop_iwindow_invoke_listener( win.oniconify, evt );
   
   if( !evt.isStopped ) {
      if( win.iconifiedp ) {
	 win.iconifiedp = false;
	 if( win.ondeiconify ) win.ondeiconify();
      } else {
	 if( win.style.position == "fixed" ) {
	    var old = win.offsetTop;
	    // MS 3may2006: I think that for IE, we have to use
	    // document.documentElement.scrollTop or document.body.scrollTop
	    node_style_set( win, "top", (old + window.pageYOffset) + "px" );
	    node_style_set( win, "position", "absolute" );
	 } else {
	    var old = win.offsetTop;
	    node_style_set( win, "top", (old - window.pageYOffset) + "px" );
	    node_style_set( win, "position", "fixed" );
	 }

	 /* user event */
	 win.iconifiedp = true;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_raise ...                                            */
/*---------------------------------------------------------------------*/
function hop_iwindow_raise( win ) {
   if( hop_iwindow_zindex < 1000 ) {
      node_style_set( win, "z-index", ++hop_iwindow_zindex );
   } else {
      var w = document.getElementsByName( "hop-window" );
      var i;

      for( i = 0; i < w.length; i++ ) {
	 if( w[ i ].style[ "z-index" ] > 0 )
	    node_style_set( w[ i ], "z-index", w[ i ].style[ "z-index" ] - 1 );
      }
      node_style_set( win, "z-index", 999 );
   }

   /* user event */
   if( win.onraise ) {
      hop_iwindow_invoke_listener( win.onraise, new HopWindowEvent() );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_drag ...                                             */
/*---------------------------------------------------------------------*/
function hop_iwindow_drag( event, win ) {
   var dx0 = hop_event_mouse_x( event ) - hop_element_x( win );
   var dy0 = hop_event_mouse_y( event ) - hop_element_y( win );
   var dx = hop_event_mouse_x( event ) - win.offsetLeft;
   var dy = hop_event_mouse_y( event ) - win.offsetTop;
   var ocursor = win.el_handle.style.cursor;
   var obg = node_style_get( win.el_win, "background" );
   var p = win.parentNode;
   var px = hop_element_x( p );
   var py = hop_element_y( p );

   hop_iwindow_raise( win );
   
   node_style_set( win.el_content, "visibility", "hidden" );
   node_style_set( win.el_handle, "cursor", "move" );
   node_style_set( win.el_win, "background", "white" );

   var mousemove = function( event ) {
      var nx = (hop_event_mouse_x( event ) - dx);
      var ny = (hop_event_mouse_y( event ) - dy);

      if( win.user_parent ) {
	 var nx0 = hop_event_mouse_x( event ) - dx0;
	 var ny0 = hop_event_mouse_y( event ) - dy0;
	 
	 if( (nx0 > px) && ((nx0 + win.offsetWidth) < (px + p.offsetWidth)) ) {
	    node_style_set( win, "left", nx + "px" );
	 }
	 if( (ny0 > py) && ((ny0 + win.offsetHeight) < (py + p.offsetHeight)) ) {
	    node_style_set( win, "top", ny + "px" );
	 }
      } else {
	 if( nx > 0 ) node_style_set( win, "left", nx + "px" );
	 if( ny > 0 ) node_style_set( win, "top", ny + "px" );
      }
   }

   var mouseup = function( event ) {
      var evt = new HopWindowEvent();
      
      hop_remove_event_listener( document, "mousemove", mousemove );
      hop_remove_event_listener( document, "mouseup", mouseup );
      node_style_set( win.el_handle, "cursor", ocursor );
      node_style_set( win.el_content, "visibility", "visible" );
      node_style_set( win.el_win, "background", obg );

      /* user event */
      if( win.onmove ) hop_iwindow_invoke_listener( win.onmove, evt );
   }
   
   hop_add_event_listener( document, "mousemove", mousemove );
   hop_add_event_listener( document, "mouseup", mouseup );

   hop_stop_propagation( event );
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_resize ...                                           */
/*---------------------------------------------------------------------*/
function hop_iwindow_resize( win, w, h ) {
   if( win.resizable ) {
      node_style_set( win.el_win, "width",
		      ((typeof w)=="number") ? (w + 1 + "px") : w);
      node_style_set( win.el_win, "height",
		      ((typeof h)=="number") ? (h + 25 + 1 + "px") : h);
   }
   return win;
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_evresize_inner ...                                   */
/*---------------------------------------------------------------------*/
function hop_iwindow_evresize_inner( event, win, widthp, heightp ) {
   var x0 = hop_event_mouse_x( event );
   var y0 = hop_event_mouse_y( event );
   var w0 = win.el_win.offsetWidth;
   var h0 = win.el_win.offsetHeight;
   var obg = node_style_get( win.el_win, "background" );
   var wx0 = hop_element_x( win );
   var wy0 = hop_element_y( win );
   var pow = hop_element_x( win.parentNode ) + win.parentNode.offsetWidth - 8;
   var poh = hop_element_y( win.parentNode) + win.parentNode.offsetHeight - 8;
   var mousemove;

   node_style_set( win.el_content, "border", "0" );
   node_style_set( win.el_content, "display", "none" );
   node_style_set( win.el_win, "background", "white" );

   if( widthp && heightp ) {
      mousemove = function( event ) {
	 var nw = (w0 + (hop_event_mouse_x( event ) - x0));
	 var nh = (h0 + (hop_event_mouse_y( event ) - y0));

	 if( (nw > 0) && ((nw + wx0) < pow) &&
	     (nh > 0) && ((nh + wy0) < poh) ) {
	    node_style_set( win.el_win, "width", nw + "px" );
	    node_style_set( win.el_win, "height", nh + "px" );
	 }
      };
      hop_add_event_listener( document, "mousemove", mousemove );
   } else {
      if( widthp ) {
	 mousemove = function( event ) {
	    var nw = (w0 + (hop_event_mouse_x( event ) - x0));

	    if( (nw > 0) && ((nw + wx0) < pow) )
	       node_style_set( win.el_win, "width", nw + "px" );
	 };
	 hop_add_event_listener( document, "mousemove", mousemove );
      } else {
	 if( heightp ) {
	    mousemove = function( event ) {
	       var nh = (h0 + (hop_event_mouse_y( event ) - y0));

	       if( (nh > 0) && ((nh + wy0) < poh) )
		  node_style_set( win.el_win, "height", nh + "px" );
	    };
	    hop_add_event_listener( document, "mousemove", mousemove );
	 } else {
	    var l0 = win.offsetLeft;
	    mousemove = function( event ) {
	       var w = w0 + (x0 - hop_event_mouse_x( event ));

	       node_style_set( win ,"left", ((l0 + w0) - w) + "px" );
	       node_style_set( win.el_win, "width", w + "px" );
	       node_style_set( win.el_win, "height", (h0 + (hop_event_mouse_y( event ) - y0)) + "px" );
	    }
	    hop_add_event_listener( document, "mousemove", mousemove );
	 }
      }
   }

   var mouseup = function( event ) {
      node_style_set( win.el_win, "background", obg );
      node_style_set( win.el_content, "display", "block" );
      hop_remove_event_listener( document, "mousemove", mousemove );
      hop_remove_event_listener( document, "mouseup", mouseup );

      /* user event */
      if( win.onresize ) {
	 hop_iwindow_invoke_listener( win.onresize, new HopWindowEvent() );
      }
   }

   hop_add_event_listener( document, "mouseup", mouseup );
   
   hop_stop_propagation( event );
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_evresize ...                                         */
/*---------------------------------------------------------------------*/
function hop_iwindow_evresize( event, win, widthp, heightp ) {
   if( win.resizable )
      return hop_iwindow_evresize_inner( event, win, widthp, heightp );
   else
      return false;
}

/*---------------------------------------------------------------------*/
/*    make_hop_iwindow ...                                             */
/*---------------------------------------------------------------------*/
function make_hop_iwindow( id, klass, parent ) {
   var win = document.createElement( "div" );
   var cla = (klass == "hop-window" ? klass : "hop-window " + klass);
   win.id = id;
   win.className = cla;
   win.name = "hop-window";

   var handle = "\
<table id='" + id + "-handle' class='hop-window-handle' width='100%'\n\
       cellpadding='0' cellspacing='0' border='0'>\n\
  <tr class='hop-window-handle'>\n\
    <td class='hop-window-iconify' align='left' onclick='hop_iwindow_iconify( document.getElementById( \"" + id + "\") )'>&#160;</td>\n\
    <td class='hop-window-maximize' align='left' onclick='hop_iwindow_maximize( document.getElementById( \"" + id + "\") )'>&#160;</td>\n\
    <td class='hop-window-up-title'>\n\
      <table class='hop-window-title' width='100%' border='0' cellspacing='0' cellpadding='0'>\n\
        <tr>\n\
          <td class='hop-window-title-left'>&#160;</td>\n\
          <td id='" + id + "-title' class='hop-window-title-middle'>title</td>\n\
          <td class='hop-window-title-right'>&#160;</td>\n\
        </tr>\n\
      </table>\n\
    </td> \n\
    <td class='hop-window-close' align='right' onclick='hop_iwindow_close( document.getElementById( \"" + id + "\" ) )'>&#160;</td>\n\
  </tr>\n\
</table>\n";

   var foot = "\
<table width='100%' border='0' cellspacing='0' cellpadding='0'>\n\
  <tr>\n\
    <td id='" + id + "-resize-left' class='hop-window-bottom-left'>&#160;</td>\n\
    <td id='" + id + "-resize-middle' class='hop-window-bottom-middle'>&#160;</td>\n\
    <td id='" + id + "-resize-right' class='hop-window-bottom-right'>&#160;</td>\n\
  </tr>\n\
</table>";

   var t = "\n\
<div class='hop-window-inner' id='" + id + "-win'>\n\
  <div class='hop-window-content' id='" + id + "-content'>\n\</div>\n\
  <div class='hop-window-handle' id='" + id + "-handle'>" + handle + "</div>\n\
  <div class='hop-window-foot'>" + foot + "</div>\n\
</div>";

   win.innerHTML = hop_fx_make_shadow( 'hop-window-shadow', t );

   if( parent ) {
      if( (parent instanceof String) || (typeof parent === "string") )
	 parent = document.getElementById( parent );
   
      parent.appendChild( win );
   } else {
      document.body.appendChild( win );
   }

   win.user_parent = (parent && parent != undefined && parent != document.body)
      ? parent : false;

   win.el_handle = document.getElementById( id + "-handle" );
   win.el_win = document.getElementById( id + "-win" );
   win.el_content = document.getElementById( id + "-content" );
   win.el_resize_left = document.getElementById( id + "-resize-left" );
   win.el_resize_middle = document.getElementById( id + "-resize-middle" );
   win.el_resize_right = document.getElementById( id + "-resize-right" );
   
   win.el_title = document.getElementById( id + "-title" );

   win.el_shadow = win.childNodes[ 0 ];
   win.el_shadow_box = document.getElementById( id + "-shadow-box" );
   win.resizable = true;
   win.iconifiedp = false;
   win.oniconify = false;
   win.onresize = false;
   win.onmaximize = false;

   hop_add_event_listener(
      win.el_handle,
      "mousedown",
      function( event ) { hop_iwindow_drag( event, win ) } );

   hop_add_event_listener(
      win.el_resize_middle,
      "mousedown",
      function( event ) { hop_iwindow_evresize( event, win, false, true ) } );

   hop_add_event_listener(
      win.el_resize_right,
      "mousedown",
      function( event ) { hop_iwindow_evresize( event, win, true, true ) } );

   hop_add_event_listener(
      win.el_resize_left,
      "mousedown",
      function( event ) { hop_iwindow_evresize( event, win, false, false ) } );

   win.hop_add_event_listener = hop_iwindow_add_event_listener;
   win.hop_remove_event_listener = hop_iwindow_remove_event_listener;
   
   return win;
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_src_set ...                                          */
/*---------------------------------------------------------------------*/
function hop_iwindow_src_set( win, src, width, height ) {
   if( hop_is_html_element( src ) ) {
      var c = win.el_content.childNodes;
      var i = c.length;

      while( i > 0 ) {
	 i--;
	 win.el_content.removeChild( c[ i ] );
      }
      win.el_content.appendChild( src );
   } else {
      var cb = function( html ) {
	 if( html ) {
	    hop_innerHTML_set( win.el_content, html );
		   
	    if( !width && !height ) {
	       var c = win.el_content.childNodes[ 0 ];

	       if( hop_is_html_element( c ) ) {
		  var w = node_style_get( c, "width" );
		  var h = node_style_get( c, "height" );

		  if( w ) node_style_set( win.el_win, "width", w );
		  if( h ) node_style_set( win.el_win, "height", h );
	       }
	    }
	 }
      };

      if( typeof src == "function" ) {
	 hop( src(), cb );
      } else {
	 if( (src instanceof String) || (typeof src == "string") ) {
	    hop( src, cb );
	 } else {
	    alert( "*** Hop Error, Illegal `iwindow' content -- " + src
		   + " (" + typeof src + ")" );
	 }
      }
   }

}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_open ...                                             */
/*---------------------------------------------------------------------*/
function hop_iwindow_open( id, src, title, klass, width, height, x, y, bg, resizable, parent ) {
   var win = document.getElementById( id );
   var isnew = false;

   klass = klass ? ("hop-window " + klass) : "hop-window";
   if( win == null ) {
      win = make_hop_iwindow( id, klass, parent );
      isnew = true;
   } else {
      node_style_set( win, "display", "block" );
   }

   /* start hidden otherwise we loose the border on drag! */
   if( bg ) {
      win.el_content.bg = bg;
      node_style_set( win.el_content, "background", bg );
   }
   node_style_set( win.el_content, "display", "none" );

   hop_iwindow_src_set( win, src, width, height );
   
   win.el_title.innerHTML = title ? title : id;

   if( isnew ) {
      var val_to_px = function( x, def ) {
	 if( !x ) return def;
	 if( typeof x === "number" ) return x + "px";
	 return x;
      }
      
      node_style_set( win, "left", val_to_px( x, "0px" ) );
      node_style_set( win, "top", val_to_px( y, "0px" ) );
      
      node_style_set( win.el_win, "width", val_to_px( width, "200px" ) );
      node_style_set( win.el_win, "height", val_to_px( height, "200px" ) );
   }

   win.resizable = resizable;
   node_style_set( win.el_content, "display", "block" );

   hop_iwindow_raise( win );

   return win;
}


/*---------------------------------------------------------------------*/
/*    pre-allocated keywords                                           */
/*---------------------------------------------------------------------*/
var Ktitle = sc_jsstring2keyword( "title" );
var Kid = sc_jsstring2keyword( "id" );
var Kparent = sc_jsstring2keyword( "parent" );
var Ksrc = sc_jsstring2keyword( "src" );
var Kclass = sc_jsstring2keyword( "class" );
var Kwidth = sc_jsstring2keyword( "width" );
var Kheight = sc_jsstring2keyword( "height" );
var Kleft = sc_jsstring2keyword( "left" );
var Ktop = sc_jsstring2keyword( "top" );
var Kbackground = sc_jsstring2keyword( "background" );
var Kbg = sc_jsstring2keyword( "bg" );
var Kresizable = sc_jsstring2keyword( "resizable" );
var Kprop = sc_jsstring2keyword( "prop" );
var Kfullscreen = sc_jsstring2keyword( "fullscreen" );

/*---------------------------------------------------------------------*/
/*    hop_window_open ...                                              */
/*---------------------------------------------------------------------*/
/*** META ((export window-open)) */
function hop_window_open() {
   var title = "Hop", id, parent, src, klass, width, height, left, top;
   var background, resizable = true;
   var prop = "";
   var i = 0, l = arguments.length;
   var body = false;

   function prop_to_string( val ) {
      if( (val instanceof Boolean) || (typeof val == "boolean") )
	 return  val ? "yes" : "no";
      return val + "";
   }

   function unpx( x ) {
      if( (x instanceof String) || (typeof x == "string" ) 
	  && ( x.lastIndexOf( "px" ) === x.length ) ) {
	 return x.substring( 0, x.length - 2 );
      } else {
	 return x;
      }
   }
   
   function native_window_open() {
      if( typeof src == "function" ) src = src();

      if( (src instanceof String) || (typeof src === "string" ) ) {
	 if( width ) prop += ",width=" + unpx( width );
	 if( height ) prop += ",height=" + unpx( height );
	 if( left != undefined ) prop += ",screenX=" + left + ",left=" + left;
	 if( top != undefined  ) prop += ",screenY=" + top + ",top=" + top;

	 if( prop.charAt( 0 ) == ',' ) {
	    prop = prop.substring( 1, prop.length );
	 }

	 var win = window.open( src, title, prop );
	 win.iconify = function( w ) { ; };
	 win.maximize = function( w ) { ; };
	 win.raise = function( w ) { ; };
	 win.resize = function( w, h ) { win.resizeTo( w, h ); };

	 if( width && height ) win.resizeTo( width, height );
	 
	 return win;
      } else {
	 if( src ) {
	    throw new Error( "window-open: illegal :src " + src );
	 } else {
	    throw new Error( "window-open: no :src specified" );
	 }
      }
   }

   function inner_window_open() {
      if( !id ) id = "hop-window" + hop_iwindow_count++;
      
      var win = hop_iwindow_open( id, src ? src : body, title, klass, width, height, left, top, background, resizable, parent );
      win.close = function() { return hop_iwindow_close( win ); };
      win.iconify = function() { return hop_iwindow_iconify( win ); };
      win.maximize = function() { return hop_iwindow_maximize( win ); };
      win.raise = function() { return hop_iwindow_raise( win ); };
      win.resize = function( w, h ) { return hop_iwindow_resize( win, w, h ); };

      return win;
   }

   // arguments parsing
   while( i < l ) {
      var k = arguments[ i++ ];

      if( sc_isKeyword( k ) ) {
	 if( k === Ktitle ) {
	    title = arguments[ i++ ];
	 } else {
	    if( k === Kid ) {
	       id = arguments[ i++ ];
	    } else {
	       if( k === Kparent ) {
		  parent = arguments[ i++ ];
	       } else {
		  if( k === Ksrc ) {
		     if( body ) {
			throw new Error( "window-open: both :src and body specified: " + arguments[ i++ ] );
		     } else {
			src = arguments[ i++ ];
		     }
		  } else {
		     if( k === Kclass ) {
			klass = arguments[ i++ ];
		     } else {
			if( k === Kwidth ) {
			   width = arguments[ i++ ];
			} else {
			   if( k === Kheight ) {
			      height = arguments[ i++ ];
			   } else {
			      if( k === Kleft ) {
				 left = arguments[ i++ ];
			      } else {
				 if( k === Ktop ) {
				    top = arguments[ i++ ];
				 } else {
				    if( k === Kresizable ) {
				       resizable = arguments[ i++ ];
				       prop += ",resizable=" + 
					  prop_to_string( resizable );
				    } else {
				       if( k === Kbackground || k === Kbg ) {
					  background = arguments[ i++ ];
				       } else {
					  if( k === Kprop ) {
					     prop += arguments[ i++ ];
					  } else {
					     prop +=
						"," + sc_keyword2jsstring(k) + "=" + 
						prop_to_string(arguments[i++]);
					  }
				       }
				    }
				 }
			      }
			   }
			}
		     }
		  }
	       }
	    }
	 }
      } else {
	 if( !src && parent ) {
	    if( body ) {
	       dom_append_child( div, k );
	    } else {
	       body = dom_create( "div", k );
	    }
	 } else if( i < l ) {
	    throw new Error( "window-open: illegal argument -- "
			     + ">" + k + "<"
			     + ", " + arguments[ i ] + " ...");
	 } else if( i > 0 ) {
	    throw new Error( "window-open: illegal argument -- "
			     + "... " + arguments[ i - 2 ] + ","
			     + ">" + k + "<" );
			     
	 } else {
	    throw new Error( "window-open: illegal argument -- "
			     + k );
	 }
      }
   }

   if( !parent ) {
      // this is a plain native window
      return native_window_open();
   } else {
      return inner_window_open();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_iconify ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export window-iconify)) */
function hop_window_iconify( o ) {
   return hop_get_window( o ).iconify();
}

/*---------------------------------------------------------------------*/
/*    hop_window_close ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export window-close)) */
function hop_window_close( o ) {
   return hop_get_window( o ).close();
}

/*---------------------------------------------------------------------*/
/*    hop_window_maximize ...                                          */
/*---------------------------------------------------------------------*/
function hop_window_maximize( o ) {
   return hop_get_window( o ).maximize();
}

/*---------------------------------------------------------------------*/
/*    hop_window_raise ...                                             */
/*---------------------------------------------------------------------*/
function hop_window_raise( o ) {
   return hop_get_window( o ).raise();
}

/*---------------------------------------------------------------------*/
/*    hop_window_resize ...                                            */
/*---------------------------------------------------------------------*/
function hop_window_resize( o, w, h ) {
   return hop_get_window( o ).resize( w, h );
}

/*---------------------------------------------------------------------*/
/*    hop_is_iwindow ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export iwindow?)) */
function hop_is_iwindow( o ) {
   return o.hop_add_event_listener == hop_iwindow_add_event_listener;
}

/*---------------------------------------------------------------------*/
/*    hop_is_window ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export window?)) */
function hop_is_window( o ) {
   return hop_is_iwindow( o ) || (win instanceof Window);
}

/*---------------------------------------------------------------------*/
/*    hop_window_x ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export window-x)) */
function hop_window_x( win ) {
   if( win instanceof Window ) {
      if( "left" in win ) return win.left;
      if( "screenX" in win ) return win.screenX;
      return 0;
   } else {
      return win.offsetLeft;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_x_set ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export window-x-set!)) */
function hop_window_x_set( win, x ) {
   if( win instanceof Window ) {
      if( "top" in win ) return win.moveTo( x, win.top );
      if( "screenY" in win ) return win.moveTo( x, win.screenY );
   } else {
      if( typeof x === "number" )
	 node_style_set( win, "left", (x + "px") );
      else
	 node_style_set( win, "left", x );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_y ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export window-y)) */
function hop_window_y( win ) {
   if( win instanceof Window ) {
      if( "top" in win ) return win.top;
      if( "screenY" in win ) return win.screenY;
      return 0;
   } else {
      return win.offsetTop;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_y_set ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export window-y-set!)) */
function hop_window_y_set( win, y ) {
   if( win instanceof Window ) {
      if( "left" in win ) return win.moveTo( win.left, y );
      if( "screenX" in win ) return win.moveTo( win.screenX, y );
   } else {
      if( typeof x === "number" )
	 node_style_set( win, "top", (y + "px") );
      else
	 node_style_set( win, "top", y );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_width ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export window-width)) */
function hop_window_width( win ) {
   if( win instanceof Window ) {
      if( "outerWidth" in win ) return win.outerWidth;
      if( "innerWidth" in win ) return win.innerWidth;
      return 0;
   } else {
      return node_style_get( win.el_win, "width" );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_width_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export window-width-set!)) */
function hop_window_width_set( win, width ) {
   if( win instanceof Window ) {
      if( "outerHeight" in win ) return win.resizeTo( width, win.outerHeight );
      if( "innerHeight" in win ) return win.resizeTo( width, win.innerHeight );
      return 0;
   } else {
      if( typeof width === "number" ) {
	 return node_style_set( win.el_win, "width", (width + "px") );
      } else {
	 return node_style_set( win.el_win, "width", width );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_height ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export window-height)) */
function hop_window_height( win ) {
   if( win instanceof Window ) {
      if( "outerHeight" in win ) return win.outerHeight;
      if( "innerHeight" in win ) return win.innerHeight;
      return 0;
   } else {
      return node_style_get( win.el_win, "height" );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_height_set ...                                        */
/*---------------------------------------------------------------------*/
/*** META ((export window-height-set!)) */
function hop_window_height_set( win, height ) {
   if( win instanceof Window ) {
      if( "outerHeight" in win ) return win.resizeTo( win.outerWidth, height );
      if( "innerHeight" in win ) return win.resizeTo( win.innerWidth, height ); 
      return 0;
   } else {
      if( typeof height === "number" ) {
	 return node_style_set( win.el_win, "height", (height + "px") );
      } else {
	 return node_style_set( win.el_win, "height", height );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_title ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export window-title)) */
function hop_window_title( win ) {
   if( win instanceof Window ) {
      return win.name;
   } else {
      return win.el_title.innerHTML;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_title_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export window-title-set!)) */
function hop_window_title_set( win, title ) {
   if( win instanceof Window ) {
      try {
	 return win.name = title;
      } catch( _ ) {
      }
   } else {
      return hop_innerHTML_set( win.el_title, title );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_window_style_get ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export window-style-get)) */
function hop_window_style_set( win, prop ) {
   return node_style_get( win.el_win, prop );
}

/*---------------------------------------------------------------------*/
/*    hop_window_style_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export window-style-set!)) */
function hop_window_style_set( win, prop, val ) {
   return node_style_set( win.el_win, prop, val );
}
