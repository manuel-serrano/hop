/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-window.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Sep 19 14:46:53 2007                          */
/*    Last change :  Sat Sep 29 08:19:37 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HOP unified window API                                           */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Dynamic load                                                     */
/*---------------------------------------------------------------------*/
dom_add_head_script( hop_share_directory() + "/hop-fx.js" );
   
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
	 throw new Error( "Cannot find window " + o );
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
   node_style_set( win, "display", "none" );
   if( win.onclose ) win.onclose();
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_maximize ...                                         */
/*---------------------------------------------------------------------*/
function hop_iwindow_maximize( win ) {
   if( win.resizable ) {
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

      /* user event */
      if( win.onresize ) win.onresize();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_iconify ...                                          */
/*---------------------------------------------------------------------*/
function hop_iwindow_iconify( win ) {
   if( win.iconifiedp ) {
      win.iconifiedp = false;
   } else {
   }
   
   if( win.iconify ) {
      win.iconify();
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
   }

   /* user event */
   if( win.iconifiedp ) {
      if( win.ondeiconify ) win.ondeiconify();
      win.iconifiedp = false;
   } else {
      if( win.oniconify ) win.oniconify();
      win.iconifiedp = true;
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
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_drag ...                                             */
/*---------------------------------------------------------------------*/
function hop_iwindow_drag( event, win ) {
   var dx = hop_event_mouse_x( event ) - win.offsetLeft;
   var dy = hop_event_mouse_y( event ) - win.offsetTop;
   var ocursor = win.el_handle.style.cursor;

   hop_iwindow_raise( win );

   node_style_set( win.el_content, "visibility", "hidden" );
   node_style_set( win.el_handle, "cursor", "move" );
   node_style_set( win.el_win, "background", "white" );

   var mousemove = function( event ) {
      var nx = (hop_event_mouse_x( event ) - dx);
      var ny = (hop_event_mouse_y( event ) - dy);

      if( win.user_parent ) {
	 var p = win.parentNode;
	 var px = hop_element_x( p );
	 var py = hop_element_y( p );

	 if( (nx > px) && ((nx + win.offsetWidth) < (px + p.offsetWidth)) ) {
	    node_style_set( win, "left", nx + "px" );
	 }
	 if( (ny > py) && ((ny + win.offsetHeight) < (py + p.offsetHeight)) ) {
	    node_style_set( win, "top", ny + "px" );
	 }
      } else {
	 if( nx > 0 ) node_style_set( win, "left", nx + "px" );
	 if( ny > 0 ) node_style_set( win, "top", ny + "px" );
      }
   }

   var mouseup = function( event ) {
      hop_remove_event_listener( document, "mousemove", mousemove );
      hop_remove_event_listener( document, "mouseup", mouseup );
      node_style_set( win.el_handle, "cursor", ocursor );
      node_style_set( win.el_content, "visibility", "visible" );
      node_style_set( win.el_win, "background", "none" );

      /* user event */
      if( win.ondrag ) win.ondrag();
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
   var mousemove;

   node_style_set( win.el_content, "border", "0" );
   node_style_set( win.el_content, "display", "none" );
   node_style_set( win.el_win, "background", "white" );
   
   if( widthp && heightp ) {
      mousemove = function( event ) {
	 node_style_set( win.el_win, "width", (w0 + (hop_event_mouse_x( event ) - x0)) + "px" );
	 node_style_set( win.el_win, "height", (h0 + (hop_event_mouse_y( event ) - y0)) + "px" );
      };
      hop_add_event_listener( document, "mousemove", mousemove );
   } else {
      if( widthp ) {
	 mousemove = function( event ) {
	    node_style_set( win.el_win, "width", (w0 + (hop_event_mouse_x( event ) - x0)) + "px" );
	 };
	 hop_add_event_listener( document, "mousemove", mousemove );
      } else {
	 if( heightp ) {
	    mousemove = function( event ) {
	       node_style_set( win.el_win, "height", (h0 + (hop_event_mouse_y( event ) - y0)) + "px" );
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
      node_style_set( win.el_win, "background", "none" );
      node_style_set( win.el_content, "display", "block" );
      hop_remove_event_listener( document, "mousemove", mousemove );
      hop_remove_event_listener( document, "mouseup", mouseup );

      /* user event */
      if( win.onresize ) win.onresize();
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

   if( parent )
      parent.appendChild( win );
   else
      document.body.appendChild( win );

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
   
   return win;
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

   win.el_title.innerHTML = title ? title : id;

   if( isnew ) {
      if( x ) node_style_set( win, "left",
			      ((typeof x)=="number") ? (x + "px") : x );
      if( y ) node_style_set( win, "top",
			      ((typeof y) =="number") ? (y + "px") : y );

      if( width ) {
	 node_style_set( win.el_win, "width",
			 ((typeof width) == "number") ? (width + "px") : width );
      } else {
	 node_style_set( win.el_win, "width", "200px" );
      }
      if( height ) {
	 node_style_set( win.el_win, "height",
			 ((typeof height) == "number") ? (height + "px") : height );
      } else {
	 node_style_set( win.el_win, "height", "200px" );
      }
      
      if( win.onresize ) win.onresize();
   }

   win.resizable = resizable;
   node_style_set( win.el_content, "display", "block" );

   hop_iwindow_raise( win );

   return win;
}

/*---------------------------------------------------------------------*/
/*    pre-allocated keywords                                           */
/*---------------------------------------------------------------------*/
var Ktitle = sc_string2keyword_immutable( "title" );
var Kid = sc_string2keyword_immutable( "id" );
var Kparent = sc_string2keyword_immutable( "parent" );
var Ksrc = sc_string2keyword_immutable( "src" );
var Kclass = sc_string2keyword_immutable( "class" );
var Kwidth = sc_string2keyword_immutable( "width" );
var Kheight = sc_string2keyword_immutable( "height" );
var Kleft = sc_string2keyword_immutable( "left" );
var Ktop = sc_string2keyword_immutable( "top" );
var Kbackground = sc_string2keyword_immutable( "background" );
var Kbg = sc_string2keyword_immutable( "bg" );
var Kresizable = sc_string2keyword_immutable( "resizable" );
var Kprop = sc_string2keyword_immutable( "prop" );

/*---------------------------------------------------------------------*/
/*    hop_window_open ...                                              */
/*---------------------------------------------------------------------*/
function hop_window_open() {
   var title = "Hop", id, parent, src, klass, width, height, left, top,
      background, resizable = true;
   var prop = "";
   var i = 0, l = arguments.length;
   var body = false;

   function prop_to_string( val ) {
      if( (val instanceof Boolean) || (typeof val == "boolean") )
	 return  val ? "yes" : "no";
      return val + "";
   }

   function native_window_open() {
      if( typeof src == "function" ) src = src();

      if( (src instanceof String) || (typeof src == "string" ) ) {
	 if( width ) prop += ",width=" + width;
	 if( height ) prop += ",height=" + height;
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
	 throw new Error( "window-open: no :src specified" );
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
      
      if( sci_isKeyword( k ) ) {
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
						"," + k.toJSString() + "=" + 
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
	       body = dom_create_div( k );
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
function hop_window_iconify( o ) {
   return hop_get_window( o ).iconify();
}

/*---------------------------------------------------------------------*/
/*    hop_window_close ...                                             */
/*---------------------------------------------------------------------*/
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
