/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-iwindow.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Mar  1 14:09:36 2006                          */
/*    Last change :  Wed Aug 30 19:05:24 2006 (serrano)                */
/*    -------------------------------------------------------------    */
/*    HOP IWINDOW implementation                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_iwindow_zindex ...                                           */
/*---------------------------------------------------------------------*/
var hop_iwindow_zindex = 0;

/*---------------------------------------------------------------------*/
/*    hop_iwindow_close ...                                            */
/*---------------------------------------------------------------------*/
function hop_iwindow_close( id ) {
   var win = hop_is_html_element( id ) ? id : document.getElementById( id );

   win.style.display = "none";

   /* user event */
   if( win.onclose ) win.onclose();
}

/*---------------------------------------------------------------------*/
/*    function                                                         */
/*    hop_window_close ...                                             */
/*---------------------------------------------------------------------*/
function hop_window_close( win ) {
   return win.close( win );
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_maximize ...                                         */
/*---------------------------------------------------------------------*/
function hop_iwindow_maximize( id ) {
   var win = hop_is_html_element( id ) ? id : document.getElementById( id );

   if( win.maximize ) {
      win.maximize();
   } else {
      if( win.maximized ) {
	 win.maximized = false;
      
	 win.el_main.style.width = win.oldwidth;
	 win.el_main.style.height = win.oldheight;
	 win.style.top = win.oldtop;
	 win.style.left = win.oldleft;
      } else {
	 win.maximized = true;
      
	 win.oldwidth = win.el_main.style.width;
	 win.oldheight = win.el_main.style.height;
	 win.oldtop = win.style.top;
	 win.oldleft = win.style.left;

	 if( win.user_parent ) {
	    var p = win.parentNode;
	    
	    win.el_main.style.width = p.offsetWidth -
	       win.el_shadow_box.offsetWidth;
	    win.el_main.style.height = p.offsetHeight -
	       win.el_shadow_box.offsetHeight;

	    win.style.top = hop_element_y( p ) + 1;
	    win.style.left = hop_element_x( p ) + 1;
	 } else {
	    win.el_main.style.width = window.innerWidth -
	       win.el_shadow_box.offsetWidth - 2;
	    win.el_main.style.height = window.innerHeight -
	       win.el_shadow_box.offsetHeight - 2;

	    win.style.top = 0;
	    win.style.left = 0;
	 }
      }
   }

   /* user event */
   if( win.onresize ) win.onresize();
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_iconify ...                                          */
/*---------------------------------------------------------------------*/
function hop_iwindow_iconify( id ) {
   var win = hop_is_html_element( id ) ? id : document.getElementById( id );

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
	 win.style.top = old + window.pageYOffset;
	 hop_style_set( win, "position", "absolute" );
      } else {
	 var old = win.offsetTop;
	 win.style.top = old - window.pageYOffset;
	 hop_style_set( win, "position", "fixed" );
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
      hop_style_set( win, "z-index", ++hop_iwindow_zindex );
   } else {
      var w = document.getElementsByName( "hop-iwindow" );
      var i;

      for( i = 0; i < w.length; i++ ) {
	 if( w[ i ].style[ "z-index" ] > 0 ) w[ i ].style[ "z-index" ]--;
      }
      hop_style_set( win, "z-index", 999 );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_drag ...                                             */
/*---------------------------------------------------------------------*/
function hop_iwindow_drag( event, win ) {
   var dx = event.clientX - win.offsetLeft;
   var dy = event.clientY - win.offsetTop;
   var ocursor = win.el_handle.style.cursor;

   hop_iwindow_raise( win );
   
   win.el_main.style.height = win.el_main.offsetHeight;
   win.el_main.style.width = win.el_main.offsetWidth;

   win.el_body.style.visibility = "hidden";
   win.el_handle.style.cursor = "move";

   var mousemove = function( event ) {
      var nx = (event.clientX - dx);
      var ny = (event.clientY - dy);

      if( win.user_parent ) {
	 var p = win.parentNode;
	 var px = hop_element_x( p );
	 var py = hop_element_y( p );
      
	 if( (nx > px) && ((nx + win.offsetWidth) < (px + p.offsetWidth)) ) {
	    win.style.left = nx;
	 }
	 if( (ny > py) && ((ny + win.offsetHeight) < (py + p.offsetHeight)) ) {
	    win.style.top = ny;
	 }
      } else {
	 if( nx > 0 ) win.style.left = nx;
	 if( ny > 0 ) win.style.top = ny;
      }
   }
   hop_add_event_listener( document, "mousemove", mousemove );

   document.onmouseup = function( event ) {
      hop_remove_event_listener( document, "mousemove", mousemove );
      win.el_handle.style.cursor = ocursor;
      win.el_body.style.visibility = "visible";

      /* user event */
      if( win.ondrag ) win.ondrag();
   }

   hop_stop_propagation( event );
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_resize ...                                           */
/*---------------------------------------------------------------------*/
function hop_iwindow_resize( event, win, widthp, heightp ) {
   var x0 = event.clientX;
   var y0 = event.clientY;
   var w0 = win.el_main.offsetWidth;
   var h0 = win.el_main.offsetHeight;
   var mousemove;

   win.el_body.style.display = "none";
   
   if( widthp && heightp ) {
      mousemove = function( event ) {
	 win.el_main.style.width = w0 + (event.clientX - x0);
	 win.el_main.style.height = h0 + (event.clientY - y0);
      };
      hop_add_event_listener( document, "mousemove", mousemove );
   } else {
      if( widthp ) {
	 mousemove = function( event ) {
	    win.el_main.style.width = w0 + (event.clientX - x0);
	 };
	 hop_add_event_listener( document, "mousemove", mousemove );
      } else {
	 if( heightp ) {
	    mousemove = function( event ) {
	       win.el_main.style.height = h0 + (event.clientY - y0);
	    };
	    hop_add_event_listener( document, "mousemove", mousemove );
	 } else {
	    var l0 = win.offsetLeft;
	    mousemove = function( event ) {
	       var w = w0 + (x0 - event.clientX);
	       win.style.left = (l0 + w0) - w;
	       win.el_main.style.width = w;
	       win.el_main.style.height = h0 + (event.clientY - y0);
	    }
	    hop_add_event_listener( document, "mousemove", mousemove );
	 }
      }
   }

   document.onmouseup = function( event ) {
      win.el_body.style.display = "block";
      hop_remove_event_listener( document, "mousemove", mousemove );

      /* user event */
      if( win.onresize ) win.onresize();
   }
   
   hop_stop_propagation( event );
}

/*---------------------------------------------------------------------*/
/*    make_hop_iwindow ...                                             */
/*---------------------------------------------------------------------*/
function make_hop_iwindow( id, klass, parent ) {
   var win = document.createElement( "div" );

   win.id = id;
   win.className = klass;
   win.name = "hop-iwindow";

   var t = "\n\
<TABLE id='" + id + "-main' class='hop-iwindow' \n\
       cellpadding='0' cellspacing='0' border='0'>\n\
  <TR>\n\
    <TD class='hop-iwindow-top' valign='top'>\n\
      <TABLE id='" + id + "-handle' class='hop-iwindow-handle' width='100%'\n\
             cellpadding='0' cellspacing='0' border='0'>\n\
        <TR class='hop-iwindow-handle'>\n\
          <TD class='hop-iwindow-iconify' align='left' onclick='hop_iwindow_iconify( \"" + id + "\" )'>&nbsp;</TD>\n\
          <TD class='hop-iwindow-maximize' align='left' onclick='hop_iwindow_maximize( \"" + id + "\" )'>&nbsp;</TD>\n\
          <TD class='hop-iwindow-up-title'>\n\
            <TABLE class='hop-iwindow-title' width='100%' border='0' cellspacing='0' cellpadding='0'>\n\
              <TR>\n\
                <TD class='hop-iwindow-title-left'>&nbsp;</TD>\n\
                <TD id='" + id + "-title' class='hop-iwindow-title-middle'>title</TD>\n\
                <TD class='hop-iwindow-title-right'>&nbsp;</TD>\n\
              </TR>\n\
            </TABLE>\n\
          </TD> \n\
          <TD class='hop-iwindow-close' align='right' onclick='hop_iwindow_close( \"" + id + "\" )'>&nbsp;</TD>\n\
        </TR>\n\
      </TABLE>\n\
    </TD>\n\
  </TR>\n\
  <TR class='hop-iwindow-body'>\n\
    <TD id='" + id + "-content'  class='hop-iwindow-content' valign='top'>\n\
      <TABLE class='hop-iwindow-body' width='100%' height='100%' border='0' cellspacing='0' cellpadding='0'>\n\
        <TR>\n\
          <TD id='" + id + "-body' class='hop-iwindow-body' height='100%'></TD>\n\
        </TR>\n\
      </TABLE>\n\
    </TD>\n\
  </TR>\n\
  <TR class='hop-iwindow-bottom'>\n\
    <TD>\n\
      <TABLE width='100%' border='0' cellspacing='0' cellpadding='0'>\n\
        <TR>\n\
          <TD id='" + id + "-resize-left' class='hop-iwindow-bottom-left'>&nbsp;</TD>\n\
          <TD id='" + id + "-resize-middle' class='hop-iwindow-bottom-middle'>&nbsp;</TD>\n\
          <TD id='" + id + "-resize-right' class='hop-iwindow-bottom-right'>&nbsp;</TD>\n\
        </TR>\n\
      </TABLE> \n\
    </TD>\n\
  </TR>\n\
</TABLE>";

   win.innerHTML = "\n\
<TABLE class='hop-iwindow-shadow' cellspacing='0' cellpadding='0' border='0'>\n\
  <TR>\n\
    <TD class='hop-iwindow-shadow-nw' rowspan='2' colspan='2'>\n" + t + "</TD>\n\
    <TD id='" + id + "-shadow-box' class='hop-iwindow-shadow-ne'>&nbsp;</TD>\n\
  </TR>\n\
  <TR>\n\
    <TD class='hop-iwindow-shadow-e'>&nbsp;</TD>\n\
  </TR>\n\
  <TR>\n\
    <TD class='hop-iwindow-shadow-sw'>&nbsp;</TD>\n\
    <TD class='hop-iwindow-shadow-s'>&nbsp;</TD>\n\
    <TD class='hop-iwindow-shadow-se'>&nbsp;</TD>\n\
  </TR>\n\
</TABLE>";

   if( parent )
      parent.appendChild( win );
   else
      document.body.appendChild( win );

   win.user_parent = (parent && parent != undefined) ? parent : false;
   
   win.el_title = document.getElementById( id + "-title" );
   win.el_handle = document.getElementById( id + "-title" );
   win.el_resize_left = document.getElementById( id + "-resize-left" );
   win.el_resize_middle = document.getElementById( id + "-resize-middle" );
   win.el_resize_right = document.getElementById( id + "-resize-right" );
   win.el_content = document.getElementById( id + "-content" );
   win.el_body = document.getElementById( id + "-body" );
   win.el_main = document.getElementById( id + "-main" );
   win.el_shadow = win.childNodes[ 0 ];
   win.el_shadow_box = document.getElementById( id + "-shadow-box" );

   hop_add_event_listener(
      win.el_handle,
      "mousedown",
      function( event ) { hop_iwindow_drag( event, win ) } );

   hop_add_event_listener(
      win.el_resize_middle,
      "mousedown",
      function( event ) { hop_iwindow_resize( event, win, false, true ) } );

   hop_add_event_listener(
      win.el_resize_right,
      "mousedown",
      function( event ) { hop_iwindow_resize( event, win, true, true ) } );
   
   hop_add_event_listener(
      win.el_resize_left,
      "mousedown",
      function( event ) { hop_iwindow_resize( event, win, false, false ) } );
   
   return win;
}
   
/*---------------------------------------------------------------------*/
/*    hop_iwindow_open ...                                             */
/*---------------------------------------------------------------------*/
function hop_iwindow_open( id, obj, title, klass, width, height, x, y, parent ) {
   var win = document.getElementById( id );
   var isnew = false;

   klass = klass ? ("hop-iwindow " + klass) : "hop-iwindow";
   if( win == null ) {
      win = make_hop_iwindow( id, klass, parent );
      isnew = true;
   } else {
      win.style.display = "block";
   }

   /* start hidden otherwise we loose the border on drag! */
   win.el_body.style.display = "none";

   if( hop_is_html_element( obj ) ) {
      var c = win.el_body.childNodes;
      var i = c.length;

      while( i > 0 ) {
	 i--;
	 win.el_body.removeChild( c[ i ] );
      }
      win.el_body.appendChild( obj );
   } else {
      var cb = function( http ) {
     	         if( http.responseText != null ) {
	           win.el_body.innerHTML = http.responseText;
	           hop_js_eval( http );
	         }
               };

      if( typeof obj == "function" ) {
	 hop( obj(), cb );
      } else {
	 if( (obj instanceof String) || (typeof obj == "string") ) {
	    hop( obj, cb );
	 } else {
	    alert( "*** Hop Error, Illegal `iwindow' content -- " + obj
	       + " (" + typeof obj + ")" );
	 }
      }
   }

   win.el_title.innerHTML = title ? title : id;

   if( isnew ) {
      if( x ) win.style.left = x;
      if( y ) win.style.top = y;

      if( width ) {
	 win.el_main.style.width = width;
	 win.el_content.style.width = "100%";
      }
      if( height ) {
	 win.el_main.style.height = height;
/* 	 win.el_content.style.height = "100%";                         */
      }
      
      if( win.onresize ) win.onresize();
   } else {
      win.el_body.style.top = 0;
   }
   
   win.el_body.style.display = "block";
   hop_iwindow_raise( win );

   return win;
}

/*---------------------------------------------------------------------*/
/*    hop_window_open ...                                              */
/*---------------------------------------------------------------------*/
function hop_window_open( url, title, klass, width, height, x, y ) {
   var p = klass ? klass : "toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=yes, resizable=yes, copyhistory=no, titlebar=no";
   if( width ) p += ",width=" + width; else p += ",width=640";
   if( height ) p += ",height=" + height; else p+= ",height=480";
   if( x ) p += ",screenX=" + x + ",left=" + x;
   if( y ) p += ",screenY=" + y + ",top=" + y;

   return window.open( url, title, p );
}
   

