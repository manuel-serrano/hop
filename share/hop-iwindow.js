/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-iwindow.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Mar  1 14:09:36 2006                          */
/*    Last change :  Fri Apr 21 15:41:31 2006 (serrano)                */
/*    -------------------------------------------------------------    */
/*    HOP IWINDOW implementation                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_iwindow_close ...                                            */
/*---------------------------------------------------------------------*/
function hop_iwindow_close( id ) {
   var win = (id instanceof HTMLElement) ? id : document.getElementById( id );

   if( win.close ) {
      win.close( win );
   } else {
      win.style.display = "none";
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_maximize ...                                         */
/*---------------------------------------------------------------------*/
function hop_iwindow_maximize( id ) {
   var win = (id instanceof HTMLElement) ? id : document.getElementById( id );

   if( win.maximize ) {
      win.maximize( win );
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

	 win.el_main.style.width = window.innerWidth;
	 win.el_main.style.height = window.innerHeight;

	 win.style.top = 0;
	 win.style.left = 0;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_iconify ...                                          */
/*---------------------------------------------------------------------*/
function hop_iwindow_iconify( id ) {
   var win = (id instanceof HTMLElement) ? id : document.getElementById( id );

   if( win.iconify ) {
      win.iconify( win );
   } else {
      /* nothing yet */
   }
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_drag ...                                             */
/*---------------------------------------------------------------------*/
function hop_iwindow_drag( event, win ) {
   var dx = event.clientX - hop_element_x( win );
   var dy = event.clientY - hop_element_y( win );
   var ocursor = win.el_handle.style.cursor;

   win.el_main.style.height = win.el_content.offsetHeight;
   win.el_main.style.width = win.el_content.offsetWidth;

   win.el_body.style.display = "none";
   win.el_handle.style.cursor = "move";

   document.onmousemove = function( event ) {
      win.style.left = event.clientX - dx;
      win.style.top = event.clientY - dy;
   }

   document.onmouseup = function( event ) {
      document.onmousemove = false;
      win.el_handle.style.cursor = ocursor;
      win.el_body.style.display = "block";
   }
   
   event.preventDefault();
   event.stopPropagation();
}

/*---------------------------------------------------------------------*/
/*    hop_iwindow_resize ...                                           */
/*---------------------------------------------------------------------*/
function hop_iwindow_resize( event, win, widthp, heightp ) {
   var x0 = event.clientX;
   var y0 = event.clientY;
   var w0 = win.el_main.offsetWidth;
   var h0 = win.el_main.offsetHeight;

   if( widthp && heightp ) {
      document.onmousemove = function( event ) {
	 win.el_main.style.width = w0 + (event.clientX - x0);
	 win.el_main.style.height = h0 + (event.clientY - y0);
      }
   } else {
      if( widthp ) {
	 document.onmousemove = function( event ) {
	    win.el_main.style.width = w0 + (event.clientX - x0);
	 }
      } else {
	 if( heightp ) {
	    document.onmousemove = function( event ) {
	       win.el_main.style.height = h0 + (event.clientY - y0);
	    }
	 } else {
	    var l0 = win.offsetLeft;

	    document.onmousemove = function( event ) {
	       var w = w0 + (x0 - event.clientX);
	       win.style.left = (l0 + w0) - w;
	       win.el_main.style.width = w;
	       win.el_main.style.height = h0 + (event.clientY - y0);
	    }
	 }
      }
   }

   document.onmouseup = function( event ) {
      document.onmousemove = false;
   }
   
   event.preventDefault();
   event.stopPropagation();
}

/*---------------------------------------------------------------------*/
/*    make_hop_iwindow ...                                             */
/*---------------------------------------------------------------------*/
function make_hop_iwindow( id, class ) {
   var win = document.createElement( "div" );
   
   win.id = id;
   win.className = class;

   var t = "\
<TABLE id='" + id + "-main' class='hop-iwindow' \
       cellpadding='0' cellspacing='0' border='0'>\
  <TR>\
    <TD class='hop-iwindow-top' valign='top'>\
      <TABLE id='" + id + "-handle' class='hop-iwindow-handle' width='100%'\
             cellpadding='0' cellspacing='0' border='0'>\
        <TR class='hop-iwindow-handle'>\
          <TD class='hop-iwindow-iconify' align='left' onclick='hop_iwindow_iconify( \"" + id + "\" )'>&nbsp;</TD>\
          <TD class='hop-iwindow-maximize' align='left' onclick='hop_iwindow_maximize( \"" + id + "\" )'>&nbsp;</TD>\
          <TD class='hop-iwindow-up-title'>\
            <TABLE class='hop-iwindow-title' width='100%' border='0' cellspacing='0' cellpadding='0'>\
              <TR>\
                <TD class='hop-iwindow-title-left'>&nbsp;</TD>\
                <TD id='" + id + "-title' class='hop-iwindow-title-middle'>title</TD>\
                <TD class='hop-iwindow-title-right'>&nbsp;</TD>\
              </TR>\
            </TABLE>\
          </TD> \
          <TD class='hop-iwindow-close' align='right' onclick='hop_iwindow_close( \"" + id + "\" )'>&nbsp;</TD>\
        </TR>\
      </TABLE>\
    </TD>\
  </TR>\
  <TR class='hop-iwindow-body'>\
    <TD id='" + id + "-content'  class='hop-iwindow-content'>\
      <TABLE class='hop-iwindow-body' width='100%' border='0' cellspacing='0' cellpadding='0'>\
        <TR>\
          <TD id='" + id + "-body' class='hop-iwindow-body'></TD>\
        </TR>\
      </TABLE>\
    </TD>\
  </TR>\
  <TR class='hop-iwindow-bottom'>\
    <TD>\
      <TABLE width='100%' border='0' cellspacing='0' cellpadding='0'>\
        <TR>\
          <TD id='" + id + "-resize-left' class='hop-iwindow-bottom-left'>&nbsp;</TD>\
          <TD id='" + id + "-resize-middle' class='hop-iwindow-bottom-middle'>&nbsp;</TD>\
          <TD id='" + id + "-resize-right' class='hop-iwindow-bottom-right'>&nbsp;</TD>\
        </TR>\
      </TABLE> \
    </TD>\
  </TR>\
</TABLE>";

   win.innerHTML = "\
<TABLE class='hop-iwindow-shadow' cellspacing='0' cellpadding='0' border='0'>\
  <TR>\
    <TD class='hop-iwindow-shadow-nw' rowspan='2' colspan='2'>" + t + "</TD>\
    <TD id='" + id + "-shadow-box' class='hop-iwindow-shadow-ne'>&nbsp;</TD>\
  </TR>\
  <TR>\
    <TD class='hop-iwindow-shadow-e'>&nbsp;</TD>\
  </TR>\
  <TR>\
    <TD class='hop-iwindow-shadow-sw'>&nbsp;</TD>\
    <TD class='hop-iwindow-shadow-s'>&nbsp;</TD>\
    <TD class='hop-iwindow-shadow-se'>&nbsp;</TD>\
  </TR>\
</TABLE>";
  
   document.body.appendChild( win );
   
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

   win.el_handle.onmousedown = function( event ) {
      hop_iwindow_drag( event, win )
   };

   win.el_resize_middle.onmousedown = function( event ) {
      hop_iwindow_resize( event, win, false, true );
   };
   
   win.el_resize_right.onmousedown = function( event ) {
      hop_iwindow_resize( event, win, true, true );
   };
   
   win.el_resize_left.onmousedown = function( event ) {
      hop_iwindow_resize( event, win, false, false );
   };
   
   return win;
}
   
/*---------------------------------------------------------------------*/
/*    hop_iwindow_open ...                                             */
/*---------------------------------------------------------------------*/
function hop_iwindow_open( id, title, class, obj, width, height, x, y ) {
   var win = document.getElementById( id );

   class = class ? class : "hop-iwindow";

   if( !win ) {
      win = make_hop_iwindow( id, class );
   } else {
      win.style.display = "block";
   }

   /* start hidden otherwise we loose the border on drag! */
   win.el_body.style.display = "none";

   hop( obj(),
	function( http ) {
           if( http.responseText != null ) {
	      win.el_body.innerHTML = http.responseText;
	      hop_js_eval( http );
	   }
        } );

   win.el_title.innerHTML = title;

   if( x ) win.style.left = x;
   if( y ) win.style.top = y;

   if( width ) {
      win.style.width = width;
      win.el_main.style.width = "100%";
   }
   if( height ) {
      win.style.height = height;
      win.el_main.style.height = "100%";
   }
   
   win.el_body.style.display = "block";

   return win;
}
