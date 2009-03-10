/*=====================================================================*/
/*    serrano/prgm/project/hop/1.11.x/share/hop-dashboard.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul  8 17:03:46 2007                          */
/*    Last change :  Tue Mar 10 08:46:49 2009 (serrano)                */
/*    Copyright   :  2007-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Hop dashboard client-side driver.                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    dashboard global variables                                       */
/*---------------------------------------------------------------------*/
var hop_dashboard_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH1wcIDyYL6ytvWQAAAvBJREFUOMulkl9sU2UYh5/v9GvP0j9Lu27UOseWRR1ygVEGkTkkRo3BC0XvjOGCGEJijHoh/okXXpgMdMn0wgv1zkQxGokT5gg3Ms1EXFLYBm5MJszRdt1a2wI9pz075zufF2Llnid5b5/83vf9Ca01t4MEaBdfAxpQaLxNAUO+urE79kxXd7Td83yu/lUu51cK4+ue9SGo85oGoLimD/0r+A+NOtC/9a6R/a/0hR957A4KhRKlko/hdaQmT+X2HRud2rvw59w7wAjgNROARuMd2LNn0yfDnz5I2wZJdvk6sxeu4K0rWoKS/oEe4ok75dFvYu+fnf0Z4AMAobUmKb7Y3L8tnTlyfFdLMiW5MFPm6lKdtg4NaE58ZyN0BN+PYVXC/HDiS5Vbnd1+XY+cNQCEEC+/9Np9LcmU5NjRRc5llshM5VCewF03yOeK9N1f4srlRdaKio2dA4GAId8GMAA6U7FndTHFLycrCMPn0cfvppAFzwVfCVaymh2DPbywP8H8+cvohiQajT/RFKTT4eTEtz6H3iwwOJDg5PESyfZOzBZJOBKgXkvz8eFVduxMEU7kyWYLhGQs2jyir31qdQczJKi5AWbOKIRvsDDvIADnhiZzpkHDVnT1hFiacxBh9X8PytVq5Z4uZ0OtGKA1blIue0yMLzPxvcLF42/LYttgG9GYpLiqEYbGcat2c4VcoTjuR0tU1yRTkzZvDKVo7wyyYjUoWTbxZIR3h3u5NO8wf87GNG1sJ/9T840JMbxlc19Ppq/jYbmYr/HZWJpYVDE2aqGUZvfTrYRCQZ5/aprcooNj/OZXrJmdlj5y2rjZ6NnfFy6+tVzLkIwEeXF3gdGv1nlga4TtD8X4cazOc7vmWL7kouQfVKzpIYH89ZYEH6GxpMZ+vbe7dygeuleUS60oX+IpH6t+AyNYxDMuUrWnD4N+T2DaNf35rQL75tT7Q2bgYNhse1L4ZlQphaurtuOunXLVtWEwJkEgCNIU3A7/AOe0ahFx9Ik0AAAAAElFTkSuQmCC";

var hop_dashboard_anim_speed = 10;
var hop_dashboard = false;
var hop_dashboard_unpopulated = false;
var hop_dashboard_icon_size = 32;
var hop_dashboard_interval = false;
var hop_dashboard_opacity = "0.75";

/*---------------------------------------------------------------------*/
/*    hop_dashboard_start_applet ...                                   */
/*---------------------------------------------------------------------*/
function hop_dashboard_start_applet( name, svc ) {
   function success( obj, xhr ) {
      if( hop_is_html_element( obj ) ) {
	 document.body.appendChild( obj );
	 return true;
      }
      
      if( (obj instanceof String) || (typeof obj === "string") ) {
	 return alert( obj );
      }
   }

   function failure( obj, xhr ) {
      window.open( sc_dirname( svc ), name );
   }

   if( (svc.indexOf( "http://" ) === 0) || (svc.indexOf( "https://" ) === 0) )
      window.open( svc, name );
   else
      with_hop( svc, success, failure );
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_populate ...                                       */
/*---------------------------------------------------------------------*/
function hop_dashboard_populate( div, proc ) {
   var populate = function( h ) {
      var width = 0;
      var app_size = hop_dashboard_icon_size + 2;
      div.innerHTML = "";

      while( h !== null ) {
	 var p = h.car;
	 var app = document.createElement( "span" );
	 var img = document.createElement( "img" );
		   
	 img.src = p.cdr.car
	 img.title = p.car;
	 app.title = p.car;

	 node_style_set( app, "width", app_size + "px");
	 node_style_set( app, "height", app_size + "px");
	 node_style_set( app, "padding", "2px");
	 node_style_set( app, "margin", "2px");
	 node_style_set( app, "border", "1px solid transparent" );
	 node_style_set( app, "float", "left");
		   
	 app.onmouseover = function( e ) {
	    node_style_set( this, "background", "#fff" );
	    node_style_set( this, "border", "1px outset #542d73" );
	 }
	 app.onmouseout = function( e ) {
	    node_style_set( this, "background", "#eee" );
	    node_style_set( this, "border", "1px solid transparent" );
	 }
	 app.name = p.car
	 app.svc = p.cdr.cdr.car;
	 app.onclick = function( e ) {
	    hop_dashboard_start_applet( this.name, this.svc );
	 }
	 
	 app.appendChild( img );
	 div.appendChild( app );

	 h = h.cdr;
	 width += (app_size + 10);
      }

      node_style_set( div, "bottom", "-" + app_size + 8 + "px" );
      node_style_set( div, "left", ((hop_current_window_width()-width)/2) + "px" );
      node_style_set( div, "width", width + "px" );

      hop_dashboard = div;
      
      proc( div );
   }

   function permission_denied( h ) {
      alert( "Permission denied to run the dashboard on this host!" );
   }
   
   with_hop( "/hop/dashboard/populate", populate, permission_denied );
}   
   
/*---------------------------------------------------------------------*/
/*    hop_dashboard_activate ...                                       */
/*---------------------------------------------------------------------*/
function hop_dashboard_activate() {
   var activate = function( div ) {
      var count = -56;
      
      div.activep = true;
      
      node_style_set( div, "bottom", "-56px" );
      node_style_set( div, "display", "block" );

      if( hop_dashboard_interval ) {
	 clearInterval( hop_dashboard_interval );
	 hop_dashboard_interval = false;
      }

      hop_dashboard_interval =
      setInterval( function() {
	    if( count < 0 ) {
	       count += 4;
	       node_style_set( div, "bottom", count + "px" );
	    } else {
	       clearInterval( hop_dashboard_interval );
	       hop_dashboard_interval = false;
	    }
	 },
	 hop_dashboard_anim_speed );
   }
      
   if( !hop_dashboard.activep ) {
      hop_dashboard_populate( hop_dashboard, activate );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_deactivate ...                                     */
/*---------------------------------------------------------------------*/
function hop_dashboard_deactivate() {
   if( hop_dashboard.activep ) {
      var count = 0;
   
      hop_dashboard.activep = false;
      
      if( hop_dashboard_interval ) {
	 clearInterval( hop_dashboard_interval );
	 hop_dashboard_interval = false;
      }

      hop_dashboard_interval =
	 setInterval( function() {
	       if( count > -56 ) {
		  count -= 4;
		  node_style_set( hop_dashboard, "bottom", count+"px" );
	       } else {
		  node_style_set( hop_dashboard, "display", "none" );
		  clearInterval( hop_dashboard_interval );
		  hop_dashboard_interval = false;
	       }
	    },
	    hop_dashboard_anim_speed );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_toggle_dashboard ...                                         */
/*---------------------------------------------------------------------*/
function hop_toggle_dashboard( div ) {
   if( !hop_dashboard ) {
      hop_dashboard_populate( hop_dashboard_unpopulated, hop_toggle_dashboard );
   } else {
      if( hop_dashboard ) {
	 if( hop_dashboard.activep ) {
	    hop_dashboard_deactivate();
	 } else {
	    hop_dashboard_activate();
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_button_init ...                                    */
/*---------------------------------------------------------------------*/
function hop_dashboard_button_init() {
   var but = document.createElement( "div" );
   
   but.className = "hop-dashboard-button";
   node_style_set( but, "position", "fixed" );
   node_style_set( but, "bottom", "1px" );
   node_style_set( but, "left", "1px" );
   node_style_set( but, "z-index", "10000" );
   node_style_set( but, "background", "#eeeeee" );
   node_style_set( but, "border-color", "#542d73" );
   node_style_set( but, "border-style", "outset" );
   node_style_set( but, "border-width", "1px" );
   node_style_set( but, "padding-top", "1px" );
   node_style_set( but, "padding-left", "1px" );
   node_style_set( but, "width", "18px" );
   node_style_set( but, "height", "18px" );
   node_style_set( but, "-moz-border-radius", "2px" );
   node_style_set( but, "-moz-opacity", hop_dashboard_opacity );
   node_style_set( but, "-webkit-border-radius", "2px" );
   node_style_set( but, "-webkit-opacity", hop_dashboard_opacity );
   node_style_set( but, "opacity", hop_dashboard_opacity );
   node_style_set( but, "border-radius", "2px" );

   var icon = document.createElement( "img" );
   
   but.onmouseover = function() {
      node_style_set( but, "-moz-opacity", "1" )
      node_style_set( but, "opacity", "1" )
   };
   but.onmouseout = function() {
      node_style_set( but, "-moz-opacity", hop_dashboard_opacity )
      node_style_set( but, "opacity", hop_dashboard_opacity )
   };
   but.onclick = hop_toggle_dashboard;
   icon.title = "Toggle Hop Dashboard";

   if( hop_config.inline_image ) {
      icon.src = hop_dashboard_icon;
   } else {
      icon.src = hop_share_directory() + "/icons/dashboard.png";
   }
   
   but.appendChild( icon );
   
   document.body.appendChild( but );
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_control_panel_init ...                             */
/*---------------------------------------------------------------------*/
function hop_dashboard_control_panel_init() {
   var div = document.createElement( "div" );
   div.className = "hop-dashboard-control-panel";
   div.activep = false;
   
   node_style_set( div, "position", "fixed" );
   node_style_set( div, "display", "none" );
   node_style_set( div, "z-index", "10000" );
   node_style_set( div, "background", "#eeeeee" );
   node_style_set( div, "border-left-width", "1px" );
   node_style_set( div, "border-left-style", "solid" );
   node_style_set( div, "border-left-color", "#ccc" );
   node_style_set( div, "border-top-width", "1px" );
   node_style_set( div, "border-top-style", "solid" );
   node_style_set( div, "border-top-color", "#ccc" );
   node_style_set( div, "border-right-width", "1px" );
   node_style_set( div, "border-right-style", "solid" );
   node_style_set( div, "border-right-color", "#333" );
   node_style_set( div, "border-bottom-width", "0" );
   node_style_set( div, "-moz-opacity", hop_dashboard_opacity );
   node_style_set( div, "opacity", hop_dashboard_opacity );

   div.onmouseover = function() {
      node_style_set( div, "-moz-opacity", "1" )
      node_style_set( div, "opacity", "1" )
   };
   div.onmouseout = function() {
      node_style_set( div, "-moz-opacity", hop_dashboard_opacity )
      node_style_set( div, "opacity", hop_dashboard_opacity )
   };

   hop_dashboard_unpopulated = div;
   document.body.appendChild( div );
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_init ...                                           */
/*---------------------------------------------------------------------*/
function hop_dashboard_init() {
   hop_dashboard_control_panel_init();
   hop_dashboard_button_init();
   hop_load( "hop_window.js" );
}

/*---------------------------------------------------------------------*/
/*    runtime initialization                                           */
/*---------------------------------------------------------------------*/
hop_window_onload_add( hop_dashboard_init );
