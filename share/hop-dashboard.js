/*=====================================================================*/
/*    serrano/prgm/project/hop/2.5.x/share/hop-dashboard.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Jul  8 17:03:46 2007                          */
/*    Last change :  Mon Jan  6 18:27:13 2014 (serrano)                */
/*    Copyright   :  2007-14 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The Hop dashboard client-side driver.                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    dashboard global variables                                       */
/*---------------------------------------------------------------------*/
var hop_dashboard_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADAAAAAOCAYAAABpcp9aAAAAAXNSR0IArs4c6QAAAAZiS0dEAP8A/wD/oL2nkwAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB9oCCwoMOSyXA1oAAAAdaVRYdENvbW1lbnQAAAAAAENyZWF0ZWQgd2l0aCBHSU1QZC5lBwAAAwpJREFUSMfVlc2PlEUQxn9V1Q0uGXbB3XF2DpOFYLIYOIyGk8ZwUA8bOXGEM0dvnoQjXojh4N8A/geamAA3SUjkIu6BKIqJ4scOkQ0f6+4w3Ry633k/5kWOYiVv5unuqXqe6q7uErJd+Uw7D7f47p/tOJgEHC+hqRK8sblvr6x98HG4ASAAX1/Qj0ab8fN9HWFut0CMeaXAAsQyUnXYWJpaLH+kGmY6mcMHEAViG8eslu2n8OBhZO8euXrik/C+XLuovd/vxz8GXQEFCfDLX5GVnkDMeFnKOFUBBdbMUxFGyJqKBAChmQkQYplMk4MWLT3h2JFFvrp+n/k9nLGT78qthY7sn3sFVOGdt17jyeMnqAlvF1jBFFQFVUFUMKvg/GmeUwW15GMqiFT8AbU0pxoxIfvNcrRqEfj70Q7D1QXWf9pa063tOOjMCc4Eb8K3329w517EGSUWcCY4A2fgXWzF5Sd4Jxw9aNz5LbDLF/4R5yX7gTdJY23naNXihcl4wv75XeyM8W78FDe3uyzl4RtdnG1AA9dNnoPLOAeXhc7aDqfX4ItzypEDQqz8V2qVIq0cz9WST5BUvSljc+AN1n/YYP3ngKthweV1p3mHKthl7HPZDLrCwonxVMqp84H1uwHvitMBM8EMvEux2zj+TUthDkgT+RU4fKiL6QiJkdXXE0Yi5LsXQ76DFVzsohDpLwrzH45n9vPU+cDls8qxw0oMgMR8Wvm22izHaouW4kWbPq1Aqj8X8U748e6Im7cnOF/BuWZNBefSzlWx0+Tfe7VdfGGnPw3cvB3wHpyT6X1wTlo52rSYRZxJ4wQ80zfvwGAJZynrlZVuwpWsE5YaFkn7qAqXz+oLG5Jppea1iCUzHG1ampYSyEEi8Ou9Ed/cmnB8aCV+08rSabW0sPkYjg+t3sykMZTZZleEaHK0ahkmLbUETMoG1O8t8Z6NiAH6/YwBsdycKvVew4U6idPGHZsiY6Vwa8llvwZHf3lxVksErJFAv79U28/l3lIr/i/sRVocwKUv/+T/as8APE1e9+GhqPQAAAAASUVORK5CYII=";
var hop_dashboard_icon_height = "14px";
var hop_dashboard_icon_width = "48px";

var hop_dashboard_anim_speed = 10;
var hop_dashboard_populated = false;
var hop_dashboard_panel = false;
var hop_dashboard_container = false;
var hop_dashboard_icon_size = 32;
var hop_dashboard_interval = false;
var hop_dashboard_opacity = "0.70";
var hop_dashboard_panel_background = "#B7AE8A";
var hop_dashboard_panel_icon_background = "#E9E0BA";
var hop_dashboard_panel_opacity = "0.50";

/*---------------------------------------------------------------------*/
/*    hop_dashboard_start_applet ...                                   */
/*---------------------------------------------------------------------*/
function hop_dashboard_start_applet( name, svc ) {
   function success( obj, xhr ) {
      if( obj === true ) {
	 return true;
      }
      
      if( hop_is_html_element( obj ) ) {
	 document.body.appendChild( obj );
	 return true;
      }

      if( (obj instanceof String) || (typeof obj === "string") ) {
	 return alert( obj );
      }

      // fallback
      window.open( sc_dirname( svc ), name );
   }

   function failure( xhr ) {
      if( xhr.status == 404 ) {
	 window.open( sc_dirname( svc ), name );
      } else {
	 if( xhr.exception ) {
	    hop_callback_handler( xhr.exception, xhr.precontext );
	 } else {
	    sc_error( svc, "Cannot open dashboard", xhr.responseText );
	 }
      }
   }

   if( (svc.indexOf( "http://" ) === 0) || (svc.indexOf( "https://" ) === 0) )
      window.open( svc, name );
   else
      with_hop( svc, success, failure );
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_populate ...                                       */
/*---------------------------------------------------------------------*/
function hop_dashboard_populate( proc ) {
   var populate = function( h ) {
      var width = 0;
      var app_size = hop_dashboard_icon_size + 2;
      var div = hop_dashboard_container;
      var div2 = hop_dashboard_panel;

      div.innerHTML = "";
      div2.innerHTML = "";

      while( h !== null ) {
	 var p = h.__hop_car;
	 var app = document.createElement( "span" );
	 var img = document.createElement( "img" );
		   
	 img.src = p.__hop_cdr.__hop_car
	 img.title = p.__hop_car;
	 app.title = p.__hop_car;

	 node_style_set( app, "width", app_size + "px");
	 node_style_set( app, "height", app_size + "px");
	 node_style_set( app, "padding", "2px");
	 node_style_set( app, "margin", "2px");
	 node_style_set( app, "border", "1px solid transparent" );
	 node_style_set( app, "float", "left");
		   
	 app.onmouseover = function( e ) {
	    node_style_set( this, "background", hop_dashboard_panel_icon_background );
	    node_style_set( this, "-moz-border-radius", "4px" );
	    node_style_set( this, "-webkit-border-radius", "4px" );
	    node_style_set( this, "border", "1px solid #FFD93D" );
	 }
	 app.onmouseout = function( e ) {
	    node_style_set( this, "background", "inherit" );
	    node_style_set( this, "border", "1px solid transparent" );
	    node_style_set( this, "-moz-border-radius", "4px" );
	    node_style_set( this, "-webkit-border-radius", "4px" );
	 }
	 app.name = p.__hop_car
	 app.svc = p.__hop_cdr.__hop_cdr.__hop_car;
	 app.onclick = function( e ) {
	    hop_dashboard_start_applet( this.name, this.svc );
	 }
	 
	 app.appendChild( img );
	 div.appendChild( app );

	 h = h.__hop_cdr;
	 width += (app_size + 10);
      }

      node_style_set( div, "bottom", "-" + app_size + 8 + "px" );
      node_style_set( div, "left", ((hop_current_window_width()-width)/2) + "px" );
      node_style_set( div, "width", width + "px" );
      node_style_set( div2, "bottom", "-" + app_size + 8 + "px" );
      node_style_set( div2, "left", (1+((hop_current_window_width()-width)/2)) + "px" );
      node_style_set( div2, "width", width + "px" );
      node_style_set( div2, "height", app_size + 10 + "px" );

      hop_dashboard_populated = true;

      proc();
   }

   function permission_denied( h ) {
      alert( "Permission denied to run the dashboard on this host!" );
   }
   
   with_hop( "/hop/dashboard/populate", populate, permission_denied );
}   

/*---------------------------------------------------------------------*/
/*    hop_dashboard_key_prev ...                                       */
/*---------------------------------------------------------------------*/
var hop_dashboard_key_prev = 0;

/*---------------------------------------------------------------------*/
/*    hop_dashboard_key_listener ...                                   */
/*---------------------------------------------------------------------*/
function hop_dashboard_key_listener( event ) {
   var key = hop_event_key_code( event );
   if( (key == 72) && (hop_dashboard_key_prev == 18) ) {
      var el = document.getElementById( "hop-dashboard-frame" );
      hop_stop_propagation( event, false );
      hop_dashboard_key_prev = 0;
      
      if( el ) {
	 
	 document.body.removeChild( el );
	 document.body.removeChild( document.getElementById( "hop-dashboard-frame-mask" ) );
      } else {
	 with_hop( "/hop/hop/dashboard", false, false );
      }
   } else {
      hop_dashboard_key_prev = key;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_activate ...                                       */
/*---------------------------------------------------------------------*/
function hop_dashboard_activate() {
   var activate = function() {
      var count = -56;
      var div = hop_dashboard_container;
      var div2 = hop_dashboard_panel;
      
      div.activep = true;
      
      node_style_set( div, "bottom", "-56px" );
      node_style_set( div, "display", "block" );
      node_style_set( div2, "bottom", "-56px" );
      node_style_set( div2, "display", "block" );

      if( hop_dashboard_interval ) {
	 clearInterval( hop_dashboard_interval );
	 hop_dashboard_interval = false;
      }

      hop_dashboard_interval =
      setInterval( function() {
	    if( count < 0 ) {
	       count += 4;
	       node_style_set( div, "bottom", count + "px" );
	       node_style_set( div2, "bottom", count + "px" );
	    } else {
	       clearInterval( hop_dashboard_interval );
	       hop_dashboard_interval = false;
	    }
	 },
	 hop_dashboard_anim_speed );
   }
      
   if( !hop_dashboard_container.activep ) {
      hop_dashboard_populate( activate );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_deactivate ...                                     */
/*---------------------------------------------------------------------*/
function hop_dashboard_deactivate() {
   if( hop_dashboard_container.activep ) {
      var count = 0;
   
      hop_dashboard_container.activep = false;
      
      if( hop_dashboard_interval ) {
	 clearInterval( hop_dashboard_interval );
	 hop_dashboard_interval = false;
      }

      hop_dashboard_interval =
	 setInterval( function() {
	       var div = hop_dashboard_container;
	       var div2 = hop_dashboard_panel;
	       
	       if( count > -56 ) {
		  count -= 4;
		  node_style_set( div, "bottom", count+"px" );
		  node_style_set( div2, "bottom", count+"px" );
	       } else {
		  node_style_set( div, "display", "none" );
		  node_style_set( div2, "display", "none" );
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
function hop_toggle_dashboard() {
   if( !hop_dashboard_populated ) {
      hop_dashboard_populate( hop_toggle_dashboard );
   } else {
      if( hop_dashboard_container.activep ) {
	 hop_dashboard_deactivate();
      } else {
	 hop_dashboard_activate();
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
   node_style_set( but, "bottom", "0" );
   node_style_set( but, "left", "20px" );
   node_style_set( but, "z-index", "10000" );
   node_style_set( but, "background", "none" );
   node_style_set( but, "border-color", "#542d73" );
   node_style_set( but, "border-style", "outset" );
   node_style_set( but, "border-width", "0" );
   node_style_set( but, "padding-top", "0" );
   node_style_set( but, "padding-left", "0" );
   node_style_set( but, "width", hop_dashboard_icon_width );
   node_style_set( but, "height", hop_dashboard_icon_height );
   node_style_set( but, "-moz-opacity", hop_dashboard_opacity );
   node_style_set( but, "-webkit-opacity", hop_dashboard_opacity );
   node_style_set( but, "opacity", hop_dashboard_opacity );
   node_style_set( but, "user-select", "none" );

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
   icon.title = "Toggle Hop Dashboard [alt-h]";

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
   var div2 = document.createElement( "div" );

   hop_dashboard_container = div;
   div.activep = false;
   div.className = "hop-dashboard-control-container";
   node_style_set( div, "position", "fixed" );
   node_style_set( div, "display", "none" );
   node_style_set( div, "z-index", "10001" );
   node_style_set( div, "background", "transparent" );
   node_style_set( div, "border", "0" );
   node_style_set( div, "opacity", hop_dashboard_opacity );
   node_style_set( div, "-moz-opacity", hop_dashboard_opacity );
   node_style_set( div, "-webkit-opacity", hop_dashboard_opacity );
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

   hop_dashboard_panel = div2;
   div2.className = "hop-dashboard-control-panel";
   node_style_set( div2, "position", "fixed" );
   node_style_set( div2, "display", "block" );
   node_style_set( div2, "z-index", "10000" );
   node_style_set( div2, "background", hop_dashboard_panel_background );
   node_style_set( div2, "-moz-opacity", hop_dashboard_panel_opacity );
   node_style_set( div2, "-webkit-opacity", hop_dashboard_panel_opacity );
   node_style_set( div2, "opacity", hop_dashboard_panel_opacity );

   div.onmouseover = function() {
      node_style_set( hop_dashboard_container, "-webkit-opacity", "1" )
      node_style_set( hop_dashboard_container, "-moz-opacity", "1" )
      node_style_set( hop_dashboard_container, "opacity", "1" )
   };
   div.onmouseout = function() {
      node_style_set( hop_dashboard_container, "-moz-opacity", hop_dashboard_opacity );
      node_style_set( hop_dashboard_container, "-webkit-opacity", hop_dashboard_opacity );
      node_style_set( hop_dashboard_container, "opacity", hop_dashboard_opacity );
   };

   document.body.appendChild( div );
   document.body.appendChild( div2 );
}

/*---------------------------------------------------------------------*/
/*    hop_dashboard_init ...                                           */
/*---------------------------------------------------------------------*/
function hop_dashboard_init() {
   hop_dashboard_control_panel_init();
   hop_dashboard_button_init();
/*    hop_load( hop_share_directory() + "/hop-window.js" );            */
   hop_add_native_event_listener( document, "keydown",
				  hop_dashboard_key_listener, true );
}

/*---------------------------------------------------------------------*/
/*    runtime initialization                                           */
/*---------------------------------------------------------------------*/
hop_add_event_listener( window, "ready", hop_dashboard_init, true );
