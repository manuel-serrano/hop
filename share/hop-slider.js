/*=====================================================================*/
/*    serrano/prgm/project/hop/2.4.x/share/hop-slider.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 10 11:01:53 2005                          */
/*    Last change :  Mon May 13 15:44:17 2013 (serrano)                */
/*    Copyright   :  2005-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP slider implementation                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    slidertouch ...                                                  */
/*---------------------------------------------------------------------*/
var slidertouch = false;

/*---------------------------------------------------------------------*/
/*    hop_slider_value_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export slider-value-set!) (arity #t)) */
function hop_slider_value_set( slider, value ) {
   if( (slider instanceof String) || (typeof slider === "string") ) {
      slider = document.getElementById( slider );
   }

   value = Math.round( value / slider.step ) * slider.step;

   if( slider.value != value ) {
      if( value < slider.min ) {
	 value = slider.min;
      } else {
	 if( value > slider.max ) {
	    value = slider.max;
	 }
      }

      var v = (value - slider.min) / (slider.max - slider.min);
      var w = slider.clientWidth - slider.cursor.clientWidth;

      slider.value = value;

      if( slider.clientWidth > 0 ) {
	 node_style_set( slider.line1, "width", Math.round(v * w) + "px" );
	 node_style_set( slider.line2, "width", Math.round((1-v) * w) + "px");
      } else {
	 // a rough approximation when the actual size is not known
	 slider.line1.width = Math.round(v * 98) + "%";
	 slider.line2.width = Math.round((100-v) * 98) + "%";
      }

      if( slider.cap ) {
	 slider.cap.innerHTML = value;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_slider_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_slider_mousemove( e, slider ) {
   var val = ((hop_event_mouse_x( e ) - hop_element_x( slider ))
	      / slider.offsetWidth) * (slider.max - slider.min);
   hop_slider_value_set( slider, val + slider.min );
	 
   if( slider.onchange != undefined ) {
      slider.onchange( { value: slider.value, target: slider } );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_slider_value_get ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export slider-value)
           (arity #t))
*/
function hop_slider_value_get( slider ) {
   if( (slider instanceof String) || (typeof slider === "string") ) {
      slider = document.getElementById( slider );
   }
   return slider.value;
}

/*---------------------------------------------------------------------*/
/*    hop_make_slider ...                                              */
/*---------------------------------------------------------------------*/
function hop_make_slider( parent, klass, id, min, max, step, value, cap ) {
   var slider, tbody, tr, tr2 = false;
   var line1, line2, cursor, cursorbg, cursorimg;
   var td1, td3;
   var div;
   var caption;

   if( !parent ) { sc_error( '<SLIDER>', "Illegal parent node", parent ); }
   
   var parent = parent.parentNode;

   // the slider
   slider = document.createElement( "table" );
   slider.className = klass;
   slider.id = id;
   slider.setAttribute( "data-hss-tag", "hop-slider" );
   
   slider.onchange = undefined;
   
   slider.parent = parent;
   slider.rules = "none";
   slider.cellpadding = 0;
   slider.cellspacing = 0;
   
   parent.appendChild( slider );

   tbody = document.createElement( "tbody" );
   slider.appendChild( tbody );
   
   tr = document.createElement( "tr" );

   if( cap ) {
      if( cap == "top" ) {
	 tr2 = document.createElement( "tr" );
	 tbody.appendChild( tr2 );
	 tbody.appendChild( tr );
      } else {
	 if( cap == "bottom" ) {
	    tr2 = document.createElement( "tr" );
	    tbody.appendChild( tr );
	    tbody.appendChild( tr2 );
	 } else {
	    tbody.appendChild( tr );
	 }
      }
   } else {
      tbody.appendChild( tr );
   }

   // the two lines and the cursor
   line1 = document.createElement( "td" );
   line1.className = "line lineleft";
   line2 = document.createElement( "td" );
   line2.className = "line lineright";
   cursor = document.createElement( "td" );
   cursor.setAttribute( "data-hss-tag", "hop-slider-cursor" );
   cursorbg = document.createElement( "div" );
   cursorbg.setAttribute( "data-hss-tag", "hop-slider-cursor-background" );
   cursor.appendChild( cursorbg );
   cursorimg = document.createElement( "div" );
   cursorimg.setAttribute( "data-hss-tag", "hop-slider-cursor-image" );
   cursorbg.appendChild( cursorimg );

   slider.line1 = line1;
   slider.line2 = line2;
   slider.cursor = cursor;

   tr.appendChild( line1 );
   tr.appendChild( cursor );
   tr.appendChild( line2 );

   if( cap ) {
      td1 = document.createElement( "td" );
      caption = document.createElement( "td" );
      caption.setAttribute( "data-hss-tag", "hop-slider-caption" );
      td3 = document.createElement( "td" );

      caption.innerHTML = "";
      slider.cap = caption;

      if( tr2 ) {
	 tr2.appendChild( td1 );
	 tr2.appendChild( caption );
	 tr2.appendChild( td3 );
      } 
   } else {
      slider.cap = false;
   }

   div = document.createElement( "div" );
   div.className = "line lineleft";
   div.id = id + "-lineleft";
   line1.appendChild( div );

   div = document.createElement( "div" );
   div.className = "line lineright";
   div.id = id + "-lineright";
   line2.appendChild( div );

   slider.min = min;
   slider.max = max;
   slider.step = step;

   var mousemoved = false;
   
   // cursor event handling
   var mousemove = function( e ) {
      hop_slider_mousemove( e, slider );
      mousemoved = true;
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( document, "mousemove", mousemove, true );
   };
   
   var onmousedown = function( e ) {
      mousemoved = false;
      hop_add_event_listener( document, "mousemove", mousemove, true );
      hop_add_event_listener( document, "mouseup", delmousemove, true );
      hop_add_event_listener( document, "onblur", delmousemove, true );
   }

   hop_add_event_listener( cursor, "mousedown", onmousedown );

   // cursor click
   var oncursorclick = function( e ) {
      if( !mousemoved ) {
	 var mx = hop_event_mouse_x( e );
	 var bbox = hop_bounding_box( this );
	 var val = step * (( mx > (bbox.left + bbox.width/2) ) ? 4 : -4);

	 hop_slider_value_set( slider, slider.value + val );
      }
   }
      
   hop_add_event_listener( cursor, "click", oncursorclick );

   // line event handling
   var onlineclick = function( e ) {
      hop_slider_mousemove( e, slider );
   }

   hop_add_event_listener( line1, "click", onlineclick );
   hop_add_event_listener( line2, "click", onlineclick );

   slider.value = min - 1;

   hop_add_event_listener( window, "ready",
			   function( e ) {
			      // force a refresh
			      slider.value = min - 1;
			      if( value != undefined ) {
				 if( value < min ) value = min;
				 if( value > max ) value = max;
				 after( 10, function() { hop_slider_value_set( slider, value ); });
			      } else {
				 after( 10, function() { hop_slider_value_set( slider, min ); });
			      }
			   });

   if( slider.onchange != undefined )
      slider.onchange( {value : value, target: slider } );

   // touchmove of mobile platforms
   if( !slidertouch ) {
      slidertouch = document.createElement( "div" );
      slidertouch.setAttribute( "data-hss-tag", "hop-slider-touch" );
      document.body.appendChild( slidertouch );
   }
   
   slider.addEventListener(
      "touchstart", 
      function( evt ) {
	 evt.preventDefault();
	 var touches = evt.changedTouches;
	 var t = touches[ touches.length - 1];
	 var bbox = hop_bounding_box( slider, 0 );

	 if( bbox ) {
	    var w = parseInt( node_computed_style_get( slidertouch, "width" ) );

	    if( w == 0 ) w = 96;
	    
	    node_style_set( slidertouch, "left", (bbox.left + (bbox.width/2) - w/2) + "px" );
	    if( bbox.top > 48 ) {
	       node_style_set( slidertouch, "top", (bbox.top - 48) + "px" );
	    } else {
	       node_style_set( slidertouch, "top", (bbox.bottom + 16) + "px" );
	    }
	 } else {
	    node_style_set( slidertouch, "left", (hop_event_mouse_x( t ) - 32) + "px" );
	    node_style_set( slidertouch, "top", (hop_event_mouse_y( t ) - 48) + "px" );
	 }

	 node_style_set( slidertouch, "display", "block" );
	 slidertouch.innerHTML = slider.value + "";
      },
      false );
   
   slider.addEventListener(
      "touchend", 
      function( evt ) {
	 evt.preventDefault();
	 node_style_set( slidertouch, "display", "none" );
      },
      false );
   
   slider.addEventListener(
      "touchmove",
      function( evt ) {
	 evt.preventDefault();
	 var touches = evt.changedTouches;
	 hop_slider_mousemove( touches[ touches.length - 1], slider );
	 slidertouch.innerHTML = slider.value + "";
      },
      false );

   return slider;
}

/*---------------------------------------------------------------------*/
/*    hop_slider_onchange_get ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export slider-onchange)
           (arity #t)
           (peephole: (hole 1 "(" slider ").onchange")))
*/
function hop_slider_onchange_get( slider ) {
   return slider.onchange;
}

/*---------------------------------------------------------------------*/
/*    hop_slider_onchange_set ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export slider-onchange-set!)
           (arity #t)
           (peephole (hole 2 "(" slider ").onchange = " onchange)))
*/
function hop_slider_onchange_set( slider, onchange ) {
   slider.onchange = onchange;
}
