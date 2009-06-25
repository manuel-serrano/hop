/*=====================================================================*/
/*    serrano/prgm/project/hop/2.0.x/share/hop-slider.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 10 11:01:53 2005                          */
/*    Last change :  Wed Jun 17 08:14:37 2009 (serrano)                */
/*    Copyright   :  2005-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP slider implementation                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_slider_value_set ...                                         */
/*---------------------------------------------------------------------*/
/*** META ((export slider-value-set!) (arity #t)) */
function hop_slider_value_set( slider, value ) {
   if( (slider instanceof String) || (typeof slider === "string") ) {
      slider = document.getElementById( slider );
   }
   
   value = Math.round( value / slider.step ) * slider.step;

   if( (value < slider.min) || (value > slider.max) ) {
      return;
   }

   if( slider.value != value ) {
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

      if( slider.cap )
	 slider.cap.innerHTML = value;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_slider_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_slider_mousemove( e, slider ) {
   var val = ((hop_event_mouse_x( e ) - hop_element_x( slider ))
	      / slider.offsetWidth) * (slider.max - slider.min);
   hop_slider_value_set( slider, Math.round( val ) + slider.min );
	 
   if( slider.onchange != undefined ) slider.onchange();
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
   var line1, line2, cursor;
   var td1, td3;
   var div;
   var caption;

   if( !parent ) { sc_error( '<SLIDER>', "Illegal parent node", parent ); }
   
   var parent = parent.parentNode;

   // the slider
   slider = document.createElement( "table" );
   slider.className = klass;
   slider.id = id;
   slider.setAttribute( "hssclass", "hop-slider" );
   
   slider.onchange = undefined;
   
   slider.parent = parent;
   slider.width = "100%";
   slider.rules = "none";
   slider.cellpadding = 0;
   slider.cellspacing = 0;
   slider.border = 1;
   parent.appendChild( slider );

   tbody = document.createElement( "tbody" );
   slider.appendChild( tbody );
   
   tr = document.createElement( "tr" );

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

   // the two lines and the cursor
   line1 = document.createElement( "td" );
   line1.className = "lineleft";
   line2 = document.createElement( "td" );
   line2.className = "lineright";
   cursor = document.createElement( "td" );
   cursor.className = "cursor";

   slider.line1 = line1;
   slider.line2 = line2;
   slider.cursor = cursor;

   line1.style.border = 0;
   line1.style.margin = 0;
   line1.style.padding = 0;
   line2.style.border = 0;
   line2.style.margin = 0;
   line2.style.padding = 0;

   tr.appendChild( line1 );
   tr.appendChild( cursor );
   tr.appendChild( line2 );

   if( cap ) {
      td1 = document.createElement( "td" );
      caption = document.createElement( "td" );
      caption.className = "caption";
      caption.align = "center";
/*       caption.style.width = cursor.width + "%";                     */
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
   div.className = "lineleft";
   div.id = id + "-lineleft";
   line1.appendChild(div);

   div = document.createElement( "div" );
   div.className = "lineright";
   div.id = id + "-lineright";
   line2.appendChild(div);

   div = document.createElement( "div" );
   div.className = "cursor cursoroff";
   div.id = id + "-cursor";
   cursor.appendChild( div );

   slider.min = min;
   slider.max = max;
   slider.step = step;

   // cursor event handling
   var mousemove = function( e ) {
      hop_slider_mousemove( e, slider );
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( document, "mousemove", mousemove, true );
   };
   
   var onmouseover = function( e ) {
      div.className = "cursor cursoron";
   };

   var onmouseout = function( e ) {
      div.className = "cursor cursoroff";
   };

   var onmousedown = function( e ) {
      hop_add_event_listener( document, "mousemove", mousemove, true );
      hop_add_event_listener( document, "mouseup", delmousemove, true );
      hop_add_event_listener( document, "onblur", delmousemove, true );
   }

   hop_add_event_listener( cursor, "mouseover", onmouseover );
   hop_add_event_listener( cursor, "mouseout", onmouseout );
   hop_add_event_listener( cursor, "mousedown", onmousedown );
   
   // line event handling
   var onlineclick = function( e ) {
      hop_slider_mousemove( e, slider );
   }

   hop_add_event_listener( line1, "click", onlineclick );
   hop_add_event_listener( line2, "click", onlineclick );

   slider.value = min - 1;

   if( value != undefined ) 
      hop_slider_value_set( slider, value );
   else
      hop_slider_value_set( slider, min );

   if( slider.onchange != undefined ) slider.onchange();

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
