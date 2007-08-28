/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-slider.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 10 11:01:53 2005                          */
/*    Last change :  Sat Aug 25 14:36:58 2007 (serrano)                */
/*    Copyright   :  2005-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP slider implementation                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_slider_value_set ...                                         */
/*---------------------------------------------------------------------*/
function hop_slider_value_set( slider, value ) {
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
	 slider.line1.width = Math.round(v * 100) + "%";
	 slider.line2.width = Math.round((100-v) * 100) + "%";
      }

      if( slider.cap )
	 slider.cap.innerHTML = value;
	 
      if( slider.onchange != undefined )
	 slider.onchange();
   }
}

/*---------------------------------------------------------------------*/
/*    hop_slider_mousemove ...                                         */
/*---------------------------------------------------------------------*/
function hop_slider_mousemove( e, slider ) {
   var val = ((hop_event_mouse_x( e ) - hop_element_x( slider ))
	      / slider.offsetWidth) * (slider.max - slider.min);
   hop_slider_value_set( slider, Math.round( val ) + slider.min );
}

/*---------------------------------------------------------------------*/
/*    hop_slider_value_get ...                                         */
/*---------------------------------------------------------------------*/
function hop_slider_value_get( slider ) {
   return slider.value;
}

/*---------------------------------------------------------------------*/
/*    hop_make_slider ...                                              */
/*---------------------------------------------------------------------*/
function hop_make_slider( parent, id, min, max, step, value, cap, curw, curh ) {
   var doc = (parent == undefined ? document : parent.ownerDocument || parent.document);
   var slider, tbody, tr, tr2;
   var line1, line2, cursor;
   var td1, td3;
   var div;
   var caption;

   if( curw == undefined )
      curw = 4;
   if( curh == undefined )
      curh = "10px";

   // the slider
   slider = doc.createElement( "table" );
   slider.className = "hop-slider";
   slider.id = id;
   
   slider.onchange = undefined;
   
   slider.parent = parent;
   slider.width = "100%";
   slider.rules = "none";
   slider.cellpadding = 0;
   slider.cellspacing = 0;
   slider.border = 0;
   node_style_set( slider, "border-collapse", "collapse" );
   node_style_set( slider, "border-spacing", "0" )
   parent.appendChild( slider );

   tbody = doc.createElement( "tbody" );
   slider.appendChild( tbody );
   
   tr = doc.createElement( "tr" );
   tr2 = doc.createElement( "tr" );

   if( cap == "top" ) {
      tbody.appendChild( tr2 );
      tbody.appendChild( tr );
   } else {
      if( cap == "bottom" ) {
	 tbody.appendChild( tr );
	 tbody.appendChild( tr2 );
      } else {
	 tbody.appendChild( tr );
      }
   }

   // the two lines and the cursor
   line1 = doc.createElement( "td" );
   line1.className = "lineleft";
   line2 = doc.createElement( "td" );
   line2.className = "lineright";
   cursor = doc.createElement( "td" );
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

   cursor.width = (Math.round( curw / 2 ) * 2);
   cursor.style.width = cursor.width + "%";
   cursor.style.height = curh;
   cursor.style.border = 0;
   cursor.style.margin = 0;
   cursor.style.padding = 0;

   tr.appendChild( line1 );
   tr.appendChild( cursor );
   tr.appendChild( line2 );

   if( cap ) {
      td1 = doc.createElement( "td" );
      caption = doc.createElement( "td" );
      caption.className = "caption";
      caption.align = "center";
      caption.style.width = cursor.width + "%";
      td3 = doc.createElement( "td" );

      caption.innerHTML = "";
      slider.cap = caption;
   
      tr2.appendChild( td1 );
      tr2.appendChild( caption );
      tr2.appendChild( td3 );
   } else {
      slider.cap = false;
   }

   div = doc.createElement( "div" );
   div.className = "lineleft";
   line1.appendChild(div);

   div = doc.createElement( "div" );
   div.className = "lineright";
   line2.appendChild(div);

   div = doc.createElement( "div" );
   div.className = "cursoroff";
   div.style.height = curh;
   cursor.appendChild( div );

   slider.min = min;
   slider.max = max;
   slider.step = step;

   // cursor event handling
   var mousemove = function( e ) {
      hop_slider_mousemove( e, slider );
   };

   var delmousemove = function( e ) {
      hop_remove_event_listener( doc, "mousemove", mousemove, true );
   };
   
   var onmouseover = function( e ) {
      div.className = "cursoron";
   };

   var onmouseout = function( e ) {
      div.className = "cursoroff";
   };

   var onmousedown = function( e ) {
      hop_add_event_listener( doc, "mousemove", mousemove, true );
      hop_add_event_listener( doc, "mouseup", delmousemove, true );
      hop_add_event_listener( doc, "onblur", delmousemove, true );
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

   return slider;
}

/*---------------------------------------------------------------------*/
/*    hop_slider_onchange_get ...                                      */
/*---------------------------------------------------------------------*/
function hop_slider_onchange_get( slider ) {
   return slider.onchange;
}

/*---------------------------------------------------------------------*/
/*    hop_slider_onchange_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_slider_onchange_set( slider, onchange ) {
   slider.onchange = onchange;
}
