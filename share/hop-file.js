/*=====================================================================*/
/*    serrano/prgm/project/hop/2.0.x/share/hop-file.js                 */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Apr  2 07:05:30 2008                          */
/*    Last change :  Wed Nov 18 09:18:14 2009 (serrano)                */
/*    Copyright   :  2008-09 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Client side support for url browsers.                            */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_inputurl_key ...                                             */
/*---------------------------------------------------------------------*/
function hop_inputurl_keydown( obj, event ) {
   
   function hop_inputurl_complete( obj ) {
      if( obj.completion.length > 0 ) {
	 if( obj.completion_count >= obj.completion.length ) {
	    obj.completion_count = 0;
	 }

	 obj.value = obj.completion[ obj.completion_count++ ];

	 if( obj.completion.length == 1 ) {
	    obj.completion = false;
	    obj.completion_count = 0;
	 }
      } else {
	 var c = obj.className;
	 obj.className = c + " hop_inputurl_flash";
	 setInterval( function() {
	       obj.className = c;
	    }, 100 );
      }
   };

   function callback( v ) {
      obj.completion = v;
      obj.completion_count = 0;
      hop_inputurl_complete( obj );
   };
   
   if( hop_event_key_code( event ) == 9 ) {
      hop_stop_propagation( event, false );

      if( !(obj.completion instanceof Array ) ) {
	 // the name of the service is defined in runtime/hop-file.scm
	 var svc = hop_apply_url( hop_service_base() + "/server-file/completion",
				  [ obj.value ] );
	 
	 with_hop( svc, callback );
      } else {
	 hop_inputurl_complete( obj );
      }
   } else {
      obj.completion = false;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_button_push ...                                  */
/*---------------------------------------------------------------------*/
function hop_filechooser_button_push( button, id, url ) {
   var el = document.getElementById( id );
   var fe = document.getElementById( id + "-filters" );
   var he = document.getElementById( id + "-hidden" );
   
   // update the button row
   if( el.button ) {
      el.button.className = "filechooser-button filechooser-button-unselected";
   }
   button.className = "filechooser-button filechooser-button-selected";
   el.button = button;

   // get the files content
   // the name of the service is defined in runtime/hop-file.scm
   var svc = hop_apply_url( hop_service_base() + "/server-file/files",
			    [ id, url, fe.value, he.checked ] );

   function callback( h ) {
      hop_innerHTML_set( id + "-files", h.car )
	 
      el.selected = false;
      el.value = url;
   }
   
   with_hop( svc, callback );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_select ...                                       */
/*---------------------------------------------------------------------*/
function hop_filechooser_select( row, event, id, url ) {
   var el = document.getElementById( id );

   if( el.selected && (el.selected != undefined) ) {
      el.selected.className = el.selected.oldClassName;
   }

   if( el.selected != row ) {
      row.oldClassName = row.className;
      row.className = "selected";

      el.selected = row;
      el.value = url;

      if( el.select ) el.select( event );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_open ...                                         */
/*---------------------------------------------------------------------*/
function hop_filechooser_open( id, url ) {
   var el = document.getElementById( id );
   var fe = document.getElementById( id + "-filters" );
   var he = document.getElementById( id + "-hidden" );
   
   // the name of the service is defined in runtime/hop-file.scm
   var svc = hop_apply_url( hop_service_base() + "/server-file/files",
			    [ id, url, fe.value, he.checked ] );

   function callback( h ) {
      hop_innerHTML_set( id + "-files", h.car );
      hop_innerHTML_set( id + "-path", h.cdr );

      el.selected = false;
   }

   el.value = url;

   with_hop( svc, callback );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_add ...                                          */
/*---------------------------------------------------------------------*/
function hop_filechooser_add( id ) {
   var el = document.getElementById( id );

   // the name of the service is defined in runtime/hop-file.scm
   var svc = hop_apply_url( hop_service_base() + "/server-file/addplace",
			    [ id, el.value ] );

   function callback( h ) {
      hop_innerHTML_set( id + "-places", h )
   }

   with_hop( svc, callback );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_remove ...                                       */
/*---------------------------------------------------------------------*/
function hop_filechooser_remove( id ) {
   var el = document.getElementById( id );

   // the name of the service is defined in runtime/hop-file.scm
   var svc = hop_apply_url( hop_service_base() + "/server-file/removeplace",
			    [ id, el.value ] );

   function callback( h ) {
      hop_innerHTML_set( id + "-places", h )
   }
   
   with_hop( svc, callback );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_toggle_location ...                              */
/*---------------------------------------------------------------------*/
function hop_filechooser_toggle_location( span, id ) {
   var el = document.getElementById( id );
   var cname = span.className;
   var flag = (cname.indexOf( "filechooser-button-selected" ) == 0);
   var l = document.getElementById( id + "-location" );

   // the name of the service is defined in runtime/hop-file.scm
   var svc = hop_apply_url( hop_service_base() + "/server-file/togglelocation",
			    [ id, flag ] );

   with_hop( svc );
   
   if( flag ) {
      var i = cname.indexOf( ' ' );
      span.className = span.className.substring( i + 1, cname.length);
      node_style_set( l, "display", "none" );
   } else {
      span.className = "filechooser-button-selected " + cname;
      node_style_set( l, "display", "block" );
   }
}   

/*---------------------------------------------------------------------*/
/*    hop_filechooser_location_keypress ...                            */
/*---------------------------------------------------------------------*/
function hop_filechooser_location_keypress( input, event, id ) {
   if( hop_event_key_code( event ) == 13 ) {
      hop_filechooser_open( id, input.value );
   } else {
      id.value = input.value;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_filter ...                                       */
/*---------------------------------------------------------------------*/
function hop_filechooser_filter( id, default_url ) {
   var el = document.getElementById( id );
   var url = ((el.value == undefined) || !el.value ) ? default_url : el.value;
   
   hop_filechooser_open( id, url );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_drag_el ...                                      */
/*---------------------------------------------------------------------*/
var hop_filechooser_drag_el = undefined;
var hop_filechooser_el = undefined;

/*---------------------------------------------------------------------*/
/*    hop_filechooser_drag_and_drop_move ...                           */
/*---------------------------------------------------------------------*/
function hop_filechooser_drag_and_drop_move( event ) {
   node_style_set( hop_filechooser_drag_el, "left", (event.pageX + 10) + "px" );
   node_style_set( hop_filechooser_drag_el, "top", (event.pageY + 5) + "px" );
   node_style_set( hop_filechooser_drag_el, "visibility", "visible" );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_abort_drag ...                                   */
/*---------------------------------------------------------------------*/
function hop_filechooser_abort_drag( event ) {
   if( hop_filechooser_drag_el != undefined ) {
      hop_remove_event_listener( document, "mousemove", hop_filechooser_drag_and_drop_move );
      node_style_set( hop_filechooser_drag_el, "visibility", "hidden" );

      hop_filechooser_el.oldClassName = hop_filechooser_el.className;
      
      var nodes = event.target.childNodes;
      for( var i = 0; i < nodes.length; i++ ) {
	 var p = nodes[ i ];
	 if( p.onmouseup ) {
	    p.onmouseup( event );
	 }
      }
      hop_filechooser_drag_el = undefined;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_end_drag ...                                     */
/*---------------------------------------------------------------------*/
function hop_filechooser_end_drag( event, id ) {
   if( hop_filechooser_drag_el != undefined ) {
      hop_filechooser_add( id );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_begin_drag ...                                   */
/*---------------------------------------------------------------------*/
function hop_filechooser_begin_drag( event, id, url ) {
   var el = document.getElementById( id );

   el.oldClassName = el.className;
   el.className = el.className + " filechooser-drag-and-drop";
   hop_filechooser_el = el;

   hop_filechooser_drag_el = document.getElementById( id + "-drag" );
   el.value = url;
   
   hop_add_event_listener( document,
			   "mousemove",
			   hop_filechooser_drag_and_drop_move );
   hop_add_event_listener( document,
			   "mouseup",
			   hop_filechooser_abort_drag );
			   
   hop_stop_propagation( event, false );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_ok ...                                           */
/*---------------------------------------------------------------------*/
function hop_filechooser_ok( event, id ) {
   var el = document.getElementById( id );
   
   if( el.open ) el.open( event );
   hop_stop_propagation( event, false );
}
   
/*---------------------------------------------------------------------*/
/*    hop_filechooser_cancel ...                                       */
/*---------------------------------------------------------------------*/
function hop_filechooser_cancel( event, id ) {
   var el = document.getElementById( id );
   
   if( el.cancel ) el.cancel( event );
   hop_stop_propagation( event, false );
}
   
/*---------------------------------------------------------------------*/
/*    hop_filechooser_run ...                                          */
/*---------------------------------------------------------------------*/
function hop_filechooser_run( event, id ) {
   var el = document.getElementById( id );
   
   if( el.run ) el.run( event );
   hop_stop_propagation( event, false );
}
   
/*---------------------------------------------------------------------*/
/*    hop_filechooser_key ...                                          */
/*---------------------------------------------------------------------*/
function hop_filechooser_key( table, event, id, pep, pid, nep, nid ) {
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser ...                                              */
/*---------------------------------------------------------------------*/
function hop_filechooser( args ) {
   // the name of the service is defined in runtime/hop-file.scm
   return hop_apply_url( hop_service_base() + "/server-file/filechooser",
			 [ args ] );
}

/*---------------------------------------------------------------------*/
/*    hop_filechooser_proc_count ...                                   */
/*---------------------------------------------------------------------*/
var hop_filechooser_proc_count = 0;
var hop_filechooser_proc_table = [];

/*---------------------------------------------------------------------*/
/*    hop_filechooser_save_proc ...                                    */
/*---------------------------------------------------------------------*/
function hop_filechooser_save_proc( event, proc ) {
   var c = hop_filechooser_proc_count++;
   hop_filechooser_proc_table[ c ] = proc;
   return "this." + event + " = hop_filechooser_proc_table[" + c + "]; delete hop_filechooser_proc_table[" + c + "]; this." + event + "()";
}

/*---------------------------------------------------------------------*/
/*    <FILECHOOSER>                                                    */
/*---------------------------------------------------------------------*/
/*** META (define-macro (<FILECHOOSER> . args)
	     (let loop ((args args)
			(attrs '()))
		(cond
		   ((null? args)
		    `((@ hop_filechooser _) (list ,@(reverse! attrs))))
		   ((or (null? (cdr args)) (not (keyword? (car args))))
		    (loop (cdr args) (cons (car args) attrs)))
		   ((string-prefix? "on" (keyword->string (car args)))
		    (let ((s (keyword->string (car args))))
		       (loop (cddr args)
   		             (cons* `((@ hop_filechooser_save_proc _)
                                      ,(substring s 2 (string-length s))
			              (lambda () ,(cadr args)))
				    (car args) attrs))))
		   (else
		    (loop (cddr args)
   		          (cons* (cadr args) (car args) attrs)))))) */
