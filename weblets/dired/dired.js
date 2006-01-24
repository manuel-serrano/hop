/*=====================================================================*/
/*    serrano/prgm/project/hop/weblets/dired/dired.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 13 13:23:10 2004                          */
/*    Last change :  Tue Jan 24 17:52:04 2006 (serrano)                */
/*    Copyright   :  2004-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Dired JScript management.                                        */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    dired_image_timeout ...                                          */
/*---------------------------------------------------------------------*/
var dired_image_timeout = 5000;

/*---------------------------------------------------------------------*/
/*    dired_rename ...                                                 */
/*---------------------------------------------------------------------*/
function dired_rename( service, event, obj ) {
   if( event.keyCode == 13 ) {
      hop( service, function( http ) { obj.style.color = "red" } );
   }
}

/*---------------------------------------------------------------------*/
/*    dired_remove ...                                                 */
/*---------------------------------------------------------------------*/
function dired_remove( service, name, ident, cont ) {
   if( confirm( "Remove file \"" + name + "\"?" ) ) {
      var success = function( http ) {
	 var o = document.getElementById( ident );

	 if( o instanceof HTMLElement ) {
	    var p = o.parentNode;

	    if( hop_mozillap() ) {
	       o.style.setProperty( "filter", "alpha(opacity=30)", "" );
	       o.style.setProperty( "-moz-opacity", "0.3", "" );
	    } else {
	       p.removeChild( o );
	    }
	 }
      }
      var failure = function( http ) {
	 alert( "Error, can't remove `" + name + "'\n" + http.responseText );
      };

      hop( service, success, failure );

      return true;
   } else {
      return false;
   }
}

/*---------------------------------------------------------------------*/
/*    dired_cursor_set ...                                             */
/*---------------------------------------------------------------------*/
function dired_cursor_set( cursor ) {
   var body = document.getElementsByTagName( "body" )[ 0 ];
   var img = document.getElementsByTagName( "img" )[ 0 ];
   body.style.setProperty( "cursor", cursor, "" );
   img.style.setProperty( "cursor", cursor, "" );
}

/*---------------------------------------------------------------------*/
/*    dired_no_more_image ...                                          */
/*---------------------------------------------------------------------*/
function dired_no_more_image( http ) {
   document.open();
   if( http.status != 307 ) {
      document.write( "<html><body><center><b>No more images...</b></center></body></html>" );
      document.close();
      window.close();
   } else {
      document.open();
      document.write( http.responseText );
      document.close();
   }
}

/*---------------------------------------------------------------------*/
/*    dired_image_keypress ...                                         */
/*---------------------------------------------------------------------*/
function dired_image_keypress( evt, service, file, timer ) {
   var kc = (evt == null ? evt : evt.keyCode);

   var success = function( http ) {
      document.open();
      document.write( http.responseText );
      document.close();
   };
   
   function next_photo( rotate, d, t ) {
      var w = window.innerWidth;
      var h = window.innerHeight;

      hop( service( file, rotate, w, h, false, d, t,
		    (dired_image_popup_on &&
		     dired_image_popup instanceof HTMLDivElement &&
		     (dired_image_popup.style.visibility != "hidden")) ),
	   hop_replace_document,
	   dired_no_more_image );
   };
   
   if( (kc == null) || (kc == 39) || (kc == 40) || (kc == 13) || (kc == 10) ) {
      // next photo
      next_photo( false, 1, timer );
   } else {
      if( (kc == 37) || (kc == 38) ) {
	 // previous photo
	 next_photo( false, -1, timer );
      } else {
	 if( (kc == 27) || (kc == 46) || (evt.which == 113) ) {
	    // close
	    window.close();
	 } else {
	    switch( evt.which ) {
	       case 32:
		  // toggle slideshow
		  if( timer ) {
		     clearInterval( window.dired_interval );
		  } else {
		     dired_cursor_set( "wait" );
		     setInterval( next_photo, 1000, 0, 1, dired_image_timeout );
		  }
		  return;

	       case 49:
		  next_photo( 0, 0, timer );
		  return;
		  
	       case 50:
		  next_photo( 90, 0, timer );
		  return;
		  
	       case 51:
		  next_photo( 180, 0, timer );
		  return;
		  
	       case 52:
		  next_photo( 270, 0, timer );
		  return;
	    }
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    dired_image_popup ...                                            */
/*---------------------------------------------------------------------*/
var dired_image_icon_dir = "";
var dired_image_popup = false;
var dired_image_popup_on = false;
var dired_image_popup_file = false;
var dired_mouse_move_count = 0;

/*---------------------------------------------------------------------*/
/*    dired_image_mouse_move ...                                       */
/*---------------------------------------------------------------------*/
function dired_image_mouse_move( event, service, rm, file, tip ) {
   dired_image_popup_on = true;
   
   dired_image_popup_file = file;
   
   if( window.dired_interval != undefined ) {
      clearInterval( window.dired_interval );
      dired_cursor_set( "pointer" );
   }
   
   if( event ) {
      if( dired_mouse_move_count < 20 ) {
	 dired_mouse_move_count++;
	 return;
      }
   }
      
   if( !dired_image_popup ) {
      dired_image_popup = document.getElementById( "dired-image-popup" );

      if( dired_image_popup instanceof HTMLDivElement ) {
	 var row = document.createElement( "div" );
	 var trash, cancel, fore, back, disp, info;
	 var landscape, seescape, portrait, upsidedown;

	 trash = document.createElement( "img" );
	 trash.className = "dired-button";
	 trash.id = "trashcan";
	 trash.src = dired_image_icon_dir + "trash.png";
	 trash.title = "Remove";

	 fore = document.createElement( "img" );
	 fore.className = "dired-button";
	 fore.id = "forward";
	 fore.src = dired_image_icon_dir + "forward.png";
	 fore.title = "Next";

	 back = document.createElement( "img" );
	 back.className = "dired-button";
	 back.id = "backward";
	 back.src = dired_image_icon_dir + "back.png";
	 back.title = "Previous";

	 disp = document.createElement( "img" );
	 disp.className = "dired-button";
	 disp.id = "display";
	 disp.src = dired_image_icon_dir + "display.png";
	 disp.title = "Slideshow";

	 info = document.createElement( "img" );
	 info.className = "dired-button";
	 info.id = "info";
	 info.src = dired_image_icon_dir + "camera.png";
	 info.title = "Exif";

	 cancel = document.createElement( "img" );
	 cancel.className = "dired-button";
	 cancel.id = "cancel";
	 cancel.src = dired_image_icon_dir + "cancel.png";
	 cancel.title = "Hide";

	 landscape = document.createElement( "img" );
	 landscape.className = "dired-button";
	 landscape.id = "landscape";
	 landscape.src = dired_image_icon_dir + "landscape.png";
	 landscape.title = "Landscape";

	 seascape = document.createElement( "img" );
	 seascape.className = "dired-button";
	 seascape.id = "seascape";
	 seascape.src = dired_image_icon_dir + "seascape.png";
	 seascape.title = "Seascape";

	 portrait = document.createElement( "img" );
	 portrait.className = "dired-button";
	 portrait.id = "portrait";
	 portrait.src = dired_image_icon_dir + "portrait.png";
	 portrait.title = "Portrait";

	 upsidedown = document.createElement( "img" );
	 upsidedown.className = "dired-button";
	 upsidedown.id = "upsidedown";
	 upsidedown.src = dired_image_icon_dir + "upsidedown.png";
	 upsidedown.title = "Upsidedown";

	 row.appendChild( trash );
	 row.appendChild( info );
	 row.appendChild( landscape );
	 row.appendChild( seascape );
	 row.appendChild( portrait );
	 row.appendChild( upsidedown );
	 row.appendChild( back );
	 row.appendChild( fore );
	 row.appendChild( disp );
	 row.appendChild( cancel );
	 
	 dired_image_popup.appendChild( row );

	 dired_image_popup_fore = fore;
	 dired_image_popup_back = back;

	 // callbacks
	 function next_photo( rotate, d, tm, pp ) {
	    var w = window.innerWidth;
	    var h = window.innerHeight;
	 
	    hop( service( dired_image_popup_file, rotate, w, h, false, d, tm, pp ),
		 hop_replace_document,
		 dired_no_more_image );
	 };

	 trash.onmouseup = function( e ) {
	    if( dired_remove( rm( file ), file, false ) ) {
	       next_photo( false, 1, false, true );
	    }
	 };
	 cancel.onmouseup = function( e ) {
	    dired_mouse_move_count = 0;
	    dired_image_popup.style.visibility = "hidden";
	    dired_image_popup_on = false;
	 }
	 back.onmouseup = function( e ) {
	    next_photo( false, -1, false, true );
	 };
	 fore.onmouseup = function( e ) {
	    next_photo( false, 1, false, true );
	 };
	 disp.onmouseup = function( e ) {
	    dired_image_popup.style.visibility = "hidden";
	    dired_image_popup_on = false;
	    dired_mouse_move_count = 0;
	    next_photo( false, 1, dired_image_timeout, false );
	 };
	 info.onmouseup = function( e ) {
	    hop_tooltip_show( e, tip, window.innerWidth - 205, 43 );
	 };
	 landscape.onmouseup = function( e ) {
	    next_photo( 0, 0, false, true );
	 };
	 seascape.onmouseup = function( e ) {
	    next_photo( 180, 0, false, true );
	 };
	 portrait.onmouseup = function( e ) {
	    next_photo( 90, 0, false, true );
	 };
	 upsidedown.onmouseup = function( e ) {
	    next_photo( 270, 0, false, true );
	 };
      }
   }
   
   if( dired_image_popup instanceof HTMLDivElement ) {
      dired_image_popup.style.visibility = "visible";
      dired_image_popup.style.left = window.innerWidth - 353;
      dired_image_popup.style.top = 1;
      
   }
}

/*---------------------------------------------------------------------*/
/*    dired_image_init ...                                             */
/*---------------------------------------------------------------------*/
function dired_image_init( service, rm, file, rotation, fsize, tm, tip, pp, idir ) {
   window.onkeypress = function( e ) {
      dired_image_keypress( e, service, file, tm );
   }

   window.onresize = function( e ) {
      // Full size window are slightly shrunked, hence, they are always 
      // resized. The variable fsize prevents from re-requesting the proxy
      if( fsize ) {
	 fsize = false;
      } else {
	 var w = window.innerWidth;
	 var h = window.innerHeight;

	 hop( service( file, rotation, w, h, false, 0, true ),
	      hop_replace_document );
      }
   }

   if( tm ) {
      dired_cursor_set( "wait" );
      window.dired_interval =
	 setInterval( dired_image_keypress, tm, null, service, file, tm );
   }

   dired_image_icon_dir = idir + "/";
   document.addEventListener( "mousemove", function( e ) { dired_image_mouse_move( e, service, rm, file, tip ) }, false );

   if( pp ) {
      dired_image_mouse_move( false, service, rm, file, tip );
   }
}

/*---------------------------------------------------------------------*/
/*    dired_image_show ...                                             */
/*---------------------------------------------------------------------*/
function dired_image_show( service, file, name, fsize ) {
   var w = window.screen.availWidth;
   var h = window.screen.availHeight;
   var gprop = "toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=no, resizable=no, copyhistory=no, titlebar=no";
   var prop;
   
   if( !fsize ) {
      w /= 2;
      h /= 2;
   }

   prop = "width=" + w + ", height=" + h + "," + gprop;

   return window.open( service( file, false, w, h, fsize, 0, false, false ),
		       name,
		       prop );
}

/*---------------------------------------------------------------------*/
/*    dired_jpeg_comment ...                                           */
/*---------------------------------------------------------------------*/
var dired_jpeg_comment = false;
var dired_jpeg_comment_edit = false;
var dired_jpeg_comment_ok = false;
var dired_jpeg_comment_path = false;
var dired_jpeg_comment_id = false;

/*---------------------------------------------------------------------*/
/*    dired_exif_comment ...                                           */
/*---------------------------------------------------------------------*/
function dired_exif_comment( event, save, path, id, com ) {
   dired_jpeg_comment_path = path;
   dired_jpeg_comment_id = id;

   if( !dired_jpeg_comment ) {
      dired_jpeg_comment = document.getElementById( "dired-jpeg-comment" );

      if( dired_jpeg_comment instanceof HTMLDivElement ) {
	 var row = document.createElement( "div" );
	 var cancel, reset;
	 
	 dired_jpeg_comment_edit = document.createElement( "textarea" );
	 dired_jpeg_comment_edit.cols = 20;
	 dired_jpeg_comment_edit.rows = 3;
	 dired_jpeg_comment.appendChild( dired_jpeg_comment_edit );
	 dired_jpeg_comment.appendChild( row );

	 cancel = document.createElement( "span" );
	 cancel.id = "cancel";
	 cancel.innerHTML = "Cancel";
	 
	 reset = document.createElement( "span" );
	 reset.id = "reset";
	 reset.innerHTML = "Reset";
	 
	 dired_jpeg_comment_ok = document.createElement( "span" );
	 dired_jpeg_comment_ok.id = "ok";
	 dired_jpeg_comment_ok.innerHTML = "Ok";

	 row.appendChild( dired_jpeg_comment_ok );
	 row.appendChild( cancel );
	 row.appendChild( reset );

	 // callbacks
	 var comment = function( val ) {
	    var te = document.getElementById( dired_jpeg_comment_id );
	    hop( save( dired_jpeg_comment_path, val ), false, false );

	    if( te instanceof HTMLTextAreaElement ) {
	       te.value = val;
	    }
	    dired_jpeg_comment.style.visibility = "hidden";
	 };
	    
	 cancel.onclick = function() { 
	    dired_jpeg_comment.style.visibility = "hidden";
	 }
	 
	 reset.onclick = function() {
	    comment( "" );
	 }
	 
	 dired_jpeg_comment_ok.onclick = function() {
	    comment( dired_jpeg_comment_edit.value );
	 }
      }
   }
   
   if( dired_jpeg_comment instanceof HTMLDivElement ) {
      dired_jpeg_comment.style.visibility = "visible";
      var x = event.pageX - 100;
      var y = event.pageY - 100;
      
      dired_jpeg_comment.style.left = (x < 0) ? 0 : x;
      dired_jpeg_comment.style.top = (y < 0) ? 0: y;

      dired_jpeg_comment_edit.value = com;
   }
}

/*---------------------------------------------------------------------*/
/*    dired_quick_tree_active ...                                      */
/*---------------------------------------------------------------------*/
var dired_quick_tree_active = false;

/*---------------------------------------------------------------------*/
/*    dired_quick_tree_hide ...                                        */
/*---------------------------------------------------------------------*/
function dired_quick_tree_hide( id ) {
   el = document.getElementById( id );
   
   el.style.visibility = "hidden";
   dired_quick_tree_active = false;
}

/*---------------------------------------------------------------------*/
/*    dired_quick_tree ...                                             */
/*---------------------------------------------------------------------*/
function dired_quick_tree( event, id, tid ) {
   if( dired_quick_tree_active instanceof HTMLDivElement ) {
      dired_quick_tree_active.style.visibility = "hidden";
      dired_quick_tree_active = false;
   }
   
   el = document.getElementById( id );

   if( el instanceof HTMLDivElement ) {
      var tree = document.getElementById( tid );
      el.style.visibility = "visible";
      var x = event.pageX + 1;
      var y = event.pageY + 10;
      
      el.style.left = (x < 0) ? 0 : x;
      el.style.top = (y < 0) ? 0: y;

      dired_quick_tree_active = el;

      hop_tree_open( tree );
      
   }
}
