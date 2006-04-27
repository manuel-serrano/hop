/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-notepad.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:07:08 2005                          */
/*    Last change :  Thu Apr 27 08:14:20 2006 (serrano)                */
/*    Copyright   :  2005-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP notepad implementation                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_notepad_remote ...                                           */
/*---------------------------------------------------------------------*/
function hop_notepad_remote( service, notepad, tab ) {
   var success = function( http ) {
      if( http.responseText != null ) {
	 var found = 0;
	 var np = (notepad instanceof HTMLElement) ? notepad :
	    document.getElementById( notepad );
	 var ta = (tab instanceof HTMLElement) ? tab :
	    document.getElementById( tab );
	 var i;

	 for( i = 0; i < np.childNodes.length; i++ ) {
	    var c = np.childNodes[ i ];

	    if( c.className == "hop-notepad-tabs" ) {
	       for( j = 0; j < c.childNodes.length; j++ ) {
		  var c2 = c.childNodes[ j ];

		  if( c2 == ta ) {
		     c2.className = "hop-nptab-active";
		     found = j;
		  } else {
		     c2.className = "hop-nptab-inactive";
		  }
	       }
	    }
	    if( c.className == "hop-notepad-body" ) {
	       hop_replace_inner( c )( http );
	    }
	 }
      }
   }

   if( !notepad.remote_service ) notepad.remote_service = service;
   
   hop( service( tab ), success );
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_inline ...                                           */
/*---------------------------------------------------------------------*/
function hop_notepad_inline( notepad, tab ) {
   var found = 0;
   var np = (notepad instanceof HTMLElement) ? notepad :
      document.getElementById( notepad );
   var ta = (tab instanceof HTMLElement) ? tab :
      document.getElementById( tab );
   var i;

   for( i = 0; i < np.childNodes.length; i++ ) {
      var c = np.childNodes[ i ];
      var c2;

      if( c.className == "hop-notepad-tabs" ) {
	 for( j = 0; j < c.childNodes.length; j++ ) {
	    c2 = c.childNodes[ j ];

	    if( c2 == ta ) {
	       c2.className = "hop-nptab-active";
	       found = j;
	    } else {
	       c2.className = "hop-nptab-inactive";
	    }
	 }
      }
	    
      if( c.className == "hop-notepad-body" ) {
	 for( j = 0; j < c.childNodes.length; j++ ) {
	    c2 = c.childNodes[ j ];

	    if( j == found ) {
	       c2.style.display = "block";
	    } else {
	       c2.style.display = "none";
	    }
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_select ...                                           */
/*---------------------------------------------------------------------*/
function hop_notepad_select( id1, id2 ) {
   var notepad = document.getElementById( id1 );
   var tab = document.getElementById( id2 );

   if( notepad.remote_service != null ) {
      hop_notepad_remote( notepad.remote_service, notepad, tab );
   } else {
      hop_notepad_inline( notepad, tab );
   }
}
