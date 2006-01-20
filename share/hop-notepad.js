/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-notepad.js                    */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Aug 17 16:07:08 2005                          */
/*    Last change :  Wed Sep 28 17:08:16 2005 (serrano)                */
/*    Copyright   :  2005 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    HOP notepad implementation                                       */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_notepad_remote ...                                           */
/*---------------------------------------------------------------------*/
function hop_notepad_remote( service, tab, ghost, notepad ) {
   var success = function( http ) {
      if( http.responseText != null ) {
	 var np = document.getElementById( notepad );

	 for( i = 0; i < np.childNodes.length; i++ ) {
	    var c = np.childNodes[ i ];

	    if( c.className == "hop-notepad-tabs" ) {
	       for( j = 0; j < c.childNodes.length; j++ ) {
		  var c2 = c.childNodes[ j ];

		  if( c2.id == tab ) {
		     c2.className = "hop-nptab-active";
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
   var failure = function( http ) {
      alert( "*** Hop Notepad Tab Error: `" + http.responseText + "'" );
   }

   hop( service( tab ), success, failure);
}

/*---------------------------------------------------------------------*/
/*    hop_notepad_inline ...                                           */
/*---------------------------------------------------------------------*/
function hop_notepad_inline( tab, ghost, notepad ) {
   var np = document.getElementById( notepad );

   for( i = 0; i < np.childNodes.length; i++ ) {
      var c = np.childNodes[ i ];
      var c2;

      if( c.className == "hop-notepad-tabs" ) {
	 for( j = 0; j < c.childNodes.length; j++ ) {
	    c2 = c.childNodes[ j ];

	    if( c2.id == tab ) {
	       c2.className = "hop-nptab-active";
	    } else {
	       c2.className = "hop-nptab-inactive";
	    }
	 }
      }
	    
      if( c.className == "hop-notepad-body" ) {
	 for( j = 0; j < c.childNodes.length; j++ ) {
	    c2 = c.childNodes[ j ];

	    if( c2.id == ghost ) {
	       c2.style.display = "block";
	    } else {
	       c2.style.display = "none";
	    }
	 }
      }
   }
}
