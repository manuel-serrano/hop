/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-tabslider.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio [eg@essi.fr]                       */
/*    Creation    :  14-Sep-2005 09:24 (eg)                            */
/*    Last change :  Wed May 31 13:40:39 2006 (serrano)                */
/*    Copyright   :  2006 Inria                                        */
/*    -------------------------------------------------------------    */
/*    HOP tabslider implementation                                     */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_tabslider_select ...                                         */
/*---------------------------------------------------------------------*/
function hop_tabslider_select( item ) {
   var parent = item.parentNode;
   var totalHeight = parent.offsetHeight;
   var titlesHeight = 0;
   var selected;
   var i;

   for( i = 0; i < parent.childNodes.length; i += 2 ) {
      var title = parent.childNodes[ i ];
      var content = parent.childNodes[ i + 1 ];

      titlesHeight += title.offsetHeight;
      if( title == item ) {
	 selected = content;
	 title.className = "hop-tabslider-head-active";
	 if( content.lang == "delay" ) {
	    hop( selected.onkeyup()(),
		 function( http ) {
	            selected.innerHTML = http.responseText;
		    selected.style.display = "block";
	         } );
	 } else {
	    selected.style.display = "block";
	 }
      } else {
	 content.style.display = "none";
	 title.className = "hop-tabslider-head-inactive";
      }
   }
    
   /* Set the height of the selected item */
   selected.style.height = totalHeight - titlesHeight;
}


/*---------------------------------------------------------------------*/
/*    hop_tabslider_init ...                                           */
/*---------------------------------------------------------------------*/
function hop_tabslider_init( id, ind ) {
   var ts = document.getElementById( id );

   window.addEventListener( 'load', 
			    function( e ) {
                               hop_tabslider_select( ts.childNodes[ 2*ind ] );
                            },
			    false );
}


/* {*---------------------------------------------------------------------*} */
/* {*    Plug the tabsliders behaviour ...                                *} */
/* {*---------------------------------------------------------------------*} */
/* hopBehaviour.register('hop-tabslider-head',                         */
/* 		      function (el) {                                  */
/* 			  el.onclick = function(){                     */
/* 			      hop_tabslider_select(this)               */
/* 			  }                                            */
/* 			  // hop_tabslider_select(el.parentNode.childNodes[0]); */
/* 		      });                                              */
