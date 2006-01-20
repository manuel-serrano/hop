/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-sorttable.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 10 14:25:42 2004                          */
/*    Last change :  Mon Jan  9 14:07:39 2006 (serrano)                */
/*    Copyright   :  2004-06 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Sortable tables                                                  */
/*    -------------------------------------------------------------    */
/*    CREDITS:                                                         */
/*      This code is vaguley based on a JavaScript example from        */
/*      kryogenix.org: http://www.kryogenix.org/code/browser/sorttable */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Global variables                                                 */
/*---------------------------------------------------------------------*/
var SORT_COLUMN_INDEX;

/*---------------------------------------------------------------------*/
/*    hop_sorttable ...                                                */
/*---------------------------------------------------------------------*/
function hop_sorttable( i ) {
   make_sorttable( document.getElementById( i ).firstChild );
}

/*---------------------------------------------------------------------*/
/*    make_sorttable ...                                               */
/*---------------------------------------------------------------------*/
function make_sorttable( table ) {
   if( table.rows && (table.rows.length > 0) ) {
      var firstRow = table.rows[ 0 ];
   }
   
   if( !firstRow )
      return;
    
   // We have a first row: assume it's the header,
   // and make its contents clickable links
   for( var i = 0; i<firstRow.cells.length;i++ ) {
      var cell = firstRow.cells[ i ];
      var txt = hop_sorttable_getInnerText( cell );
      
      cell.innerHTML = '<a href="#" class="hop-sortheader" onclick="hop_sorttable_sort(this);return false;">'+ txt +'<span class="hop-sortarrow">&nbsp;</span></a>';
   }
}

/*---------------------------------------------------------------------*/
/*    hop_sorttable_getInnerText ...                                   */
/*---------------------------------------------------------------------*/
function hop_sorttable_getInnerText( el ) {
   if( el instanceof String ) return el;
   if( el.nodeType == 3 ) return el.nodeValue;
   if( typeof el == "undefined" ) return el;
   if( el.innerText ) return el.innerText;
   
   var cs = el.childNodes;
   var l = cs.length;

   if( l == 1 ) {
      return hop_sorttable_getInnerText( cs[ 0 ] );
   } else {
      var str = "";
      for( var i = 0; i < l; i++ ) {
	 str += hop_sorttable_getInnerText( cs[ i ] );
      }
   
      return str;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_sorttable_sort ...                                           */
/*---------------------------------------------------------------------*/
function hop_sorttable_sort( lnk ) {
   var td = lnk.parentNode;
   var arrow = lnk.lastChild;
   var column = td.cellIndex;
   var table = get_parent( td, 'TABLE' );

   if( table.rows.length <= 1 ) return;

   SORT_COLUMN_INDEX = column;
   var newRows = new Array();
   var itm = hop_sorttable_getInnerText( table.rows[1].cells[ column ] );
    
   for( j = 1; j < table.rows.length; j++ ) {
      var o = new Object();
      var l = table.rows[ j ].cells[ column ].getAttribute( "lang" );

      if( typeof l == "string" ) {
	 o.key = l;
	 itm = false;
      } else {
	 o.key = hop_sorttable_getInnerText( table.rows[ j ].cells[ column ] );
      }
      o.val = table.rows[ j ];
      newRows[ j - 1 ] = o;
   }
    
   if( !itm ) {
      newRows.sort( hop_sorttable_sort_default );
   } else {
      if( itm.match(/^[\d\.]+$/) ) {
	 newRows.sort( hop_sorttable_sort_numeric );
      } else {
	 newRows.sort( hop_sorttable_sort_default );
      }
   }
    
   if( lnk.getAttribute( "sortdir" ) != 'down' ) {
      newRows.reverse();
      arrow.innerHTML = "&#8593;";
      lnk.setAttribute( 'sortdir', 'down' );
   } else {
      arrow.innerHTML = "&#8595;";
      lnk.setAttribute( 'sortdir', 'up' );
   }
    
   for( i = 0; i < newRows.length; i++ ) {
      table.tBodies[ 0 ].appendChild( newRows[ i ].val );
   }
}

/*---------------------------------------------------------------------*/
/*    get_parent                                                       */
/*---------------------------------------------------------------------*/
function get_parent( el, pTagName ) {
   if( el == null )
      return null;
   else {
      if( (el.nodeType == 1) &&
	  el.tagName.toLowerCase() == pTagName.toLowerCase() ) {
	 // Gecko bug, supposed to be uppercase
	 return el;
      } else {
	 return get_parent( el.parentNode, pTagName );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_sorttable_sort_numeric ...                                   */
/*---------------------------------------------------------------------*/
function hop_sorttable_sort_numeric( a, b ) { 
   aa = parseFloat( a.key );
   if( isNaN( aa ) ) aa = 0;
   
   bb = parseFloat( b.key );
   if( isNaN( bb ) ) bb = 0;
   
   return aa - bb;
}

/*---------------------------------------------------------------------*/
/*    hop_sorttable_sort_default ...                                   */
/*---------------------------------------------------------------------*/
function hop_sorttable_sort_default( a, b ) {
   if( a.key == b.key ) return 0;
   if( a.key < b.key ) return -1;
   return 1;
}

