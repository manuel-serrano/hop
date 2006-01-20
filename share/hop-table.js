/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-table.js                      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Dec 10 14:25:42 2004                          */
/*    Last change :  Mon Jan  9 07:59:00 2006 (serrano)                */
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
/*    hop_sorttables_init ...                                          */
/*---------------------------------------------------------------------*/
function hop_sorttables_init() {
   // Find all tables with class sortable and make them sortable
   if( !document.getElementsByTagName )
      return;

   tbls = document.getElementsByTagName( "table" );
   
   for( ti = 0; ti < tbls.length; ti++ ) {
      thisTbl = tbls[ ti ];
      if( ((' '+thisTbl.className+' ').indexOf( "hoptable" ) != -1) &&
	  (thisTbl.id) ) {
	 make_sortable( thisTbl );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    make_sortable ...                                                */
/*---------------------------------------------------------------------*/
function make_sortable( table ) {
   if( table.rows && (table.rows.length > 0) ) {
      var firstRow = table.rows[ 0 ];
   }
   
   if( !firstRow )
      return;
    
   // We have a first row: assume it's the header,
   // and make its contents clickable links
   for( var i = 0; i<firstRow.cells.length;i++ ) {
      var cell = firstRow.cells[ i ];
      var txt = hop_ts_getInnerText( cell );
      
      cell.innerHTML = '<a href="#" class="hopheader" onclick="hop_ts_resortTable(this);return false;">'+ txt +'<span class="sortarrow"></span></a>';
   }
}

/*---------------------------------------------------------------------*/
/*    hop_ts_getInnerText ...                                          */
/*---------------------------------------------------------------------*/
function hop_ts_getInnerText( el ) {
   if( el instanceof String ) return el;
   if( el.nodeType == 3 ) return el.nodeValue;
   if( typeof el == "undefined" ) return el;
   if( el.innerText ) return el.innerText;
   
   var cs = el.childNodes;
   var l = cs.length;

   if( l == 1 ) {
      return hop_ts_getInnerText( cs[ 0 ] );
   } else {
      var str = "";
      for( var i = 0; i < l; i++ ) {
	 str += hop_ts_getInnerText( cs[ i ] );
      }
   
      return str;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_ts_resortTable ...                                           */
/*---------------------------------------------------------------------*/
function hop_ts_resortTable( lnk ) {
   var td = lnk.parentNode;
   var column = td.cellIndex;
   var table = get_parent( td, 'TABLE' );
    
   if( table.rows.length <= 1 ) return;

   SORT_COLUMN_INDEX = column;
   var newRows = new Array();
   var itm = hop_ts_getInnerText( table.rows[1].cells[ column ] );
    
   for( j = 1; j < table.rows.length; j++ ) {
      var o = new Object();
      var l = table.rows[ j ].cells[ column ].getAttribute( "lang" );

      if( typeof l == "string" ) {
	 o.key = l;
	 itm = false;
      } else {
	 o.key = hop_ts_getInnerText( table.rows[ j ].cells[ column ] );
      }
      o.val = table.rows[ j ];
      newRows[ j - 1 ] = o;
   }
    
   if( !itm ) {
      newRows.sort( hop_ts_sort_default );
   } else {
      if( itm.match(/^[\d\.]+$/) ) {
	 newRows.sort( hop_ts_sort_numeric );
      } else {
	 newRows.sort( hop_ts_sort_default );
      }
   }
    
   if( lnk.getAttribute( "sortdir" ) != 'down' ) {
      newRows.reverse();
      lnk.setAttribute( 'sortdir', 'down' );
   } else {
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
/*    hop_ts_sort_numeric ...                                          */
/*---------------------------------------------------------------------*/
function hop_ts_sort_numeric( a, b ) { 
   aa = parseFloat( a.key );
   if( isNaN( aa ) ) aa = 0;
   
   bb = parseFloat( b.key );
   if( isNaN( bb ) ) bb = 0;
   
   return aa - bb;
}

/*---------------------------------------------------------------------*/
/*    hop_ts_sort_default ...                                          */
/*---------------------------------------------------------------------*/
function hop_ts_sort_default( a, b ) {
   if( a.key == b.key ) return 0;
   if( a.key < b.key ) return -1;
   return 1;
}

/*---------------------------------------------------------------------*/
/*    add_event                                                        */
/*    -------------------------------------------------------------    */
/*    cross-browser event handling for IE5+,  NS6 and Mozilla          */
/*    By Scott Andrew.                                                 */
/*---------------------------------------------------------------------*/
function add_event( elm, evType, fn ) {
   if( elm.addEventListener ) {
      elm.addEventListener( evType, fn, false );
      return true;
   } else {
      if( elm.attachEvent ){
	 var r = elm.attachEvent( "on" + evType, fn );
	 return r;
      } else {
	 alert( "Handler could not be removed" );
	 return false;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    onload event                                                     */
/*---------------------------------------------------------------------*/
add_event( window, "load", hop_sorttables_init );

