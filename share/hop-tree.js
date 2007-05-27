/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-tree.js                       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Feb  6 10:51:57 2005                          */
/*    Last change :  Sun May 27 07:43:57 2007 (serrano)                */
/*    Copyright   :  2005-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP tree implementation                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop_tree_close ...                                               */
/*---------------------------------------------------------------------*/
function hop_tree_close( tree ) {
   if( tree.openp ) {
      tree.openp = false;

      if( tree.last ) {
	 tree.img_join.src = tree.icondir + "/plus.png";
      } else {
	 tree.img_join.src = tree.icondir + "/plusbottom.png";
      }
   
      tree.img_folder.src = tree.src_folderc;
      node_style_set( tree.body, "display", "none" );

      /* recursively close the subtree */
      var children = tree.body.childNodes;
      var i;

      for( i = 0; i < children.length; i++ ) {
	 if( children[ i ].className == "hop-tree" ) {
	    hop_tree_close( children[ i ] );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tree_open ...                                                */
/*---------------------------------------------------------------------*/
function hop_tree_open( tree ) {
   if( !tree.populated || !tree.cachedp ) {
      hop_tree_populate( tree );
   }
   
   tree.openp = true;
   
   if( tree.last ) {
      tree.img_join.src = tree.icondir + "/minus.png";
   } else {
      tree.img_join.src = tree.icondir + "/minusbottom.png";
   }
   
   tree.img_folder.src = tree.src_foldero;
   node_style_set( tree.body, "display", "block" );
}

/*---------------------------------------------------------------------*/
/*    hop_tree_populate ...                                            */
/*---------------------------------------------------------------------*/
function hop_tree_populate( tree ) {
   var success = function( http ) {
      tree.populated = true;
      
      if( http.responseText != null ) {
	 /* cleanup the existing tree */
	 var children = tree.body.childNodes;
	 while( children.length > 0 ) {
	    tree.body.removeChild( children[ 0 ] );
	 }

	 eval( http.responseText );
      }
   }
   
   var failure = function( http ) {
      alert( "*** Hop Tree Error: `" + http.responseText + "'" );
   }

   if( tree.service ) {
      hop( tree.service( tree.level + 1 ), success, failure );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_toggle_tree ...                                              */
/*---------------------------------------------------------------------*/
function hop_toggle_tree( tree ) {
   if( tree.openp ) {
      if( tree.history ) hop_state_history_add( tree.id, "tr", "0" );
      hop_tree_close( tree );
   } else {
      if( tree.history ) hop_state_history_add( tree.id, "tr", "1" );
      hop_tree_open( tree );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tree_children_update ...                                     */
/*---------------------------------------------------------------------*/
function hop_tree_children_update( node, level, isrc ) {
   var children = node.body.childNodes;
   var i;
   for( i = 0; i < children.length; i++ ) {
      var c = children[ i ];
      var row = c.row;
      var td = row.childNodes[ level ];
      var img = td.childNodes[ 0 ];
      img.src = isrc;
      
      if( c.className == "hop-tree" ) {
	 hop_tree_children_update( c, level, isrc );
      }
   }
 }

/*---------------------------------------------------------------------*/
/*    hop_tree_add ...                                                 */
/*---------------------------------------------------------------------*/
function hop_tree_add( tree, child ) {
   var children = tree.body.childNodes;

   if( children.length > 0 ) {
      var c = children[ children.length - 1 ];
      c.last = false;

      if( c.className == "hop-tree" ) {
	 c.img_join.src = tree.icondir + (c.openp?"/minusbottom.png":"/plusbottom.png");
	 hop_tree_children_update( c, tree.level + 1, tree.icondir + "/vline.png" );
      } else {
	 c.img_join.src = tree.icondir + "/joinbottom.png";
      }
   }

   tree.body.appendChild( child );
}

/*---------------------------------------------------------------------*/
/*    hop_push_vlines ...                                              */
/*---------------------------------------------------------------------*/
function hop_push_vlines( icondir, par, row, level ) {
   if( level > 0 ) {
      hop_push_vlines( icondir, par.parent, row, level - 1 );
      var td = document.createElement( "td" );
      var img = document.createElement( "img" );

      td.className = "hop-tree";
      td.setAttribute( "nowrap", "nowrap" );
      
      img.src = icondir + (( par && !par.last ) ? "/vline.png" : "/empty.png");
      img.className = "hop-tree";
      
      td.appendChild( img );
      row.appendChild( td );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tree_row_select ...                                          */
/*---------------------------------------------------------------------*/
function hop_tree_row_select( root, row ) {
   if( !root.multiselect && root.selection ) {
      hop_tree_row_unselect( root, root.selection );
   }
   
   row.className = "hop-tree-row-selected";
   root.value = row.value;
   root.selection = row;
   root.selections.push( row );
}
   
/*---------------------------------------------------------------------*/
/*    hop_tree_row_unselect ...                                        */
/*---------------------------------------------------------------------*/
function hop_tree_row_unselect( root, row ) {
   row.className = "hop-tree-row-unselected";
   root.selection = false;
   var i;
   root.onunselect();

   for( i = 0; i < root.selections.length; i++ ) {
      if( root.selections[ i ] == row ) {
	 root.selections.splice( i, 1 );
	 break;
      }
   }
}
   
/*---------------------------------------------------------------------*/
/*    hop_tree_row_toggle_selected ...                                 */
/*---------------------------------------------------------------------*/
function hop_tree_row_toggle_selected( tree, row ) {
   // find the root tree
   var aux = tree;
   var root = aux;

   while( aux.className == "hop-tree" ) {
      root = aux;
      aux = root.parent;
   }

   if( row.className == "hop-tree-row-selected" ) {
      if( root.onselect ) {
	 root.onselect();
      }
      hop_tree_row_unselect( root, row );
   } else {
      hop_tree_row_select( root, row );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_make_tree ...                                                */
/*---------------------------------------------------------------------*/
function hop_make_tree( parent, id, level, svc, title, openp, cachedp, icondir, fo, fc , mu, ons, onus, value, history ) {
   var tree = document.createElement( "div" );

   /* safety check */
   if( parent == null ) {
      alert( "***INTERNAL ERROR: Illegal empty tree parent -- " + id );
   }
   
   /* the tree first line */
   var table = document.createElement( "table" );
   
   table.setAttribute( "cellpadding", 0 );
   table.setAttribute( "cellspacing", 0 );
   table.setAttribute( "border", 0 );
   table.className = "hop-tree";
   
   var tb = document.createElement( "tbody" );
   var row = document.createElement( "tr" );
   row.className = "hop-tree-row-unselected";
   
   /* build the left vertical lines */
   if( level > 0 ) {
      hop_push_vlines( icondir, parent, row, level );
   }

   /* the plus/minus icon */
   var td1 = document.createElement( "td" );
   td1.className = "hop-tree";
   td1.setAttribute( "nowrap", "nowrap" );
   
   var join = document.createElement( "img" );
   join.className = "hop-tree-openclose";
   join.src = icondir + "/plus.png";
   join.onclick = function() { hop_toggle_tree( tree ) };
   
   td1.appendChild( join );
   row.appendChild( td1 );

   /* add the folder icon */
   var td2 = document.createElement( "td" );
   var folder = document.createElement( "img" );
   
   folder.src = fc;
   folder.className = "hop-tree";

   td2.className = "hop-tree";
   td2.setAttribute( "nowrap", "nowrap" );
   td2.onclick = function() {hop_tree_row_toggle_selected( tree, row, true );}
   td2.appendChild( folder );
   
   /* the title */
   var e = document.createElement( "span" );
   e.className = "hop-tree-head";  
   e.innerHTML= title;
   td2.appendChild( e );

   row.appendChild( td2 );
   row.value = value;

   tb.appendChild( row );
   table.appendChild( tb );
   tree.appendChild( table );
   
   /* the (empty) body */
   var body = document.createElement( "div" );
   node_style_set( body, "display", "none" );
   body.className = "hop-tree-body";
   tree.appendChild( body );

   /* the attribute of the tree */
   tree.className = "hop-tree";
   tree.id = id;
   tree.parent = parent;
   tree.table = table;
   tree.body = body;
   tree.row = row;
   tree.level = level;
   tree.openp = false;
   tree.history = (history != false);
   tree.cachedp = cachedp;
   tree.populated = false;
   tree.last = true;
   tree.img_join = join;
   tree.img_folder = folder;
   tree.src_foldero = fo;
   tree.src_folderc = fc;
   tree.icondir = icondir;
   tree.service = svc;

   /* append the leaf to the tree */
   if( level > 0 ) {
      hop_tree_add( parent, tree );
      
      tree.selections = [];
      tree.multiselect = false;
      tree.onselect = false;
      tree.onunselect = false;
   } else {
      parent.appendChild( tree );
   
      tree.selections = [];
      tree.multiselect = mu;
      tree.onselect = ons;
      tree.onunselect = onus;
   }

   /* open the tree if required */
   if( openp ) {
      hop_tree_open( tree );
   }
   
   return tree;
}

/*---------------------------------------------------------------------*/
/*    hop_make_tree_leaf ...                                           */
/*---------------------------------------------------------------------*/
function hop_make_tree_leaf( tree, content, value, icon ) {
   var level = tree.level + 1;
   var leaf = document.createElement( "table" );

   leaf.setAttribute( "cellpadding", 0 );
   leaf.setAttribute( "cellspacing", 0 );
   leaf.setAttribute( "border", 0 );
   leaf.className = "hop-tree-leaf";

   var tb = document.createElement( "tbody" );
   var row = document.createElement( "tr" ); 
   row.className = "hop-tree-row-unselected";

   /* build the left vertical lines */
   hop_push_vlines( tree.icondir, tree, row, level );

   /* space */
   var td1 = document.createElement( "td" );
   td1.className = "hop-tree";
   td1.setAttribute( "nowrap", "nowrap" );
   
   var join = document.createElement( "img" );
   join.className = "hop-tree";
   join.src = tree.icondir + "/join.png";
   
   td1.appendChild( join );
   row.appendChild( td1 );

   /* add the folder icon */
   var td2 = document.createElement( "td" );
   var fimg = document.createElement( "img" );
   
   fimg.src = icon;
   fimg.className = "hop-tree";

   td2.className = "hop-tree";
   td2.onclick = function() {hop_tree_row_toggle_selected( tree, row, true );}
   td2.appendChild( fimg );
   row.appendChild( td2 );
   row.value = value;

   /* the content */
   var e = document.createElement( "span" );
   e.classname = "hop-tree";
   e.innerHTML= content;
   td2.appendChild( e );
   row.appendChild( td2 );

   tb.appendChild( row );
   leaf.appendChild( tb );

   /* the attribute of the leaf */
   leaf.parent = tree;
   leaf.row = row;
   leaf.last = true;
   leaf.img_join = join;
   
   /* append the leaf to the tree */
   hop_tree_add( tree, leaf );
   
   return leaf;
}

/*---------------------------------------------------------------------*/
/*    hop_tree_reset ...                                               */
/*---------------------------------------------------------------------*/
function hop_tree_reset( tree ) {
   var sel = tree.selections;
   var len = sel.length;
   var i;
   res = new Array( len );

   for( i = 0; i < len; i++ ) {
      res[ i ] = sel[ i ].value;
   }
   
   for( i = 0; i < len; i++ ) {
      hop_tree_row_unselect( tree, sel[ 0 ] );
   }
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_tree_id_reset ...                                            */
/*---------------------------------------------------------------------*/
function hop_tree_id_reset( id ) {
   var el = document.getElementById( id );

   if( el instanceof HTMLDivElement ) {
      return hop_tree_reset( el );
   } else {
      return new Array( 0 );
   }
}
/*---------------------------------------------------------------------*/
/*    hop_tree_selection ...                                           */
/*---------------------------------------------------------------------*/
function hop_tree_selection( tree ) {
   var sel = tree.selections;
   var len = sel.length;
   var i;
   res = new Array( len );

   for( i = 0; i < len; i++ ) {
      res[ i ] = sel[ i ].value;
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_tree_id_selection ...                                        */
/*---------------------------------------------------------------------*/
function hop_tree_id_selection( id ) {
   return hop_tree_selection( document.getElementById( id ) );
}

/*---------------------------------------------------------------------*/
/*    Install the tree history state handler                           */
/*---------------------------------------------------------------------*/
hop_state_history_register_handler(
   "tr", /* key argument */
   "0",  /* reset value  */
   function( id, arg ) {
     var tr = document.getElementById( id );
     if( tr != undefined ) {
	if( arg == "0" ) {
	   hop_tree_close( tr );
	} else {
	   hop_tree_open( tr );
	}
     }
} );
