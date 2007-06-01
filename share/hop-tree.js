/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-tree.js                       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Feb  6 10:51:57 2005                          */
/*    Last change :  Fri Jun  1 13:27:49 2007 (serrano)                */
/*    Copyright   :  2005-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP tree implementation                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    hop tree default connection icons                                */
/*---------------------------------------------------------------------*/
var hop_tree_plus_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAABZSURBVCjPpZFBDoBACAMH4r91X14vbiKIhqwQODSlkIKIeSgjQpjohGdglHNNte1dZbeCFuE89ritvsJphUcl0+xz8VXZVlTZvLIUQFbRboaMD6dXf/qLdgLZWTv35WFeagAAAABJRU5ErkJggg==";
var hop_tree_plusbottom_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAABYSURBVCjPxZFBDoBACAMH4r91X14vbiLIGuJFCBwaaElBxDyUESFMdMIzMMq9Jtu2ZtmtGItwXnvcVl/htMIjk2n2KXxVthVVNn8RBZBVYzdDxovTv/z0BFmcPnXuAkthAAAAAElFTkSuQmCC";

var hop_tree_minus_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAABPSURBVCjPY/jPgAob/qOL/Gf4z8D4n4EYwIQu0IhVH5GmseA2pZ4RizJUYXRtGG4j0gtEKGP8D4MIi6EYV7Ci8qnrNqQAacQT0uTGKUXKANpKOvds6MpSAAAAAElFTkSuQmCC";
var hop_tree_minusbottom_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAABNSURBVCjPY/jPgAob/qOL/Gf4z8D4n4EYwIQu0IhVH5GmseA2pZ4RizJUYXRtGG4j0gtEKGP8D4MIi6EYV7Ci8qnrNqQAacQT0gMSpwBajT11pflliQAAAABJRU5ErkJggg==";

var hop_tree_join_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAAAqSURBVCjPY/jPgAob/qOL/Gf4z8D4n4EYwIQu0IhV33A1DROTa9qwVQYASJ4mAj+uy7cAAAAASUVORK5CYII=";
var hop_tree_joinbottom_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAAAqSURBVCjPY/jPgAob/qOL/Gf4z8D4n4EYwIQu0IhV33A1DRMPHrcNEtMAEKItfKjCO18AAAAASUVORK5CYII=";

var hop_tree_vline_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAAAlSURBVCjPY/jPgAob/qOL/Gf4z8D4n4EYwIQu0IhV36hpI9Y0AMqWJfzsxwvxAAAAAElFTkSuQmCC";
var hop_tree_empty_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAQAAABezYzpAAAAAmJLR0QAAKqNIzIAAAAJcEhZcwAAAEgAAABIAEbJaz4AAAAWSURBVCjPY/zPQAxgIkrVqLJRZQQAAJYfAR/lOWwPAAAAAElFTkSuQmCC";

var hop_tree_default_file_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQBAMAAAAG6llRAAAAMFBMVEUAAAD////n59b///f39/fv7+/Ozs6cnJyEhIRzc3NSUlIxMTH///8AAAAAAAAAAAAlS/PmAAAADXRSTlP///////////////8APegihgAAAAFiS0dEAIgFHUgAAAAJcEhZcwAAAEgAAABIAEbJaz4AAABaSURBVAjXYzgDAwcYOoCgB8JsFBQUrIIyjY2NC9fAmat3g5nNQGb6LBjTOawKyjQxCcqCME2Mg2BMY1NTJRgzGC5qaqoKFW0MVQqCMtvSgADC7FoFAmAmwpEAK+5bgL+cQWoAAAAASUVORK5CYII=";

var hop_tree_default_open_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQBAMAAAAG6llRAAAAGFBMVEUAAAD///+cnADOzmP//5z/zpz///8AAAAHOMQHAAAAB3RSTlP///////8AGksDRgAAAAFiS0dEAIgFHUgAAAAJcEhZcwAAAEgAAABIAEbJaz4AAABiSURBVAjXY0iDgQSGtCQlJRhTUVBQDcJMEnFxEVJSUoMyXQQFDWBMFxcQUwkMTEBMRaBQSDAzlBni6swMUxsSqqAAZrq6uoYaKICtCAkJDTViADEhRoCZCQxgAGIiuQzOBAA3liwhABnN2QAAAABJRU5ErkJggg==";
var hop_tree_default_close_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQBAMAAAAG6llRAAAAMFBMVEUAAAD///+cnADOzmP//5z//87/zpz39/f///8AAAAAAAAAAAAAAAAAAAAAAAAAAABwA2sJAAAACXRSTlP//////////wBTT3gSAAAAAWJLR0QAiAUdSAAAAAlwSFlzAAAASAAAAEgARslrPgAAAFRJREFUCNdj6ICBBoYOJSUFKLOpNMSJA8JUNjY2VlJSAjFVQ0HAmQPEdAGBZBjTzQ0hmgITdUMSTYGJAgXdIMwUIEgDM0FWAAGI2cAABiAmssvgTAAi3zcy8laStgAAAABJRU5ErkJggg==";
var hop_tree_default_device_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABMAAAAQBAMAAAAG6llRAAAAMFBMVEUAAAD///8AzgAAhADe3t7W1tbGxsa1tbWcnJyUlJRzc3P///8AAAAAAAAAAAAAAADp0SUnAAAADHRSTlP//////////////wAS387OAAAAAWJLR0QAiAUdSAAAAAlwSFlzAAAASAAAAEgARslrPgAAAEdJREFUCNdj2A0DGxhIYq4Cg9VA5q5yMFgAZC4UBAEJEHNpWlpaunIliLmkLC2tvBzCBOsCM5eC1YKZELNWcQOZGxjAAKfFAJqsX3JLxaIiAAAAAElFTkSuQmCC";

/*---------------------------------------------------------------------*/
/*    hop_tree_close ...                                               */
/*---------------------------------------------------------------------*/
function hop_tree_close( tree ) {
   if( tree.openp ) {
      tree.openp = false;

      if( tree.last ) {
	 tree.img_join.src = tree.iconplus;
      } else {
	 tree.img_join.src = tree.iconplusbottom;
      }

      if( tree.iconclose ) tree.img_folder.src = tree.iconclose;
	 
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
      tree.img_join.src = tree.iconminus;
   } else {
      tree.img_join.src = tree.iconminusbottom;
   }
   
   if( tree.iconopen ) tree.img_folder.src = tree.iconopen;
      
   node_style_set( tree.body, "display", "block" );
}

/*---------------------------------------------------------------------*/
/*    hop_tree_populate ...                                            */
/*---------------------------------------------------------------------*/
function hop_tree_populate( tree ) {
   if( tree.proc ) tree.proc();
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
	 c.img_join.src = (c.openp ? tree.iconminusbottom:tree.iconplusbottom);
	 
	 hop_tree_children_update( c, tree.level + 1, tree.iconvline );
      } else {
	 c.img_join.src = tree.iconjoinbottom;
      }
   }

   tree.body.appendChild( child );
}

/*---------------------------------------------------------------------*/
/*    hop_push_vlines ...                                              */
/*---------------------------------------------------------------------*/
function hop_push_vlines( par, row, level ) {
   if( level > 0 ) {
      hop_push_vlines( par.parent, row, level - 1 );
      var td = document.createElement( "td" );
      var img = document.createElement( "img" );

      td.className = "hop-tree";
      td.setAttribute( "nowrap", "nowrap" );

      img.src = ((par && !par.last) ? par.iconvline : (par.visible ? par.iconempty : ""));
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
function hop_make_tree( parent, id, visible, level, proc, title,
			openp, cachedp,
			mu, ons, onus, value, history,
			iconopen, iconclose, icondir ) {
   var tree = document.createElement( "div" );
   
   /* fixup icons */
   if( iconopen == true ) {
      iconopen = hop_tree_default_open_icon;
   } else {
      if( iconopen == 0 ) {
	 iconopen = hop_tree_default_device_icon;
      } else {
	 if( iconopen == false ) iconopen = '';
      }
   }
   
   if( iconclose == true ) {
      iconclose = hop_tree_default_close_icon;
   } else {
      if( iconclose == 0 ) {
	 iconclose = hop_tree_default_device_icon;
      } else {
	 if( iconclose == false ) iconclose = '';
      }
   }

   /* safety check */
   if( parent == null ) {
      alert( "***INTERNAL ERROR: Illegal empty tree parent -- " + id + "\n" +
	 "This may happen because the whole document is lacking a BODY node." );
      return false;
   }
   
   /* the tree first line */
   var table = document.createElement( "table" );
   
   table.setAttribute( "cellpadding", 0 );
   table.setAttribute( "cellspacing", 0 );
   table.setAttribute( "border", 0 );
   table.className = "hop-tree";
   
   /* the colgroup for the left padding and icons */
   var colgroup = document.createElement( "colgroup" );
   
   for( var i = level + 2; i > 0; i-- ) {
      var col = document.createElement( "col" );
   
      col.setAttribute( "width", "0*" );
      colgroup.appendChild( col );
   }
   table.appendChild( colgroup );

   /* the body of the table */
   var tb = document.createElement( "tbody" );
   var row = document.createElement( "tr" );
   row.className = "hop-tree-row-unselected";
   row.setAttribute( "align", "left" );
   
   /* build the left vertical lines */
   if( level > 0 ) {
      hop_push_vlines( parent, row, level );
   }

   /* the plus/minus icon */
   var td1 = document.createElement( "td" );
   td1.className = "hop-tree";
   td1.setAttribute( "nowrap", "nowrap" );
   
   var join = document.createElement( "img" );
   join.className = "hop-tree-openclose";
   join.src = icondir ? (icondir + "/plus.png") : hop_tree_plus_icon;
   join.onclick = function() { hop_toggle_tree( tree ) };
   
   td1.appendChild( join );
   row.appendChild( td1 );

   /* add the folder icon */
   var td2 = document.createElement( "td" );
   var folder = document.createElement( "img" );

   if( iconclose ) folder.src = iconclose;
   folder.className = "hop-tree";

   td2.className = "hop-tree";
   td2.setAttribute( "nowrap", "nowrap" );
   td2.onclick = function() {hop_tree_row_toggle_selected( tree, row, true );}
   td2.appendChild( folder );
   
   /* the title */
   var td3 = document.createElement( "td" );
   td3.className = "hop-tree";
   td3.setAttribute( "nowrap", "nowrap" );
   td3.onclick = function() {hop_tree_row_toggle_selected( tree, row, true );}
   td3.innerHTML = title;
   
   row.appendChild( td2 );
   row.appendChild( td3 );
   row.value = value;

   if( visible ) {
      tb.appendChild( row );
   }
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
   tree.proc = proc;
   tree.img_join = join;
   tree.img_folder = folder;
   tree.visible = visible;
   tree.iconopen = iconopen;
   tree.iconclose = iconclose;
   if( icondir ) {
      tree.iconplus = icondir + "/plus.png";
      tree.iconplusbottom = icondir + "/plusbottom.png";
      tree.iconminus = icondir + "/minus.png";
      tree.iconminusbottom = icondir + "/minusbottom.png";
      tree.iconjoin = icondir + "/join.png";
      tree.iconjoinbottom = icondir + "/joinbottom.png";
      tree.iconvline = icondir + "/vline.png";
      tree.iconempty = icondir + "/empty.png";
   } else {
      tree.iconplus = hop_tree_plus_icon;
      tree.iconplusbottom = hop_tree_plusbottom_icon;
      tree.iconminus = hop_tree_minus_icon;
      tree.iconminusbottom = hop_tree_minusbottom_icon;
      tree.iconjoin = hop_tree_join_icon;
      tree.iconjoinbottom = hop_tree_joinbottom_icon;
      tree.iconvline = hop_tree_vline_icon;
      tree.iconempty = hop_tree_empty_icon;
   }

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
   if( openp ) hop_tree_open( tree );
   
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

   /* the colgroup for the left padding and icons */
   var colgroup = document.createElement( "colgroup" );
   
   for( var i = level + 2; i > 0; i-- ) {
      var col = document.createElement( "col" );
   
      col.setAttribute( "width", "0*" );
      colgroup.appendChild( col );
   }
   leaf.appendChild( colgroup );

   /* the body of the table */
   var tb = document.createElement( "tbody" );
   var row = document.createElement( "tr" ); 
   row.className = "hop-tree-row-unselected";
   row.setAttribute( "align", "left" );

   /* build the left vertical lines */
   hop_push_vlines( tree, row, level );

   /* space */
   var td1 = document.createElement( "td" );
   td1.className = "hop-tree";
   td1.setAttribute( "nowrap", "nowrap" );
   
   var join = document.createElement( "img" );
   join.className = "hop-tree";
   join.src = tree.iconjoin;
   
   td1.appendChild( join );
   row.appendChild( td1 );

   /* add the folder icon */
   var td2 = document.createElement( "td" );
   
   /* the icon assiocated with the node */
   if( icon ) {
      var fimg = document.createElement( "img" );
      
      if( (icon instanceof String) || (typeof icon == "string") ) {
	 fimg.src = icon;
      } else {
	 fimg.src = hop_tree_default_file_icon;
      }
      fimg.className = "hop-tree";
      
      td2.appendChild( fimg );
   }

   td2.className = "hop-tree";
   td2.onclick = function() {hop_tree_row_toggle_selected( tree, row, true );}

   row.appendChild( td2 );
   row.value = value;

   /* the content */
   var td3 = document.createElement( "td" );
   td3.classname = "hop-tree";
   td3.onclick = function() {hop_tree_row_toggle_selected( tree, row, true );}
   td3.innerHTML= content;
   
   row.appendChild( td2 );
   row.appendChild( td3 );

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
