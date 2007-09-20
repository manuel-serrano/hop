/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-tree.js                       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Feb  6 10:51:57 2005                          */
/*    Last change :  Wed Sep 19 16:44:34 2007 (serrano)                */
/*    Copyright   :  2005-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    HOP tree implementation                                          */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Autoconfiguration                                                */
/*---------------------------------------------------------------------*/
var hop_tree_correct_browserp =
   hop_mozillap() && !hop_msiep() && !hop_khtmlp() && !hop_operap();

/*---------------------------------------------------------------------*/
/*    hop tree default connection icons                                */
/*---------------------------------------------------------------------*/
var hop_tree_plus_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGBSz9nQiXAAAAQUlEQVQoz2NggIOG/wwUAjJNYMSms4ERr9GofCYkNlY3MBFyAxNc9384CbUGhhF2/ifkBgZCbmDEpoCFOrFAEQAAUAAXAZHu4U4AAAAASUVORK5CYII=";
var hop_tree_plusbottom_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGBiTYa9NmAAAAQklEQVQoz2NggIOG/wwUAjJNYMSms4ERr9GofCYkNlY3MBFyAxNc9384CbUGhhF2/ifkBgZCbmDEpoCFOrFAUVwAAKFKGIBRBwqlAAAAAElFTkSuQmCC";

var hop_tree_minus_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGBx2edWovAAAAMUlEQVQoz2NggIOG/wwUAjJNYMSms4ERr9GofCZCVhCt4D8cwq2BYZq7gYU6sUARAAAzdRYBgZWhFwAAAABJRU5ErkJggg==";
var hop_tree_minusbottom_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGCAqaPvMnAAAAMklEQVQoz2NggIOG/wwUAjJNYMSms4ERr9GofCZCVhCt4D8cwq2BYZq7gYU6sUBRXAAAhL8XgGNqXUkAAAAASUVORK5CYII=";

var hop_tree_join_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGCSvPTNI4AAAAF0lEQVQoz2NggIOG/wwUguFjAjY8IgAAQ+4O99IvLs4AAAAASUVORK5CYII=";
var hop_tree_joinbottom_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGCiBzs1hzAAAAGElEQVQoz2NggIOG/wwUguFjAjY8IsIBAGAkE3R0p9F5AAAAAElFTkSuQmCC";

var hop_tree_vline_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGEBzs8d4vAAAAF0lEQVQoz2NggIIGhob/DBSBUROGgQkA2icP+TmjJRoAAAAASUVORK5CYII=";
var hop_tree_empty_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAACxMAAAsTAQCanBgAAAAHdElNRQfXBhIGDCVVgwt6AAAADklEQVQoz2NgGAWjAAEAAhAAARTCwJIAAAAASUVORK5CYII=";

var hop_tree_default_file_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH1wYSBSwKaZCs2AAAANFJREFUOMvFkz2SgzAMhb9kUqhMS8kR8FE4D6WPwRVoOQaUlCqTUqU6tljnb4nXzKTIm/HYY/s96VkWfIhDjHEtXeq67pA9jDGu/6Hv+7Vt22yQ42PpmQHDMJATOZbSH8eREAKqStM0G5HTI3rWP9M0sSwLqso8z+8EtnAHEajr+r6nqvss3MieEqsqIYRA8Q1uBBEwc8y0+A9eBER+ZzPnfBZEKtwv+wWeyWaO+wWRar/AO3Ipg1QFwcwAuF412SmT7wJ/a5uKtquZTqkf+Bp+ADzifcCAdekSAAAAAElFTkSuQmCC";

var hop_tree_default_open_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH1wYSBSkWAOYE0gAAALNJREFUOMvNk0ESgyAMRR+OF4Jdz0OOlZynOzkSXSAWFK2dbvo3DJjElx+AH+XajSp5HyDSx5xKlTzSqGiruSbHmAE7BJRzNywigps/sxkxKhC705QEsDzft8vGLdQeze55FULE+0fvwVn/hRXwZU08CcFYlvfn6fRXaV19U2SgSQRX8HuTtoSGwPNAlZXCLgjSMbn1oCOo89wo0g47DS8dIo2J26DqJKpJy5dvAa6v7X/qBRKAUS3mXATrAAAAAElFTkSuQmCC";
var hop_tree_default_close_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH1wYSBSoOOKfPRwAAAJRJREFUOMvFUzkShDAMEwwfMl8i3b5nO/Meuvg5KUUBuwMkZpKlWM1kcliSVcTAQ3Sfgyp4LYZw5rhQBVNKJOO+lKRyN+VtAlVwHCeXEONcfA9hSzYAgMjLNchrEWYLgJkAuuFYqIKdr32TyADILwZyEDcluHZuTiAFcVUCr7Pl1L5K5Ii//8DsnZuU9ptZ4NNZ+h9WRsVBhT94M5oAAAAASUVORK5CYII=";
var hop_tree_default_device_icon = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH1wYSBSsEwWkXGAAAAJRJREFUOMvtkrENwyAURB8SBS1lxvAKtB6HNWAExoCSNTxCSijpSEWU2IkclCpSrrrmnfTvH/wlhrHW9lnYey/kgI0xU3DOGaBLa213zk3BIYS7lwDbtlFrfQuUUlgvKySIS6S19hyglKKUgtb6JQwQrxGWY7jc3XSqfVcSoLXGpz2klI4Bj6VMvBBAjB30bzb047oBYBc056hSeS0AAAAASUVORK5CYII=";

if( hop_msiep() ) {
   /* IE does not support inline images! */
   var icon_dir = hop_share_directory() + "/icons/hop-tree";
   hop_tree_plus_icon = icon_dir + "plus.png";
   hop_tree_plusbottom_icon = icon_dir + "plusbottom.png";
   hop_tree_minus_icon = icon_dir + "minus.png";
   hop_tree_minusbottom_icon = icon_dir + "minusbottom.png";
   hop_tree_join_icon = icon_dir + "join.png";
   hop_tree_joinbottom_icon = icon_dir + "joinbottom.png";
   hop_tree_vline_icon = icon_dir + "vline.png";
   hop_tree_empty_icon = icon_dir + "empty.png";
   hop_tree_default_file_icon = icon_dir + "file.png";
   hop_tree_default_open_icon = icon_dir + "folder-open.png";
   hop_tree_default_close_icon = icon_dir + "folder-close.png";
   hop_tree_default_device_icon = icon_dir + "device.png";
}

/*---------------------------------------------------------------------*/
/*    hop_tree_root ...                                                */
/*---------------------------------------------------------------------*/
function hop_tree_root( tree ) {
   var aux = tree;
   var root = tree;

   while( aux && aux.className == "hop-tree" ) {
      root = aux;
      aux = root.parent;
   }
   return root;
}
   
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

      if( tree.iconclose ) {
	 tree.iconerror = tree.iconclose;
	 tree.img_folder.src = tree.iconclose;
      }
	 
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
   if( tree.proc ) hop_tree_populate( tree );
   
   tree.openp = true;
   
   if( tree.last ) {
      tree.img_join.src = tree.iconminus;
   } else {
      tree.img_join.src = tree.iconminusbottom;
   }
   
   if( tree.iconopen ) {
      tree.iconerror = tree.iconopenerr;
      tree.img_folder.src = tree.iconopen;
   }
      
   node_style_set( tree.body, "display", "block" );
}

/*---------------------------------------------------------------------*/
/*    hop_tree_populate ...                                            */
/*---------------------------------------------------------------------*/
function hop_tree_populate( tree ) {
   var success = function( html ) {
      tree.populated = true;
      
      if( html ) {
         /* cleanup the existing tree */
         var children = tree.body.childNodes;
         while( children.length > 0 ) {
            tree.body.removeChild( children[ 0 ] );
         }

         eval( html );
      }
   }
   
   return hop( tree.proc(), success );
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
      var td = document.createElement( "td" );
      td.className = "hop-tree";
      
      hop_push_vlines( par.parent, row, level - 1 );

      if( par && par.visible ) {
	 var img = document.createElement( "img" );

	 td.setAttribute( "nowrap", "nowrap" );

	 img.src = ((par && !par.last) ? par.iconvline : par.iconempty);
	 img.className = "hop-tree";
      
	 td.appendChild( img );
      } else {
	 node_style_set( td, "display", "none" );
      }
      row.appendChild( td );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tree_row_select ...                                          */
/*---------------------------------------------------------------------*/
function hop_tree_row_select( root, row, forcemulti ) {
   if( !forcemulti && !root.multiselect && root.selection ) {
      hop_tree_row_unselect( root, root.selection );
   }

   if( forcemulti ) root.forcemulti = true;
   
   row.className = "hop-tree-row-selected";
   root.value = row.value;
   root.selection = row;
   root.selections.push( row );

   if( root.onselect && (!root.multiselect || forcemulti) ) root.onselect();
}
   
/*---------------------------------------------------------------------*/
/*    hop_tree_row_unselect ...                                        */
/*---------------------------------------------------------------------*/
function hop_tree_row_unselect( root, row ) {
   row.className = "hop-tree-row-unselected";
   root.selection = false;
   var i;

   if( root.onunselect ) root.onunselect();
      
   for( i = 0; i < root.selections.length; i++ ) {
      if( root.selections[ i ] == row ) {
	 root.selections.splice( i, 1 );
	 break;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tree_row_select_all ...                                      */
/*---------------------------------------------------------------------*/
function hop_tree_row_set_select_all( tree, select ) {
   var root = hop_tree_root( tree );
   
   var traverse_lr = function( tree ) {
      if( tree.className == "hop-tree-leaf" ) {
	 if( select ) {
	    hop_tree_row_select( root, tree.row, true );
	 } else {
	    hop_tree_row_unselect( root, tree.row );
	 }
      } else {
	 var body = tree.body.childNodes;

	 if( tree.visible ) {
	    if( select ) {
	       hop_tree_row_select( root, tree.row, true );
	    } else {
	       hop_tree_row_unselect( root, tree.row );
	    }
	 }

	 for( var i = 0; i < body.length; i++ ) {
	    traverse_lr( body[ i ] );
	 }
      }
   }
   
   traverse_lr( root );
   root.forcemulti = select;
}

/*---------------------------------------------------------------------*/
/*    hop_tree_row_select_all ...                                      */
/*---------------------------------------------------------------------*/
function hop_tree_row_select_all( tree ) {
   hop_tree_row_set_select_all( tree, true );
}

/*---------------------------------------------------------------------*/
/*    hop_tree_row_toggle_selected ...                                 */
/*---------------------------------------------------------------------*/
function hop_tree_row_toggle_selected( event, tree, row ) {
   var root = hop_tree_root( tree );

   if( (root.forcemulti == true) && !event.shiftKey ) {
      hop_tree_reset( tree );
   }

   if( row.className == "hop-tree-row-selected" ) {
      hop_tree_row_unselect( root, row );
   } else {
      hop_tree_row_select( root, row, event.shiftKey );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tree_row_select_next ...                                     */
/*---------------------------------------------------------------------*/
function hop_tree_row_select_next( tree ) {
   var root = hop_tree_root( tree );

   if( !root ) return;
   
   if( !root.selection && root.visible ) {
      /* nothing is selected, select the first row and exit */
      hop_tree_row_select( root, root.row, false );
      return;
   } else {
      var row = root.selection ? root.selection : root.row;
      
      var traverse_lr = function( tree, stop ) {
	 if( tree.className == "hop-tree-leaf" ) {
	    if( stop == 1 ) {
	       hop_tree_row_select( root, tree.row, false );
	       return -1;
	    } else 
	       return (tree.row == row) ? 1 : 0;
	 } else {
	    var body = tree.body.childNodes;

	    if( stop == 1 ) {
	       if( tree.visible ) hop_tree_row_select( root, tree.row, false );
	       return -1;
	    }

	    if( tree.row == row ) stop = 1;

	    for( var i = 0; i < body.length; i++ ) {
	       stop = traverse_lr( body[ i ], stop );
	       if( stop == - 1 ) return stop;
	    }

	    return stop;
	 }
      }
   
      traverse_lr( root, 0 );
      return;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_tree_row_select_previous ...                                 */
/*---------------------------------------------------------------------*/
function hop_tree_row_select_previous( tree ) {
   var root = hop_tree_root( tree );

   if( !root ) return;
   
   if( !root.selection && root.visible ) {
      /* nothing is selected, select the first row and exit */
      hop_tree_row_select( root, root.row, false );
      return;
   } else {
      var row = root.selection ? root.selection : root.row;
      
      var traverse_rl = function( tree, stop ) {
	 if( tree.className == "hop-tree-leaf" ) {
	    if( stop == 1 ) {
	       hop_tree_row_select( root, tree.row, false );
	       return -1;
	    } else 
	       return (tree.row == row) ? 1 : 0;
	 } else {
	    var body = tree.body.childNodes;

	    for( var i = body.length - 1; i >= 0; i-- ) {
	       stop = traverse_rl( body[ i ], stop );
	       if( stop == - 1 ) return stop;
	    }

	    if( stop == 1 ) {
	       if( tree.visible ) hop_tree_row_select( root, tree.row, false );
	       return -1;
	    }

	    if( tree.row == row ) stop = 1;

	    return stop;
	 }
      }
   
      traverse_rl( root, 0 );
      return;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_make_tree ...                                                */
/*---------------------------------------------------------------------*/
function hop_make_tree( parent, id, visible, level, proc, title,
			openp, delayedp,
			mu, ons, onus, value, history,
			iconopen, iconopenerr,
			iconclose, iconcloseerr,
			icondir ) {
   var tree = document.createElement( "div" );

   /* fixup icons, we enforce that when one icon is missing, none added */
   if( iconopen == true ) {
      iconopen = hop_tree_default_open_icon;
   } else {
      if( iconopen == 0 ) {
	 iconopen = hop_tree_default_device_icon;
      } else {
	 if( iconopen == -1 ) {
	    iconopen = false;
	    iconclose = false;
	 }
      }
   }

   if( iconopen ) {
      if( iconclose == true ) {
	 iconclose = hop_tree_default_close_icon;
      } else {
	 if( iconclose == 0 ) {
	    iconclose = hop_tree_default_device_icon;
	 } else {
	    if( iconclose == -1 ) {
	       iconopen = false;
	       iconclose = false;
	    }
	 }
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
   
   /* the body of the table */
   var tb = document.createElement( "tbody" );
   var row = document.createElement( "tr" );
   row.id = id + "-trrow";
   row.className = "hop-tree-row-unselected";
   row.setAttribute( "align", "left" );
   
   /* build the left vertical lines */
   hop_push_vlines( parent, row, level );

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
   if( iconopen ) {
      var td2 = document.createElement( "td" );
      var folder = document.createElement( "img" );

      folder.src = iconclose;
      folder.className = "hop-tree";
      folder.onerror = function () {
	 if( tree.iconerr ) {
	    var icon = tree.iconerr;
	    tree.iconerr = false;
	    folder.src = icon;
	 }
      }
   
      td2.className = "hop-tree";
      td2.setAttribute( "nowrap", "nowrap" );
      td2.onclick = function( e ) {
	 hop_tree_row_toggle_selected( e == undefined ? event : e, tree, row );
      }
      td2.appendChild( folder );
      row.appendChild( td2 );
   }
   
   /* the title */
   var td3 = document.createElement( "td" );
   td3.className = "hop-tree-title";
   td3.setAttribute( "nowrap", "nowrap" );
   td3.onclick = function( e ) {
      hop_tree_row_toggle_selected( e == undefined ? event : e, tree, row );
   }
   td3.innerHTML = title;
   
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
   body.id = id + "-trbody";
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
   tree.last = true;
   tree.img_join = join;
   tree.img_folder = folder;
   tree.visible = visible;
   tree.iconopen = iconopen;
   tree.iconopenerr = iconopenerr;
   tree.iconclose = iconclose;
   tree.iconcloseerr = iconcloseerr;
   tree.iconerr = iconcloseerr;
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
      tree.selection = false;
      tree.multiselect = mu;
      tree.onselect = ons;
      tree.onunselect = onus;
      tree.forcemulti = false;
   }

   /* populate the tree is not delayed */
   if( delayedp ) {
      /* store the populate procedure and not populate yet */
      tree.proc = proc;
   } else {
      /* non delayed trees have a static body, hence, it is computed */
      /* at creation time and not updated when the tree is unfolded  */
      if( proc ) {
	 tree.proc = proc;
	 tree.proc();
      }
      /* cancel the tree populate procedure because that tree */
      /* is never updated (it is not delayed)                 */
      tree.proc = false;
   } 
   
   /* open the tree if required */
   if( openp ) hop_tree_open( tree );
   
   return tree;
}

/*---------------------------------------------------------------------*/
/*    hop_make_tree_leaf ...                                           */
/*---------------------------------------------------------------------*/
function hop_make_tree_leaf( tree, content, value, icon, iconerr ) {
   var level = tree.level + 1;
   var leaf = document.createElement( "table" );

   leaf.setAttribute( "cellpadding", 0 );
   leaf.setAttribute( "cellspacing", 0 );
   leaf.setAttribute( "border", 0 );
   leaf.className = "hop-tree-leaf";

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

   /* add the icon assiocated with the node, if any */
   if( icon && (icon != -1) ) {
      var td2 = document.createElement( "td" );
      var fimg = document.createElement( "img" );
      fimg.onerror = function () {
	 if( iconerr ) {
	    var ie = iconerr;
	    iconerr = false;
	    fimg.src = ie;
	 }
      }
      
      if( (icon instanceof String) || (typeof icon == "string") ) {
	 fimg.src = icon;
      } else {
	 fimg.src = hop_tree_default_file_icon;
      }
      fimg.className = "hop-tree";
      
      td2.appendChild( fimg );
      td2.className = "hop-tree";
      td2.onclick = function( e ) {
	 hop_tree_row_toggle_selected( e == undefined ? event : e, tree, row );
      }

      row.appendChild( td2 );
   }

   row.value = value;

   /* the content */
   var td3 = document.createElement( "td" );
   td3.className = "hop-tree-title";
   td3.onclick = function( e ) {
      hop_tree_row_toggle_selected( e == undefined ? event : e, tree, row );
   }
   td3.innerHTML= content;
   
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
   if( !tree ) {
      return false;
   } else {
      var sel = tree.selections;
      var len = sel.length;
      var i;
      var res = new Array( len );

      for( i = 0; i < len; i++ ) {
	 res[ i ] = sel[ i ].value;
      }
   
      for( i = 0; i < len; i++ ) {
	 hop_tree_row_unselect( tree, sel[ 0 ] );
      }
   
      return res;
   }
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
   var res = new Array( len );
   var i;

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

	 return true;
      } else {
	 return false;
      }
   }
);
