/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-dom.js                        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat May  6 14:10:27 2006                          */
/*    Last change :  Mon May 28 10:47:58 2007 (serrano)                */
/*    Copyright   :  2006-07 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The DOM component of the HOP runtime library.                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    dom_add_child ...                                                */
/*---------------------------------------------------------------------*/
function dom_add_child( node, e ) {
   if( hop_is_html_element( e ) ) {
      node.appendChild( e );
   } else {
      if( (e instanceof String) || (typeof e == "string") || (typeof e == "number") ) {
	 node.innerHTML = e;
      } else {
	 if( sc_isPair( e ) ) {
	    dom_add_child( node, e.car );
	    dom_add_child( node, e.cdr );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    dom_set_child_node ...                                           */
/*---------------------------------------------------------------------*/
function dom_set_child_node( parent, node ) {
   parent.innerHTML = "";
   parent.appendChild( node );
   return node;
}
   
/*---------------------------------------------------------------------*/
/*    dom_create ...                                                   */
/*---------------------------------------------------------------------*/
function dom_create( tag, args ) {
   var el = document.createElement( tag );
   var l = args.length;
   var i = 0;

   while( i < l ) {
      var k = args[ i ];
      
      if( sci_isKeyword( k ) ) {
	 if( i < (l - 1) ) {
	    var at = args[ i + 1 ];
	    if( (at instanceof String) || (typeof at == "string") ) {
	       if( sc_isSymbol_immutable( at ) ) {
		  el.setAttribute( k.toJSString(),
				   sc_symbol2string_immutable( at ) );
	       } else {
		  el.setAttribute( k.toJSString(), at );
	       }
	    } else {
	       el.setAttribute( k.toJSString(), at + "" );
	    }
	    i += 2;
	 }
      } else {
	 dom_add_child( el, args[ i ] );
	 i++;
      }
   }

   return el;
}

/*---------------------------------------------------------------------*/
/*    dom_add_head_script ...                                          */
/*---------------------------------------------------------------------*/
function dom_add_head_script( pathname, id ) {
   var head = document.getElementsByTagName( "head" )[0];
   var script = document.createElement( 'script' );

   script.type = 'text/javascript';
   script.src = pathname;
   
   if( id != undefined ) script.id = id;
   
   head.appendChild( script );
}

/*---------------------------------------------------------------------*/
/*    DOM creator interface ...                                        */
/*---------------------------------------------------------------------*/
function dom_create_a() {
   return dom_create( "a", arguments );
}
function dom_create_abbr() {
   return dom_create( "abbr", arguments );
}
function dom_create_acronym() {
   return dom_create( "acronym", arguments );
}
function dom_create_applet() {
   return dom_create( "applet", arguments );
}
function dom_create_area() {
   return dom_create( "area", arguments );
}
function dom_create_b() {
   return dom_create( "b", arguments );
}
function dom_create_base() {
   return dom_create( "base", arguments );
}
function dom_create_basefont() {
   return dom_create( "basefont", arguments );
}
function dom_create_bdo() {
   return dom_create( "bdo", arguments );
}
function dom_create_big() {
   return dom_create( "big", arguments );
}
function dom_create_blockquote() {
   return dom_create( "blockquote", arguments );
}
function dom_create_body() {
   return dom_create( "body", arguments );
}
function dom_create_br() {
   return dom_create( "br", arguments );
}
function dom_create_button() {
   return dom_create( "button", arguments );
}
function dom_create_canvas() {
   return dom_create( "canvas", arguments );
}
function dom_create_caption() {
   return dom_create( "caption", arguments );
}
function dom_create_center() {
   return dom_create( "center", arguments );
}
function dom_create_cite() {
   return dom_create( "cite", arguments );
}
function dom_create_code() {
   return dom_create( "code", arguments );
}
function dom_create_col() {
   return dom_create( "col", arguments );
}
function dom_create_colgroup() {
   return dom_create( "colgroup", arguments );
}
function dom_create_dd() {
   return dom_create( "dd", arguments );
}
function dom_create_del() {
   return dom_create( "del", arguments );
}
function dom_create_dfn() {
   return dom_create( "dfn", arguments );
}
function dom_create_dir() {
   return dom_create( "dir", arguments );
}
function dom_create_div() {
   return dom_create( "div", arguments );
}
function dom_create_dl() {
   return dom_create( "dl", arguments );
}
function dom_create_dt() {
   return dom_create( "dt", arguments );
}
function dom_create_em() {
   return dom_create( "em", arguments );
}
function dom_create_fieldset() {
   return dom_create( "fieldset", arguments );
}
function dom_create_font() {
   return dom_create( "font", arguments );
}
function dom_create_form() {
   return dom_create( "form", arguments );
}
function dom_create_frame() {
   return dom_create( "frame", arguments );
}
function dom_create_frameset() {
   return dom_create( "frameset", arguments );
}
function dom_create_h1() {
   return dom_create( "h1", arguments );
}
function dom_create_h2() {
   return dom_create( "h2", arguments );
}
function dom_create_h3() {
   return dom_create( "h3", arguments );
}
function dom_create_h4() {
   return dom_create( "h4", arguments );
}
function dom_create_h5() {
   return dom_create( "h5", arguments );
}
function dom_create_h6() {
   return dom_create( "h6", arguments );
}
function dom_create_hr() {
   return dom_create( "hr", arguments );
}
function dom_create_html() {
   return dom_create( "html", arguments );
}
function dom_create_i() {
   return dom_create( "i", arguments );
}
function dom_create_iframe() {
   return dom_create( "iframe", arguments );
}
function dom_create_input() {
   return dom_create( "input", arguments );
}
function dom_create_ins() {
   return dom_create( "ins", arguments );
}
function dom_create_isindex() {
   return dom_create( "isindex", arguments );
}
function dom_create_kbd() {
   return dom_create( "kbd", arguments );
}
function dom_create_label() {
   return dom_create( "label", arguments );
}
function dom_create_legend() {
   return dom_create( "legend", arguments );
}
function dom_create_li() {
   return dom_create( "li", arguments );
}
function dom_create_link() {
   return dom_create( "link", arguments );
}
function dom_create_map() {
   return dom_create( "map", arguments );
}
function dom_create_marquee() {
   return dom_create( "marquee", arguments );
}
function dom_create_menu() {
   return dom_create( "menu", arguments );
}
function dom_create_meta() {
   return dom_create( "meta", arguments );
}
function dom_create_noframes() {
   return dom_create( "noframes", arguments );
}
function dom_create_noscript() {
   return dom_create( "noscript", arguments );
}
function dom_create_object() {
   return dom_create( "object", arguments );
}
function dom_create_ol() {
   return dom_create( "ol", arguments );
}
function dom_create_optgroup() {
   return dom_create( "optgroup", arguments );
}
function dom_create_option() {
   return dom_create( "option", arguments );
}
function dom_create_p() {
   return dom_create( "p", arguments );
}
function dom_create_param() {
   return dom_create( "param", arguments );
}
function dom_create_pre() {
   return dom_create( "pre", arguments );
}
function dom_create_q() {
   return dom_create( "q", arguments );
}
function dom_create_s() {
   return dom_create( "s", arguments );
}
function dom_create_samp() {
   return dom_create( "samp", arguments );
}
function dom_create_script() {
   return dom_create( "script", arguments );
}
function dom_create_select() {
   return dom_create( "select", arguments );
}
function dom_create_small() {
   return dom_create( "small", arguments );
}
function dom_create_span() {
   return dom_create( "span", arguments );
}
function dom_create_strike() {
   return dom_create( "strike", arguments );
}
function dom_create_strong() {
   return dom_create( "strong", arguments );
}
function dom_create_style() {
   return dom_create( "style", arguments );
}
function dom_create_sub() {
   return dom_create( "sub", arguments );
}
function dom_create_sup() {
   return dom_create( "sup", arguments );
}
function dom_create_table() {
   return dom_create( "table", arguments );
}
function dom_create_tbody() {
   return dom_create( "tbody", arguments );
}
function dom_create_td() {
   return dom_create( "td", arguments );
}
function dom_create_textarea() {
   return dom_create( "textarea", arguments );
}
function dom_create_tfoot() {
   return dom_create( "tfoot", arguments );
}
function dom_create_th() {
   return dom_create( "th", arguments );
}
function dom_create_thead() {
   return dom_create( "thead", arguments );
}
function dom_create_title() {
   return dom_create( "title", arguments );
}
function dom_create_tr() {
   return dom_create( "tr", arguments );
}
function dom_create_tt() {
   return dom_create( "tt", arguments );
}
function dom_create_u() {
   return dom_create( "u", arguments );
}
function dom_create_ul() {
   return dom_create( "ul", arguments );
}
function dom_create_var() {
   return dom_create( "var", arguments );
}
function dom_create_img() {
   return dom_create( "img", arguments );
}
function dom_create_head() {
   return dom_create( "head", arguments );
}

/*---------------------------------------------------------------------*/
/*    Server side constructors                                         */
/*---------------------------------------------------------------------*/
function dom_create_delay() {
   return ( "*** Hop Error, `delay' can only be created on server" );
}
function dom_create_inline() {
   return ( "*** Hop Error, `inline' can only be created on server" );
}
function dom_create_notepad() {
   return ( "*** Hop Error, `notepad' can only be created on server" );
}
function dom_create_nphead() {
   return ( "*** Hop Error, `nphead' can only be created on server" );
}
function dom_create_nptab() {
   return ( "*** Hop Error, `nptab' can only be created on server" );
}
function dom_create_nptabhead() {
   return ( "*** Hop Error, `nptabhead' can only be created on server" );
}
function dom_create_pan() {
   return ( "*** Hop Error, `pan' can only be created on server" );
}
function dom_create_paned() {
   return ( "*** Hop Error, `paned' can only be created on server" );
}
function dom_create_slider() {
   return ( "*** Hop Error, `slider' can only be created on server" );
}
function dom_create_sorttable() {
   return ( "*** Hop Error, `sorttable' can only be created on server" );
}

/*---------------------------------------------------------------------*/
/*    DOM functional interface ...                                     */
/*    -------------------------------------------------------------    */
/*    These definitions should be removed when the alias               */
/*    facilities of scheme2js will support simple re-writting.         */
/*---------------------------------------------------------------------*/
function dom_has_attributes( node ) {
   return node.hasAttributes();
}
function dom_get_attributes( node ) {
   return sci_vector2list( node.getAttributes() );
}
function dom_has_attribute( node, string ) {
   return node.hasAttribute( string );
}
function dom_get_attribute( node, string ) {
   return node.getAttribute( string );
}
function dom_remove_attribute( node, string ) {
   return node.removeAttribute( string );
}
function dom_set_attribute( node, string, value ) {
   return node.setAttribute( string, value );
}
function dom_owner_document( node ) {
   return node.ownerDocument();
}
function dom_has_child_nodes( node ) {
   return node.hasChildNodes();
}
function dom_child_nodes( node ) {
   return sci_vector2list( node.childNodes );
}
function dom_first_child( node ) {
   return node.firstChild;
}
function dom_last_child( node ) {
   return node.lastChild;
}
function dom_next_sibling( node ) {
   return node.nextSibling;
}
function dom_previous_sibling( node ) {
   return node.previousSibling;
}
function dom_node_name( node ) {
   return node.nodeName;
}
function dom_node_type( node ) {
   return node.nodeType;
}
function dom_parent_node( node ) {
   return node.parentNode;
}
function dom_append_child( node, n ) {
   return node.appendChild( n );
}
function dom_remove_child( node, n ) {
   return node.removeChild( n );
}
function dom_clone_node( node, b ) {
   return node.cloneNode( b );
}
function dom_insert_before( node, n, r ) {
   return node.insertBefore( n, r );
}
function dom_replace_child( node, n, r ) {
   return node.replaceChild( n, r );
}
function dom_get_element_by_id( doc, id ) {
   if( (doc instanceof String) || (typeof doc == "string") ) {
      return document.getElementById( doc );
   } else {
      return doc.getElementById( id );
   }
}
function dom_get_elements_by_tag_name( doc, name ) {
   if( (doc instanceof String) || (typeof doc == "string") ) {
      return sci_vector2list( document.getElementsByTagName( doc ) );
   } else {
      return sci_vector2list( doc.getElementsByTagName( name ) );
   }
}

/*---------------------------------------------------------------------*/
/*    dom_get_element_by_class ...                                     */
/*---------------------------------------------------------------------*/
function dom_get_elements_by_class( doc, name ) {
   var res = new Array();
   var n = 0;
   var all, re;
   
   if( (doc instanceof String) || (typeof doc == "string") ) {
      all = document.getElementsByTagName( "*" );
      re = new RegExp( doc + " |" + doc + "$", "g" );
   } else {
      all = doc.getElementsByTagName( "*" );
      re = new RegExp( name + " |" + name + "$", "g" );
   }

   for( var i = 0; i < all.length; i++ ) {
      if( re.exec( all[ i ].className ) ) {
	 res[ n++ ] = all[ i ];
      }
   }
   
   return sci_vector2list( res );
}

document.getElementsByClass = function( className ) {
   var all = document.getElementsByTagName( "*" );
   var res = new Array();
   var n = 0;
   var re = new RegExp( name + " |" + name + "$", "g" );
    
   for( var i = 0; i < all.length; i++ ) {
      if( re.exec( all[ i ].className ) ) {
	 res[ n++ ] = all[ i ];
      }
   }
   
   return res;
}
