/*=====================================================================*/
/*    serrano/prgm/project/hop/2.4.x/share/hop-dom.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat May  6 14:10:27 2006                          */
/*    Last change :  Thu Nov 15 09:45:22 2012 (serrano)                */
/*    Copyright   :  2006-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The DOM component of the HOP runtime library.                    */
/*    -------------------------------------------------------------    */
/*    This assumes that hop-autoconf is already loaded.                */
/*=====================================================================*/

/*** META ((export document) (JS document)) */
/*** META ((export window) (JS window)) */
/*** META ((export hop_create_lframe) (JS hop_create_lframe)) */
/*** META ((export hop_create_lflabel) (JS hop_create_lflabel)) */
/*** META ((export Image) (JS Image)) */
/*** META ((export getElementById) (JS getElementById)) */

/*---------------------------------------------------------------------*/
/*    dom_add_child ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export dom-append-child!) (arity #t)) */
function dom_add_child( node, e ) {
   if( hop_is_html_element( e ) ) {
      /* we no longer need to clone a node, even if it is already  */
      /* in the document because the server side implementation    */
      /* of dom-add-child checks if the node is already in the     */
      /* same tree and if it is, it removes it first               */
      node.appendChild( e );
   } else {
      if( (e instanceof String) ||
	  (typeof e === "string") ||
	  (typeof e === "number") ) {
	 node.appendChild( document.createTextNode( e ) );
      } else {
	 if( sc_isPair( e ) ) {
	    dom_add_child( node, e.car );
	    dom_add_child( node, e.cdr );
	 } else if( typeof e === "boolean" || e == null || e == undefined ) {
	    return;
	 } else {
	    sc_error( "dom_add_child",
		      "illegal child node (" + (typeof e) + ")",
		      e );
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    dom_set_child_node ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export dom-set-child-node!) (arity #t)) */
function dom_set_child_node( parent, node ) {
   var childs = parent.childNodes;

   for( var nc = childs.length - 1; nc >= 0; nc-- )
      parent.removeChild( childs[ nc ] );

   dom_add_child( parent, node );
   return node;
}

/*---------------------------------------------------------------------*/
/*    dom_node_elementp ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-element?)
           (peephole (postfix ".nodeType == 1"))
           (arity #t))
*/
function dom_node_elementp( node ) {
   return node.nodeType == 1;
}

/*---------------------------------------------------------------------*/
/*    dom_node_textp ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-text?)
           (peephole (postfix ".nodeType == 3"))
           (arity #t))
*/
function dom_node_textp( node ) {
   return node.nodeType == 3;
}

/*---------------------------------------------------------------------*/
/*    dom_node_documentp ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-document?)
           (peephole (postfix ".nodeType == 9"))
           (arity #t))
*/
function dom_node_documentp( node ) {
   return node.nodeType == 9;
}

/*---------------------------------------------------------------------*/
/*    dom_node_commentp ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-comment?)
           (peephole (postfix ".nodeType == 8"))
           (arity #t))
*/
function dom_node_commentp( node ) {
   return node.nodeType == 8;
}

/*---------------------------------------------------------------------*/
/*    dom_node_document_fragmentp ...                                  */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-document-fragment?)
           (peephole (postfix ".nodeType == 11"))
           (arity #t))
*/
function dom_node_document_fragmentp( node ) {
   return node.nodeType == 11;
}

/*---------------------------------------------------------------------*/
/*    dom_node_document_attrp ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-attr?)
           (peephole (postfix ".nodeType == 2"))
           (arity #t))
*/
function dom_node_document_attrp( node ) {
   return node.nodeType == 2;
}

/*---------------------------------------------------------------------*/
/*    dom_create ...                                                   */
/*---------------------------------------------------------------------*/
function dom_create( tag, _ ) {
   var el = document.createElement( tag );
   var l = arguments.length;
   var i = 1;

   while( i < l ) {
      var k = arguments[ i ];

      if( sc_isKeyword( k ) ) {
	 if( i < (l - 1) ) {
	    var at = arguments[ i + 1 ];
	    var prop = sc_keyword2jsstring( k );

	    if( prop === "class" ) {
	       el.className = at;
	    } else if( prop === "style" ) {
	       if( hop_config.navigator_family === "msie" ) {
		  el.style.setAttribute( "cssText", at );
	       } else {
		  el.setAttribute( prop, at );
	       }
	    } else {
	       if( (at instanceof String) || (typeof at == "string") ) {
		  if( sc_isSymbol( at ) ) {
		     el.setAttribute( prop, sc_symbol2jsstring( at ) );
		  } else {
		     el.setAttribute( prop, at );
		  }
	       } else {
		  el.setAttribute( prop, at + "" );
		  try {
		     el[ prop ] = at;
		  } catch( _ ) { ; }
	       }
	    }
	    i += 2;
	 } else {
	    var prop = sc_keyword2jsstring( k );
	    el.setAttribute( prop, prop );
	    i++;
	 }
      } else {
	 dom_add_child( el, k );
	 i++;
      }
   }

   return el;
}

/*---------------------------------------------------------------------*/
/*    hop_dom_create_msie_radio ...                                    */
/*    -------------------------------------------------------------    */
/*    MSIE input names are immutable! That is, they cannot be set      */
/*    as other attributes.                                             */
/*---------------------------------------------------------------------*/
function hop_dom_create_msie_radio( name, _ ) {
   arguments[ 0 ] = "<INPUT name='" + name + "'>";
   return dom_create.apply( null, arguments );
}

/*---------------------------------------------------------------------*/
/*    hop_dom_create_custom                                            */
/*---------------------------------------------------------------------*/
/*** META (define-macro (hop_dom_create_custom kons . args)
	     (let loop ((args args)
			(attrs '())
			(body '())
			(listeners '()))
		(cond
		   ((null? args)
		    (let ((v (gensym)))
		       `(let ((,v (,kons (list ,@(reverse! attrs))
		                         (list ,@(reverse! body)))))
			   ,@(map (lambda (listener)
				     `(add-event-listener! ,v ,@listener))
				  listeners)
			   ,v)))
		   ((or (null? (cdr args)) (not (keyword? (car args))))
		    (loop (cdr args) attrs (cons (car args) body) listeners))
		   ((string-prefix? "on" (keyword->string (car args)))
		    (let ((s (keyword->string (car args)))
		          (tmp (gensym)))
		       (loop (cddr args)
			     attrs
			     body
			     (cons (list (substring s 2 (string-length s))
					 `(lambda (event)
					     (let ((,tmp ,(cadr args)))
					        (unless ,tmp
						   (stop-event-propagation event #f))
					       ,tmp)))
				   listeners))))
		   (else
		    (loop (cddr args)
			  (cons* (cadr args) (car args) attrs)
			  body
			  listeners))))) */

/*---------------------------------------------------------------------*/
/*    hop_dom_create                                                   */
/*---------------------------------------------------------------------*/
/*** META (define-macro (hop_dom_create tag . args)
	     (let loop ((args args)
			(attrs '())
			(listeners '()))
		(cond
		   ((null? args)
		    (let ((v (gensym)))
		       `(let ((,v ((@ dom_create _) ,tag ,@(reverse! attrs))))
			   ,@(map (lambda (listener)
				     `(add-event-listener! ,v ,@listener))
				  listeners)
			   ,v)))
		   ((or (null? (cdr args)) (not (keyword? (car args))))
		    (loop (cdr args) (cons (car args) attrs) listeners))
		   ((string-prefix? "on" (keyword->string (car args)))
		    (let ((s (keyword->string (car args)))
   		          (tmp (gensym)))
		       (loop (cddr args)
			     attrs
			     (cons (list (substring s 2 (string-length s))
					 `(lambda (event)
					    (let ((,tmp ,(cadr args)))
					        (unless ,tmp
						   (stop-event-propagation event #f))
						   ,tmp)))
				   listeners))))
		   (else
		    (loop (cddr args)
			  (cons* (cadr args) (car args) attrs)
			  listeners))))) */

/*---------------------------------------------------------------------*/
/*    dom_add_head_script ...                                          */
/*---------------------------------------------------------------------*/
function dom_add_head_script( pathname, id ) {
   var head = document.getElementsByTagName( "head" )[ 0 ];
   var script = document.createElement( 'script' );

   script.type = 'text/javascript';
   script.src = pathname;
   
   if( id != undefined ) script.id = id;
   
   head.appendChild( script );
}

/*---------------------------------------------------------------------*/
/*    hop_create_lframe ...                                            */
/*---------------------------------------------------------------------*/
function hop_create_lframe( attrs, body ) {
   var hc = sc_jsstring2keyword( "hssclass" );
   var bd = dom_create( "div", hc, "hop-lfborder", body );
   return dom_create( "div", hc, "hop-lframe", bd );
}

/*---------------------------------------------------------------------*/
/*    hop_create_lflabel ...                                           */
/*---------------------------------------------------------------------*/
function hop_create_lflabel( attrs, body ) {
   var hc = sc_jsstring2keyword( "hssclass" );
   var ct = dom_create( "span", body );
   return dom_create( "div", hc, "hop-lflabel", ct );
}

/*---------------------------------------------------------------------*/
/*    DOM creator interface ...                                        */
/*---------------------------------------------------------------------*/
/*** META (define-macro (<A> . args)
     `(hop_dom_create "a" ,@args)) */

/*** META (define-macro (<ABBR> . args)
     `(hop_dom_create "abbr" ,@args)) */

/*** META (define-macro (<ACRONYM> . args)
     `(hop_dom_create "acronym" ,@args)) */

/*** META (define-macro (<APPLET> . args)
     `(hop_dom_create "applet" ,@args)) */

/*** META (define-macro (<AREA> . args)
     `(hop_dom_create "area" ,@args)) */

/*** META (define-macro (<B> . args)
     `(hop_dom_create "b" ,@args)) */

/*** META (define-macro (<BASE> . args)
     `(hop_dom_create "base" ,@args)) */

/*** META (define-macro (<BASEFONT> . args)
     `(hop_dom_create "basefont" ,@args)) */

/*** META (define-macro (<BDO> . args)
     `(hop_dom_create "bdo" ,@args)) */

/*** META (define-macro (<BIG> . args)
     `(hop_dom_create "big" ,@args)) */

/*** META (define-macro (<BLOCKQUOTE> . args)
     `(hop_dom_create "blockquote" ,@args)) */

/*** META (define-macro (<BODY> . args)
     `(hop_dom_create "body" ,@args)) */

/*** META (define-macro (<BR> . args)
     `(hop_dom_create "br" ,@args)) */

/*** META (define-macro (<BUTTON> . args)
     `(hop_dom_create "button" ,@args)) */

/*** META (define-macro (<CANVAS> . args)
     `(hop_dom_create "canvas" ,@args)) */

/*** META (define-macro (<CAPTION> . args)
     `(hop_dom_create "caption" ,@args)) */

/*** META (define-macro (<CENTER> . args)
     `(hop_dom_create "center" ,@args)) */

/*** META (define-macro (<CITE> . args)
     `(hop_dom_create "cite" ,@args)) */

/*** META (define-macro (<CODE> . args)
     `(hop_dom_create "code" ,@args)) */

/*** META (define-macro (<COL> . args)
     `(hop_dom_create "col" ,@args)) */

/*** META (define-macro (<COLGROUP> . args)
     `(hop_dom_create "colgroup" ,@args)) */

/*** META (define-macro (<DD> . args)
     `(hop_dom_create "dd" ,@args)) */

/*** META (define-macro (<DEL> . args)
     `(hop_dom_create "del" ,@args)) */

/*** META (define-macro (<DFN> . args)
     `(hop_dom_create "dfn" ,@args)) */

/*** META (define-macro (<DIR> . args)
     `(hop_dom_create "dir" ,@args)) */

/*** META (define-macro (<DIV> . args)
     `(hop_dom_create "div" ,@args)) */

/*** META (define-macro (<DL> . args)
     `(hop_dom_create "dl" ,@args)) */

/*** META (define-macro (<DT> . args)
     `(hop_dom_create "dt" ,@args)) */

/*** META (define-macro (<EM> . args)
     `(hop_dom_create "em" ,@args)) */

/*** META (define-macro (<FIELDSET> . args)
     `(hop_dom_create "fieldset" ,@args)) */

/*** META (define-macro (<FONT> . args)
     `(hop_dom_create "font" ,@args)) */

/*** META (define-macro (<FORM> . args)
     `(hop_dom_create "form" ,@args)) */

/*** META (define-macro (<FRAME> . args)
     `(hop_dom_create "frame" ,@args)) */

/*** META (define-macro (<FRAMESET> . args)
     `(hop_dom_create "frameset" ,@args)) */

/*** META (define-macro (<H1> . args)
     `(hop_dom_create "h1" ,@args)) */

/*** META (define-macro (<H2> . args)
     `(hop_dom_create "h2" ,@args)) */

/*** META (define-macro (<H3> . args)
     `(hop_dom_create "h3" ,@args)) */

/*** META (define-macro (<H4> . args)
     `(hop_dom_create "h4" ,@args)) */

/*** META (define-macro (<H5> . args)
     `(hop_dom_create "h5" ,@args)) */

/*** META (define-macro (<H6> . args)
     `(hop_dom_create "h6" ,@args)) */

/*** META (define-macro (<HR> . args)
     `(hop_dom_create "hr" ,@args)) */

/*** META (define-macro (<HTML> . args)
     `(hop_dom_create "html" ,@args)) */

/*** META (define-macro (<I> . args)
     `(hop_dom_create "i" ,@args)) */

/*** META (define-macro (<IFRAME> . args)
     `(hop_dom_create "iframe" ,@args)) */

/*** META (define-macro (<INPUT> . args)
     (let ((k (memq :type args)))
         (if (and (pair? k) (pair? (cdr k)))
	     (cond
	        ((or (equal? (cadr k) '(quote url)) (equal? (cadr k) "url"))
	         `(hop_dom_create "input" :onkeydown (hop_inputurl_keydown this event)
      		                  ,@args))
	        ((or (equal? (cadr k) '(quote radio)) (equal? (cadr k) "radio"))
		 (let ((n (memq :name args)))
		    (if (and (pair? n) (pair? (cdr n)))
			`(if (string=? hop_config.navigator_family "msie")
			     (hop_dom_create_msie_radio ,(cadr n) ,@args)
			     (hop_dom_create "input" ,@args))
			`(hop_dom_create "input" ,@args))))
		(else
		 `(hop_dom_create "input" ,@args)))
	     `(hop_dom_create "input" ,@args)))) )*/

/*** META (define-macro (<INS> . args)
     `(hop_dom_create "ins" ,@args)) */

/*** META (define-macro (<ISINDEX> . args)
     `(hop_dom_create "isindex" ,@args)) */

/*** META (define-macro (<KBD> . args)
     `(hop_dom_create "kbd" ,@args)) */

/*** META (define-macro (<LABEL> . args)
     `(hop_dom_create "label" ,@args)) */

/*** META (define-macro (<LEGEND> . args)
     `(hop_dom_create "legend" ,@args)) */

/*** META (define-macro (<LI> . args)
     `(hop_dom_create "li" ,@args)) */

/*** META (define-macro (<LINK> . args)
     `(hop_dom_create "link" ,@args)) */

/*** META (define-macro (<MAP> . args)
     `(hop_dom_create "map" ,@args)) */

/*** META (define-macro (<MARQUEE> . args)
     `(hop_dom_create "marquee" ,@args)) */

/*** META (define-macro (<MENU> . args)
     `(hop_dom_create "menu" ,@args)) */

/*** META (define-macro (<META> . args)
     `(hop_dom_create "meta" ,@args)) */

/*** META (define-macro (<NOFRAMES> . args)
     `(hop_dom_create "noframes" ,@args)) */

/*** META (define-macro (<NOSCRIPT> . args)
     `(hop_dom_create "noscript" ,@args)) */

/*** META (define-macro (<OBJECT> . args)
     `(hop_dom_create "object" ,@args)) */

/*** META (define-macro (<OL> . args)
     `(hop_dom_create "ol" ,@args)) */

/*** META (define-macro (<OPTGROUP> . args)
     `(hop_dom_create "optgroup" ,@args)) */

/*** META (define-macro (<OPTION> . args)
     `(hop_dom_create "option" ,@args)) */

/*** META (define-macro (<P> . args)
     `(hop_dom_create "p" ,@args)) */

/*** META (define-macro (<PARAM> . args)
     `(hop_dom_create "param" ,@args)) */

/*** META (define-macro (<PRE> . args)
     `(hop_dom_create "pre" ,@args)) */

/*** META (define-macro (<Q> . args)
     `(hop_dom_create "q" ,@args)) */

/*** META (define-macro (<S> . args)
     `(hop_dom_create "s" ,@args)) */

/*** META (define-macro (<SAMP> . args)
     `(hop_dom_create "samp" ,@args)) */

/*** META (define-macro (<SCRIPT> . args)
     `(hop_dom_create "script" ,@args)) */

/*** META (define-macro (<SELECT> . args)
     `(hop_dom_create "select" ,@args)) */

/*** META (define-macro (<SMALL> . args)
     `(hop_dom_create "small" ,@args)) */

/*** META (define-macro (<SPAN> . args)
     `(hop_dom_create "span" ,@args)) */

/*** META (define-macro (<STRIKE> . args)
     `(hop_dom_create "strike" ,@args)) */

/*** META (define-macro (<STRONG> . args)
     `(hop_dom_create "strong" ,@args)) */

/*** META (define-macro (<STYLE> . args)
     `(hop_dom_create "style" ,@args)) */

/*** META (define-macro (<SUB> . args)
     `(hop_dom_create "sub" ,@args)) */

/*** META (define-macro (<SUP> . args)
     `(hop_dom_create "sup" ,@args)) */

/*** META (define-macro (<TABLE> . args)
     `(hop_dom_create "table" ,@args)) */

/*** META (define-macro (<TBODY> . args)
     `(hop_dom_create "tbody" ,@args)) */

/*** META (define-macro (<TD> . args)
     `(hop_dom_create "td" ,@args)) */

/*** META (define-macro (<TEXTAREA> . args)
     `(hop_dom_create "textarea" ,@args)) */

/*** META (define-macro (<TFOOT> . args)
     `(hop_dom_create "tfoot" ,@args)) */

/*** META (define-macro (<TH> . args)
     `(hop_dom_create "th" ,@args)) */

/*** META (define-macro (<THEAD> . args)
     `(hop_dom_create "thead" ,@args)) */

/*** META (define-macro (<TITLE> . args)
     `(hop_dom_create "title" ,@args)) */

/*** META (define-macro (<TR> . args)
     `(hop_dom_create "tr" ,@args)) */

/*** META (define-macro (<TT> . args)
     `(hop_dom_create "tt" ,@args)) */

/*** META (define-macro (<U> . args)
     `(hop_dom_create "u" ,@args)) */

/*** META (define-macro (<UL> . args)
     `(hop_dom_create "ul" ,@args)) */

/*** META (define-macro (<VAR> . args)
     `(hop_dom_create "var" ,@args)) */

/*** META (define-macro (<IMG> . args)
     `(hop_dom_create "img" ,@args)) */

/*** META (define-macro (<HEAD> . args)
     `(hop_dom_create "head" ,@args)) */

/*** META (define-macro (<LFRAME> . args)
     `(hop_dom_create_custom hop_create_lframe ,@args)) */

/*** META (define-macro (<LFLABEL> . args)
     `(hop_dom_create_custom hop_create_lflabel ,@args)) */

/*** META (define-macro (<SPINBUTTON> . args)
     `(hop_dom_create_custom hop_create_spinbutton ,@args)) */

/*** META (define-macro (<GAUGE> . args)
     `(hop_dom_create_custom hop_create_gauge ,@args)) */

/*---------------------------------------------------------------------*/
/*    Server side constructors                                         */
/* --------------------------------------------------------------------*}
/*** META ((export <DELAY>)) */
function dom_create_delay() {
   return ( "*** Hop Error, `delay' can only be created on server" );
}
/*** META ((export <INLINE>)) */
function dom_create_inline() {
   return ( "*** Hop Error, `inline' can only be created on server" );
}
/*** META ((export <NOTEPAD>)) */
function dom_create_notepad() {
   return ( "*** Hop Error, `notepad' can only be created on server" );
}
/*** META ((export <NPHEAD>)) */
function dom_create_nphead() {
   return ( "*** Hop Error, `nphead' can only be created on server" );
}
/*** META ((export <NPTAB>)) */
function dom_create_nptab() {
   return ( "*** Hop Error, `nptab' can only be created on server" );
}
/*** META ((export <NPTABHEAD>)) */
function dom_create_nptabhead() {
   return ( "*** Hop Error, `nptabhead' can only be created on server" );
}
/*** META ((export <PAN>)) */
function dom_create_pan() {
   return ( "*** Hop Error, `pan' can only be created on server" );
}
/*** META ((export <PANED>)) */
function dom_create_paned() {
   return ( "*** Hop Error, `paned' can only be created on server" );
}
/*** META ((export <SLIDER>)) */
function dom_create_slider() {
   return ( "*** Hop Error, `slider' can only be created on server" );
}
/*** META ((export <SORTTABLE>)) */
function dom_create_sorttable() {
   return ( "*** Hop Error, `sorttable' can only be created on server" );
}
/*** META ((export <SPAGE>)) */
function dom_create_spage() {
   return ( "*** Hop Error, `spage' can only be created on server" );
}
/*** META ((export <SPHEAD>)) */
function dom_create_sphead() {
   return ( "*** Hop Error, `sptab' can only be created on server" );
}
/*** META ((export <SPTAB>)) */
function dom_create_sptab() {
   return ( "*** Hop Error, `sptab' can only be created on server" );
}
/*** META ((export <SPTABHEAD>)) */
function dom_create_sptabhead() {
   return ( "*** Hop Error, `sptabhead' can only be created on server" );
}

/*---------------------------------------------------------------------*/
/*    DOM functional interface ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export dom-add-class!)
           (arity #t)) */
function dom_add_class( obj, string ) {
   if( (obj instanceof String) || (typeof obj === "string") )
      obj = document.getElementById( obj );

   if( obj.className.search( new RegExp( string + "\\b" ) ) < 0 ) {
      obj.className = obj.className + " " + string;
   }
}
	   
/*** META ((export dom-remove-class!)
           (arity #t)) */
function dom_remove_class( obj, string ) {
   if( (obj instanceof String) || (typeof obj === "string") )
      obj = document.getElementById( obj );

   var re = new RegExp( "[ \\t]*" + string + "\\b" );
   obj.className = obj.className.replace( re, "" );
}
	   
/*** META ((export dom-has-attributes?)
           (arity #t)
           (type bool)
           (peephole (postfix ".hasAttributes()")))
*/
function dom_has_attributes( node ) {
   return node.hasAttributes();
}
/*** META ((export #t)
           (arity #t)
           (peephole (hole 1 "sc_vector2list(" node ".getAttributes())")))
*/
function dom_get_attributes( node ) {
   return sc_vector2list( node.getAttributes() );
}
/*** META ((export dom-has-attribute?)
           (arity #t)
           (type bool)
           (peephole (hole 2 node ".hasAttribute(" string ")")))
*/
function dom_has_attribute( node, string ) {
   return node.hasAttribute( string );
}
/*** META ((export #t)
           (arity #t)
           (peephole (hole 2 node ".getAttribute(" string ")")))
*/
function dom_get_attribute( node, string ) {
   return node.getAttribute( string );
}
/*** META ((export dom-remove-attribute!)
           (arity #t)
           (peephole (hole 2 node ".removeAttribute(" string ")")))
*/
function dom_remove_attribute( node, string ) {
   return node.removeAttribute( string );
}
/*** META ((export dom-set-attribute! dom-attribute-set!)
           (arity #t)
           (peephole (hole 3 node ".setAttribute(" string ", " value ")")))
*/
function dom_set_attribute( node, string, value ) {
   return node.setAttribute( string, value );
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".ownerDocument()")))
*/
function dom_owner_document( node ) {
   return node.ownerDocument();
}
/*** META ((export dom-has-child-nodes?)
           (arity #t)
           (type bool)
           (peephole (postfix ".hasChildNodes()")))
*/
function dom_has_child_nodes( node ) {
   return node.hasChildNodes();
}
/*** META ((export #t)
           (arity #t)
           (peephole (hole 1 "sc_vector2list(" node ".childNodes)")))
*/
function dom_child_nodes( node ) {
   return sc_vector2list( node.childNodes );
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".firstChild")))
*/
function dom_first_child( node ) {
   return node.firstChild;
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".lastChild")))
*/
function dom_last_child( node ) {
   return node.lastChild;
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".nextSibling")))
*/
function dom_next_sibling( node ) {
   return node.nextSibling;
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".previousSibling")))
*/
function dom_previous_sibling( node ) {
   return node.previousSibling;
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".nodeName")))
*/
function dom_node_name( node ) {
   return node.nodeName;
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".nodeType")))
*/
function dom_node_type( node ) {
   return node.nodeType;
}
/*** META ((export #t)
           (arity #t)
           (peephole (postfix ".parentNode")))
*/
function dom_parent_node( node ) {
   return node.parentNode;
}
/*** META ((export dom-remove-child!)
           (arity #t)
           (peephole (hole 2 node ".removeChild(" n ")")))
*/
function dom_remove_child( node, n ) {
   return node.removeChild( n );
}
/*** META ((export #t)
           (arity #t)
           (peephole (hole 2 node ".cloneNode(" b ")")))
*/
function dom_clone_node( node, b ) {
   return node.cloneNode( b );
}
/*** META ((export dom-insert-before!)
           (arity #t)
           (peephole (hole 3 node ".insertBefore(" n ", " r ")")))
*/
function dom_insert_before( node, n, r ) {
   return node.insertBefore( n, r );
}
/*** META ((export dom-replace-child!)
           (arity #t)
           (peephole (hole 3 node ".replaceChild(" n ", " r ")")))
*/
function dom_replace_child( node, n, r ) {
   return node.replaceChild( n, r );
}
/*** META ((export #t) (arity -2)) */
function dom_get_element_by_id( doc, id ) {
   if( (doc instanceof String) || (typeof doc === "string") ) {
      var res = document.getElementById( doc );
      if( res == null ) {
	 return false;
      } else {
	 return res;
      }
   } else {
      var res = doc.getElementById( id );
      if( res == null )
	 return false;
      else
	 return res;
   }
}
/*** META ((export #t) (arity -2)) */
function dom_get_elements_by_tag_name( doc, name ) {
   if( (doc instanceof String) || (typeof doc === "string") ) {
      return sc_vector2list( document.getElementsByTagName( doc ) );
   } else {
      return sc_vector2list( doc.getElementsByTagName( name ) );
   }
}

/*** META ((export #t) (arity -2)) */
function dom_get_elements_by_name( doc, name ) {
   if( (doc instanceof String) || (typeof doc === "string") ) {
      return sc_vector2list( document.getElementsByName( doc ) );
   } else {
      return sc_vector2list( doc.getElementsByName( name ) );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_css_add_style_sheet ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export css-add-style-sheet!) (arity #t)) */
function hop_css_add_style_sheet( document, rules ) {
   try {
      var els = document.getElementsByTagName( "head" );
      if( (els != null) && (els[ 0 ].appendChild != undefined) ) {
	 var st = document.createElement( "style" );

	 st.appendChild( document.createTextNode( rules ) );
	 els[ 0 ].appendChild( st );
      }
   } catch( e ) {
      ;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_load_css ...                                                 */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function hop_load_css( url ) {
   try {
      var els = document.getElementsByTagName( "head" );
      if( (els != null) && (els[ 0 ].appendChild != undefined) ) {
	 var st = document.createElement( "link" );

	 if( url.lastIndexOf( ".hss" ) === url.length ) {
	    st.href = url + "?hss";
	 } else {
	    st.href = url;
	 }
	 
	 st.rel = "stylsheet";
	 st.type = "text/css";
	 
	 els[ 0 ].appendChild( st );
      }
   } catch( e ) {
      ;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_load_jscript ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity #t)) */
function hop_load_jscript( url ) {
   try {
      var els = document.getElementsByTagName( "head" );
      if( (els != null) && (els[ 0 ].appendChild != undefined) ) {
	 var sc = document.createElement( "script" );
	 sc.src = url;
	 sc.type = "text/javascript";
	 
	 els[ 0 ].appendChild( sc );
      }
   } catch( e ) {
      ;
   }
}

/*---------------------------------------------------------------------*/
/*    dom_get_elements_by_class ...                                    */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity -2)) */
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
   
   return sc_vector2list( res );
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

/*---------------------------------------------------------------------*/
/*    dom_get_elements_by_attribute ...                                */
/*---------------------------------------------------------------------*/
/*** META ((export #t) (arity -2)) */
function dom_get_elements_by_attribute( doc, name, value ) {
   var res = new Array();
   var n = 0;
   var all;

   if( (doc instanceof String) || (typeof doc == "string") ) {
      all = document.getElementsByTagName( "*" );
      value = name;
      name = sc_isKeyword( doc ) ? sc_keyword2jsstring( doc ) : doc;
   } else {
      all = doc.getElementsByTagName( "*" );

      if( sc_isKeyword( name ) ) name = sc_keyword2jsstring( name );
   }

   for( var i = 0; i < all.length; i++ ) {
      if( all[ i ].getAttribute( name ) == value ) {
	 res[ n++ ] = all[ i ];
      }
   }
   
   return sc_vector2list( res );
}

document.getElementsByAttribute = function( name, value ) {
   var all = document.getElementsByTagName( "*" );
   var res = new Array();
   var n = 0;
    
   if( sc_isKeyword( name ) ) name = sc_keyword2jsstring( name );
   
   for( var i = 0; i < all.length; i++ ) {
      if( all[ i ].getAttribute( name ) == value ) {
	 res[ n++ ] = all[ i ];
      }
   }
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    hop_node_eval ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-eval) (arity #t)) */
function hop_node_eval( node, text ) {
   var res;

   function hop_node_eval_from_text( text ) {
      var res;
      var start_re = /<script[^>]*>/ig;
      var end_re = /<\/script>/i;
      var script;

      while( (script=start_re.exec( text )) != null ) {
	 /* I don't understand why yet, IE 7 does not include */
	 /* SCRIPT nodes in the resulting node!               */
	 var start = script.index + script[0].length;
	 var end = text.indexOf( "<\u002fscript>", start );
	 if( end == -1 ) end = text.indexOf( "<\u002fSCRIPT>", start );
	 if( (end > start) ) {
	    res = eval( text.substr( start, end - start ) );
	 }
      }

      return res;
   }

   /* some browsers (guess who) are supporting getElementsByTagName */
   /* only for the entire document and not for individual nodes.    */
   if( "getElementsByTagName" in node ) {
      var scripts = node.getElementsByTagName( "script" );

      if( scripts && scripts.length > 0 ) {
	 for ( var j = 0; j < scripts.length; j++ ) {
	    res = eval( scripts[ j ].innerHTML );
	 }
      } else {
	 return hop_node_eval_from_text( text );
      }
   } else {
      return hop_node_eval_from_text( text );
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    node_style_get ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export node-style-get node-style) (arity #t)) */
function node_style_get( obj, prop ) {
   if( (obj instanceof String) || (typeof obj === "string") )
      obj = document.getElementById( obj );

   if( sc_isKeyword( prop ) )
      prop = sc_keyword2jsstring( prop );

   if( prop in obj.style ) 
      return obj.style[ prop ];
   else
      return false;
}

/*---------------------------------------------------------------------*/
/*    node_computed_style_get ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export node-computed-style-get node-computed-style)
           (arity #t))
*/
function node_computed_style_get( obj, prop ) {
   var el = obj;
   
   if( (obj instanceof String) || (typeof obj === "string") )
      el = document.getElementById( obj );
   
   if( sc_isKeyword( prop ) )
      prop = sc_keyword2jsstring( prop );

   var t = window.getComputedStyle( el, null );

   if( t != null && (prop in t) )
      return t[ prop ];
   else
      return false;
}

/*---------------------------------------------------------------------*/
/*    hop_start_tag ...                                                */
/*---------------------------------------------------------------------*/
var hop_start_tag = new RegExp( "^<([a-zA-Z]+)" );
var hop_tags_parent = {
   'tr' : 'tbody',
   'td' : 'tr',
   'th' : 'tr',
   'li' : 'ul'
};

/*---------------------------------------------------------------------*/
/*    hop_create_element ...                                           */
/*---------------------------------------------------------------------*/
function hop_create_element( html ) {
   var m = html.match( hop_start_tag );
   var tag;
   
   if( m ) {
      var t = m[ 1 ];
      tag = ( t in hop_tags_parent ) ? hop_tags_parent[ t ] : "div";
   } else {
      tag = "div";
   }

   var el = document.createElement( tag );
   el.innerHTML = "" + html;
   
   if( hop_config.clone_innerHTML ) {
      // As of Feb 2010, webkit based browsers (Feb 2010) requires a deep
      // clone to accept evaluating embedded scripts when the resulting
      // node is inserted in the DOM!
      if( html.search( /<script[ >]/i ) >= 0 )
	 return cloneScriptNode( el.childNodes[ 0 ] );
      else
	 // Remove the node otherwise it has a parentNode set to non-null
	 // which confused functions such as dom_add_child
	 return el.removeChild( el.childNodes[ 0 ] );
   } else {
      // See the remark above for removeChild
      return el.removeChild( el.childNodes[ 0 ] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_create_encoded_element ...                                   */
/*---------------------------------------------------------------------*/
function hop_create_encoded_element( html ) {
   try {
      return hop_create_element( decodeURIComponent( html ) );
   } catch( e ) {
      alert( "*** hop_create_encoded_element, cannot decode: " + html );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_innerHTML_set ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export innerHTML-set!) (arity #t)) */
function hop_innerHTML_set( nid, html ) {
   var el;

   if( (nid instanceof String) || (typeof nid == "string") ) {
      el = document.getElementById( nid );

      if( el == undefined ) {
	 sc_error( "innerHTML-set!", "Cannot find element", nid );
      }
   } else {
      if( !nid ) {
	 sc_error( "innerHTML-set!", "illegal element", nid );
	 return;
      }
      el = nid;
   }

   if( (html instanceof String) || (typeof html == "string") ) {
      el.innerHTML = html;
      if( !hop_config.eval_innerHTML ) hop_node_eval( el, html );
   } else if( hop_is_html_element( html ) || sc_isPair( html ) ) {
      dom_set_child_node( el, html );
   } else {
      el.innerHTML = html;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_style_attribute_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_style_attribute_set( obj, val ) {
   var expr;
   if( (val instanceof String) || (typeof val == "string") )
      expr = eval( val );
   
   for( var p in expr ) {
      node_style_set( obj, p, expr[ p ] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_element_x ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export node-bounding-box-x) (arity 1)) */
function hop_element_x( obj ) {
   if( "getBoundingClientRect" in obj ) {
      return obj.getBoundingClientRect().left + document.body.scrollLeft;
   } else {
      var res = 0;

      while( obj != null ) {
	 if( typeof obj.offsetLeft == "number" )
	    res += obj.offsetLeft;
	 else {
	    break;
	 }
	 obj = obj.offsetParent;
      }

      return res;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_element_y ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export node-bounding-box-y) (arity 1)) */
function hop_element_y( obj ) {
   if( "getBoundingClientRect" in obj ) {
      return obj.getBoundingClientRect().top + document.body.scrollTop;
   } else {
      var res = 0;

      while( obj != null ) {
	 if( typeof obj.offsetTop == "number" )
	    res += obj.offsetTop;
	 else {
	    break;
	 }
	 obj = obj.offsetParent;
      }
      
      return res;
   }
}

/*---------------------------------------------------------------------*/
/*    hop_bounding_box ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export node-bounding-box) (arity -2)) */
function hop_bounding_box( e, m ) {
   n = (e instanceof String) || (typeof e == "string") ?
      document.getElementById( e ) : e;
   
   if( n == undefined ) sc_error( "bounding-box", "illegal node", e );
   if( !m ) m = 0;
   
   if( "getBoundingClientRect" in n ) {
      var rect = n.getBoundingClientRect();

      return { 'left': rect.left - m + document.body.scrollLeft,
	       'top': rect.top - m + document.body.scrollTop,
	       'width': rect.width + (2 * m),
	       'height': rect.height + (2 * m) };
   } else {
      return { 'left': hop_element_x( n ) - m,
	       'top': hop_element_y( n ) - m,
	       'width': n.offsetWidth + (2*m),
	       'height': n.offsetHeight + (2*m) };
   }
}

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_to_list ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export bounding-box->list) (arity #t)) */
function hop_bounding_box_to_list( bbox ) {
   return sc_list( bbox.left, bbox.top, bbox.width, bbox.height );
}

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_x ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export bounding-box-x) (arity -2)) */
function hop_bounding_box_x( bbox, loc ) {
   if( arguments.length == 1 )
      return bbox.left;
   if( (loc == "w") || (loc == "nw") || (loc == "sw") )
      return bbox.left;
   if( (loc == "n") || (loc == "s"))
      return bbox.left + (bbox.width/2);
   if( (loc == "ne") || (loc == "e") || (loc == "se") )
      return bbox.left + bbox.width;
   return 0;
}

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_y ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export bounding-box-y) (arity -2)) */
function hop_bounding_box_y( bbox, loc ) {
   if( arguments.length == 1 )
      return bbox.top;
   if( (loc == "nw") || (loc == "n") || (loc == "ne") )
      return bbox.top;
   if( (loc == "e") || (loc == "w"))
      return bbox.top + (bbox.height/2);
   if( (loc == "se") || (loc == "s") || (loc == "sw") )
      return bbox.top + bbox.height;
   return 0;
}
