/*=====================================================================*/
/*    serrano/prgm/project/hop/1.9.x/share/hop-dom.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat May  6 14:10:27 2006                          */
/*    Last change :  Thu Jun 12 09:30:21 2008 (serrano)                */
/*    Copyright   :  2006-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The DOM component of the HOP runtime library.                    */
/*    -------------------------------------------------------------    */
/*    This assumes that hop-autoconf is already loaded.                */
/*=====================================================================*/

/* {*---------------------------------------------------------------------*} */
/* {*    document ...                                                     *} */
/* {*---------------------------------------------------------------------*} */
/* {*** META ((export #t)) *}                                          */
/* var document;                                                       */

/*---------------------------------------------------------------------*/
/*    dom_add_child ...                                                */
/*---------------------------------------------------------------------*/
function dom_add_child( node, e ) {
   if( hop_is_html_element( e ) ) {
      node.appendChild( e );
   } else {
      if( (e instanceof String) ||
	  (typeof e == "string") ||
	  (typeof e == "number") ) {
	 node.appendChild( document.createTextNode( e ) );
      } else {
	 if( sc_isPair( e ) ) {
	    dom_add_child( node, e.car );
	    dom_add_child( node, e.cdr );
	 } else {
	    if( e ) {
	       alert( "dom_add_child: illegal child node -- " + e );
	    }
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    dom_set_child_node ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export dom-set-child-node!)) */
function dom_set_child_node( parent, node ) {
   var childs = parent.childNodes;

   for( var nc = childs.length - 1; nc >=0; nc-- )
      parent.removeChild( childs[ nc ] );

   dom_add_child( parent, node );
   return node;
}

/*---------------------------------------------------------------------*/
/*    dom_node_elementp ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-element?) (peephole (postfix ".nodeType == 1"))) */
function dom_node_elementp( node ) {
   return node.nodeType == 1;
}

/*---------------------------------------------------------------------*/
/*    dom_node_textp ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-text?)) */
function dom_node_textp( node ) {
   return node.nodeType == 3;
}

/*---------------------------------------------------------------------*/
/*    dom_node_documentp ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-document?)) */
function dom_node_documentp( node ) {
   return node.nodeType == 9;
}

/*---------------------------------------------------------------------*/
/*    dom_node_commentp ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-comment?)) */
function dom_node_commentp( node ) {
   return node.nodeType == 8;
}

/*---------------------------------------------------------------------*/
/*    dom_node_document_fragmentp ...                                  */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-document-fragment?)) */
function dom_node_document_fragmentp( node ) {
   return node.nodeType == 11;
}

/*---------------------------------------------------------------------*/
/*    dom_node_document_attrp ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-attr?)) */
function dom_node_document_attrp( node ) {
   return node.nodeType == 2;
}

/*---------------------------------------------------------------------*/
/*    dom_create ...                                                   */
/*---------------------------------------------------------------------*/
/* we need dom_create (as macros are depending on it. But still call
 * it dom_create and not dom-create */
/*** META ((export dom_create)) */
function dom_create( tag, args ) {
   var el = document.createElement( tag );
   var l = arguments.length;
   var i = 1;

   while( i < l ) {
      var k = arguments[ i ];
      
      if( sc_isKeyword( k ) ) {
	 if( i < (l - 1) ) {
	    var at = arguments[ i + 1 ];
	    var prop = sc_keyword2jsstring( k );

	    if( prop == "class" ) {
	       el.className = at;
	    } else {
	       if( (at instanceof String) || (typeof at == "string") ) {
		  if( sc_isSymbol( at ) ) {
		     el.setAttribute( prop, sc_symbol2jsstring( at ) );
		  } else {
		     el.setAttribute( prop, at );
		  }
	       } else {
		  el.setAttribute( prop, at + "" );
	       }
	    }
	    i += 2;
	 }
      } else {
	 dom_add_child( el, k );
	 i++;
      }
   }

   return el;
}

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
		       `(let ((,v (dom_create ,tag ,@(reverse! attrs))))
			   ,@(map (lambda (listener)
				     `(add-event-listener! ,v ,@listener))
				  listeners)
			   ,v)))
		   ((or (null? (cdr args)) (not (keyword? (car args))))
		    (loop (cdr args) (cons (car args) attrs) listeners))
		   ((string-prefix? "on" (keyword->string (car args)))
		    (let ((s (keyword->string (car args))))
		       (loop (cddr args)
			     attrs
			     (cons (list (substring s 2 (string-length s))
					 `(lambda () ,(cadr args)))
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
         (if (and (pair? k) (pair? (cdr k))
		  (or (eq? (cadr k) 'url))
		  (or (equal? (cadr k) "url")))
	     `(hop_dom_create "input" :onkeydown (hop_inputurl_keydown this event)
			     ,@args)
	     `(hop_dom_create "input" ,@args)))) */

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

/* {*** META ((export <BASE>)) *}                                      */
/* function dom_create_base() {                                        */
/*    return dom_create( "base", arguments );                          */
/* }                                                                   */
/* {*** META ((export <BASEFONT>)) *}                                  */
/* function dom_create_basefont() {                                    */
/*    return dom_create( "basefont", arguments );                      */
/* }                                                                   */
/* {*** META ((export <BDO>)) *}                                       */
/* function dom_create_bdo() {                                         */
/*    return dom_create( "bdo", arguments );                           */
/* }                                                                   */
/* {*** META ((export <BIG>)) *}                                       */
/* function dom_create_big() {                                         */
/*    return dom_create( "big", arguments );                           */
/* }                                                                   */
/* {*** META ((export <BLOCKQUOTE>)) *}                                */
/* function dom_create_blockquote() {                                  */
/*    return dom_create( "blockquote", arguments );                    */
/* }                                                                   */
/* {*** META ((export <BODY>)) *}                                      */
/* function dom_create_body() {                                        */
/*    return dom_create( "body", arguments );                          */
/* }                                                                   */
/* {*** META ((export <BR>)) *}                                        */
/* function dom_create_br() {                                          */
/*    return dom_create( "br", arguments );                            */
/* }                                                                   */
/* {*** META ((export <BUTTON>)) *}                                    */
/* function dom_create_button() {                                      */
/*    return dom_create( "button", arguments );                        */
/* }                                                                   */
/* {*** META ((export <CANVAS>)) *}                                    */
/* function dom_create_canvas() {                                      */
/*    return dom_create( "canvas", arguments );                        */
/* }                                                                   */
/* {*** META ((export <CAPTION>)) *}                                   */
/* function dom_create_caption() {                                     */
/*    return dom_create( "caption", arguments );                       */
/* }                                                                   */
/* {*** META ((export <CENTER>)) *}                                    */
/* function dom_create_center() {                                      */
/*    return dom_create( "center", arguments );                        */
/* }                                                                   */
/* {*** META ((export <CITE>)) *}                                      */
/* function dom_create_cite() {                                        */
/*    return dom_create( "cite", arguments );                          */
/* }                                                                   */
/* {*** META ((export <CODE>)) *}                                      */
/* function dom_create_code() {                                        */
/*    return dom_create( "code", arguments );                          */
/* }                                                                   */
/* {*** META ((export <COL>)) *}                                       */
/* function dom_create_col() {                                         */
/*    return dom_create( "col", arguments );                           */
/* }                                                                   */
/* {*** META ((export <COLGROUP>)) *}                                  */
/* function dom_create_colgroup() {                                    */
/*    return dom_create( "colgroup", arguments );                      */
/* }                                                                   */
/* {*** META ((export <DD>)) *}                                        */
/* function dom_create_dd() {                                          */
/*    return dom_create( "dd", arguments );                            */
/* }                                                                   */
/* {*** META ((export <DEL>)) *}                                       */
/* function dom_create_del() {                                         */
/*    return dom_create( "del", arguments );                           */
/* }                                                                   */
/* {*** META ((export <DFN>)) *}                                       */
/* function dom_create_dfn() {                                         */
/*    return dom_create( "dfn", arguments );                           */
/* }                                                                   */
/* {*** META ((export <DIR>)) *}                                       */
/* function dom_create_dir() {                                         */
/*    return dom_create( "dir", arguments );                           */
/* }                                                                   */
/*                                                                     */
/* function dom_create_div() {                                         */
/*    return dom_create( "div", arguments );                           */
/* }                                                                   */
/* {*** META ((export <DL>)) *}                                        */
/* function dom_create_dl() {                                          */
/*    return dom_create( "dl", arguments );                            */
/* }                                                                   */
/* {*** META ((export <DT>)) *}                                        */
/* function dom_create_dt() {                                          */
/*    return dom_create( "dt", arguments );                            */
/* }                                                                   */
/* {*** META ((export <EM>)) *}                                        */
/* function dom_create_em() {                                          */
/*    return dom_create( "em", arguments );                            */
/* }                                                                   */
/* {*** META ((export <FIELDSET>)) *}                                  */
/* function dom_create_fieldset() {                                    */
/*    return dom_create( "fieldset", arguments );                      */
/* }                                                                   */
/* {*** META ((export <FONT>)) *}                                      */
/* function dom_create_font() {                                        */
/*    return dom_create( "font", arguments );                          */
/* }                                                                   */
/* {*** META ((export <FORM>)) *}                                      */
/* function dom_create_form() {                                        */
/*    return dom_create( "form", arguments );                          */
/* }                                                                   */
/* {*** META ((export <FRAME>)) *}                                     */
/* function dom_create_frame() {                                       */
/*    return dom_create( "frame", arguments );                         */
/* }                                                                   */
/* {*** META ((export <FRAMESET>)) *}                                  */
/* function dom_create_frameset() {                                    */
/*    return dom_create( "frameset", arguments );                      */
/* }                                                                   */
/* {*** META ((export <H1>)) *}                                        */
/* function dom_create_h1() {                                          */
/*    return dom_create( "h1", arguments );                            */
/* }                                                                   */
/* {*** META ((export <H2>)) *}                                        */
/* function dom_create_h2() {                                          */
/*    return dom_create( "h2", arguments );                            */
/* }                                                                   */
/* {*** META ((export <H3>)) *}                                        */
/* function dom_create_h3() {                                          */
/*    return dom_create( "h3", arguments );                            */
/* }                                                                   */
/* {*** META ((export <H4>)) *}                                        */
/* function dom_create_h4() {                                          */
/*    return dom_create( "h4", arguments );                            */
/* }                                                                   */
/* {*** META ((export <H5>)) *}                                        */
/* function dom_create_h5() {                                          */
/*    return dom_create( "h5", arguments );                            */
/* }                                                                   */
/* {*** META ((export <H6>)) *}                                        */
/* function dom_create_h6() {                                          */
/*    return dom_create( "h6", arguments );                            */
/* }                                                                   */
/* {*** META ((export <HR>)) *}                                        */
/* function dom_create_hr() {                                          */
/*    return dom_create( "hr", arguments );                            */
/* }                                                                   */
/* {*** META ((export <HTML>)) *}                                      */
/* function dom_create_html() {                                        */
/*    return dom_create( "html", arguments );                          */
/* }                                                                   */
/* {*** META ((export <I>)) *}                                         */
/* function dom_create_i() {                                           */
/*    return dom_create( "i", arguments );                             */
/* }                                                                   */
/* {*** META ((export <IFRAME>)) *}                                    */
/* function dom_create_iframe() {                                      */
/*    return dom_create( "iframe", arguments );                        */
/* }                                                                   */
/* {*** META ((export <INPUT>)) *}                                     */
/* function dom_create_input() {                                       */
/*    return dom_create( "input", arguments );                         */
/* }                                                                   */
/* {*** META ((export <INS>)) *}                                       */
/* function dom_create_ins() {                                         */
/*    return dom_create( "ins", arguments );                           */
/* }                                                                   */
/* {*** META ((export <ISINDEX>)) *}                                   */
/* function dom_create_isindex() {                                     */
/*    return dom_create( "isindex", arguments );                       */
/* }                                                                   */
/* {*** META ((export <KBD>)) *}                                       */
/* function dom_create_kbd() {                                         */
/*    return dom_create( "kbd", arguments );                           */
/* }                                                                   */
/* {*** META ((export <LABEL>)) *}                                     */
/* function dom_create_label() {                                       */
/*    return dom_create( "label", arguments );                         */
/* }                                                                   */
/* {*** META ((export <LEGEND>)) *}                                    */
/* function dom_create_legend() {                                      */
/*    return dom_create( "legend", arguments );                        */
/* }                                                                   */
/* {*** META ((export <LI>)) *}                                        */
/* function dom_create_li() {                                          */
/*    return dom_create( "li", arguments );                            */
/* }                                                                   */
/* {*** META ((export <LINK>)) *}                                      */
/* function dom_create_link() {                                        */
/*    return dom_create( "link", arguments );                          */
/* }                                                                   */
/* {*** META ((export <MAP>)) *}                                       */
/* function dom_create_map() {                                         */
/*    return dom_create( "map", arguments );                           */
/* }                                                                   */
/* {*** META ((export <MARQUEE>)) *}                                   */
/* function dom_create_marquee() {                                     */
/*    return dom_create( "marquee", arguments );                       */
/* }                                                                   */
/* {*** META ((export <MENU>)) *}                                      */
/* function dom_create_menu() {                                        */
/*    return dom_create( "menu", arguments );                          */
/* }                                                                   */
/* {*** META ((export <META>)) *}                                      */
/* function dom_create_meta() {                                        */
/*    return dom_create( "meta", arguments );                          */
/* }                                                                   */
/* {*** META ((export <NOFRAMES>)) *}                                  */
/* function dom_create_noframes() {                                    */
/*    return dom_create( "noframes", arguments );                      */
/* }                                                                   */
/* {*** META ((export <NOSCRIPT>)) *}                                  */
/* function dom_create_noscript() {                                    */
/*    return dom_create( "noscript", arguments );                      */
/* }                                                                   */
/* {*** META ((export <OBJECT>)) *}                                    */
/* function dom_create_object() {                                      */
/*    return dom_create( "object", arguments );                        */
/* }                                                                   */
/* {*** META ((export <OL>)) *}                                        */
/* function dom_create_ol() {                                          */
/*    return dom_create( "ol", arguments );                            */
/* }                                                                   */
/* {*** META ((export <OPTGROUP>)) *}                                  */
/* function dom_create_optgroup() {                                    */
/*    return dom_create( "optgroup", arguments );                      */
/* }                                                                   */
/* {*** META ((export <OPTION>)) *}                                    */
/* function dom_create_option() {                                      */
/*    return dom_create( "option", arguments );                        */
/* }                                                                   */
/* {*** META ((export <P>)) *}                                         */
/* function dom_create_p() {                                           */
/*    return dom_create( "p", arguments );                             */
/* }                                                                   */
/* {*** META ((export <PARAM>)) *}                                     */
/* function dom_create_param() {                                       */
/*    return dom_create( "param", arguments );                         */
/* }                                                                   */
/* {*** META ((export <PRE>)) *}                                       */
/* function dom_create_pre() {                                         */
/*    return dom_create( "pre", arguments );                           */
/* }                                                                   */
/* {*** META ((export <Q>)) *}                                         */
/* function dom_create_q() {                                           */
/*    return dom_create( "q", arguments );                             */
/* }                                                                   */
/* {*** META ((export <S>)) *}                                         */
/* function dom_create_s() {                                           */
/*    return dom_create( "s", arguments );                             */
/* }                                                                   */
/* {*** META ((export <SAMP>)) *}                                      */
/* function dom_create_samp() {                                        */
/*    return dom_create( "samp", arguments );                          */
/* }                                                                   */
/* {*** META ((export <SCRIPT>)) *}                                    */
/* function dom_create_script() {                                      */
/*    return dom_create( "script", arguments );                        */
/* }                                                                   */
/* {*** META ((export <SELECT>)) *}                                    */
/* function dom_create_select() {                                      */
/*    return dom_create( "select", arguments );                        */
/* }                                                                   */
/* {*** META ((export <SMALL>)) *}                                     */
/* function dom_create_small() {                                       */
/*    return dom_create( "small", arguments );                         */
/* }                                                                   */
/* {*** META ((export <SPAN>)) *}                                      */
/* function dom_create_span() {                                        */
/*    return dom_create( "span", arguments );                          */
/* }                                                                   */
/* {*** META ((export <STRIKE>)) *}                                    */
/* function dom_create_strike() {                                      */
/*    return dom_create( "strike", arguments );                        */
/* }                                                                   */
/* {*** META ((export <STRONG>)) *}                                    */
/* function dom_create_strong() {                                      */
/*    return dom_create( "strong", arguments );                        */
/* }                                                                   */
/* {*** META ((export <STYLE>)) *}                                     */
/* function dom_create_style() {                                       */
/*    return dom_create( "style", arguments );                         */
/* }                                                                   */
/* {*** META ((export <SUB>)) *}                                       */
/* function dom_create_sub() {                                         */
/*    return dom_create( "sub", arguments );                           */
/* }                                                                   */
/* {*** META ((export <SUP>)) *}                                       */
/* function dom_create_sup() {                                         */
/*    return dom_create( "sup", arguments );                           */
/* }                                                                   */
/* {*** META ((export <TABLE>)) *}                                     */
/* function dom_create_table() {                                       */
/*    return dom_create( "table", arguments );                         */
/* }                                                                   */
/* {*** META ((export <TBODY>)) *}                                     */
/* function dom_create_tbody() {                                       */
/*    return dom_create( "tbody", arguments );                         */
/* }                                                                   */
/* {*** META ((export <TD>)) *}                                        */
/* function dom_create_td() {                                          */
/*    return dom_create( "td", arguments );                            */
/* }                                                                   */
/* {*** META ((export <TEXTAREA>)) *}                                  */
/* function dom_create_textarea() {                                    */
/*    return dom_create( "textarea", arguments );                      */
/* }                                                                   */
/* {*** META ((export <TFOOT>)) *}                                     */
/* function dom_create_tfoot() {                                       */
/*    return dom_create( "tfoot", arguments );                         */
/* }                                                                   */
/* {*** META ((export <TH>)) *}                                        */
/* function dom_create_th() {                                          */
/*    return dom_create( "th", arguments );                            */
/* }                                                                   */
/* {*** META ((export <THEAD>)) *}                                     */
/* function dom_create_thead() {                                       */
/*    return dom_create( "thead", arguments );                         */
/* }                                                                   */
/* {*** META ((export <TITLE>)) *}                                     */
/* function dom_create_title() {                                       */
/*    return dom_create( "title", arguments );                         */
/* }                                                                   */
/* {*** META ((export <TR>)) *}                                        */
/* function dom_create_tr() {                                          */
/*    return dom_create( "tr", arguments );                            */
/* }                                                                   */
/* {*** META ((export <TT>)) *}                                        */
/* function dom_create_tt() {                                          */
/*    return dom_create( "tt", arguments );                            */
/* }                                                                   */
/* {*** META ((export <U>)) *}                                         */
/* function dom_create_u() {                                           */
/*    return dom_create( "u", arguments );                             */
/* }                                                                   */
/* {*** META ((export <UL>)) *}                                        */
/* function dom_create_ul() {                                          */
/*    return dom_create( "ul", arguments );                            */
/* }                                                                   */
/* {*** META ((export <VAR>)) *}                                       */
/* function dom_create_var() {                                         */
/*    return dom_create( "var", arguments );                           */
/* }                                                                   */
/* {*** META ((export <IMG>)) *}                                       */
/* function dom_create_img() {                                         */
/*    return dom_create( "img", arguments );                           */
/* }                                                                   */
/* {*** META ((export <HEAD>)) *}                                      */
/* function dom_create_head() {                                        */
/*    return dom_create( "head", arguments );                          */
/* }                                                                   */

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

/*---------------------------------------------------------------------*/
/*    DOM functional interface ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export dom-has-attributes?)
           (type bool)
           (peephole (postfix ".hasAttributes()")))
*/
function dom_has_attributes( node ) {
   return node.hasAttributes();
}
/*** META ((export #t)
           (peephole (hole 1 "sc_vector2list(" node ".getAttributes())")))
*/
function dom_get_attributes( node ) {
   return sc_vector2list( node.getAttributes() );
}
/*** META ((export dom-has-attribute?)
           (type bool)
           (peephole (hole 2 node ".hasAttribute(" string ")")))
*/
function dom_has_attribute( node, string ) {
   return node.hasAttribute( string );
}
/*** META ((export #t)
           (peephole (hole 2 node ".getAttribute(" string ")")))
*/
function dom_get_attribute( node, string ) {
   return node.getAttribute( string );
}
/*** META ((export dom-remove-attribute!)
           (peephole (hole 2 node ".removeAttribute(" string ")")))
*/
function dom_remove_attribute( node, string ) {
   return node.removeAttribute( string );
}
/*** META ((export dom-set-attribute! dom-attribute-set!)
           (peephole (hole 3 node ".setAttribute(" string ", " value ")")))
*/
function dom_set_attribute( node, string, value ) {
   return node.setAttribute( string, value );
}
/*** META ((export #t)
           (peephole (postfix ".ownerDocument()")))
*/
function dom_owner_document( node ) {
   return node.ownerDocument();
}
/*** META ((export dom-has-child-nodes?)
           (type bool)
           (peephole (postfix ".hasChildNodes()")))
*/
function dom_has_child_nodes( node ) {
   return node.hasChildNodes();
}
/*** META ((export #t)
           (peephole (hole 1 "sc_vector2list(" node ".childNodes)")))
*/
function dom_child_nodes( node ) {
   return sc_vector2list( node.childNodes );
}
/*** META ((export #t)
           (peephole (postfix ".firstChild")))
*/
function dom_first_child( node ) {
   return node.firstChild;
}
/*** META ((export #t)
           (peephole (postfix ".lastChild")))
*/
function dom_last_child( node ) {
   return node.lastChild;
}
/*** META ((export #t)
           (peephole (postfix ".nextSibling")))
*/
function dom_next_sibling( node ) {
   return node.nextSibling;
}
/*** META ((export #t)
           (peephole (postfix ".previousSibling")))
*/
function dom_previous_sibling( node ) {
   return node.previousSibling;
}
/*** META ((export #t)
           (peephole (postfix ".nodeName")))
*/
function dom_node_name( node ) {
   return node.nodeName;
}
/*** META ((export #t)
           (peephole (postfix ".nodeType")))
*/
function dom_node_type( node ) {
   return node.nodeType;
}
/*** META ((export #t)
           (peephole (postfix ".parentNode")))
*/
function dom_parent_node( node ) {
   return node.parentNode;
}
/*** META ((export dom-append-child!)) */
function dom_append_child( node, n ) {
   if( (n instanceof String) ||
       (typeof n == "string") ||
       (typeof n == "number") ) {
      return node.appendChild( document.createTextNode( n ) );
   } else {
      return node.appendChild( n );
   }
}
/*** META ((export dom-remove-child!)
           (peephole (hole 2 node ".removeChild(" n ")")))
*/
function dom_remove_child( node, n ) {
   return node.removeChild( n );
}
/*** META ((export #t)
           (peephole (hole 2 node ".cloneNode(" b ")")))
*/
function dom_clone_node( node, b ) {
   return node.cloneNode( b );
}
/*** META ((export dom-insert-before!)
           (peephole (hole 3 node ".insertBefore(" n ", " r ")")))
*/
function dom_insert_before( node, n, r ) {
   return node.insertBefore( n, r );
}
/*** META ((export dom-replace-child!)
           (peephole (hole 3 node ".replaceChild(" n ", " r ")")))
*/
function dom_replace_child( node, n, r ) {
   return node.replaceChild( n, r );
}
/*** META ((export #t)) */
function dom_get_element_by_id( doc, id ) {
   if( (doc instanceof String) || (typeof doc == "string") ) {
      var res = document.getElementById( doc );
      if( res == null ) {
	 return false;
      }
      else
	 return res;
   } else {
      var res = doc.getElementById( id );
      if( res == null )
	 return false;
      else
	 return res;
   }
}
/*** META ((export #t)) */
function dom_get_elements_by_tag_name( doc, name ) {
   if( (doc instanceof String) || (typeof doc == "string") ) {
      return sc_vector2list( document.getElementsByTagName( doc ) );
   } else {
      return sc_vector2list( doc.getElementsByTagName( name ) );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_css_add_style_sheet ...                                      */
/*---------------------------------------------------------------------*/
/*** META ((export css-add-style-sheet!)) */
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
/*    dom_get_element_by_class ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export #t)) */
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
/*    hop_node_eval ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export dom-node-eval)) */
function hop_node_eval( node, text ) {
   var res;
   var scripts = node.getElementsByTagName( "script" );

   function hop_node_eval_from_text( text ) {
      var res;
      var start_re = /<script[^>]*>/ig;
      var end_re = /<\/script>/i;
      var script;

      while( (script=start_re.exec( text )) != null ) {
	 /* I don't understand why yet, IE 7 does not include */
	 /* SCRIPT nodes in the resulting node!               */
	 var start = script.index + script[0].length;
	 var end = text.indexOf( "</script>", start );
	 if( end == -1 ) end = text.indexOf( "</SCRIPT>", start );
	 if( (end > start) ) {
	    res = eval( text.substr( start, end - start ) );
	 }
      }

      return res;
   }

   try {
      if( scripts.length > 0 ) {
	 for ( var j = 0; j < scripts.length; j++ ) {
	    if( false && scripts[ j ].childNodes.length > 0 ) {
	       res = eval( scripts[ j ].childNodes[ 0 ].nodeValue );
	    } else {
	       /* this is a buggy browser (Opera 8?) that does not */
	       /* correctly implement script nodes                 */
	       res = eval( scripts[ j ].innerHTML );
	    }
	 }
      } else {
	 return hop_node_eval_from_text( text );
      }
   } catch( e ) {
      alert( e );
      throw e;
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    node_style_get ...                                               */
/*---------------------------------------------------------------------*/
/*** META ((export node-style-get node-style))
*/
function node_style_get( obj, prop ) {
   if( (obj instanceof String) || (typeof obj === "string") )
      obj = document.getElementById( obj );
   
   return obj.style[ prop ];
}

/*---------------------------------------------------------------------*/
/*    hop_create_element ...                                           */
/*    -------------------------------------------------------------    */
/*    For a reason that I don't understand (a bug?) IE7 refuses to     */
/*    create elements whose body is composed of a single SCRIPT        */
/*    node. To prevent this but, this function always append an        */
/*    extra node that is then ignored when returning!                  */
/*---------------------------------------------------------------------*/
function hop_create_element( html ) {
   var div = document.createElement( 'div' );

   div.innerHTML = "<span>IE7 bug</span>" + html;
   return div.childNodes[ 1 ];
}

/*---------------------------------------------------------------------*/
/*    hop_create_encoded_element ...                                   */
/*---------------------------------------------------------------------*/
function hop_create_encoded_element( html ) {
   return hop_create_element( decodeURIComponent( html ) );
}

/*---------------------------------------------------------------------*/
/*    hop_innerHTML_set ...                                            */
/*---------------------------------------------------------------------*/
/*** META ((export innerHTML-set!)) */
function hop_innerHTML_set( nid, html ) {
   var el;

   if( (nid instanceof String) || (typeof nid == "string") ) {
      el = document.getElementById( nid );

      if( el == undefined ) {
	 alert( "*** ERROR:innerHTML-set! -- cannot find element \""
		+ nid + "\"");
	 return;
      }
   } else {
      if( !nid ) {
	 alert( "*** ERROR:innerHTML-set! -- illegal element \"" + nid + "\"");
	 return;
      }
      el = nid;
   }

   if( (html instanceof String) || (typeof html == "string") ) {
      el.innerHTML = html;
      hop_node_eval( el, html );
   } else if( hop_is_html_element( html ) || sc_isPair( html ) ) {
      dom_set_child_node( el, html );
      if( hop_innerHTML_need_evalp ) hop_node_eval( el, html );
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
function hop_element_x( obj ) {
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

/*---------------------------------------------------------------------*/
/*    hop_element_y ...                                                */
/*---------------------------------------------------------------------*/
function hop_element_y( obj ) {
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

/*---------------------------------------------------------------------*/
/*    hop_bounding_box ...                                             */
/*---------------------------------------------------------------------*/
/*** META ((export node-bounding-box)) */
function hop_bounding_box( e, m ) {
   var n;
   
   if( (e instanceof String) || (typeof e == "string") ) {
      n = document.getElementById( e );
   } else {
      n = e;
   }

   if( !m ) m = 0;
   
   return [ hop_element_x( n ) - m, hop_element_y( n ) - m,
	    n.offsetWidth + (2*m), n.offsetHeight + (2*m) ];
}

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_to_list ...                                     */
/*---------------------------------------------------------------------*/
/*** META ((export bounding-box->list)) */
function hop_bounding_box_to_list( bbox ) {
   return sc_vector2list( bbox );
}

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_x ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export bounding-box-x)) */
function hop_bounding_box_x( bbox, loc ) {
   if( arguments.length == 1 )
      return bbox[ 0 ];
   if( (loc == "w") || (loc == "nw") || (loc == "sw") )
      return bbox[ 0 ];
   if( (loc == "n") || (loc == "s"))
      return bbox[ 0 ] + (bbox[ 2 ]/2);
   if( (loc == "ne") || (loc == "e") || (loc == "se") )
      return bbox[ 0 ] + bbox[ 2 ];
   return 0;
}

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_y ...                                           */
/*---------------------------------------------------------------------*/
/*** META ((export bounding-box-y)) */
function hop_bounding_box_y( bbox, loc ) {
   if( arguments.length == 1 )
      return bbox[ 1 ];
   if( (loc == "nw") || (loc == "n") || (loc == "ne") )
      return bbox[ 1 ];
   if( (loc == "e") || (loc == "w"))
      return bbox[ 1 ] + (bbox[ 3 ]/2);
   if( (loc == "se") || (loc == "s") || (loc == "sw") )
      return bbox[ 1 ] + bbox[ 3 ];
   return 0;
}

