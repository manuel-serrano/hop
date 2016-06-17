/*=====================================================================*/
/*    serrano/prgm/project/hop/3.1.x/share/hop-dom.js                  */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sat May  6 14:10:27 2006                          */
/*    Last change :  Sat Jun 25 08:12:37 2016 (serrano)                */
/*    Copyright   :  2006-16 Manuel Serrano                            */
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
/*    hop_tilde ...                                                    */
/*---------------------------------------------------------------------*/
function hop_tilde( fun ) {
   this.fun = fun;
}

/*---------------------------------------------------------------------*/
/*    hop_add ...                                                      */
/*---------------------------------------------------------------------*/
function hop_add( id, e, insert ) {
   var node;
   
   if( (id instanceof String) || (typeof id == "string") ) {
      node = document.getElementById( id );
   } else {
      node = id;
   }

   if( node == null || node == undefined ) {
      sc_error( "dom-append-child!", "illegal node", id );
   }

   function add( e ) {
      if( (e instanceof Node) || hop_is_html_element( e ) ) {
	 /* we no longer need to clone a node, even if it is already  */
	 /* in the document because the server side implementation    */
	 /* of dom-add-child checks if the node is already in the     */
	 /* same tree and if it is, it removes it first               */
	 insert( node, e );
      } else {
	 if( (e instanceof String) ||
	     (typeof e === "string") ||
	     (typeof e === "number") ) {
	    insert( node, document.createTextNode( e ) );
	 } else if( e instanceof hop_tilde ) {
	    var sc = document.createElement( "script" );
	    var src = "(" + e.fun + ")()";
	    sc.type = "text/javascript";
	    if( "text" in sc ) {
	       sc.text = src;
	    } else {
	       sc.appendChild( src );
	    }
	    insert( node, sc );
	 } else {
	    if( sc_isPair( e ) ) {
	       sc_forEach( add, e );
	    } else if( sc_isVector( e ) ) {
	       e.forEach( add );
	    } else if( typeof e === "boolean" || e == null || e == undefined ) {
	       return;
	    } else {
	       sc_error( "dom-append-child!",
			 "illegal child node (" + (typeof e) + ")",
			 e );
	    }
	 }
      }
   }

   return add( e );
}

/*---------------------------------------------------------------------*/
/*    dom_add_child ...                                                */
/*---------------------------------------------------------------------*/
/*** META ((export dom-append-child!) (arity #t)) */
function dom_add_child( id, e ) {
   return hop_add( id, e, function( node, e ) { return node.appendChild( e ); } );
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
#if HOP_SCHEME
/*** META ((export dom-node-element?)
           (peephole (postfix ".nodeType == 1"))
           (arity #t))
*/
function dom_node_elementp( node ) {
   return node.nodeType == 1;
}
#endif

/*---------------------------------------------------------------------*/
/*    dom_node_textp ...                                               */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export dom-node-text?)
           (peephole (postfix ".nodeType == 3"))
           (arity #t))
*/
function dom_node_textp( node ) {
   return node.nodeType == 3;
}
#endif

/*---------------------------------------------------------------------*/
/*    dom_node_documentp ...                                           */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export dom-node-document?)
           (peephole (postfix ".nodeType == 9"))
           (arity #t))
*/
function dom_node_documentp( node ) {
   return node.nodeType == 9;
}
#endif

/*---------------------------------------------------------------------*/
/*    dom_node_commentp ...                                            */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export dom-node-comment?)
           (peephole (postfix ".nodeType == 8"))
           (arity #t))
*/
function dom_node_commentp( node ) {
   return node.nodeType == 8;
}
#endif

/*---------------------------------------------------------------------*/
/*    dom_node_document_fragmentp ...                                  */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export dom-node-document-fragment?)
           (peephole (postfix ".nodeType == 11"))
           (arity #t))
*/
function dom_node_document_fragmentp( node ) {
   return node.nodeType == 11;
}
#endif

/*---------------------------------------------------------------------*/
/*    dom_node_document_attrp ...                                      */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export dom-node-attr?)
           (peephole (postfix ".nodeType == 2"))
           (arity #t))
*/
function dom_node_document_attrp( node ) {
   return node.nodeType == 2;
}
#endif

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
		          (tmp (gensym))
			  (tilde (gensym)))
		       (loop (cddr args)
			     attrs
			     body
			     (cons (list (substring s 2 (string-length s))
					 `(lambda (event)
					     (let* ((,tilde ,(cadr args))
					            (,tmp (js-call this (js-ref ,tilde "fun"))))
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
   		          (tmp (gensym))
			  (tilde (gensym)))
		       (loop (cddr args)
			     attrs
			     (cons (list (substring s 2 (string-length s))
					 `(lambda (event)
					    (let* ((,tilde ,(cadr args))
					           (,tmp (js-call this (js-ref ,tilde "fun"))))
					        (unless ,tmp
						   (stop-event-propagation event #f))
						   ,tmp)))
				   listeners))))
		   (else
		    (loop (cddr args)
			  (cons* (cadr args) (car args) attrs)
			  listeners))))) */

function hop_dom_create( tag, args ) {
   var el = document.createElement( tag );
   var attrs = args[ 0 ];
   var len = args.length
   var m;

   function valstr( val, k ) {
      if( (val instanceof String) || (typeof val == "string") ) {
	 return el[ attr ] = val;
      } if( typeof( val ) === "function" ) {
	 return window.hop.reactAttribute( function() { k( val() ) } );
      } else {
	 return el[ attr ] = val.toString();
      }
   }
	 
   // the attributes
   if( args[ 0 ] instanceof Object ) {
      for( var attr in args[ 0 ] ) {
	 var val = args[ 0 ][ attr ];
	 
	 if( attr === "class" ) {
	    valstr( val, function( v ) { el.className = v } );
	 } else if( attr === "style" ) {
	    valstr( val, function( v ) { hop_style_attribute_set( el, v ); } );
	 } else if( attr === "%location" ) {
	    ; /* ignore */
	 } else {
	    m = attr.match( "^on(.*)" );
	    if( m ) {
	       hop_add_event_listener( el, m[ 1 ], val, true );
	    } else {
	       valstr( val, function( v ) { el.setAttribute( attr, v ) } );
	       try {
		  valstr( val, function( v ) { el[ attr ] = v } );
	       } catch( _ ) { ; }
	    }
	 }
      }
   }
   
   // the children
   for( var i = 1; i < len; i++ ) {
      dom_add_child( el, args[ i ] );
   }

   return el;
}

/*---------------------------------------------------------------------*/
/*    dom_add_head_script ...                                          */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
function dom_add_head_script( pathname, id ) {
   var head = document.getElementsByTagName( "head" )[ 0 ];
   var script = document.createElement( 'script' );

   script.type = 'text/javascript';
   script.src = pathname;
   
   if( id != undefined ) script.id = id;
   
   head.appendChild( script );
}
#endif

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
/*    <TILDE> ...                                                      */
/*---------------------------------------------------------------------*/
/*** META (define-macro (<TILDE> arg)
	     ;; see the file hopscheme/tilde.scm the two expansions
	     ;; must be compatible
	     (match-case arg
		((let* ?bindings (vector (quote ?expr) . ?-))
		 `(let* ,(map (lambda (b)
				 (cons (symbol-append '$ (car b)) (cdr b)))
			    bindings)
		     (js-new (@ hop_tilde js) (lambda () ,expr))))
		(else
		 (error "hop" "Illegal tilde format" `(<TILDE> ,arg)))))
*/

/*---------------------------------------------------------------------*/
/*    DOM creator interface ...                                        */
/*---------------------------------------------------------------------*/
/*** META (define-macro (<A> . args)
     `(hop_dom_create "a" ,@args)) */

function A( attrs ) {
   return hop_dom_create( "a", arguments );
}

/*** META (define-macro (<ABBR> . args)
     `(hop_dom_create "abbr" ,@args)) */

function ABBR( attrs ) {
   return hop_dom_create( "abbr", arguments );
}

/*** META (define-macro (<ACRONYM> . args)
     `(hop_dom_create "acronym" ,@args)) */

function ACRONYM( attrs ) {
   return hop_dom_create( "acronym", arguments );
}

/*** META (define-macro (<APPLET> . args)
     `(hop_dom_create "applet" ,@args)) */

function APPLET( attrs ) {
   return hop_dom_create( "applet", arguments );
}

/*** META (define-macro (<AREA> . args)
     `(hop_dom_create "area" ,@args)) */

function AREA( attrs ) {
   return hop_dom_create( "area", arguments );
}

/*** META (define-macro (<B> . args)
     `(hop_dom_create "b" ,@args)) */

function B( attrs ) {
   return hop_dom_create( "b", arguments );
}

/*** META (define-macro (<BASE> . args)
     `(hop_dom_create "base" ,@args)) */

function BASE( attrs ) {
   return hop_dom_create( "base", arguments );
}

/*** META (define-macro (<BASEFONT> . args)
     `(hop_dom_create "basefont" ,@args)) */

function BASEFONT( attrs ) {
   return hop_dom_create( "basefont", arguments );
}

/*** META (define-macro (<BDO> . args)
     `(hop_dom_create "bdo" ,@args)) */

function BDO( attrs ) {
   return hop_dom_create( "bdo", arguments );
}

/*** META (define-macro (<BIG> . args)
     `(hop_dom_create "big" ,@args)) */

function BIG( attrs ) {
   return hop_dom_create( "big", arguments );
}

/*** META (define-macro (<BLOCKQUOTE> . args)
     `(hop_dom_create "blockquote" ,@args)) */

function BLOCKQUOTE( attrs ) {
   return hop_dom_create( "blockquote", arguments );
}

/*** META (define-macro (<BODY> . args)
     `(hop_dom_create "body" ,@args)) */

function BODY( attrs ) {
   return hop_dom_create( "body", arguments );
}

/*** META (define-macro (<BR> . args)
     `(hop_dom_create "br" ,@args)) */

function BR( attrs ) {
   return hop_dom_create( "br", arguments );
}

/*** META (define-macro (<BUTTON> . args)
     `(hop_dom_create "button" ,@args)) */

function BUTTON( attrs ) {
   return hop_dom_create( "button", arguments );
}

/*** META (define-macro (<CANVAS> . args)
     `(hop_dom_create "canvas" ,@args)) */

function CANVAS( attrs ) {
   return hop_dom_create( "canvas", arguments );
}

/*** META (define-macro (<CAPTION> . args)
     `(hop_dom_create "caption" ,@args)) */

function CAPTION( attrs ) {
   return hop_dom_create( "caption", arguments );
}

/*** META (define-macro (<CENTER> . args)
     `(hop_dom_create "center" ,@args)) */

function CENTER( attrs ) {
   return hop_dom_create( "center", arguments );
}

/*** META (define-macro (<CITE> . args)
     `(hop_dom_create "cite" ,@args)) */

function CITE( attrs ) {
   return hop_dom_create( "cite", arguments );
}

/*** META (define-macro (<CODE> . args)
     `(hop_dom_create "code" ,@args)) */

function CODE( attrs ) {
   return hop_dom_create( "code", arguments );
}

/*** META (define-macro (<COL> . args)
     `(hop_dom_create "col" ,@args)) */

function COL( attrs ) {
   return hop_dom_create( "col", arguments );
}

/*** META (define-macro (<COLGROUP> . args)
     `(hop_dom_create "colgroup" ,@args)) */

function COLGROUP( attrs ) {
   return hop_dom_create( "colgroup", arguments );
}

/*** META (define-macro (<DD> . args)
     `(hop_dom_create "dd" ,@args)) */

function DD( attrs ) {
   return hop_dom_create( "dd", arguments );
}

/*** META (define-macro (<DEL> . args)
     `(hop_dom_create "del" ,@args)) */

function DEL( attrs ) {
   return hop_dom_create( "del", arguments );
}

/*** META (define-macro (<DFN> . args)
     `(hop_dom_create "dfn" ,@args)) */

function DFN( attrs ) {
   return hop_dom_create( "dfn", arguments );
}

/*** META (define-macro (<DIR> . args)
     `(hop_dom_create "dir" ,@args)) */

function DIR( attrs ) {
   return hop_dom_create( "dir", arguments );
}

/*** META (define-macro (<DIV> . args)
     `(hop_dom_create "div" ,@args)) */

function DIV( attrs ) {
   return hop_dom_create( "div", arguments );
}

/*** META (define-macro (<DL> . args)
     `(hop_dom_create "dl" ,@args)) */

function DL( attrs ) {
   return hop_dom_create( "dl", arguments );
}

/*** META (define-macro (<DT> . args)
     `(hop_dom_create "dt" ,@args)) */

function DT( attrs ) {
   return hop_dom_create( "dt", arguments );
}

/*** META (define-macro (<EM> . args)
     `(hop_dom_create "em" ,@args)) */

function EM( attrs ) {
   return hop_dom_create( "em", arguments );
}

/*** META (define-macro (<FIELDSET> . args)
     `(hop_dom_create "fieldset" ,@args)) */

function FIELDSET( attrs ) {
   return hop_dom_create( "fieldset", arguments );
}

/*** META (define-macro (<FONT> . args)
     `(hop_dom_create "font" ,@args)) */

function FONT( attrs ) {
   return hop_dom_create( "font", arguments );
}

/*** META (define-macro (<FOOTER> . args)
     `(hop_dom_create "footer" ,@args)) */

function FOOTER( attrs ) {
   return hop_dom_create( "footer", arguments );
}

/*** META (define-macro (<FORM> . args)
     `(hop_dom_create "form" ,@args)) */

function FORM( attrs ) {
   return hop_dom_create( "form", arguments );
}

/*** META (define-macro (<FRAME> . args)
     `(hop_dom_create "frame" ,@args)) */

function FRAME( attrs ) {
   return hop_dom_create( "frame", arguments );
}

/*** META (define-macro (<FRAMESET> . args)
     `(hop_dom_create "frameset" ,@args)) */

function FRAMESET( attrs ) {
   return hop_dom_create( "frameset", arguments );
}

/*** META (define-macro (<H1> . args)
     `(hop_dom_create "h1" ,@args)) */

function H1( attrs ) {
   return hop_dom_create( "h1", arguments );
}

/*** META (define-macro (<H2> . args)
     `(hop_dom_create "h2" ,@args)) */

function H2( attrs ) {
   return hop_dom_create( "h2", arguments );
}

/*** META (define-macro (<H3> . args)
     `(hop_dom_create "h3" ,@args)) */

function H3( attrs ) {
   return hop_dom_create( "h3", arguments );
}

/*** META (define-macro (<H4> . args)
     `(hop_dom_create "h4" ,@args)) */

function H4( attrs ) {
   return hop_dom_create( "h4", arguments );
}

/*** META (define-macro (<H5> . args)
     `(hop_dom_create "h5" ,@args)) */

function H5( attrs ) {
   return hop_dom_create( "h5", arguments );
}

/*** META (define-macro (<H6> . args)
     `(hop_dom_create "h6" ,@args)) */

function H6( attrs ) {
   return hop_dom_create( "h6", arguments );
}

/*** META (define-macro (<HR> . args)
     `(hop_dom_create "hr" ,@args)) */

function HR( attrs ) {
   return hop_dom_create( "hr", arguments );
}

/*** META (define-macro (<HEADER> . args)
     `(hop_dom_create "header" ,@args)) */

function HEADER( attrs ) {
   return hop_dom_create( "header", arguments );
}

/*** META (define-macro (<HGROUP> . args)
     `(hop_dom_create "hgroup" ,@args)) */

function HGROUP( attrs ) {
   return hop_dom_create( "hgroup", arguments );
}

/*** META (define-macro (<HTML> . args)
     `(hop_dom_create "html" ,@args)) */

function HTML( attrs ) {
   return hop_dom_create( "html", arguments );
}

/*** META (define-macro (<I> . args)
     `(hop_dom_create "i" ,@args)) */

function I( attrs ) {
   return hop_dom_create( "i", arguments );
}

/*** META (define-macro (<IFRAME> . args)
     `(hop_dom_create "iframe" ,@args)) */

function IFRAME( attrs ) {
   return hop_dom_create( "iframe", arguments );
}

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

function INPUT( attrs ) {
   return hop_dom_create( "input", arguments );
}


/*** META (define-macro (<INS> . args)
     `(hop_dom_create "ins" ,@args)) */

function INS( attrs ) {
   return hop_dom_create( "ins", arguments );
}

/*** META (define-macro (<ISINDEX> . args)
     `(hop_dom_create "isindex" ,@args)) */

function ISINDEX( attrs ) {
   return hop_dom_create( "isindex", arguments );
}

/*** META (define-macro (<KBD> . args)
     `(hop_dom_create "kbd" ,@args)) */

function KBD( attrs ) {
   return hop_dom_create( "kbd", arguments );
}

/*** META (define-macro (<LABEL> . args)
     `(hop_dom_create "label" ,@args)) */

function LABEL( attrs ) {
   return hop_dom_create( "label", arguments );
}

/*** META (define-macro (<LEGEND> . args)
     `(hop_dom_create "legend" ,@args)) */

function LEGEND( attrs ) {
   return hop_dom_create( "legend", arguments );
}

/*** META (define-macro (<LI> . args)
     `(hop_dom_create "li" ,@args)) */

function LI( attrs ) {
   return hop_dom_create( "li", arguments );
}

/*** META (define-macro (<LINK> . args)
     `(hop_dom_create "link" ,@args)) */

function LINK( attrs ) {
   return hop_dom_create( "link", arguments );
}

/*** META (define-macro (<MAP> . args)
     `(hop_dom_create "map" ,@args)) */

function MAP( attrs ) {
   return hop_dom_create( "map", arguments );
}

/*** META (define-macro (<MARQUEE> . args)
     `(hop_dom_create "marquee" ,@args)) */

function MARQUEE( attrs ) {
   return hop_dom_create( "marquee", arguments );
}

/*** META (define-macro (<MENU> . args)
     `(hop_dom_create "menu" ,@args)) */

function MENU( attrs ) {
   return hop_dom_create( "menu", arguments );
}

/*** META (define-macro (<META> . args)
     `(hop_dom_create "meta" ,@args)) */

function META( attrs ) {
   return hop_dom_create( "meta", arguments );
}

/*** META (define-macro (<NOFRAMES> . args)
     `(hop_dom_create "noframes" ,@args)) */

function NOFRAMES( attrs ) {
   return hop_dom_create( "noframes", arguments );
}

/*** META (define-macro (<NOSCRIPT> . args)
     `(hop_dom_create "noscript" ,@args)) */

function NOSCRIPT( attrs ) {
   return hop_dom_create( "noscript", arguments );
}

/*** META (define-macro (<OBJECT> . args)
     `(hop_dom_create "object" ,@args)) */

function OBJECT( attrs ) {
   return hop_dom_create( "object", arguments );
}

/*** META (define-macro (<OL> . args)
     `(hop_dom_create "ol" ,@args)) */

function OL( attrs ) {
   return hop_dom_create( "ol", arguments );
}

/*** META (define-macro (<OPTGROUP> . args)
     `(hop_dom_create "optgroup" ,@args)) */

function OPTGROUP( attrs ) {
   return hop_dom_create( "optgroup", arguments );
}

/*** META (define-macro (<OPTION> . args)
     `(hop_dom_create "option" ,@args)) */

function OPTION( attrs ) {
   return hop_dom_create( "option", arguments );
}

/*** META (define-macro (<P> . args)
     `(hop_dom_create "p" ,@args)) */

function P( attrs ) {
   return hop_dom_create( "p", arguments );
}

/*** META (define-macro (<PARAM> . args)
     `(hop_dom_create "param" ,@args)) */

function PARAM( attrs ) {
   return hop_dom_create( "param", arguments );
}

/*** META (define-macro (<PRE> . args)
     `(hop_dom_create "pre" ,@args)) */

function PRE( attrs ) {
   return hop_dom_create( "pre", arguments );
}

/*** META (define-macro (<Q> . args)
     `(hop_dom_create "q" ,@args)) */

function Q( attrs ) {
   return hop_dom_create( "q", arguments );
}

/*** META (define-macro (<S> . args)
     `(hop_dom_create "s" ,@args)) */

function S( attrs ) {
   return hop_dom_create( "s", arguments );
}

/*** META (define-macro (<SAMP> . args)
     `(hop_dom_create "samp" ,@args)) */

function SAMP( attrs ) {
   return hop_dom_create( "samp", arguments );
}

/*** META (define-macro (<SCRIPT> . args)
     `(hop_dom_create "script" ,@args)) */

function SCRIPT( attrs ) {
   return hop_dom_create( "script", arguments );
}

/*** META (define-macro (<SELECT> . args)
     `(hop_dom_create "select" ,@args)) */

function SELECT( attrs ) {
   return hop_dom_create( "select", arguments );
}

/*** META (define-macro (<SMALL> . args)
     `(hop_dom_create "small" ,@args)) */

function SMALL( attrs ) {
   return hop_dom_create( "small", arguments );
}

/*** META (define-macro (<SPAN> . args)
     `(hop_dom_create "span" ,@args)) */

function SPAN( attrs ) {
   return hop_dom_create( "span", arguments );
}

/*** META (define-macro (<STRIKE> . args)
     `(hop_dom_create "strike" ,@args)) */

function STRIKE( attrs ) {
   return hop_dom_create( "strike", arguments );
}

/*** META (define-macro (<STRONG> . args)
     `(hop_dom_create "strong" ,@args)) */

function STRONG( attrs ) {
   return hop_dom_create( "strong", arguments );
}

/*** META (define-macro (<STYLE> . args)
     `(hop_dom_create "style" ,@args)) */

function STYLE( attrs ) {
   return hop_dom_create( "style", arguments );
}

/*** META (define-macro (<SUB> . args)
     `(hop_dom_create "sub" ,@args)) */

function SUB( attrs ) {
   return hop_dom_create( "sub", arguments );
}

/*** META (define-macro (<SUP> . args)
     `(hop_dom_create "sup" ,@args)) */

function SUP( attrs ) {
   return hop_dom_create( "sup", arguments );
}

/*** META (define-macro (<TABLE> . args)
     `(hop_dom_create "table" ,@args)) */

function TABLE( attrs ) {
   return hop_dom_create( "table", arguments );
}

/*** META (define-macro (<TBODY> . args)
     `(hop_dom_create "tbody" ,@args)) */

function TBODY( attrs ) {
   return hop_dom_create( "tbody", arguments );
}

/*** META (define-macro (<TD> . args)
     `(hop_dom_create "td" ,@args)) */

function TD( attrs ) {
   return hop_dom_create( "td", arguments );
}

/*** META (define-macro (<TEXTAREA> . args)
     `(hop_dom_create "textarea" ,@args)) */

function TEXTAREA( attrs ) {
   return hop_dom_create( "textarea", arguments );
}

/*** META (define-macro (<TFOOT> . args)
     `(hop_dom_create "tfoot" ,@args)) */

function TFOOT( attrs ) {
   return hop_dom_create( "tfoot", arguments );
}

/*** META (define-macro (<TH> . args)
     `(hop_dom_create "th" ,@args)) */

function TH( attrs ) {
   return hop_dom_create( "th", arguments );
}

/*** META (define-macro (<THEAD> . args)
     `(hop_dom_create "thead" ,@args)) */

function THEAD( attrs ) {
   return hop_dom_create( "thead", arguments );
}

/*** META (define-macro (<TITLE> . args)
     `(hop_dom_create "title" ,@args)) */

function TITLE( attrs ) {
   return hop_dom_create( "title", arguments );
}

/*** META (define-macro (<TR> . args)
     `(hop_dom_create "tr" ,@args)) */

function TR( attrs ) {
   return hop_dom_create( "tr", arguments );
}

/*** META (define-macro (<TT> . args)
     `(hop_dom_create "tt" ,@args)) */

function TT( attrs ) {
   return hop_dom_create( "tt", arguments );
}

/*** META (define-macro (<U> . args)
     `(hop_dom_create "u" ,@args)) */

function U( attrs ) {
   return hop_dom_create( "u", arguments );
}

/*** META (define-macro (<UL> . args)
     `(hop_dom_create "ul" ,@args)) */

function UL( attrs ) {
   return hop_dom_create( "ul", arguments );
}

/*** META (define-macro (<VAR> . args)
     `(hop_dom_create "var" ,@args)) */

function VAR( attrs ) {
   return hop_dom_create( "var", arguments );
}

/*** META (define-macro (<IMG> . args)
     `(hop_dom_create "img" ,@args)) */

function IMG( attrs ) {
   return hop_dom_create( "img", arguments );
}

/*** META (define-macro (<HEAD> . args)
     `(hop_dom_create "head" ,@args)) */

function HEAD( attrs ) {
   return hop_dom_create( "head", arguments );
}

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
#if HOP_SCHEME
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
           (arity #t))
*/
function dom_insert_before( id, n, r ) {
   return hop_add( id, n, function( node, e ) { return node.insertBefore( e, r ); } );
}

/*** META ((export dom-replace-child!)
           (arity #t))
*/
function dom_replace_child( id, n, r ) {
   return hop_add( id, n, function( node, e ) { return node.replaceChild( e, r ); } );
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_css_add_style_sheet ...                                      */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_load_css ...                                                 */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_load_jscript ...                                             */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    dom_get_elements_by_class ...                                    */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export #t) (arity -2)) */
function dom_get_elements_by_class( doc, name ) {
   if( ("getElementsByClassName" in document) &&
       ( (doc instanceof String) || (typeof doc == "string") ) ) {
      return sc_vector2list( document.getElementsByClassName( doc ) );
   } else {
      var res = new Array();
      var n = 0;
      var all, re;
   
      if( (doc instanceof String) || (typeof doc == "string") ) {
	 all = document.getElementsByTagName( "*" );
	 re = new RegExp( "\\b" + doc + "\\b", "g" );
      } else {
	 all = doc.getElementsByTagName( "*" );
	 re = new RegExp( "\\b" + name + "\\b", "g" );
      }

      for( var i = 0; i < all.length; i++ ) {
	 if( re.exec( all[ i ].className ) ) {
	    res[ n++ ] = all[ i ];
	 }
      }
      
      return sc_vector2list( res );
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    dom_get_elements_by_attribute ...                                */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_add_reactive_attribute ...                                   */
/*---------------------------------------------------------------------*/
function hop_add_reactive_attribute( id, value ) {
   hop_add_event_listener( id, "ready", function( e ) {
   } );
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
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    node_computed_style_get ...                                      */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_start_tag ...                                                */
/*---------------------------------------------------------------------*/
var hop_start_tag = new RegExp( "^<([a-zA-Z]+)" );
var hop_tags_parent = {
   'tr' : 'tbody',
   'td' : 'tr',
   'th' : 'tr',
   'li' : 'ul',
   'body': 'html'
};

/*---------------------------------------------------------------------*/
/*    hop_create_element ...                                           */
/*---------------------------------------------------------------------*/
function hop_create_element( html ) {
   var m = html.match( hop_start_tag );
   var tag;
   var idx = 0;

   if( m ) {
      var t = m[ 1 ];
      tag = ( t in hop_tags_parent ) ? hop_tags_parent[ t ] : "div";
      idx = ( tag == "html" ) ? 1 : 0;
   } else {
      tag = "div";
      idx = 0;
   }

   var el = document.createElement( tag );
   el.innerHTML = "" + html;
   
   if( hop_config.clone_innerHTML ) {
      // As of Feb 2010, webkit based browsers (Feb 2010) requires a deep
      // clone to accept evaluating embedded scripts when the resulting
      // node is inserted in the DOM!
      if( html.search( /<script[ >]/i ) >= 0 ) {
	 return cloneScriptNode( el.childNodes[ idx ] );
      } else {
	 // Remove the node otherwise it has a parentNode set to non-null
	 // which confused functions such as dom_add_child
	 return el.removeChild( el.childNodes[ idx ] );
      }
   } else {
      // See the remark above for removeChild
      return el.removeChild( el.childNodes[ idx ] );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_create_encoded_element ...                                   */
/*    -------------------------------------------------------------    */
/*    Don't remove this function, see HOP_FIND_CLASS_UNSERIALIZER      */
/*    (hop-serialize.js).                                              */
/*---------------------------------------------------------------------*/
function hop_create_encoded_element( html ) {
   try {
      var o = hop_create_element( decodeURIComponent( html ) );
      return o;
   } catch( e ) {
      /* decoding has hitted an illegal UTF-8 surrogate, decode by hand */
      var i = 0;
      var l = html.length;
      var r = "";

      while( i < l ) {
	 var j = html.indexOf( '%', i );

	 if( j == -1 ) {
	    return r + html.substring( i );
	 } else {
	    if( j > l - 3 )
	       return r + html.substring( i );

	    if( j > i )
	       r += html.substring( i, j );

	    r += string_hex_intern( html.substring( j + 1, j + 3 ) );

	    i = j + 3;
	 }
      }

      return hop_create_element( r );
   }
}

/*---------------------------------------------------------------------*/
/*    hop_innerHTML_set ...                                            */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export innerHTML-set!) (arity #t)) */
function hop_innerHTML_set( nid, html ) {
   var el;

   if( (nid instanceof String) || (typeof nid == "string") ) {
      el = document.getElementById( nid );

      if( el == undefined ) {
	 sc_error( "innerHTML-set!", "Cannot find element", nid, 2 );
      }
   } else {
      if( !nid ) {
	 sc_error( "innerHTML-set!", "illegal element", nid, 2 );
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_style_attribute_set ...                                      */
/*---------------------------------------------------------------------*/
function hop_style_attribute_set( obj, val ) {
   if( (typeof val == "string") || (val instanceof String) ) {
      while( val.length > 1 ) {
	 var m = /[ \n\t]*([^ \n\t:]+)[ \n\t]*:[ \n\t]*([^;]+);?/.exec( val );

	 if( m ) {
	    node_style_set( obj, m[ 1 ], m[ 2 ] );
	    val = val.substring( m[ 0 ].length, val.length );
	 } else {
	    val = "";
	 }
      }
   } else {
      for( var p in val ) {
	 node_style_set( obj, p, val[ p ] );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    hop_element_x ...                                                */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_element_y ...                                                */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_bounding_box ...                                             */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export node-bounding-box) (arity -2)) */
function hop_bounding_box( e, m ) {
   var  n = (e instanceof String) || (typeof e == "string") ?
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_to_list ...                                     */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
/*** META ((export bounding-box->list) (arity #t)) */
function hop_bounding_box_to_list( bbox ) {
   return sc_list( bbox.left, bbox.top, bbox.width, bbox.height );
}
#endif

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_x ...                                           */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif

/*---------------------------------------------------------------------*/
/*    hop_bounding_box_y ...                                           */
/*---------------------------------------------------------------------*/
#if HOP_SCHEME
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
#endif
