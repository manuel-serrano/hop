;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/xml.scm                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  8 05:43:46 2004                          */
;*    Last change :  Wed Aug  8 15:12:38 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple XML producer/writer for HOP.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_xml

   (library web)
   
   (include "param.sch"
	    "xml.sch")

   (import  __hop_types
	    __hop_mime
	    __hop_misc
	    __hop_param
	    __hop_configure
	    __hop_css)
   
   (export  (class xml-backend
	      (id::symbol read-only)
	      (mime-type::bstring read-only)
	      (doctype::bstring read-only)
	      (header-format::bstring read-only)
	      (html-attributes::pair-nil read-only (default '()))
	      (no-end-tags-elements::pair-nil read-only (default '()))
	      (script-start (default #f))
	      (script-stop (default #f))
	      (meta-format::bstring read-only)
	      (abbrev-emptyp::bool (default #f)))

	    (class xml
	       (%xml-constructor))

	    (class css::xml)
	    
	    (class xml-if::xml
	       (test::procedure read-only)
	       (then read-only)
	       (otherwise read-only))
	    
	    (class xml-delay::xml
	       (id read-only (default #unspecified))
	       (thunk::procedure read-only)
	       (value::obj (default #f)))

	    (class xml-markup::xml
	       (markup::symbol read-only)
	       (attributes::pair-nil (default '()))
	       (initializations::pair-nil (default '()))
	       body::pair-nil)

	    (class xml-html::xml-markup)

	    (class xml-element::xml-markup
	       (id read-only (default #unspecified))
	       (parent (default #unspecified)))

	    (class xml-empty-element::xml-element)

	    (class xml-script::xml-element)
	    
	    (class xml-tilde::xml
	       (body read-only)
	       (parent (default #unspecified)))

	    (class xml-meta::xml-markup)

	    (generic %xml-constructor ::xml)
	    (xml-constructor-add! ::symbol ::procedure)
	    (%make-xml-element ::symbol ::pair-nil)

	    (xml-markup-is? ::obj ::symbol)

	    (xml-make-id::bstring #!optional id (markup 'HOP))

	    (xml-event-handler-attribute?::bool ::keyword)
	    
	    (hop-get-xml-backend::xml-backend ::symbol)

	    (hop-javascript-mime-type::bstring)
	    (hop-javascript-mime-type-set! ::bstring)
	    
	    (hop-xhtml-xmlns::pair-nil)
	    (hop-xhtml-xmlns-set! ::pair-nil)

	    (hop-xml-backend::xml-backend)
	    (hop-xml-backend-set! ::obj)

 	    (generic xml-write ::obj ::output-port ::symbol ::xml-backend)
	    (generic xml-write-attribute ::obj ::obj ::output-port)
	    (generic xml-write-initializer ::obj ::output-port)
	    (xml-write-attributes ::pair-nil ::output-port)

	    (xml->string ::obj ::symbol ::xml-backend)
	    
	    (string->html ::bstring)
	    (string->xml ::bstring)

	    (string->tilde::xml-tilde ::bstring)
	    (tilde->string::bstring ::xml-tilde)
	    (tilde-compose::xml-tilde ::xml-tilde ::xml-tilde)
	    (tilde-make-thunk::xml-tilde ::xml-tilde)
	    (xml-write-tilde-as-expression ::xml-tilde ::output-port)

	    (img-base64-encode::bstring ::bstring)
	    
	    (<A> . ::obj)
	    (<ABBR> . ::obj)
	    (<ACRONYM> . ::obj)
	    (<ADDRESS> . ::obj)
	    (<APPLET> . ::obj)
	    (<AREA> . ::obj)
	    (<B> . ::obj)
	    (<BASE> . ::obj)
	    (<BASEFONT> . ::obj)
	    (<BDO> . ::obj)
	    (<BIG> . ::obj)
	    (<BLOCKQUOTE> . ::obj)
	    (<BODY> . ::obj)
	    (<BR> . ::obj)
	    (<BUTTON> . ::obj)
	    (<CANVAS> . ::obj)
	    (<CAPTION> . ::obj)
	    (<CENTER> . ::obj)
	    (<CITE> . ::obj)
	    (<CODE> . ::obj)
	    (<COL> . ::obj)
	    (<COLGROUP> . ::obj)
	    (<DD> . ::obj)
	    (<DEL> . ::obj)
	    (<DFN> . ::obj)
	    (<DIR> . ::obj)
 	    (<DIV> . ::obj)
	    (<DL> . ::obj)
	    (<DT> . ::obj)
	    (<EM> . ::obj)
	    (<EMBED> . ::obj)
	    (<FIELDSET> . ::obj)
	    (<FONT> . ::obj)
	    (<FORM> . ::obj)
	    (<FRAME> . ::obj)
	    (<FRAMESET> . ::obj)
	    (<H1> . ::obj)
	    (<H2> . ::obj)
	    (<H3> . ::obj)
	    (<H4> . ::obj)
	    (<H5> . ::obj)
	    (<H6> . ::obj)
	    (<HR> . ::obj)
	    (<I> . ::obj)
	    (<IFRAME> . ::obj)
	    (<IMG> . ::obj)
	    (<INPUT> . ::obj)
	    (<INS> . ::obj)
	    (<ISINDEX> . ::obj)
	    (<KBD> . ::obj)
	    (<LABEL> . ::obj)
	    (<LEGEND> . ::obj)
	    (<LI> . ::obj)
	    (<MAP> . ::obj)
	    (<MARQUEE> . ::obj)
	    (<MENU> . ::obj)
	    (<META> . ::obj)
	    (<NOFRAMES> . ::obj)
	    (<NOSCRIPT> . ::obj)
	    (<OBJECT> . ::obj)
	    (<OL> . ::obj)
	    (<OPTGROUP> . ::obj)
	    (<OPTION> . ::obj)
	    (<P> . ::obj)
	    (<PARAM> . ::obj)
	    (<PRE> . ::obj)
	    (<Q> . ::obj)
	    (<S> . ::obj)
	    (<SAMP> . ::obj)
	    (<SELECT> . ::obj)
	    (<SMALL> . ::obj)
	    (<SPAN> . ::obj)
	    (<STRIKE> . ::obj)
	    (<STRONG> . ::obj)
	    (<STYLE> . ::obj)
	    (<SUB> . ::obj)
	    (<SUP> . ::obj)
	    (<TABLE> . ::obj)
	    (<TBODY> . ::obj)
	    (<TD> . ::obj)
	    (<TEXTAREA> . ::obj)
	    (<TFOOT> . ::obj)
	    (<TH> . ::obj)
	    (<THEAD> . ::obj)
	    (<TITLE> . ::obj)
	    (<TR> . ::obj)
	    (<TT> . ::obj)
	    (<U> . ::obj)
	    (<UL> . ::obj)
	    (<VAR> . ::obj)

	    (<TILDE> ::obj)
	    (<DELAY> . ::obj)))

;*---------------------------------------------------------------------*/
;*    object-print ::xml-element ...                                   */
;*    -------------------------------------------------------------    */
;*    Because of their parent slot xml-element are cyclic and cannot   */
;*    thus be display as is.                                           */
;*---------------------------------------------------------------------*/
(define-method (object-print o::xml-element p print-slot)
   (with-access::xml-element o (markup attributes initializations body id)
      (display "#|xml-element markup=" p)
      (print-slot markup p)
      (display " id=" p)
      (print-slot id p)
      (display " parent=..." p)
      (display " attributes=" p)
      (print-slot attributes p)
      (display " initializations=" p)
      (print-slot initializations p)
      (display " body=" p)
      (print-slot body p)
      (display "|" p)))

;*---------------------------------------------------------------------*/
;*    hop-javascript-mime-type ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter hop-javascript-mime-type
   (hop-configure-javascript-mime-type))
   
;*---------------------------------------------------------------------*/
;*    hop-xhtml-xmlns ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-xhtml-xmlns
   '((xmlns . "http://www.w3.org/1999/xhtml"))
   (lambda (v)
      (if (every? (lambda (x)
		     (and (pair? x) (symbol? (car x)) (string? (cdr x))))
		  v)
	  v
	  (error 'hop-xhtml-xmlns "Illegal namespaces" v))))
				   
;*---------------------------------------------------------------------*/
;*    *html-backend* ...                                               */
;*---------------------------------------------------------------------*/
(define *html-4.01-backend*
   (instantiate::xml-backend
      (id 'html-4.01)
      (mime-type "text/html")
      (doctype "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Strict//EN\">\n")
      (html-attributes '())
      (header-format "")
      (no-end-tags-elements '(link))
      ;; the meta-format contains the closing >
      (meta-format "content=\"~a; charset=~a\">")))

;*---------------------------------------------------------------------*/
;*    *xhtml-backend* ...                                              */
;*---------------------------------------------------------------------*/
(define *xhtml-backend*
   (instantiate::xml-backend
      (id 'xhtml-1.0)
      (mime-type "application/xhtml+xml")
      (doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
      (html-attributes (hop-xhtml-xmlns))
      (header-format "<?xml version=\"1.0\" encoding=\"~a\"?>")
      (no-end-tags-elements '())
      ;; XHTML scripts have to be protected
      (script-start "\n<![CDATA[\n")
      (script-stop "]]>\n")
      ;; the meta-format contains the closing />
      (meta-format "/>")))

;*---------------------------------------------------------------------*/
;*    hop-xml-backend ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-xml-backend
   *html-4.01-backend*
   (lambda (v)
      (if (xml-backend? v)
	  v
	  (hop-get-xml-backend v))))

;*---------------------------------------------------------------------*/
;*    hop-get-xml-backend ...                                          */
;*---------------------------------------------------------------------*/
(define (hop-get-xml-backend id)
   (case id
      ((html html-4.01)
       *html-4.01-backend*)
      ((xhtml xhtml-1.0)
       *xhtml-backend*)
      (else
       (error 'hop-get-xml-backend "Illegal backend" id))))
   
;*---------------------------------------------------------------------*/
;*    *xml-constructors* ...                                           */
;*---------------------------------------------------------------------*/
(define *xml-constructors* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    xml-constructor-add! ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-constructor-add! id proc)
   (if (not (correct-arity? proc 1))
       (error 'xml-constructor-add! "Illegal constructor" proc)
       (hashtable-put! *xml-constructors* id proc)))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (%xml-constructor o::xml)
   o)

;*---------------------------------------------------------------------*/
;*    %xml-constructor ...                                             */
;*---------------------------------------------------------------------*/
(define-method (%xml-constructor o::xml-markup)
   (call-next-method)
   (with-access::xml-markup o (body markup)
      (let loop ((es body))
	 (cond
	    ((pair? es)
	     (let ((e (car es)))
		(cond
		   ((xml-element? e)
		    (xml-element-parent-set! e o))
		   ((xml-tilde? e)
		    (xml-tilde-parent-set! e o))
		   ((pair? e)
		    (loop e))))
	     (loop (cdr es)))
	    ((xml-element? es)
	     (xml-element-parent-set! es o))))
      o))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ::xml-element ...                               */
;*---------------------------------------------------------------------*/
(define-method (%xml-constructor o::xml-element)
   (call-next-method)
   (with-access::xml-element o (id)
      (let ((hook (hashtable-get *xml-constructors* id)))
	 (when (procedure? hook)
	    (hook o)))
      o))

;*---------------------------------------------------------------------*/
;*    %make-xml-element ...                                            */
;*---------------------------------------------------------------------*/
(define (%make-xml-element el args)
   (let loop ((a args)
	      (attr '())
	      (init '())
	      (body '())
	      (id #unspecified))
      (cond
	 ((null? a)
	  (instantiate::xml-element
	     (markup (string->symbol
		      (string-downcase
		       (symbol->string el))))
	     (attributes (reverse! attr))
	     (initializations (reverse! init))
	     (id (xml-make-id id el))
	     (body (reverse! body))))
	 ((keyword? (car a))
	  (cond
	     ((not (pair? (cdr a)))
	      (error (symbol-append '< el '>)
		     "Illegal attribute"
		     (car a)))
	     ((eq? (car a) :id)
	      (if (string? (cadr a))
		  (loop (cddr a) attr init body (cadr a))
		  (bigloo-type-error el "string" (cadr a))))
	     ((and (xml-tilde? (cadr a))
		   (not (xml-event-handler-attribute? (car a))))
	      (loop (cddr a)
		    attr
		    (cons (cons (keyword->symbol (car a)) (cadr a))
			  init)
		    body
		    id))
	     (else
	      (loop (cddr a)
		    (cons (cons (keyword->string (car a)) (cadr a))
			  attr)
		    init
		    body
		    id))))
	 ((null? (car a))
	  (loop (cdr a) attr init body id))
	 ((pair? (car a))
	  (if (not (and (or (null? (cdr a)) (pair? (cdr a))) (list? (car a))))
	      (error (symbol-append '< el '>)
		     "Illegal arguments"
		     `(,(symbol-append '< el '>) ,@args))
	      (loop (append (car a) (cdr a)) attr init body id)))
	 (else
	  (loop (cdr a) attr init (cons (car a) body) id)))))

;*---------------------------------------------------------------------*/
;*    xml-markup-is? ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-markup-is? o markup)
   (and (xml-markup? o) (eq? (xml-markup-markup o) markup)))

;*---------------------------------------------------------------------*/
;*    if-debug ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (if-debug exp1 exp2)
   (if (>fx (bigloo-debug) 0)
       exp1
       exp2))

;*---------------------------------------------------------------------*/
;*    xml-make-id ...                                                  */
;*---------------------------------------------------------------------*/
(define (xml-make-id #!optional id (markup 'HOP))
   (if (string? id)
       id
       (if-debug (symbol->string (gensym markup)) (symbol->string (gensym)))))

;*---------------------------------------------------------------------*/
;*    xml-event-handler-attribute? ...                                 */
;*    -------------------------------------------------------------    */
;*    This is a gross hack. Currently, we consider that all attributes */
;*    whose name start with "on" are event handlers!                   */
;*---------------------------------------------------------------------*/
(define (xml-event-handler-attribute? keyword)
   (substring-at? (keyword->string! keyword) "on" 0))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (xml-write obj p encoding backend)
   (cond
      ((string? obj)
       (display obj p))
      ((or (number? obj) (symbol? obj))
       (display obj p))
      ((pair? obj)
       (for-each (lambda (o) (xml-write o p encoding backend)) obj))
      ((date? obj)
       (display obj p))
      ((null? obj)
       #unspecified)
      ((eq? obj #unspecified)
       #unspecified)
      ((eq? obj #f)
       #unspecified)
      ((eq? obj #t)
       #unspecified)
      (else
       (error 'xml-write "Illegal xml object" obj))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-if ...                                           */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-if p encoding backend)
   (with-access::xml-if obj (test then otherwise)
      (if (test)
	  (xml-write then p encoding backend)
	  (xml-write otherwise p encoding backend))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-script ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-script p encoding backend)
   (with-access::xml-script obj (body attributes)
      (with-access::xml-backend backend (script-start script-stop)
	 (if (pair? attributes)
	     (begin
		(display "<script" p)
		(xml-write-attributes attributes p)
		(display ">" p))
	     (begin
		(display "<script type='" p)
		(display (hop-javascript-mime-type) p)
		(display "'>" p)))
	 (when (pair? body)
	    (when script-start (display script-start p))
	    (xml-write body p encoding backend)
	    (when script-stop (display script-stop p)))
	 (display "</script>\n" p))))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-tilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-tilde p encoding backend)
   (with-access::xml-tilde obj (body parent)
      (if (and (xml-markup? parent) (eq? (xml-markup-markup parent) 'script))
	  (xml-write body p encoding backend)
	  (with-access::xml-backend backend (script-start script-stop)
	     (display "<script type='" p)
	     (display (hop-javascript-mime-type) p)
	     (display "'>" p)
	     (when script-start (display script-start p))
	     (xml-write body p encoding backend)
	     (when script-stop (display script-stop p))
	     (display "</script>\n" p)))))
      
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-delay ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-delay p encoding backend)
   (with-access::xml-delay obj (thunk)
      (xml-write (thunk) p encoding backend)))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-markup p encoding backend)
   (with-access::xml-markup obj (markup attributes body)
      (display "<" p)
      (display markup p)
      (xml-write-attributes attributes p)
      (cond
	 ((or (pair? body) (eq? markup 'script))
	  (display ">" p)
	  (for-each (lambda (b) (xml-write b p encoding backend)) body)
	  (display "</" p)
	  (display markup p)
	  (display ">\n" p))
	 ((memq markup (xml-backend-no-end-tags-elements backend))
	  (display ">\n" p))
	 (else
	  (display "/>\n" p)))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-meta ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-meta p encoding backend)
   (with-access::xml-meta obj (markup attributes body)
      (display "<" p)
      (display markup p)
      (xml-write-attributes attributes p)
      (with-access::xml-backend backend (meta-format mime-type)
	 (fprintf p meta-format mime-type encoding))
      (newline p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-element ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-element p encoding backend)
   (with-access::xml-element obj (markup id attributes body)
      (cond
	 ((and (null? body) (null? attributes))
	  (display "<" p)
	  (display markup p)
	  (display " id=\"" p)
	  (display id p)
	  (display "\"" p)
	  (if (xml-backend-abbrev-emptyp backend)
	      (display "/>" p)
	      (begin
		 (display "></" p)
		 (display markup p)
		 (display ">" p))))
	 ((null? body)
	  (display "<" p)
	  (display markup p)
	  (display " id=\"" p)
	  (display id p)
	  (display "\"" p)
	  (xml-write-attributes attributes p)
	  (if (xml-backend-abbrev-emptyp backend)
	      (display "/>" p)
	      (begin
		 (display ">" p)
		 (display "</" p)
		 (display markup p)
		 (display ">" p))))
	 (else
	  (display "<" p)
	  (display markup p)
	  (display " id=\"" p)
	  (display id p)
	  (display "\"" p)
	  (xml-write-attributes attributes p)
	  (display ">" p)
	  (for-each (lambda (b) (xml-write b p encoding backend)) body)
	  (display "</" p)
	  (display markup p)
	  (display ">" p)))
      (xml-write-initializations obj p encoding backend)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-empty-element ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-empty-element p encoding backend)
   (with-access::xml-empty-element obj (markup id attributes)
      (display "<" p)
      (display markup p)
      (display " id=\"" p)
      (display id p)
      (display "\"" p)
      (xml-write-attributes attributes p)
      (display "/>\n" p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-html ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-html p encoding backend)
   (with-access::xml-backend backend (header-format doctype html-attributes)
      (fprintf p header-format encoding)
      (newline p)
      (display doctype p)
      (newline p)
      (with-access::xml-html obj (markup attributes body)
	 (display "<" p)
	 (display markup p)
	 (xml-write-attributes (filter (lambda (h)
					  (not (assq (car h) attributes)))
				       html-attributes)
			       p)
	 (xml-write-attributes attributes p)
	 (display ">\n" p)
	 (for-each (lambda (b) (xml-write b p encoding backend)) body)
	 (display "</" p)
	 (display markup p)
	 (display ">\n" p))))

;*---------------------------------------------------------------------*/
;*    xml-write-attributes ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-write-attributes attr p)
   (for-each (lambda (a)
		(display " " p)
		(xml-write-attribute (cdr a) (car a) p))
	     attr))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::obj ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (xml-write-attribute attr::obj id p)
   ;; boolean false attribute has no value
   (unless (eq? attr #f)
      (display id p)
      ;; boolean true attribute has no value
      (display "='" p)
      (cond
	 ((eq? attr #t)
	  (display id p))
	 ((procedure? attr)
	  (error 'xml "Illegal procedure argument in XML attribute" id))
	 (else
	  (display (xml-attribute-encode attr) p)))
      (display "'" p)))

;*---------------------------------------------------------------------*/
;*    xml-write-initializations ...                                    */
;*---------------------------------------------------------------------*/
(define (xml-write-initializations obj p encoding backend)
   (with-access::xml-element obj (id initializations)
      (when (pair? initializations)
	 (with-access::xml-backend backend (script-start script-stop)
	    (display "<script type='" p)
	    (display (hop-javascript-mime-type) p)
	    (display "'>" p)
	    (let ((var (gensym)))
	       (display "var " p)
	       (display var p)
	       (display " = document.getElementById( \"" p)
	       (display id p)
	       (display "\" );" p)
	       (when script-start (display script-start p))
	       (for-each (lambda (a)
			    (xml-write-initialization (cdr a) (car a) var p)
			    (newline p))
			 initializations)
	       (when script-stop (display script-stop p))
	       (display "</script>\n" p))))))

;*---------------------------------------------------------------------*/
;*    xml-write-initialization ...                                     */
;*---------------------------------------------------------------------*/
(define (xml-write-initialization tilde id var p)
   (if (eq? id 'style)
       (xml-write-style-initialization tilde var p)
       (begin
	  (display var p)
	  (display "[\"" p)
	  (display (if (eq? id 'class) "className" id) p)
	  (display "\"]=" p)
	  (xml-write-initializer tilde p)
	  (display ";" p))))

;*---------------------------------------------------------------------*/
;*    xml-write-style-initialization ...                               */
;*    -------------------------------------------------------------    */
;*    Style is special because it is a read-only attributes and        */
;*    because its value can be evaluated to a JavaScript object.       */
;*---------------------------------------------------------------------*/
(define (xml-write-style-initialization tilde var p)
   (display "hop_style_attribute_set(" p)
   (display var p)
   (display "," p)
   (xml-write-initializer tilde p)
   (display ");" p))
   
;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::xml-tilde ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::xml-tilde id p)
   (with-access::xml-tilde attr (body)
      (xml-write-attribute body id p)))
   
;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::hop-service ...                            */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::hop-service id p)
   (display id p)
   (display "='" p)
   (display (hop-service-path attr) p)
   (display "'" p))

;*---------------------------------------------------------------------*/
;*    xml->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (xml->string obj encoding backend)
   (with-output-to-string
      (lambda ()
	 (xml-write obj (current-output-port) encoding backend))))

;*---------------------------------------------------------------------*/
;*    string->html ...                                                 */
;*---------------------------------------------------------------------*/
(define (string->html h)
   (with-input-from-string h
      (lambda ()
	 (car (html-parse
	       (current-input-port)
	       0
	       (lambda (markup attributes body)
		  (let* ((m (string->symbol
			     (string-append
			      "<"
			      (string-upcase (symbol->string markup))
			      ">")))
			 (a (append-map (lambda (a)
					   (list (symbol->keyword (car a))
						 (cdr a)))
					attributes))
			 (e `(,m ,@a ,@body)))
		     (eval e))))))))

;*---------------------------------------------------------------------*/
;*    string->xml ...                                                  */
;*---------------------------------------------------------------------*/
(define (string->xml h)
   (with-input-from-string h
      (lambda ()
	 (car (xml-parse
	       (current-input-port)
	       0
	       (lambda (markup attributes body)
		  (let* ((m (string->symbol
			     (string-append
			      "<"
			      (string-upcase (symbol->string markup))
			      ">")))
			 (a (append-map (lambda (a)
					   (list (symbol->keyword (car a))
						 (cdr a)))
					attributes))
			 (e `(,m ,@a ,@body)))
		     (eval e))))))))

;*---------------------------------------------------------------------*/
;*    HTML 4.01 elements ...                                           */
;*---------------------------------------------------------------------*/
(define-xml-element <A>)
(define-xml-element <ABBR>)
(define-xml-element <ACRONYM>)
(define-xml-element <ADDRESS>)
(define-xml-element <APPLET>)
(define-xml xml-empty-element #t <AREA>)
(define-xml-element <B>)
(define-xml xml-empty-element #t <BASE>)
(define-xml xml-empty-element #t <BASEFONT>)
(define-xml-element <BDO>)
(define-xml-element <BIG>)
(define-xml-element <BLOCKQUOTE>)
(define-xml-element <BODY>)
(define-xml xml-empty-element #t <BR>)
(define-xml-element <BUTTON>)
(define-xml-element <CANVAS>)
(define-xml-element <CAPTION>)
(define-xml-element <CENTER>)
(define-xml-element <CITE>)
(define-xml-element <CODE>)
(define-xml xml-empty-element #t <COL>)
(define-xml-element <COLGROUP>)
(define-xml-element <DD>)
(define-xml-element <DEL>)
(define-xml-element <DFN>)
(define-xml-element <DIR>)
(define-xml-element <DIV>)
(define-xml-element <DL>)
(define-xml-element <DT>)
(define-xml-element <EM>)
(define-xml-element <EMBED>)
(define-xml-element <FIELDSET>)
(define-xml-element <FONT>)
(define-xml-element <FORM>)
(define-xml xml-empty-element #t <FRAME>)
(define-xml-element <FRAMESET>)
(define-xml-element <H1>)
(define-xml-element <H2>)
(define-xml-element <H3>)
(define-xml-element <H4>)
(define-xml-element <H5>)
(define-xml-element <H6>)
(define-xml xml-empty-element #t <HR>)
(define-xml-element <I>)
(define-xml-element <IFRAME>)
(define-xml xml-empty-element #t <INPUT>)
(define-xml-element <INS>)
(define-xml-element <ISINDEX>)
(define-xml-element <KBD>)
(define-xml-element <LABEL>)
(define-xml-element <LEGEND>)
(define-xml-element <LI>)
(define-xml-element <MAP>)
(define-xml-element <MARQUEE>)
(define-xml-element <MENU>)
(define-xml xml-meta #f <META>)
(define-xml-element <NOFRAMES>)
(define-xml-element <NOSCRIPT>)
(define-xml-element <OBJECT>)
(define-xml-element <OL>)
(define-xml-element <OPTGROUP>)
(define-xml-element <OPTION>)
(define-xml-element <P>)
(define-xml xml-empty-element #t <PARAM>)
(define-xml-element <PRE>)
(define-xml-element <Q>)
(define-xml-element <S>)
(define-xml-element <SAMP>)
(define-xml-element <SELECT>)
(define-xml-element <SMALL>)
(define-xml-element <SPAN>)
(define-xml-element <STRIKE>)
(define-xml-element <STRONG>)
(define-xml-markup <STYLE>)
(define-xml-element <SUB>)
(define-xml-element <SUP>)
(define-xml-element <TABLE>)
(define-xml-element <TBODY>)
(define-xml-element <TD>)
(define-xml-element <TEXTAREA>)
(define-xml-element <TFOOT>)
(define-xml-element <TH>)
(define-xml-element <THEAD>)
(define-xml-markup <TITLE>)
(define-xml-element <TR>)
(define-xml-element <TT>)
(define-xml-element <U>)
(define-xml-element <UL>)
(define-xml-element <VAR>)

;*---------------------------------------------------------------------*/
;*    <TILDE> ...                                                      */
;*---------------------------------------------------------------------*/
(define (<TILDE> body)
   (instantiate::xml-tilde
      (body body)))

;*---------------------------------------------------------------------*/
;*    <DELAY> ...                                                      */
;*---------------------------------------------------------------------*/
(define-xml-compound <DELAY> ((id #unspecified string)
			      body)
   (if (and (pair? body)
	    (procedure? (car body))
	    (correct-arity? (car body) 0))
       (instantiate::xml-delay
	  (id (xml-make-id id 'DELAY))
	  (thunk (car body)))
       (error '<DELAY> "Illegal delay's thunk" (car body))))

;*---------------------------------------------------------------------*/
;*    img-base64-encode ...                                            */
;*---------------------------------------------------------------------*/
(define (img-base64-encode src)
   (if (file-exists? src)
       (let ((p (open-input-file src)))
	  (if (input-port? p)
	      (unwind-protect
		 (format "data:~a;base64,~a"
			 (mime-type src (format "image/~a" (suffix src)))
			 (base64-encode (read-string p) -1))
		 (close-input-port p))
	      src))
       src))

;*---------------------------------------------------------------------*/
;*    onerror-img ...                                                  */
;*---------------------------------------------------------------------*/
(define (onerror-img attributes src)
   (let* ((val (format "if( !this.onhoperror ) { this.onhoperror = true; hop_deinline_image(this, ~s) }" src))
	  (onerror (when (pair? attributes) (assq 'onerror attributes)))
	  (oval (when (pair? onerror) (cdr onerror))))
      (cond
	 ((string? oval)
	  (set-cdr! onerror (string-append oval "; " val))
	  attributes)
	 ((xml-tilde? oval)
	  (let ((tilde (string->tilde val)))
	     (set-cdr! onerror (tilde-compose oval tilde)))
	  attributes)
	 (else
	  (cons `(onerror . ,val) attributes)))))

;*---------------------------------------------------------------------*/
;*    IMG ...                                                          */
;*---------------------------------------------------------------------*/
(define-xml-compound <IMG> ((id #unspecified string)
			    (inline #f boolean)
			    (alt #f)
			    (src #unspecified string)
			    (attributes)
			    body)
   
   (if (not (string? src))
       (error '<IMG> "Illegal image src" src)
       (let ((src (if inline (img-base64-encode src) src))
	     (attrs (if (eq? inline #t)
			(onerror-img attributes src)
			attributes)))
	  (instantiate::xml-empty-element
	     (markup 'img)
	     (id (xml-make-id id 'img))
	     (attributes (cons* `(src . ,src)
				`(alt . ,(or alt src))
				attrs))
	     (body '())))))

;*---------------------------------------------------------------------*/
;*    string->tilde ...                                                */
;*---------------------------------------------------------------------*/
(define (string->tilde string)
   (instantiate::xml-tilde
      (body string)))

;*---------------------------------------------------------------------*/
;*    tilde->string ...                                                */
;*---------------------------------------------------------------------*/
(define (tilde->string t)
   (with-access::xml-tilde t (body)
      body))

;*---------------------------------------------------------------------*/
;*    tilde-compose ...                                                */
;*---------------------------------------------------------------------*/
(define (tilde-compose t1 t2)
   (instantiate::xml-tilde
      (body (string-append (xml-tilde-body t1) "\n" (xml-tilde-body t2)))))

;*---------------------------------------------------------------------*/
;*    tilde-make-thunk ...                                             */
;*---------------------------------------------------------------------*/
(define (tilde-make-thunk t)
   (instantiate::xml-tilde
      (body (string-append "function() { return " (xml-tilde-body t) "}"))))

;*---------------------------------------------------------------------*/
;*    xml-write-tilde-as-expression ...                                */
;*---------------------------------------------------------------------*/
(define (xml-write-tilde-as-expression t::xml-tilde p::output-port)
   (let* ((t (tilde->string t))
	  (l (string-length t)))
      (let loop ((i (-fx l 1)))
	 (if (<fx i 0)
	     ;; the tilde expression is empty!
	     #unspecified
	     (let ((c (string-ref t i)))
		(case c
		   ((#\Newline #\Return #\Space #\Tab)
		    ;; skip the trailing space and newline
		    (loop (-fx i 1)))
		   ((#\;)
		    ;; a statement
		    (display "(function(){ return " p)
		    (display t p)
		    (display "})()" p))
		   ((#\})
		    ;; a block
		    (display "(function()" p)
		    (display t p)
		    (display ")()" p))
		   (else
		    ;; a regular expression
		    (display t p))))))))
      
;*---------------------------------------------------------------------*/
;*    xml-write-initializer ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (xml-write-initializer obj p)
   (cond
      ((string? obj)
       (write obj p))
      (else
       (display obj p))))

;*---------------------------------------------------------------------*/
;*    xml-write-initializer ::xml-tilde ...                            */
;*---------------------------------------------------------------------*/
(define-method (xml-write-initializer obj::xml-tilde p)
   (display "(function(){return " p)
   (display (xml-attribute-encode (xml-tilde-body obj)) p)
   (display "})()" p))
   
