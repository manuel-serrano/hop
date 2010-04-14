;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/xml.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  8 05:43:46 2004                          */
;*    Last change :  Wed Apr 14 10:41:46 2010 (serrano)                */
;*    Copyright   :  2004-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple XML producer/writer for HOP.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_xml

   (library  web)
   
   (include "param.sch"
	    "xml.sch")

   (import  __hop_types
	    __hop_mime
	    __hop_misc
	    __hop_param
	    __hop_configure
	    __hop_css
	    __hop_clientc
	    __hop_priv
	    __hop_read-js
	    __hop_http-error)

   (use     __hop_js-lib)

   (export  (class xml-backend
	      (id::symbol read-only)
	      (mime-type::bstring read-only)
	      (doctype::bstring read-only)
	      (header-format::bstring read-only)
	      (html-attributes::pair-nil read-only (default '()))
	      (no-end-tags-elements::pair-nil read-only (default '()))
	      (cdata-start (default #f))
	      (cdata-stop (default #f))
	      (css-start (default #f))
	      (css-stop (default #f))
	      (meta-format::bstring read-only)
	      (abbrev-emptyp::bool (default #f)))

	    (class xml
	       (%xml-constructor))

	    (class xml-verbatim::xml
	       (body::bstring read-only))
	    
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
	       body::pair-nil)

	    (class xml-html::xml-markup)

	    (class xml-document::xml-markup
	       (id read-only)
	       (%idtable read-only (default (make-hashtable))))

	    (class xml-element::xml-markup
	       (id read-only (default #unspecified))
	       (parent (default #unspecified)))

	    (class xml-empty-element::xml-element)

	    (class xml-cdata::xml-element)
	    
	    (class xml-tilde::xml
	       (body read-only)
	       (parent (default #unspecified))
	       (src read-only (default #f))
	       (loc read-only (default #f))
	       (%js-expression (default #f))
	       (%js-statement (default #f))
	       (%js-return (default #f))
	       (%js-attribute (default #f)))

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

 	    (generic xml-write ::obj ::output-port ::xml-backend)
	    (generic xml-write-attribute ::obj ::obj ::output-port ::xml-backend)
	    (generic xml-write-expression ::obj ::output-port)
	    (xml-write-attributes ::pair-nil ::output-port ::xml-backend)

	    (xml->string ::obj ::xml-backend)
	    
	    (string->html ::bstring)
	    (string->xml ::bstring)

	    (xml-tilde->expression::bstring ::xml-tilde)
	    (xml-tilde->statement::bstring ::xml-tilde)
	    (xml-tilde->return::bstring ::xml-tilde)

	    (xml-tilde->sexp ::xml-tilde)
	    (sexp->xml-tilde::xml-tilde expr)

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
	    (<SOURCE> . ::obj)
	    (<SPAN> . ::obj)
	    (<STRIKE> . ::obj)
	    (<STRONG> . ::obj)
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

	    (<TILDE> ::obj #!key src loc)
	    (<DELAY> . ::obj)))

;*---------------------------------------------------------------------*/
;*    object-print ::xml-element ...                                   */
;*    -------------------------------------------------------------    */
;*    Because of their parent slot xml-element are cyclic and cannot   */
;*    thus be display as is.                                           */
;*---------------------------------------------------------------------*/
(define-method (object-print o::xml-element p print-slot)
   (with-access::xml-element o (markup attributes body id)
      (display "#|xml-element markup=" p)
      (print-slot markup p)
      (display " id=" p)
      (print-slot id p)
      (display " parent=..." p)
      (display " attributes=" p)
      (print-slot attributes p)
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
   '(:xmlns "http://www.w3.org/1999/xhtml"
     :xmlns:svg "http://www.w3.org/2000/svg")
   (lambda (v)
      (let loop ((l v))
	 (cond
	    ((null? l)
	     v)
	    ((and (keyword? (car l)) (pair? (cdr l)) (string? (cadr l)))
	     (loop (cddr l)))
	    (else
	     (error 'hop-xhtml-xmlns "Illegal namespaces" v))))))
				   
;*---------------------------------------------------------------------*/
;*    *html-backend* ...                                               */
;*---------------------------------------------------------------------*/
(define *html-4.01-backend*
   (instantiate::xml-backend
      (id 'html-4.01)
      (mime-type "text/html")
      (doctype "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Strict//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
      (html-attributes '())
      (header-format "")
      (no-end-tags-elements '(link))
      ;; the meta-format contains the closing >
      (meta-format " content=\"~a; charset=~a\">")))

;*---------------------------------------------------------------------*/
;*    *html5-backend* ...                                              */
;*---------------------------------------------------------------------*/
(define *html5-backend*
   (instantiate::xml-backend
      (id 'html-5)
      (mime-type "text/html")
      (doctype "<!DOCTYPE html>")
      (html-attributes '())
      (header-format "")
      (no-end-tags-elements '(link))
      ;; the meta-format contains the closing >
      (meta-format " content=\"~a; charset=~a\">")))

;*---------------------------------------------------------------------*/
;*    *xhtml-backend* ...                                              */
;*---------------------------------------------------------------------*/
(define *xhtml-backend*
   (instantiate::xml-backend
      (id 'xhtml-1.0)
      (mime-type "application/xhtml+xml")
;*       (doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">") */
      (doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN\" \"http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd\" [<!ENTITY nbsp \"&#160;\">]>")
      (html-attributes (hop-xhtml-xmlns))
      (header-format "<?xml version=\"1.0\" encoding=\"~a\"?>\n")
      (no-end-tags-elements '())
      ;; XHTML scripts have to be protected
      (cdata-start "\n<![CDATA[\n")
      (cdata-stop "]]>\n")
      ;; the meta-format contains the closing />
      (meta-format " content=\"~a; charset=~a\"/>")))

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
      ((html5 html-5)
       *html5-backend*)
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
	      (body '())
	      (id #unspecified))
      (cond
	 ((null? a)
	  (instantiate::xml-element
	     (markup (string->symbol (string-downcase (symbol->string el))))
	     (attributes (reverse! attr))
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
		  (loop (cddr a) attr body (cadr a))
		  (bigloo-type-error el "string" (cadr a))))
	     (else
	      (loop (cddr a) (cons* (cadr a) (car a) attr) body id))))
	 ((null? (car a))
	  (loop (cdr a) attr body id))
	 ((pair? (car a))
	  (if (not (and (or (null? (cdr a)) (pair? (cdr a))) (list? (car a))))
	      (error (symbol-append '< el '>)
		     "Illegal arguments"
		     `(,(symbol-append '< el '>) ,@args))
	      (loop (append (car a) (cdr a)) attr body id)))
	 (else
	  (loop (cdr a) attr (cons (car a) body) id)))))

;*---------------------------------------------------------------------*/
;*    xml-markup-is? ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-markup-is? o markup)
   (and (xml-markup? o) (eq? (xml-markup-markup o) markup)))

;*---------------------------------------------------------------------*/
;*    xml-make-id ...                                                  */
;*---------------------------------------------------------------------*/
(define (xml-make-id #!optional id (markup 'HOP))
   (cond
      ((string? id)
       id)
      ((=fx (bigloo-debug) 0)
       (symbol->string (gensym)))
      ((symbol? id)
       (symbol->string (gensym id)))
      (else
       (symbol->string (gensym markup)))))

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
(define-generic (xml-write obj p backend)
   (cond
      ((string? obj)
       (display obj p))
      ((number? obj)
       (display obj p))
      ((symbol? obj)
       ;; don't display symbols otherwise inner defines generate HTML codes!
       #unspecified)
      ((pair? obj)
       (for-each (lambda (o) (xml-write o p backend)) obj))
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
;*    xml-write ::xml-verbatim ...                                     */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-verbatim p backend)
   (with-access::xml-verbatim obj (body)
      (display body p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-if ...                                           */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-if p backend)
   (with-access::xml-if obj (test then otherwise)
      (if (test)
	  (xml-write then p backend)
	  (xml-write otherwise p backend))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-cdata ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-cdata p backend)
   (with-access::xml-cdata obj (markup body attributes)
      (with-access::xml-backend backend (cdata-start cdata-stop)
	 (display "<" p)
	 (display markup p)
	 (xml-write-attributes attributes p backend)
	 (display ">" p)
	 (unless (or (not body) (null? body))
	    (when cdata-start (display cdata-start p))
	    (xml-write body p backend)
	    (when cdata-stop (display cdata-stop p)))
	 (display "</" p)
	 (display markup p)
	 (display ">\n" p))))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-tilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-tilde p backend)
   (with-access::xml-tilde obj (body parent)
      (if (and (xml-markup? parent) (eq? (xml-markup-markup parent) 'script))
	  (xml-write (xml-tilde->statement obj) p backend)
	  (with-access::xml-backend backend (cdata-start cdata-stop)
	     (display "<script type='" p)
	     (display (hop-javascript-mime-type) p)
	     (display "'>" p)
	     (when cdata-start (display cdata-start p))
	     (display (xml-tilde->statement obj) p)
	     (when cdata-stop (display cdata-stop p))
	     (display "</script>\n" p)))))
      
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-delay ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-delay p backend)
   (with-access::xml-delay obj (thunk)
      (xml-write (thunk) p backend)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-markup ...                                       */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-markup p backend)
   (with-access::xml-markup obj (markup attributes body)
      (display "<" p)
      (display markup p)
      (xml-write-attributes attributes p backend)
      (cond
	 ((or (pair? body) (eq? markup 'script))
	  (display ">" p)
	  (for-each (lambda (b) (xml-write b p backend)) body)
	  (display "</" p)
	  (display markup p)
	  (display ">\n" p))
	 ((memq markup (xml-backend-no-end-tags-elements backend))
	  (display ">\n" p))
	 (else
	  (display "/>" p)))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-meta ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-meta p backend)
   (with-access::xml-meta obj (markup attributes body)
      (display "<" p)
      (display markup p)
      (xml-write-attributes attributes p backend)
      (with-access::xml-backend backend (meta-format mime-type)
	 (fprintf p meta-format mime-type (hop-charset)))
      (newline p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-element ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-element p backend)
   (with-access::xml-element obj (markup id attributes body)
      (cond
	 ((and (null? body) (null? attributes))
	  (display "<" p)
	  (display markup p)
	  (display " id='" p)
	  (display id p)
	  (display "'" p)
	  (if (xml-backend-abbrev-emptyp backend)
	      (display "/>" p)
	      (begin
		 (display "></" p)
		 (display markup p)
		 (display ">" p))))
	 ((null? body)
	  (display "<" p)
	  (display markup p)
	  (display " id='" p)
	  (display id p)
	  (display "'" p)
	  (xml-write-attributes attributes p backend)
	  (cond
	     ((xml-backend-abbrev-emptyp backend)
	      (display "/>" p))
	     ((memq markup (xml-backend-no-end-tags-elements backend))
	      (display ">" p))
	     (else
	      (display ">" p)
	      (display "</" p)
	      (display markup p)
	      (display ">" p))))
	 (else
	  (display "<" p)
	  (display markup p)
	  (display " id='" p)
	  (display id p)
	  (display "'" p)
	  (xml-write-attributes attributes p backend)
	  (display ">" p)
	  (for-each (lambda (b) (xml-write b p backend)) body)
	  (display "</" p)
	  (display markup p)
	  (display ">" p)))
      (xml-write-initializations obj p backend)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-empty-element ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-empty-element p backend)
   (with-access::xml-empty-element obj (markup id attributes)
      (display "<" p)
      (display markup p)
      (display " id='" p)
      (display id p)
      (display "'" p)
      (xml-write-attributes attributes p backend)
      (display "/>" p)
      (xml-write-initializations obj p backend)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-html ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-html p backend)
   (if (>fx (bigloo-debug) 0)
       (xml-write-html-debug obj p backend)
       (xml-write-html obj p backend)))

;*---------------------------------------------------------------------*/
;*    xml-write-html-debug ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-write-html-debug obj::xml-html p backend)
   (with-access::xml-html obj (body)
      ;; check the unbound variables
      (let ((env (make-hashtable)))
	 (xml-tilde-unbound body env)
	 (let* ((l (hashtable-map env (lambda (k v)
					 (when v (cons k v)))))
		(lf (let loop ((x l))
		       (when (pair? x) (or (car x) (loop (cdr x)))))))
	    (if (pair? lf)
		;; we got at least one
		(with-handler
		   (lambda (e)
		      (exception-notify e)
		      (let* ((m (if (epair? (cdr lf))
				    (match-case (cer (cdr lf))
				       ((at ?file . ?-)
					(format "~a: unbound variable" file))
				       (else
					"Unbound variables"))
				    "Unbound variables"))
			     (r (http-internal-error e m)))
			 (xml-write-html (http-response-hop-xml r) p backend)))
		   (error/source '<HTML>
				 (format "Unbound client-side variable: ~a"
					 (car lf))
				 (cdr lf)
				 (cdr lf)))
		;; everything is fine
		(xml-write-html obj p backend))))))
       
;*---------------------------------------------------------------------*/
;*    xml-write-html ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-write-html obj::xml-html p backend)	    
   (with-access::xml-backend backend (header-format doctype html-attributes)
      (fprintf p header-format (hop-charset))
      (display doctype p)
      (newline p)
      (with-access::xml-html obj (markup attributes body)
	 (display "<" p)
	 (display markup p)
	 (let ((hattr (let loop ((hattr html-attributes))
			 (cond
			    ((null? hattr)
			     '())
			    ((plist-assq (car hattr) attributes)
			     (loop (cddr hattr)))
			    (else
			     (cons* (car hattr)
				    (cadr hattr)
				    (loop (cddr hattr))))))))
	    (xml-write-attributes hattr p backend))
	 (xml-write-attributes attributes p backend)
	 (display ">\n" p)
	 (for-each (lambda (b) (xml-write b p backend)) body)
	 (display "</" p)
	 (display markup p)
	 (display ">\n" p))))

;*---------------------------------------------------------------------*/
;*    xml-write-attributes ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-write-attributes attr p backend)
   (let loop ((a attr))
      (when (pair? a)
	 (display " " p)
	 (unless (pair? (cdr a))
	    (error 'xml-write-attributes "Illegal attributes" attr))
	 (xml-write-attribute (cadr a) (car a) p backend)
	 (loop (cddr a)))))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::obj ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (xml-write-attribute attr::obj id p backend)
   ;; boolean false attribute has no value, xml-tilde are initialized
   (when attr
      (display (keyword->string! id) p)
      ;; boolean true attribute has no value
      (display "='" p)
      (cond
	 ((eq? attr #t)
	  (display (keyword->string! id) p))
	 ((procedure? attr)
	  (if (hop-service? (procedure-attr attr))
	      (display (hop-service-path (procedure-attr attr)) p)
	      (error 'xml "Illegal procedure argument in XML attribute" id)))
	 ((and (>fx (hop-security) 1) (string? attr) (attr-event-handler? id))
	  (raise
	   (instantiate::&hop-injection-error
	      (proc id)
	      (msg "Illegal handler attribute value type")
	      (obj attr))))
	 (else
	  (display (xml-attribute-encode attr) p)))
      (display "'" p)))

;*---------------------------------------------------------------------*/
;*    attr-event-handler? ...                                          */
;*---------------------------------------------------------------------*/
(define (attr-event-handler? id)
   (string-prefix-ci? "on" (keyword->string! id)))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::xml-tilde ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::xml-tilde id p backend)
   (when (xml-event-handler-attribute? id)
      (display (keyword->string! id) p)
      (display "='" p)
      (display (xml-tilde->attribute attr) p)
      (display "'" p)))
   
;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::hop-service ...                            */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::hop-service id p backend)
   (display (keyword->string! id) p)
   (display "='" p)
   (display (hop-service-path attr) p)
   (display "'" p))

;*---------------------------------------------------------------------*/
;*    xml-write-initializations ...                                    */
;*---------------------------------------------------------------------*/
(define (xml-write-initializations obj p backend)
   (with-access::xml-element obj (id attributes)
      (with-access::xml-backend backend (cdata-start cdata-stop)
	 (let loop ((attrs attributes)
		    (var #f))
	    (cond
	       ((null? attrs)
		(when var
		   (when cdata-stop (display cdata-stop p))
		   (display "</script>\n" p)))
	       ((and (xml-tilde? (cadr attrs))
		     (not (xml-event-handler-attribute? (car attrs))))
		(if var
		    (begin
		       (xml-write-initialization (car attrs) (cadr attrs) var p)
		       (newline p)
		       (loop (cddr attrs) var))
		    (let ((var (gensym)))
		       (display "<script type='" p)
		       (display (hop-javascript-mime-type) p)
		       (display "'>" p)
		       (when cdata-start (display cdata-start p))
		       (display "var " p)
		       (display var p)
		       (display " = document.getElementById( \"" p)
		       (display id p)
		       (display "\" );" p)
		       (loop attrs var))))
	       (else
		(loop (cddr attrs) var)))))))

;*---------------------------------------------------------------------*/
;*    xml-write-initialization ...                                     */
;*---------------------------------------------------------------------*/
(define (xml-write-initialization id tilde var p)
   (if (eq? id :style)
       (xml-write-style-initialization tilde var p)
       (begin
	  (display var p)
	  (display "[\"" p)
	  (display (if (eq? id :class) "className" (keyword->string! id)) p)
	  (display "\"]=" p)
	  (xml-write-expression tilde p)
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
   (xml-write-expression tilde p)
   (display ");" p))
   
;*---------------------------------------------------------------------*/
;*    xml->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (xml->string obj backend)
   (with-output-to-string
      (lambda ()
	 (xml-write obj (current-output-port) backend))))

;*---------------------------------------------------------------------*/
;*    eval-markup ...                                                  */
;*---------------------------------------------------------------------*/
(define (eval-markup constr attributes body)
   (case constr
      ((<HEAD>)
       ;; The Hop head constructor is special because it implicitly introduces
       ;; the Hop rts. In order to avoid this here, head is explicitly created.
       (instantiate::xml-markup
	  (markup 'head)
	  (attributes attributes)
	  (body body)))
      (else
       (let ((a (append-map (lambda (a)
			       (list (symbol->keyword (car a)) (cdr a)))
			    attributes))
	     (constr (eval constr)))
	  (if (procedure? constr)
	      (apply constr (append a body))
	      (error 'string->xml "Illegal markup" constr))))))

;*---------------------------------------------------------------------*/
;*    string->html ...                                                 */
;*---------------------------------------------------------------------*/
(define (string->html h)
   (with-input-from-string h
      (lambda ()
	 (html-parse
	  (current-input-port)
	  :content-length 0
	  :procedure (lambda (markup attributes body)
			(let ((constr (string->symbol
				       (string-append
					"<"
					(string-upcase (symbol->string markup))
					">"))))
			   (eval-markup constr attributes body)))))))

;*---------------------------------------------------------------------*/
;*    string->xml ...                                                  */
;*---------------------------------------------------------------------*/
(define (string->xml h)
   (with-input-from-string h
      (lambda ()
	 (xml-parse
	  (current-input-port)
	  :content-length 0
	  :procedure (lambda (markup attributes body)
			(let ((constr (string->symbol
				       (string-append
					"<"
					(string-upcase (symbol->string markup))
					">"))))
			   (eval-markup constr attributes body)))))))

;*---------------------------------------------------------------------*/
;*    xml-tilde->expression ...                                        */
;*---------------------------------------------------------------------*/
(define (xml-tilde->expression::bstring obj)
   (with-access::xml-tilde obj (%js-expression body)
      (when (not (string? %js-expression))
	 (set! %js-expression
	       ((clientc-precompiled->JS-expression (hop-clientc)) body)))
      %js-expression))

;*---------------------------------------------------------------------*/
;*    xml-tilde->statement ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-tilde->statement::bstring obj)
   (with-access::xml-tilde obj (%js-statement body)
      (when (not (string? %js-statement))
	 (set! %js-statement
	       ((clientc-precompiled->JS-statement (hop-clientc)) body)))
      %js-statement))

;*---------------------------------------------------------------------*/
;*    xml-tilde->return ...                                            */
;*---------------------------------------------------------------------*/
(define (xml-tilde->return::bstring obj)
   (with-access::xml-tilde obj (%js-return body)
      (when (not (string? %js-return))
	 (set! %js-return
	       ((clientc-precompiled->JS-return (hop-clientc)) body)))
      %js-return))

;*---------------------------------------------------------------------*/
;*    xml-tilde->attribute ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-tilde->attribute obj)
   (with-access::xml-tilde obj (%js-attribute)
      (if (string? %js-attribute)
	  %js-attribute
	  (let ((js-attr (xml-attribute-encode (xml-tilde->statement obj))))
	     (set! %js-attribute js-attr)
	     js-attr))))

;*---------------------------------------------------------------------*/
;*    xml-tilde->sexp ...                                              */
;*---------------------------------------------------------------------*/
(define (xml-tilde->sexp obj)
   
   (define (wrapper o)
      `(pragma ,(hop->javascript o #f)))
   
   (with-access::xml-tilde obj (body)
      ((clientc-precompiled->sexp (hop-clientc)) body wrapper)))

;*---------------------------------------------------------------------*/
;*    sexp->xml-tilde ...                                              */
;*---------------------------------------------------------------------*/
(define (sexp->xml-tilde obj)
   (let ((c ((clientc-sexp->precompiled (hop-clientc)) obj)))
      (<TILDE> c :src obj)))

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
(define-xml-element <SOURCE>)
(define-xml-element <SPAN>)
(define-xml-element <STRIKE>)
(define-xml-element <STRONG>)
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
;*    <FORM> ...                                                       */
;*---------------------------------------------------------------------*/
(define-markup <FORM> ((id #unspecified string)
		       (onsubmit #f)
		       (onreset #f)
		       (action #f)
		       (attrs)
		       body)
   (let* ((attrs (cond
		    ((xml-tilde? onsubmit)
		     `(:onsubmit ,(xml-tilde->return onsubmit) ,@attrs))
		    (onsubmit
		     `(:onsubmit ,onsubmit ,@attrs))
		    (else
		     attrs)))
	  (attrs (cond
		    ((xml-tilde? onreset)
		     `(:onreset ,(xml-tilde->return onreset) ,@attrs))
		    (onreset
		     `(:onreset ,onreset ,@attrs))
		    (else
		     attrs)))
	  (attrs (cond
		    ((xml-tilde? action)
		     `(:action ,(format "javascript: ~a"
					(xml-tilde->statement action))
			       ,@attrs))
		    (action
		     `(:action ,action ,@attrs))
		    (else
		     attrs))))
      (instantiate::xml-element
	 (markup 'form)
	 (id (xml-make-id id 'FORM))
	 (attributes attrs)
	 (body body))))

;*---------------------------------------------------------------------*/
;*    <TILDE> ...                                                      */
;*---------------------------------------------------------------------*/
(define (<TILDE> body #!key src loc)
   (instantiate::xml-tilde
      (body body)
      (src src)
      (loc loc)))

;*---------------------------------------------------------------------*/
;*    <DELAY> ...                                                      */
;*---------------------------------------------------------------------*/
(define-markup <DELAY> ((id #unspecified string)
			body)
   (if (and (pair? body)
	    (procedure? (car body))
	    (correct-arity? (car body) 0))
       (instantiate::xml-delay
	  (id (xml-make-id id 'DELAY))
	  (thunk (car body)))
       (error '<DELAY> "Illegal delay's thunk" (car body))))

;*---------------------------------------------------------------------*/
;*    xml-write-expression ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (xml-write-expression obj p)
   (cond
      ((string? obj)
       (display "'" p)
       (display (string-escape obj #\') p)
       (display "'" p))
      ((eq? obj #t)
       (display "true" p))
      ((eq? obj #f)
       (display "false" p))
      ((eq? obj #unspecified)
       (display "undefined" p))
      (else
       (display obj p))))

;*---------------------------------------------------------------------*/
;*    xml-write-expression ::xml-tilde ...                             */
;*---------------------------------------------------------------------*/
(define-method (xml-write-expression obj::xml-tilde p)
   (display (xml-tilde->expression obj) p))

;*---------------------------------------------------------------------*/
;*    xml-tilde-unbound ::obj ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (xml-tilde-unbound obj::obj env)
   (if (pair? obj)
       (for-each (lambda (x) (xml-tilde-unbound x env)) obj)
       '()))

;*---------------------------------------------------------------------*/
;*    xml-tilde-unbound ::xml-if ...                                   */
;*---------------------------------------------------------------------*/
(define-method (xml-tilde-unbound obj::xml-if env)
   (with-access::xml-if obj (test then otherwise)
      (xml-tilde-unbound test env)
      (xml-tilde-unbound then env)
      (xml-tilde-unbound otherwise env)))

;*---------------------------------------------------------------------*/
;*    xml-tilde-unbound ::xml-markup ...                               */
;*---------------------------------------------------------------------*/
(define-method (xml-tilde-unbound obj::xml-markup env)
   (with-access::xml-markup obj (markup body attributes)
      (xml-tilde-unbound body env)
      (let ((old (hashtable-get env 'event)))
	 (unless old (hashtable-put! env 'event #f))
	 (for-each (lambda (a)
		      (when (xml-tilde? a)
			 (xml-tilde-unbound a env)))
		   attributes)
	 (unless old (hashtable-remove! env 'event)))))

;*---------------------------------------------------------------------*/
;*    xml-tilde-unbound ::xml-tilde ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-tilde-unbound obj::xml-tilde env)
   (with-access::xml-tilde obj (body src)
      (for-each (lambda (v)
		   (let ((v (car v)))
		      (hashtable-update! env v (lambda (x) #f) #f)))
		((clientc-precompiled-declared-variables (hop-clientc)) body))
      (for-each (lambda (v)
		   (let ((v (car v)))
		      (hashtable-update! env v (lambda (x) #f) src)))
		((clientc-precompiled-free-variables (hop-clientc)) body))))
