;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/xml.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  8 05:43:46 2004                          */
;*    Last change :  Wed Oct 20 09:26:47 2010 (serrano)                */
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
	    __hop_xml-types
	    __hop_mime
	    __hop_misc
	    __hop_param
	    __hop_configure
	    __hop_clientc
	    __hop_priv
	    __hop_read-js
	    __hop_http-error)

   (use     __hop_js-lib)

   (export  (xml-constructor-add! ::symbol ::procedure)
	    (%make-xml-element ::symbol ::pair-nil)

	    (xml-markup-is? ::obj ::symbol)

	    (xml-make-id::bstring #!optional id tag)

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
	    (xml-tilde->attribute::bstring ::xml-tilde)

	    (xml-tilde->sexp ::xml-tilde)
	    (sexp->xml-tilde::xml-tilde expr)

	    (<TILDE> ::obj #!key src loc)
	    (<DELAY> . ::obj)))

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
	     (error "hop-xhtml-xmlns" "Illegal namespaces" v))))))
				   
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
       (error "hop-get-xml-backend" "Illegal backend" id))))

;*---------------------------------------------------------------------*/
;*    *xml-constructors* ...                                           */
;*---------------------------------------------------------------------*/
(define *xml-constructors* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    xml-constructor-add! ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-constructor-add! id proc)
   (if (not (correct-arity? proc 1))
       (error "xml-constructor-add!" "Illegal constructor" proc)
       (hashtable-put! *xml-constructors* id proc)))

;*---------------------------------------------------------------------*/
;*    %xml-constructor ...                                             */
;*---------------------------------------------------------------------*/
(define-method (%xml-constructor o::xml-markup)
   (call-next-method)
   (with-access::xml-markup o (body tag)
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
   
   (define (symbol-upcase s)
      (string->symbol (string-upcase! (symbol->string s))))
   
   (let loop ((a args)
	      (attr '())
	      (body '())
	      (id #unspecified))
      (cond
	 ((null? a)
	  (instantiate::xml-element
	     (tag el)
	     (attributes (reverse! attr))
	     (id (xml-make-id id))
	     (body (reverse! body))))
	 ((keyword? (car a))
	  (cond
	     ((not (pair? (cdr a)))
	      (error (string-append "<" (string-upcase (symbol->string! el)) ">")
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
	      (error (string-append "<" (string-upcase (symbol->string! el)) ">")
		     "Illegal arguments"
		     `(,(string-append "<" (string-upcase (symbol->string! el)) ">")
		       ,@args))
	      (loop (append (car a) (cdr a)) attr body id)))
	 (else
	  (loop (cdr a) attr (cons (car a) body) id)))))

;*---------------------------------------------------------------------*/
;*    xml-markup-is? ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-markup-is? o tag)
   (and (xml-markup? o) (eq? (xml-markup-tag o) tag)))

;*---------------------------------------------------------------------*/
;*    id-count ...                                                     */
;*---------------------------------------------------------------------*/
(define id-count
   0)

;*---------------------------------------------------------------------*/
;*    xml-make-id ...                                                  */
;*---------------------------------------------------------------------*/
(define (xml-make-id #!optional id tag)
   (if (string? id)
       id
       ;; CARE, MS 24 apr 2010 !!!
       ;; I don't think we need a lock here because
       ;; I don't think it's important to have a really unique ID generator.
       ;; I think we are only interested in uniqueness inside a single
       ;; thread. If two threads call xml-make-id in parallel it's unlikely
       ;; that they are constructing a shared tree.
       (let ((n (fixnum->string id-count)))
	  (set! id-count (+fx 1 id-count))
	  (cond
	     ((symbol? id)
	      (string-append (symbol->string! id) n))
	     ((symbol? tag)
	      (string-append (symbol->string! tag) n))
	     (else
	      (string-append "_" n))))))

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
       (with-access::xml-backend backend (security)
	  (if (security-manager? security)
	      (with-access::security-manager security (string-sanitize)
		 (let ((s (string-sanitize obj)))
		    (when (string? s)
		       (display s p))))
	      (display obj p))))
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
       (error "xml-write" "Illegal xml object" obj))))

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
   (with-access::xml-cdata obj (tag body attributes)
      (with-access::xml-backend backend (cdata-start cdata-stop)
	 (display "<" p)
	 (display tag p)
	 (xml-write-attributes attributes p backend)
	 (display ">" p)
	 (unless (or (not body) (null? body))
	    (when cdata-start (display cdata-start p))
	    (xml-write body p backend)
	    (when cdata-stop (display cdata-stop p)))
	 (display "</" p)
	 (display tag p)
	 (display ">\n" p))))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-tilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-tilde p backend)
   (with-access::xml-tilde obj (body parent)
      (if (and (xml-markup? parent) (eq? (xml-markup-tag parent) 'script))
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
   (with-access::xml-markup obj (tag attributes body)
      (display "<" p)
      (display tag p)
      (xml-write-attributes attributes p backend)
      (cond
	 ((and (eq? tag 'head) (>=fx (hop-security) 3))
	  (display ">" p)
	  (for-each (lambda (b) (xml-write b p backend)) body)
	  (with-access::xml-backend backend (security)
	     (when (security-manager? security)
		(for-each (lambda (r)
			     (display "<script type='" p)
			     (display (hop-javascript-mime-type) p)
			     (fprintf p "' src='~a'>" r)
			     (display "</script>\n" p))
			  (security-manager-runtime security))))
	  (display "</" p)
	  (display tag p)
	  (display ">\n" p))
	 ((or (pair? body) (eq? tag 'script))
	  (display ">" p)
	  (for-each (lambda (b) (xml-write b p backend)) body)
	  (display "</" p)
	  (display tag p)
	  (display ">\n" p))
	 ((memq tag (xml-backend-no-end-tags-elements backend))
	  (display ">\n" p))
	 (else
	  (display "/>" p)))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-meta ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-meta p backend)
   (with-access::xml-meta obj (tag attributes body)
      (display "<" p)
      (display tag p)
      (xml-write-attributes attributes p backend)
      (if (pair? (plist-assq :content attributes))
	  (display ">" p)
	  (with-access::xml-backend backend (meta-format mime-type)
	     (fprintf p meta-format mime-type (hop-charset))))
      (newline p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-element ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-element p backend)
   (with-access::xml-element obj (tag id attributes body)
      (cond
	 ((and (null? body) (null? attributes))
	  (display "<" p)
	  (display tag p)
	  (display " id='" p)
	  (display id p)
	  (display "'" p)
	  (if (xml-backend-abbrev-emptyp backend)
	      (display "/>" p)
	      (begin
		 (display "></" p)
		 (display tag p)
		 (display ">" p))))
	 ((null? body)
	  (display "<" p)
	  (display tag p)
	  (display " id='" p)
	  (display id p)
	  (display "'" p)
	  (xml-write-attributes attributes p backend)
	  (cond
	     ((xml-backend-abbrev-emptyp backend)
	      (display "/>" p))
	     ((memq tag (xml-backend-no-end-tags-elements backend))
	      (display ">" p))
	     (else
	      (display ">" p)
	      (display "</" p)
	      (display tag p)
	      (display ">" p))))
	 (else
	  (display "<" p)
	  (display tag p)
	  (display " id='" p)
	  (display id p)
	  (display "'" p)
	  (xml-write-attributes attributes p backend)
	  (display ">" p)
	  (for-each (lambda (b) (xml-write b p backend)) body)
	  (display "</" p)
	  (display tag p)
	  (display ">" p)))
      (xml-write-initializations obj p backend)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-empty-element ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-empty-element p backend)
   (with-access::xml-empty-element obj (tag id attributes)
      (display "<" p)
      (display tag p)
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
   (if (>=fx (hop-clientc-debug-unbound) 1)
       (xml-write-html/unbound-check obj p backend)
       (xml-write-html obj p backend)))

;*---------------------------------------------------------------------*/
;*    xml-write-html/unbound-check ...                                 */
;*---------------------------------------------------------------------*/
(define (xml-write-html/unbound-check obj::xml-html p backend)
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
      (with-access::xml-html obj (tag attributes body)
	 (display "<" p)
	 (display tag p)
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
	 (display tag p)
	 (display ">\n" p))))

;*---------------------------------------------------------------------*/
;*    xml-write-attributes ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-write-attributes attr p backend)
   (let loop ((a attr))
      (when (pair? a)
	 (display " " p)
	 (unless (pair? (cdr a))
	    (error "xml-write-attributes" "Illegal attributes" attr))
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
	      (error "xml"
		     "Illegal procedure argument in XML attribute"
		     id)))
	 ((xml-backend-security backend)
	  =>
	  (lambda (sm)
	     (if (security-manager? sm)
		 (let ((a (xml-attribute-encode attr)))
		    (display ((security-manager-attribute-sanitize sm) a id) p))
		 (error "xml-write-attribute" "Illegal security-manager" sm))))
	 (else
	  (display (xml-attribute-encode attr) p)))
      (display "'" p)))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::xml-tilde ...                              */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::xml-tilde id p backend)
   (when (xml-event-handler-attribute? id)
      (display (keyword->string! id) p)
      (display "='" p)
      (let ((sm (xml-backend-security backend)))
	 (if (security-manager? sm)
	     (display ((security-manager-attribute-sanitize sm) attr id) p)
	     (display (xml-tilde->attribute attr) p)))
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
	  (tag 'head)
	  (attributes attributes)
	  (body body)))
      (else
       (let ((a (append-map (lambda (a)
			       (list (symbol->keyword (car a)) (cdr a)))
			    attributes))
	     (constr (eval constr)))
	  (if (procedure? constr)
	      (apply constr (append a body))
	      (error "string->xml" "Illegal markup" constr))))))

;*---------------------------------------------------------------------*/
;*    string->html ...                                                 */
;*---------------------------------------------------------------------*/
(define (string->html h)
   (with-input-from-string h
      (lambda ()
	 (html-parse
	  (current-input-port)
	  :content-length 0
	  :procedure (lambda (tag attributes body)
			(let ((constr (string->symbol
				       (string-append
					"<"
					(string-upcase (symbol->string! tag))
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
	  :procedure (lambda (tag attributes body)
			(let ((constr (string->symbol
				       (string-append
					"<"
					(string-upcase (symbol->string! tag))
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
      `(pragma ,(call-with-output-string
		 (lambda (op)
		    (obj->javascript o op #f)))))
   
   (with-access::xml-tilde obj (body)
      ((clientc-precompiled->sexp (hop-clientc)) body wrapper)))

;*---------------------------------------------------------------------*/
;*    sexp->xml-tilde ...                                              */
;*---------------------------------------------------------------------*/
(define (sexp->xml-tilde obj)
   (let ((c ((clientc-sexp->precompiled (hop-clientc)) obj)))
      (<TILDE> c :src obj)))

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
	  (id (xml-make-id id))
	  (thunk (car body)))
       (error "<DELAY>" "Illegal delay's thunk" (car body))))

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
   (with-access::xml-markup obj (tag body attributes)
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
