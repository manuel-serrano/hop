;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/xml.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  8 05:43:46 2004                          */
;*    Last change :  Fri Dec 18 08:37:55 2015 (serrano)                */
;*    Copyright   :  2004-15 Manuel Serrano                            */
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
	    __hop_http-error
	    __hop_css
	    __hop_charset)

   (use     __hop_js-comp)

   (export  (xml-constructor-add! ::symbol ::procedure)
	    (%make-xml-element ::symbol ::pair-nil)

	    (xml-markup-is? ::obj ::symbol)

	    (xml-make-id::obj #!optional id tag)

	    (xml-event-handler-attribute?::bool ::keyword)
	    
	    (hop-get-xml-backend::xml-backend ::symbol)

	    (hop-xhtml-xmlns::pair-nil)
	    (hop-xhtml-xmlns-set! ::pair-nil)

	    (hop-xml-backend::xml-backend)
	    (hop-xml-backend-set! ::obj)

	    (xml-body ::obj)
	    (generic xml-body-element ::obj)
	    (generic xml-unpack ::obj)

 	    (generic xml-write ::obj ::output-port ::xml-backend)
	    (generic xml-write-attribute ::obj ::keyword ::output-port ::xml-backend)
	    (generic xml-write-expression ::obj ::output-port)
	    (xml-write-attributes ::pair-nil ::output-port ::xml-backend)
	    (generic xml-attribute-encode ::obj)

	    (generic xml-primitive-value ::obj)
	    (generic xml-to-errstring::bstring ::obj)

	    (xml->string ::obj ::xml-backend)

	    (parse-html ::input-port ::long)
	    (string->html ::bstring)
	    (string->xml ::bstring)

	    (xml-tilde->expression::bstring ::xml-tilde)
	    (xml-tilde->statement::bstring ::xml-tilde)
	    (xml-tilde->return::bstring ::xml-tilde)
	    (xml-tilde->attribute::bstring ::xml-tilde)

	    (xml-tilde->sexp ::xml-tilde)
	    (sexp->xml-tilde::xml-tilde expr #!optional env menv)

	    (<TILDE> ::obj #!key src loc env menv)
	    (<DELAY> . ::obj)
	    (<PRAGMA> . ::obj)))

;*---------------------------------------------------------------------*/
;*    object-serializer ::xml-markup ...                               */
;*    -------------------------------------------------------------    */
;*    WARNING: Module initialization prevents this declaration to be   */
;*    moved to xml_types!                                              */
;*---------------------------------------------------------------------*/
(register-class-serialization! xml-markup
   (lambda (o mode)
      (if (eq? mode 'hop-to-hop)
	  o
	  (let ((p (open-output-string)))
	     (obj->javascript-expr o p)
	     (close-output-port p))))
   (lambda (o)
      o))

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
      (doctype "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
;*       (doctype "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">") */
      (html-attributes '())
      (no-end-tags-elements '(link))
      (empty-end-tag #f)
      ;; the meta-format contains the closing >
      (meta-delimiter ">")))

;*---------------------------------------------------------------------*/
;*    *html5-backend* ...                                              */
;*---------------------------------------------------------------------*/
(define *html5-backend*
   (instantiate::xml-backend
      (id 'html-5)
      (mime-type "text/html")
      (doctype "<!DOCTYPE html>")
      (html-attributes '())
      (no-end-tags-elements '(link))
      ;; the meta-format contains the closing >
      (meta-delimiter ">")))

;*---------------------------------------------------------------------*/
;*    *xhtml-backend* ...                                              */
;*---------------------------------------------------------------------*/
(define *xhtml-backend*
   (instantiate::xml-backend
      (id 'xhtml-1.0)
      (mime-type "application/xhtml+xml")
      (doctype "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN\" \"http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd\" [<!ENTITY nbsp \"&#160;\"><!ENTITY OverBar \"&#xaf;\"><!ENTITY OverBrace \"&#xFE37;\">]>")
      (html-attributes (hop-xhtml-xmlns))
      (header-format "<?xml version=\"1.0\" encoding=\"~a\"?>\n")
      (no-end-tags-elements '())
      ;; XHTML scripts have to be protected
      (cdata-start "\n<![CDATA[\n")
      (cdata-stop "]]>\n")
      ;; the meta-format contains the closing />
      (meta-delimiter "/>")))

;*---------------------------------------------------------------------*/
;*    hop-xml-backend ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter hop-xml-backend
   *html5-backend*
   (lambda (v)
      (if (isa? v xml-backend)
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
   (with-access::xml-markup o (body tag attributes)
      (for-each (lambda (attr)
		   (when (isa? attr xml-tilde)
		      (with-access::xml-tilde attr (parent)
			 (set! parent o))))
	 attributes)
      (let loop ((es body))
	 (cond
	    ((pair? es)
	     (let ((e (car es)))
		(cond
		   ((isa? e xml-element)
		    (with-access::xml-element e (parent)
		       (set! parent o)))
		   ((isa? e xml-tilde)
		    (with-access::xml-tilde e (parent)
		       (set! parent o)))
		   ((pair? e)
		    (loop e))))
	     (loop (cdr es)))
	    ((isa? es xml-element)
	     (with-access::xml-element es (parent)
		(set! parent o)))
	    ((isa? es xml-tilde)
	     (with-access::xml-tilde es (parent)
		(set! parent o)))))
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

   (define (el->string el)
      (string-append "<" (string-upcase (symbol->string! el)) ">"))

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
	      (error (el->string el) "Illegal attribute" (car a)))
	     ((eq? (car a) :id)
	      (let ((v (xml-primitive-value (cadr a))))
		 (if (or (string? v) (not v))
		     (loop (cddr a) attr body v)
		     (bigloo-type-error el "string" (cadr a)))))
	     (else
	      (loop (cddr a) (cons* (cadr a) (car a) attr) body id))))
	 ((null? (car a))
	  (loop (cdr a) attr body id))
	 ((xml-unpack (car a))
	  =>
	  (lambda (l)
	     (if (not (or (null? (cdr a)) (pair? (cdr a))))
		 (error (el->string el) "Illegal arguments"
		    `(,(el->string el) ,@args))
		 (if (or (pair? l) (null? l))
		     (loop (append l (cdr a)) attr body id)
		     (loop (cdr a) attr (cons (car a) body) id)))))
	 (else
	  (loop (cdr a) attr (cons (car a) body) id)))))

;*---------------------------------------------------------------------*/
;*    xml-markup-is? ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-markup-is? o t)
   (when (isa? o xml-markup)
      (with-access::xml-markup o (tag)
	 (eq? tag t))))

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
       (let ((n (fixnum->string id-count)))
	  (set! id-count (+fx 1 id-count))
	  (cond
	     ((symbol? id)
	      (string-append (symbol->string! id) n))
	     ((symbol? tag)
	      (string-append (symbol->string! tag) n))
	     (else
	      (string-append "G" n))))))

;*---------------------------------------------------------------------*/
;*    xml-event-handler-attribute? ...                                 */
;*    -------------------------------------------------------------    */
;*    This is a gross hack. Currently, we consider that all attributes */
;*    whose name start with "on" are event handlers!                   */
;*---------------------------------------------------------------------*/
(define (xml-event-handler-attribute? keyword)
   (substring-at? (keyword->string! keyword) "on" 0))

;*---------------------------------------------------------------------*/
;*    xml-body ...                                                     */
;*---------------------------------------------------------------------*/
(define (xml-body body)
   (if (null? body)
       body
       (let ((el (xml-body-element (car body))))
	  (if (pair? el)
	      (append el (xml-body (cdr body)))
	      (cons el (xml-body (cdr body)))))))

;*---------------------------------------------------------------------*/
;*    xml-body ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (xml-body-element obj)
   obj)

;*---------------------------------------------------------------------*/
;*    xml-unpack ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (xml-unpack obj::obj)
   (when (pair? obj)
      (when (list? obj)
	 obj)))

;*---------------------------------------------------------------------*/
;*    xml-write ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (xml-write obj p backend)
   (cond
      ((string? obj)
       (with-access::xml-backend backend (security)
	  (if (isa? security security-manager)
	      (with-access::security-manager security (string-sanitize)
		 (let ((s (string-sanitize obj)))
		    (when (string? s)
		       (display s p))))
	      (display obj p))))
      ((integer? obj)
       (display obj p))
      ((flonum? obj)
       (if (nanfl? obj)
	   (display "NaN" p)
	   (display obj p)))
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
      ((char? obj)
       (display obj p))
      ((ucs2-string? obj)
       (let ((s (charset-convert (ucs2-string->utf8-string obj)
		   'UTF-8 (hop-charset))))
	  (xml-write s p backend)))
      (else
       (error "xml" "bad XML object"
	  (xml-to-errstring obj)))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-verbatim ...                                     */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-verbatim p backend)
   (with-access::xml-verbatim obj (data)
      (display data p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-comment ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-comment p backend)
   (with-access::xml-comment obj (data)
      (display "<!--" p)
      (display-string data p)
      (display "-->" p)))

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
	 (display ">" p))))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-style ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-style p backend)
   
   (define (xml-write-style el p)
      (call-with-input-string el
	 (lambda (ip)
	    (let ((hss (hop-read-hss ip)))
	       (if (isa? hss css-stylesheet)
		   (css-write (hss-compile hss) p)
		   (error "xml-write" "Illegal style sheet" el))))))
   
   (with-access::xml-style obj (tag body attributes)
      (with-access::xml-backend backend (cdata-start cdata-stop)
	 (display "<" p)
	 (display tag p)
	 (xml-write-attributes attributes p backend)
	 (display ">" p)
	 (unless (or (not body) (null? body))
	    (when cdata-start (display cdata-start p))
	    (for-each (lambda (el)
			 (if (string? el)
			     (xml-write-style el p)
			     (xml-write el p backend)))
	       body)
	    (when cdata-stop (display cdata-stop p)))
	 (display "</" p)
	 (display tag p)
	 (display ">" p))))
   
;*---------------------------------------------------------------------*/
;*    xml-write ::xml-tilde ...                                        */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-tilde p backend)
   (with-access::xml-tilde obj (body parent)
      (if (xml-markup-is? parent 'script)
	  (xml-write (xml-tilde->statement obj) p backend)
	  (with-access::xml-backend backend (cdata-start cdata-stop)
	     (display "<script type='" p)
	     (display (hop-mime-type) p)
	     (display "'>" p)
	     (when cdata-start (display cdata-start p))
	     (display (xml-tilde->statement obj) p)
	     (when cdata-stop (display cdata-stop p))
	     (display "</script>" p)))))
      
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
      (with-access::xml-backend backend (security no-end-tags-elements)
	 (cond
	    ((and (eq? tag 'head) (>=fx (hop-security) 3))
	     (display ">" p)
	     (for-each (lambda (b) (xml-write b p backend)) body)
	     (when (isa? security security-manager)
		(for-each (lambda (r)
			     (display "<script type='" p)
			     (display (hop-mime-type) p)
			     (fprintf p "' src='~a'>" r)
			     (display "</script>" p))
		   (with-access::security-manager security (runtime) runtime)))
	     (display "</" p)
	     (display tag p)
	     (display ">" p))
	    ((or (pair? body) (eq? tag 'script))
	     (display ">" p)
	     (for-each (lambda (b) (xml-write b p backend)) body)
	     (display "</" p)
	     (display tag p)
	     (display ">" p))
	    ((memq tag no-end-tags-elements)
	     (display ">" p))
	    (else
	     (display "/>" p))))))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-meta ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-meta p backend)
   (with-access::xml-meta obj (tag attributes content body)
      (display "<" p)
      (display tag p)
      (xml-write-attributes attributes p backend)
      (with-access::xml-backend backend (mime-type meta-delimiter)
	 (cond
	    ((string? content)
	     (display " content='" p)
	     (fprintf p content mime-type (hop-charset))
	     (display "'" p))
	    (content
	     (display " content='" p)
	     (display mime-type p)
	     (display "; charset=" p)
	     (display (hop-charset) p)
	     (display "'" p)))
	 (display meta-delimiter p))
      (newline p)))

;*---------------------------------------------------------------------*/
;*    xml-write ::xml-element ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-write obj::xml-element p backend)
   (with-access::xml-element obj (tag id attributes body)
      (with-access::xml-backend backend (abbrev-emptyp no-end-tags-elements)
	 (cond
	    ((and (null? body) (null? attributes))
	     (display "<" p)
	     (display tag p)
	     (display " id='" p)
	     (display id p)
	     (display "'" p)
	     (if abbrev-emptyp
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
		(abbrev-emptyp
		   (display "/>" p))
		((memq tag no-end-tags-elements)
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
	 (xml-write-initializations obj p backend))))

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
      (with-access::xml-backend backend (empty-end-tag)
	 (display (if empty-end-tag "/>" ">") p))
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
	 (let* ((l (hashtable-map env
		      (lambda (k v) (when v (cons k v)))))
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
			     (r (http-internal-error e m #f)))
			 (with-access::http-response-xml r (xml)
			    (xml-write-html xml p backend))))
		   (error/source-location "<HTML>"
		      (format "Unbound client-side variable: ~a" (car lf))
		      (cadr lf)
		      (cddr lf)))
		;; everything is fine
		(xml-write-html obj p backend))))))
       
;*---------------------------------------------------------------------*/
;*    xml-write-html ...                                               */
;*---------------------------------------------------------------------*/
(define (xml-write-html obj::xml-html p backend)	    
   (with-access::xml-backend backend (header-format doctype html-attributes)
      (when header-format
	 (fprintf p header-format (hop-charset)))
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
	 (display ">" p)
	 (for-each (lambda (b) (xml-write b p backend)) body)
	 (display "</" p)
	 (display tag p)
	 (display ">" p))))

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
   (unless (or (eq? attr #f) (eq? attr #unspecified) (eq? id :%location))
      (display (keyword->string! id) p)
      ;; boolean true attribute has no value
      (display "='" p)
      (cond
	 ((eq? attr #t)
	  (display (keyword->string! id) p))
	 ((procedure? attr)
	  (if (isa? (procedure-attr attr) hop-service)
	      (with-access::hop-service (procedure-attr attr) (path)
		 (display path p))
	      (error "xml"
		     "Illegal procedure argument in XML attribute"
		     id)))
	 ((with-access::xml-backend backend (security) security)
	  =>
	  (lambda (sm)
	     (if (isa? sm security-manager)
		 (with-access::security-manager sm (attribute-sanitize)
		    (let ((a (xml-attribute-encode attr)))
		       (display (attribute-sanitize a id) p)))
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
      (with-access::xml-backend backend ((sm security))
	 (if (isa? sm security-manager)
	     (with-access::security-manager sm (attribute-sanitize)
		(display (attribute-sanitize attr id) p))
	     (display (xml-tilde->attribute attr) p)))
      (display "'" p)))
   
;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::hop-service ...                            */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::hop-service id p backend)
   (display (keyword->string! id) p)
   (display "='" p)
   (with-access::hop-service p (path)
      (display path p))
   (display "'" p))

;*---------------------------------------------------------------------*/
;*    xml-write-attribute ::xml-lazy-attribute ...                     */
;*---------------------------------------------------------------------*/
(define-method (xml-write-attribute attr::xml-lazy-attribute id p backend)
   (with-access::xml-lazy-attribute attr (proc)
      (xml-write-attribute (proc) id p backend)))

;*---------------------------------------------------------------------*/
;*    xml-attribute-encode ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (xml-attribute-encode obj)
   (if (not (string? obj))
       obj
       (let ((ol (string-length obj)))
	  (define (count str ol)
	     (let loop ((i 0)
			(j 0))
		(if (=fx i ol)
		    j
		    (let ((c (string-ref str i)))
		       (if (char=? c #\')
			   (loop (+fx i 1) (+fx j 5))
			   (loop (+fx i 1) (+fx j 1)))))))
	  (define (encode str ol nl)
	     (if (=fx nl ol)
		 obj
		 (let ((nstr (make-string nl)))
		    (let loop ((i 0)
			       (j 0))
		       (if (=fx j nl)
			   nstr
			   (let ((c (string-ref str i)))
			      (case c
				 ((#\')
				  (string-set! nstr j #\&)
				  (string-set! nstr (+fx j 1) #\#)
				  (string-set! nstr (+fx j 2) #\3)
				  (string-set! nstr (+fx j 3) #\9)
				  (string-set! nstr (+fx j 4) #\;)
				  (loop (+fx i 1) (+fx j 5)))
				 (else
				  (string-set! nstr j c)
				  (loop (+fx i 1) (+fx j 1))))))))))
	  (encode obj ol (count obj ol)))))

;*---------------------------------------------------------------------*/
;*    xml-primitive-value ::obj ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (xml-primitive-value x::obj)
   x)

;*---------------------------------------------------------------------*/
;*    xml-to-errstring ::obj ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (xml-to-errstring o::obj)
   (call-with-output-string
      (lambda (op)
	 (write-circle o op))))

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
		   (display "}, false );</script>" p)))
	       ((and (isa? (cadr attrs) xml-tilde)
		     (not (xml-event-handler-attribute? (car attrs))))
		(if var
		    (begin
		       (xml-write-initialization (car attrs) (cadr attrs) var p)
		       (newline p)
		       (loop (cddr attrs) var))
		    (let ((var (gensym)))
		       (display "<script type='" p)
		       (display (hop-mime-type) p)
		       (display "'>" p)
		       (when cdata-start (display cdata-start p))
		       (display "hop_add_event_listener( \"" p)
		       (display id p)
		       (display "\", \"ready\", function (e) {" p)
		       (display "var " p)
		       (display var p)
		       (display " = e.value;" p)
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
       (let* ((a (append-map (lambda (a)
				(list (symbol->keyword (car a)) (cdr a)))
		    attributes))
	      (constr (with-handler
			 (lambda (e)
			    (with-access::&error e (msg)
			       (if (string=? msg "Unbound variable")
				   ;; create an opaque XML object
				   (lambda l
				      (instantiate::xml-markup
					 (tag constr)
					 (attributes a)
					 (body body)))
				   ;; re-raise the other errors
				   (raise e))))
			 (eval constr))))
	  (if (procedure? constr)
	      (apply constr (append a body))
	      (error "string->xml" "Illegal markup" constr))))))

;*---------------------------------------------------------------------*/
;*    parse-html ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-html ip clen)
   (html-parse ip
      :content-length clen
      :procedure (lambda (tag attributes body)
		    (let ((constr (string->symbol
				     (string-append
					"<"
					(string-upcase (symbol->string! tag))
					">"))))
		       (eval-markup constr attributes body)))))

;*---------------------------------------------------------------------*/
;*    string->html ...                                                 */
;*---------------------------------------------------------------------*/
(define (string->html h)
   (call-with-input-string h (lambda (ip) (parse-html ip 0))))

;*---------------------------------------------------------------------*/
;*    string->xml ...                                                  */
;*---------------------------------------------------------------------*/
(define (string->xml h)
   (call-with-input-string h
      (lambda (in)
	 (xml-parse in
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
      (unless (string? %js-expression)
	 (with-access::clientc (hop-clientc) (precompiled->JS-expression)
	    (set! %js-expression (precompiled->JS-expression body))))
      %js-expression))

;*---------------------------------------------------------------------*/
;*    xml-tilde->statement ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-tilde->statement::bstring obj)

   (define (element-attribute el)
      (with-access::xml-element el (attributes)
	 (let loop ((attributes attributes))
	    (if (or (null? attributes) (null? (cdr attributes)))
		#f
		(if (and (keyword? (car attributes))
			 (eq? (cadr attributes) obj))
		    (car attributes)
		    (loop (cddr attributes)))))))
   
   (define (parent-context parent)
      (cond
	 ((string? parent)
	  ;; I'm not sure this will be ever used...
	  parent)
	 ((isa? parent xml-element)
	  ;; find the attribute (if any)
	  (with-access::xml-element parent (tag id)
	     (let ((attr (element-attribute parent)))
		(if attr
		    (format "~a#~a.~a" tag id (keyword->string attr))
		    (format "~a#~a" tag id)))))
	 (else
	  "")))

   (define (js-catch-callback/location stmt parent file point)
      ;; this is an inlined version of hop_callback (hop-lib.js)
      (let ((ctx (gensym 'ctx)))
	 (format "var ~a=hop_callback_html_context( \"~a\", \"~a\", ~a );
hop_current_stack_context = ~a;
try { ~a } catch( e ) {
hop_callback_handler(e, ~a); }"
	    ctx
	    (string-replace (xml-attribute-encode (parent-context parent))
	       #\Newline #\Space)
	    file point ctx stmt
	    ctx)))

   (define (js-catch-callback stmt parent)
      (let ((ctx (gensym 'ctx)))
	 (format "var ~a=hop_callback_listener_context( \"~a\" );
hop_current_stack_context = ~a;
try { ~a } catch( e ) { hop_callback_handler(e, ~a); }"
	    ctx
	    (string-replace (xml-attribute-encode (parent-context parent))
	       #\Newline #\Space)
	    ctx
	    stmt
	    ctx)))
   
   (with-access::xml-tilde obj (%js-statement body loc parent)
      (unless (string? %js-statement)
	 (with-access::clientc (hop-clientc) (precompiled->JS-statement)
	    (let ((stmt (precompiled->JS-statement body)))
	       (if (>fx (bigloo-debug) 0)
		   (match-case loc
		      ((at (and (? string?) ?file) (and (? integer?) ?point))
		       (set! %js-statement
			  (js-catch-callback/location stmt parent file point)))
		      (else
		       (set! %js-statement
			  (js-catch-callback stmt parent))))
		   (set! %js-statement stmt)))))
      %js-statement))

;*---------------------------------------------------------------------*/
;*    xml-tilde->return ...                                            */
;*---------------------------------------------------------------------*/
(define (xml-tilde->return::bstring obj)
   (with-access::xml-tilde obj (%js-return body)
      (when (not (string? %js-return))
	 (with-access::clientc (hop-clientc) (precompiled->JS-return)
	    (set! %js-return (precompiled->JS-return body))))
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
      `(pragma
	  ,(call-with-output-string
	      (lambda (op)
		 (obj->javascript-attr o op)))))
   
   (with-access::xml-tilde obj (lang body %js-expression)
      (if (eq? lang 'javascript)
	  `(pragma ,%js-expression)
	  (with-access::clientc (hop-clientc) (precompiled->sexp)
	     (precompiled->sexp body wrapper)))))

;*---------------------------------------------------------------------*/
;*    sexp->xml-tilde ...                                              */
;*---------------------------------------------------------------------*/
(define (sexp->xml-tilde obj #!optional env menv)
   (with-access::clientc (hop-clientc) (macroe sexp->precompiled)
      (let* ((env (or env (current-module-clientc-import)))
	     (menv (or menv (macroe)))
	     (c (sexp->precompiled obj env menv)))
	 (<TILDE> c :src obj :env env :menv menv))))

;*---------------------------------------------------------------------*/
;*    <TILDE> ...                                                      */
;*---------------------------------------------------------------------*/
(define (<TILDE> body #!key src loc env menv)
   (instantiate::xml-tilde
      (body body)
      (src src)
      (loc loc)
      (env env)
      (menv menv)))

;*---------------------------------------------------------------------*/
;*    <DELAY> ...                                                      */
;*---------------------------------------------------------------------*/
(define-tag <DELAY> ((id #unspecified string)
		     body)
   (if (and (pair? body) (procedure? (car body)) (correct-arity? (car body) 0))
       (instantiate::xml-delay
	  (id (xml-make-id id))
	  (thunk (car body)))
       (error "<DELAY>" "Illegal thunk" (car body))))

;*---------------------------------------------------------------------*/
;*    <PRAGMA> ...                                                     */
;*---------------------------------------------------------------------*/
(define (<PRAGMA> . obj)
   (cond
      ((and (pair? obj) (null? (cdr obj)) (string? (car obj)))
       (instantiate::xml-verbatim (data (car obj))))
      ((every string? obj)
       (instantiate::xml-verbatim (data (apply string-append obj))))
      (else
       (error "<PRAGMA>" "Illegal arguments" obj))))

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
		      (when (isa? a xml-tilde)
			 (xml-tilde-unbound a env)))
		   attributes)
	 (unless old (hashtable-remove! env 'event)))))

;*---------------------------------------------------------------------*/
;*    xml-tilde-unbound ::xml-tilde ...                                */
;*---------------------------------------------------------------------*/
(define-method (xml-tilde-unbound obj::xml-tilde env)

   (define (source-location src)
      (when (epair? src) (cer src)))
   
   (with-access::xml-tilde obj (body src)
      (with-access::clientc (hop-clientc)
	    (precompiled-declared-variables precompiled-free-variables)
	 (when (vector? body)
	    (for-each (lambda (v)
			 (let ((v (car v)))
			    (hashtable-update! env v (lambda (x) #f) #f)))
	       (precompiled-declared-variables body))
	    (for-each (lambda (v)
			 (let ((v (car v))
			       (loc (caddr v)))
			    (hashtable-update! env v (lambda (x) #f)
			       (cons src (or loc (source-location src))))))
	       (precompiled-free-variables body))))))


