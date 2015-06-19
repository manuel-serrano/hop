;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/security.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 22 17:58:28 2009                          */
;*    Last change :  Fri Jun 19 15:57:41 2015 (serrano)                */
;*    Copyright   :  2009-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Security management.                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_security

   (include "param.sch")

   (library web)
	    
   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml-types
	    __hop_xml
	    __hop_dom
	    __hop_http-error)

   (static  (class xml-secure-attribute::xml-tilde))

   (export  (secure-javascript-attr ::obj)

	    (hop-xml-backend-secure::xml-backend)

	    (hop-security-manager::obj)
	    (hop-security-manager-set! ::obj)

	    (xml-tree-compare::obj ::obj ::xml-backend ::http-request)
	    (generic xml-compare a1::obj a2::obj)
	    (xml-string-sanitize::bstring ::bstring)
	    (xml-attribute-sanitize::obj ::obj ::keyword)))

;*---------------------------------------------------------------------*/
;*    secure-javascript-attr ...                                       */
;*---------------------------------------------------------------------*/
(define (secure-javascript-attr obj)
   (if (and (>=fx (hop-security) 2) (string? obj))
       (instantiate::xml-secure-attribute
	  (body '())
	  (%js-attribute obj))
       obj))

;*---------------------------------------------------------------------*/
;*    hop-xml-backend-secure ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-xml-backend-secure)
   (let ((be (hop-xml-backend)))
      (if (<fx (hop-security) 2)
	  be
	  (duplicate::xml-backend be
	     (security (hop-security-manager))))))

;*---------------------------------------------------------------------*/
;*    security-manager-default ...                                     */
;*---------------------------------------------------------------------*/
(define security-manager-default 
   (instantiate::security-manager
      (name "Unsecure")
      (xml-sanitize (lambda (xml be req) xml))
      (string-sanitize (lambda (s) s))
      (attribute-sanitize (lambda (a id) a))
      (inline-sanitize (lambda (n) n))
      (script-sanitize (lambda (n) n))
      (runtime '())))

;*---------------------------------------------------------------------*/
;*    security-manager-tree-compare ...                                */
;*---------------------------------------------------------------------*/
(define security-manager-tree-compare
   (instantiate::security-manager
      (name "Secure tree")
      (xml-sanitize xml-tree-compare)
      (string-sanitize (lambda (s) s))
      (attribute-sanitize (lambda (a id) a))
      (inline-sanitize (lambda (n) n))
      (script-sanitize (lambda (n) n))
      (runtime '())))
 
;*---------------------------------------------------------------------*/
;*    security-manager ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-security-manager
   security-manager-default
   (lambda (v)
      (cond
;* 	 ((hop-rc-loaded?)                                             */
;* 	  (error "hop-security-manager-set!"                           */
;* 		 "Security managers can be specified once hoprc.hop loaded" */
;* 		 #f))                                                  */
	 ((eq? v 'tree)
	  security-manager-tree-compare)
	 ((eq? v 'unsecure)
	  security-manager-default)
	 ((not (isa? v security-manager))
	  (bigloo-type-error "hop-security-manager-set!" "security-manager" v))
	 (else
	  v))))

;*---------------------------------------------------------------------*/
;*    attr-event-handler? ...                                          */
;*---------------------------------------------------------------------*/
(define (attr-event-handler? id)
   (string-prefix-ci? "on" (keyword->string! id)))

;*---------------------------------------------------------------------*/
;*    xml-attribute-sanitize ...                                       */
;*---------------------------------------------------------------------*/
(define (xml-attribute-sanitize attr id)
   (cond
      ((string? attr)
       (if (and (attr-event-handler? id) (>fx (string-length attr) 0))
	   (raise
	    (instantiate::&hop-injection-error
	       (proc id)
	       (msg "Illegal handler attribute")
	       (obj attr)))
	   attr))
      ((isa? attr xml-tilde)
       (xml-tilde->attribute attr))
      (else
       attr)))

;*---------------------------------------------------------------------*/
;*    xml-tree-compare ...                                             */
;*---------------------------------------------------------------------*/
(define (xml-tree-compare::obj xml backend req)
   (let ((p (open-output-string)))
      (xml-write xml p (duplicate::xml-backend backend
			  (security security-manager-tree-compare)))
      (let* ((s (close-output-port p))
	     (ast (skip-declaration (string->html s))))
	 (with-handler
	    (lambda (e)
	       (let ((rep (http-error e req)))
		  (if (isa? rep http-response-xml)
		      (begin
			 (exception-notify e)
			 (with-access::http-response-xml rep (xml) xml))
		      (raise e))))
	    (begin
	       (xml-compare (normalize-ast xml) (normalize-ast ast))
	       xml)))))

;*---------------------------------------------------------------------*/
;*    skip-declaration ...                                             */
;*---------------------------------------------------------------------*/
(define (skip-declaration ast)
   (match-case ast
      (((declaration . ?-) . ?rest) rest)
      (else ast)))

;*---------------------------------------------------------------------*/
;*    xml-compare-error ...                                            */
;*---------------------------------------------------------------------*/
(define (xml-compare-error a1 a2)
   (raise
    (instantiate::&hop-injection-error
       (proc "xml-compare")
       (msg "Infected tree")
       (obj (format "\n tree1: ~a\n tree2: ~a" (ast->string-list a1) (ast->string-list a2))))))

;*---------------------------------------------------------------------*/
;*    xml-compare ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (xml-compare a1::obj a2::obj)
   
   (define (ast-constant? a)
      (or (string? a)
	  (number? a)
	  (symbol? a)
	  (boolean? a)
	  (null? a)
	  (and (list? a) (every ast-constant? a))))
   
   (cond
      ((ast-constant? a1)
       (unless (ast-constant? a2)
	  (xml-compare-error a1 a2)))
      ((list? a1)
       (let ((na1 (normalize-ast a1))
	     (na2 (normalize-ast a2)))
	  (if (and (list? na2) (=fx (length na1) (length na2)))
	      (let liip ((a1 na1)
			 (a2 na2))
		 (when (pair? a1)
		    (xml-compare (normalize-ast (car a1)) (normalize-ast (car a2)))
		    (liip (cdr a1) (cdr a2))))
	      (xml-compare-error na1 na2))))
      (else
       (raise (instantiate::&io-parse-error
		 (proc "compare-ast")
		 (msg "Illegal XML tree")
		 (obj a1))))))

;*---------------------------------------------------------------------*/
;*    xml-compare ::object ...                                         */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::object a2::obj)
   (unless (and (object? a2) (eq? (object-class a1) (object-class a2)))
      (xml-compare-error a1 a2)))

;*---------------------------------------------------------------------*/
;*    xml-compare ::xml-markup ...                                     */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::xml-markup a2::obj)
   (if (and (isa? a2 xml-markup)
	    (with-access::xml-markup a1 ((tag1 tag))
	       (with-access::xml-markup a2 ((tag2 tag))
		  (eq? tag1 tag2))))
       (if (safe-attributes? a2)
	   (xml-compare
	      (with-access::xml-markup a1 (body)
		 (normalize-ast body))
	      (with-access::xml-markup a2 (body)
		 (normalize-ast body)))
	   (xml-compare-error a1 a2))
       (xml-compare-error a1 a2)))

;*---------------------------------------------------------------------*/
;*    xml-compare ::xml-element ...                                    */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::xml-element a2::obj)
   (cond
      ((and (isa? a2 xml-markup)
	    (with-access::xml-element a1 ((tag1 tag))
	       (with-access::xml-element a2 ((tag2 tag))
		  (eq? tag1 tag2))))
       (if (safe-attributes? a2)
	   (xml-compare
	      (with-access::xml-element a1 (body)
		 (normalize-ast body))
	      (with-access::xml-element a2 (body)
		 (normalize-ast body)))
	   (xml-compare-error a1 a2)))
      ((and (any (lambda (a) (isa? a xml-tilde)) (dom-get-attributes a1))
	    (pair? a2) (pair? (cdr a2)) (null? (cddr a2))
	    (isa? (cadr a2) xml-cdata))
       (xml-compare a1 (car a2)))
      (else
       (xml-compare-error a1 a2))))

;*---------------------------------------------------------------------*/
;*    xml-compare ::xml-tilde ...                                      */
;*---------------------------------------------------------------------*/
(define-method (xml-compare a1::xml-tilde a2)
   (unless (and (isa? a1 xml-tilde)
		(isa? a2 xml-markup)
		(with-access::xml-markup a2 (tag body)
		   (and (eq? tag 'script)
			(pair? body)
			(string? (car body))
			(null? (cdr body)))))
      (xml-compare-error a1 a2)))

;*---------------------------------------------------------------------*/
;*    safe-attributes? ...                                             */
;*---------------------------------------------------------------------*/
(define (safe-attributes? o::xml-markup)
   (with-access::xml-markup o (attributes)
      (let loop ((a attributes))
	 (if (null? a)
	     #t
	     (when (pair? (cdr a))
		(when (or (not (string? (cadr a)))
			  (not (string-prefix? "javascript:" (cadr a))))
		   (loop (cddr a))))))))

;*---------------------------------------------------------------------*/
;*    normalize-ast ...                                                */
;*---------------------------------------------------------------------*/
(define (normalize-ast ast)
   (cond
      ((and (pair? ast) (null? (cdr ast)))
       (normalize-ast (car ast)))
      ((isa? ast xml)
       ast)
      ((pair? ast)
       (let ((l (filter (lambda (a) (isa? a xml)) ast)))
	  (if (and (pair? l) (null? (cdr l)))
	      (car l)
	      l)))))

;*---------------------------------------------------------------------*/
;*    ast->string-list ...                                             */
;*---------------------------------------------------------------------*/
(define (ast->string-list ast)
   (cond
      ((or (string? ast) (number? ast))
       "_")
      ((list? ast)
       (map ast->string-list ast))
      ((isa? ast xml-element)
       (with-access::xml-element ast (tag body id)
	  (let ((c (dom-get-attribute ast "class")))
	     (if c
		 `(,(symbol-append '< tag '>) :class
		     ,(string-append "\"" c "\"")
		     :id ,(format "\"~a\"" id)
		     ,@(map ast->string-list body))
		 `(,(symbol-append '< tag '>)
		   :id ,(format "\"~a\"" id)
		   ,@(map ast->string-list body))))))
      ((isa? ast xml-markup)
       (with-access::xml-markup ast (tag body)
	  `(,(symbol-append '< tag '>)
	    ,@(map ast->string-list body))))
      ((isa? ast xml-tilde)
       (with-access::xml-tilde ast (body)
	  `(~ -)))
      ((symbol? ast)
       (symbol->string ast))
      (else
       (typeof ast))))

;*---------------------------------------------------------------------*/
;*    string-substitute ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (string-substitute s charset . strings)
   ;; assume S is an indentifier, CHARSET a string, STRINGS a list of strings
   `(if (string-index ,s ,charset)
	,(let ((l (gensym)))
	    `(let ((,l (string-length ,s)))
		(define (count i acc)
		   (if (=fx i ,l)
		       acc
		       (case (string-ref ,s i)
			  ,@(map (lambda (c s)
				    `((,c)
				      (count (+fx i 1)
					     (+fx acc ,(string-length s)))))
				 (string->list charset) strings)
			  (else (count (+fx i 1) (+fx acc 1))))))
		(define (replace new i j)
		   (if (=fx i ,l)
		       new
		       (case (string-ref ,s i)
			  ,@(map (lambda (c s)
				    `((,c)
				      (blit-string! ,s 0 new j ,(string-length s))
				      (replace new
					       (+fx i 1)
					       (+fx j ,(string-length s)))))
				 (string->list charset) strings)
			  (else
			   (string-set! new j (string-ref ,s i))
			   (replace new (+fx i 1) (+fx j 1))))))
		(replace (make-string (count 0 0)) 0 0)))
	,s))

;*---------------------------------------------------------------------*/
;*    xml-string-sanitize ...                                          */
;*---------------------------------------------------------------------*/
(define (xml-string-sanitize::bstring str::bstring)
   (if (string-index str "<>")
       (string-substitute str "<>" "&lt;" "&gt;")
       str))
