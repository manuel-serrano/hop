;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/runtime/security.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 22 17:58:28 2009                          */
;*    Last change :  Wed Jul  7 08:40:25 2010 (serrano)                */
;*    Copyright   :  2009-10 Manuel Serrano                            */
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
	    __hop_xml)

   (export  (class security-manager
	       (xml-sanitize::procedure read-only)
	       (string-sanitize::procedure read-only)
	       (attribute-sanitize::procedure read-only)
	       (inline-sanitize::procedure read-only)
	       (script-sanitize::procedure read-only)
	       (runtime::pair-nil read-only))

	    (secure-javascript-attr ::obj)

	    (hop-security-manager::obj)
	    (hop-security-manager-set! ::obj)

	    (xml-tree-compare::bstring ::obj ::xml-backend)
	    (xml-attribute-sanitize ::obj ::obj)
	    (xml-string-sanitize::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    secure-javascript-attr ...                                       */
;*---------------------------------------------------------------------*/
(define (secure-javascript-attr obj)
   (if (and (>=fx (hop-security) 1) (string? obj))
       (sexp->xml-tilde `(pragma ,obj))
       obj))

;*---------------------------------------------------------------------*/
;*    security-manager ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter hop-security-manager
   (instantiate::security-manager
      (xml-sanitize (lambda (xml be) xml))
      (string-sanitize xml-string-sanitize)
      (attribute-sanitize xml-attribute-sanitize)
      (inline-sanitize (lambda (n) n))
      (script-sanitize (lambda (n) n))
      (runtime '()))
   (lambda (v)
      (cond
	 ((hop-rc-loaded?)
	  (error "hop-security-manager-set!"
		 "Security managers can be specified once hoprc.hop loaded"
		 #f))
	 ((not (security-manager? v))
	  (bigloo-type-error "hop-security-manager-set!" "security-manager" v))
	 (else
	  v))))

;*---------------------------------------------------------------------*/
;*    security-manager-tree-compare ...                                */
;*---------------------------------------------------------------------*/
(define security-manager-tree-compare
   (instantiate::security-manager
      (xml-sanitize xml-tree-compare)
      (string-sanitize (lambda (s) s))
      (inline-sanitize (lambda (n) n))
      (script-sanitize (lambda (n) n))
      (attribute-sanitize (lambda (attr id) "_"))
      (runtime '())))

;*---------------------------------------------------------------------*/
;*    attr-event-handler? ...                                          */
;*---------------------------------------------------------------------*/
(define (attr-event-handler? id)
   (string-prefix-ci? "on" (keyword->string! id)))

;*---------------------------------------------------------------------*/
;*    xml-attribute-sanitize ...                                       */
;*---------------------------------------------------------------------*/
(define (xml-attribute-sanitize attr id)
   (if (and (string? attr) (attr-event-handler? id))
       (raise
	(instantiate::&hop-injection-error
	   (proc id)
	   (msg "Illegal handler attribute value type")
	   (obj attr)))
       (xml-attribute-encode attr)))

;*---------------------------------------------------------------------*/
;*    xml-tree-compare ...                                             */
;*---------------------------------------------------------------------*/
(define (xml-tree-compare::bstring xml backend)
   (let ((p (open-output-string)))
      (xml-write xml p (duplicate::xml-backend backend
			  (security security-manager-tree-compare)))
      (let* ((s (close-output-port p))
	     (ast (string->html s))
	     (cmp (compare-ast xml ast)))
	 (if (pair? cmp)
	     ;; the tree differs
	     (raise
	      (instantiate::&hop-injection-error
		 (proc "default-security-manager")
		 (msg "Infected tree")
		 (obj (cons (ast->string-list (car cmp))
			    (ast->string-list (cdr cmp))))))
	     ;; the tree are equivalent
	     s))))

;*---------------------------------------------------------------------*/
;*    compare-ast ...                                                  */
;*---------------------------------------------------------------------*/
(define (compare-ast ast1 ast2)
   
   (define (normalize-ast ast)
      (if (pair? ast)
	  (let ((l (filter (lambda (x)
			      (match-case x
				 ((? xml?)
				  #t)
				 ((? string?)
				  #t)
				 ((? number?)
				  #t)
				 (((and ?sym (? symbol?)) . ?val)
				  (not (eq? sym 'declaration)))
				 (else
				  #t)))
			   ast)))
	     (match-case l
		((?x) (normalize-ast x))
		(else (map normalize-ast l))))
	  ast))
   
   (define (ast-constant? a)
      (or (string? a)
	  (number? a)
	  (symbol? a)
	  (and (list? a) (every ast-constant? a))))
   
   (let loop ((a1 (normalize-ast ast1))
	      (a2 (normalize-ast ast2)))
      (cond
	 ((null? a1)
	  (or (null? a2) (cons a1 a2)))
	 ((null? a2)
	  (cons a1 a2))
	 ((ast-constant? a1)
	  (or (ast-constant? a2) (cons a1 a2)))
	 ((list? a1)
	  (if (and (list? a2) (=fx (length a1) (length a2)))
	      (or (every? loop a1 a2) (cons a1 a2))
	      (cons a1 a2)))
	 ((xml-markup? a1)
	  (if (and (xml-markup? a2)
		   (eq? (xml-markup-tag a1) (xml-markup-tag a2)))
	      (if (safe-attributes? a2)
		  (loop (normalize-ast (xml-markup-body a1))
			(normalize-ast (xml-markup-body a2)))
		  (raise (instantiate::&hop-injection-error
			    (proc "default-security-manager")
			    (msg "Illegal attributes")
			    (obj (xml-markup-attributes a2)))))
	      (cons a1 a2)))
	 ((object? a1)
	  (or (and (object? a2) (eq? (object-class a1) (object-class a2)))
	      (cons a1 a2)))
	 (else
	  (raise (instantiate::&io-parse-error
		    (proc "default-security-manager")
		    (msg "Illegal XML tree")
		    (obj a1)))))))

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
;*    ast->string-list ...                                             */
;*---------------------------------------------------------------------*/
(define (ast->string-list ast)
   (cond
      ((or (string? ast) (number? ast))
       "-")
      ((list? ast)
       (map ast->string-list ast))
      ((xml-markup? ast)
       (with-access::xml-markup ast (tag body)
	  `(,tag ,@(map ast->string-list body))))
      ((symbol? ast)
       (string-upcase (symbol->string ast)))
      (else
       (find-runtime-type ast))))

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
