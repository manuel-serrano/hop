;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/security.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 22 17:58:28 2009                          */
;*    Last change :  Mon Apr 19 16:15:44 2010 (serrano)                */
;*    Copyright   :  2009-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Security management.                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_security

   (import  __hop_param
	    __hop_configure
	    __hop_types
	    __hop_misc
	    __hop_xml)

   (export  (secure-javascript-attr ::obj)
	    (security-manager::bstring ::http-response-hop)))

;*---------------------------------------------------------------------*/
;*    secure-javascript-attr ...                                       */
;*---------------------------------------------------------------------*/
(define (secure-javascript-attr obj)
   (if (and (string? obj) (> (hop-security) 1))
       (sexp->xml-tilde `(pragma ,obj))
       obj))

;*---------------------------------------------------------------------*/
;*    security-manager ...                                             */
;*---------------------------------------------------------------------*/
(define (security-manager r)
   (if (procedure? (hop-security-manager))
       ((hop-security-manager) r)
       (default-security-manager r)))

;*---------------------------------------------------------------------*/
;*    default-security-manager ...                                     */
;*---------------------------------------------------------------------*/
(define (default-security-manager::bstring r::http-response-hop)
   (let ((p (open-output-string)))
      (with-access::http-response-hop r (backend xml)
	 (xml-write xml p backend)
	 (let* ((s (close-output-port p))
		(ast (string->html s))
		(cmp (compare-ast xml ast)))
	    (if (pair? cmp)
		;; the tree differs
		(raise
		 (instantiate::&hop-injection-error
		    (proc 'default-security-manager)
		    (msg "Infected tree")
		    (obj (cons (ast->string-list (car cmp))
			       (ast->string-list (cdr cmp))))))
		;; the tree are equivalent
		s)))))

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
		   (eq? (xml-markup-markup a1) (xml-markup-markup a2)))
	      (if (safe-attributes? a2)
		  (loop (normalize-ast (xml-markup-body a1))
			(normalize-ast (xml-markup-body a2)))
		  (raise (instantiate::&hop-injection-error
			    (proc 'default-security-manager)
			    (msg "Illegal attributes")
			    (obj (xml-markup-attributes a2)))))
	      (cons a1 a2)))
	 ((object? a1)
	  (or (and (object? a2) (eq? (object-class a1) (object-class a2)))
	      (cons a1 a2)))
	 (else
	  (raise (instantiate::&io-parse-error
		    (proc 'default-security-manager)
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
       (with-access::xml-markup ast (markup body)
	  `(,markup ,@(map ast->string-list body))))
      ((symbol? ast)
       (string-upcase (symbol->string ast)))
      (else
       (find-runtime-type ast))))
