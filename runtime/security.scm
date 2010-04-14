;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/security.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct 22 17:58:28 2009                          */
;*    Last change :  Wed Apr 14 11:31:50 2010 (serrano)                */
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

   (export  (security-manager::bstring ::http-response-hop)))

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
		(ast (string->html s)))
	    (when (> (bigloo-debug) 2)
	       (display "----- security check..." (current-error-port))
	       (display "s=" (current-error-port))
	       (display s (current-error-port))
	       (newline (current-error-port))
	       (display "ast=" (current-error-port))
	       (write ast (current-error-port))
	       (newline (current-error-port)))
	    (if (same-ast? xml ast)
		s
		(raise
		 (instantiate::&hop-injection-error
		    (proc 'default-security-manager)
		    (msg "Infected tree")
		    (obj r))))))))

;*---------------------------------------------------------------------*/
;*    same-ast? ...                                                    */
;*---------------------------------------------------------------------*/
(define (same-ast? ast1 ast2)
   
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

   (define (constant? a)
      (or (string? a)
	  (number? a)
	  (and (list? a) (every constant? a))))
   
   (let loop ((a1 (normalize-ast ast1))
	      (a2 (normalize-ast ast2)))
      (cond
	 ((null? a1)
	  (null? a2))
	 ((null? a2)
	  #f)
	 ((constant? a1)
	  (constant? a2))
	 ((list? a1)
	  (when (and (list? a2) (=fx (length a1) (length a2)))
	     (every? loop a1 a2)))
	 ((xml-markup? a1)
	  (when (and (xml-markup? a2)
		     (eq? (xml-markup-markup a1) (xml-markup-markup a2)))
	     (loop (normalize-ast (xml-markup-body a1))
		   (normalize-ast (xml-markup-body a2)))))
	 ((object? a1)
	  (and (object? a2) (eq? (object-class a1) (object-class a2))))
	 (else
	  (raise (instantiate::&io-parse-error
		    (proc 'same-ast)
		    (msg "Illegal XML tree")
		    (obj a1)))))))
   
