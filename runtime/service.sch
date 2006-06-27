;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 14:53:24 2005                          */
;*    Last change :  Tue Jun 20 13:40:43 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop macros                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    $service/filter ...                                              */
;*---------------------------------------------------------------------*/
(define-pervasive-macro ($service/filter svcurl timeout ttl args . body)
   (define (service-js-arguments formals)
      (if (null? formals)
	  "[]"
	  (let loop ((l formals)
		     (res '()))
	     (cond
		((null? l)
		 (apply string-append (reverse! (cons "]" res))))
		(else
		 (loop (cdr l)
		       (cons (format "~a\"~a\""
				     (if (null? res) "[" ", ") (car l))
			     res)))))))
   (define (jscript vargs path)
      `(format
	,(if vargs
	     (string-append "function() { return hop_service_url( ~s, " vargs ", arguments ) }")
	     "function() { return hop_service_url_varargs( ~s, arguments ) }")
	,path))
   (let* ((ca (gensym 'ca))
	  (url (gensym 'url))
	  (req (gensym 'req))
	  (proc (gensym 'proc))
	  (exec (gensym 'exec))
	  (hop (gensym 'hop))
	  (path (gensym 'path))
	  (svc (gensym 'svc))
	  (vargs (if (list? args) (service-js-arguments args) #f)))
      `(let* ((,proc (lambda ,args ,@body))
	      (,exec (lambda (,req)
			(let ((,ca (http-request-cgi-args ,req)))
			   ,(if (or (null? args) (list? args))
				`(if (equal? (cgi-arg "hop-encoding" ,ca) "hop")
				     (begin
					(http-request-char-encoding-set! ,req 'UTF-8)
					(,proc ,@(map (lambda (a)
							 `(serialized-cgi-arg ,(symbol->string a) ,ca))
						      args)))
				     (,proc ,@(map (lambda (a)
						      `(cgi-arg ,(symbol->string a) ,ca))
						   args)))
				`(,proc (error '$service/filter "not implement" "yet"))))))
	      (,url ,svcurl)
	      (,path (make-url-name (hop-service-base) ,url))
	      (svc (instantiate::hop-service
		      (id (string->symbol ,url))
		      (path ,path)
		      (args ',args)
		      (%exec ,exec)
		      (proc ,proc)
		      (javascript ,(jscript vargs path))
		      (creation (date->seconds (current-date)))
		      (timeout ,timeout)
		      (ttl ,ttl))))
	  (register-service! svc))))

;*---------------------------------------------------------------------*/
;*    define-service ...                                               */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-service decl . body)
   (let ((id (car decl))
	 (args (cdr decl)))
      (if (not (and (pair? decl) (every? symbol? decl)))
	  (error 'define-service "Illegal service declaration" decl)
	  `(define ,id
	      ($service/filter ,(symbol->string id) -1 -1 ,args ,@body)))))

;*---------------------------------------------------------------------*/
;*    service ...                                                      */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (service . args)
   (let loop ((a args)
	      (tmt '(hop-service-default-timeout))
	      (ttl -1))
      (cond
	 ((or (symbol? (car a))
	      (and (list? (car a)) (every? symbol? (car a))))
	  (if (null? (cdr a))
	      (error 'service
		     "Illegal service (empty body)"
		     (cons 'service args))
	      `($service/filter (get-service-url) ,tmt ,ttl ,(car a) ,@(cdr a))))
	 ((eq? (car a) :timeout)
	  (if (null? (cdr a))
	      (error 'service
		     "Illegal service declaration (missing timeout)"
		     (cons 'service args))
	      (loop (cddr a)
		    (cadr a)
		    ttl)))
	 ((eq? (car a) :ttl)
	  (if (null? (cdr a))
	      (error 'service
		     "Illegal service declaration (missing ttl)"
		     (cons 'service args))
	      (loop (cddr a)
		    tmt
		    (cadr a))))
	 (else
	  (error 'service
		 "Illegal service declaration"
		 (cons 'service args))))))
	      
;*---------------------------------------------------------------------*/
;*    $roundtrip ...                                                   */
;*---------------------------------------------------------------------*/
(define-pervasive-macro ($roundtrip bindings . body)
   (for-each (lambda (binding)
		(match-case binding
		   (((and (? symbol?) ?id) ?-)
		    #t)
		   (else
		    (error '$roundtrip "Illegal binding" binding))))
	     bindings)
   (cond
      ((null? bindings)
       `(begin ,body))
      ((null? (cdr bindings))
       `(%eval ,(cadr (car bindings))
	       (lambda (,(caar bindings)) ,@body)))
      (else
       (let ((vec (gensym)))
	  (let loop ((bindings bindings)
		     (i 0)
		     (str '())
		     (nbindings '()))
	     (if (null? bindings)
		 `(%eval ,(apply string-append "new Array(" (reverse! str))
			 (lambda (,vec) (let ,nbindings ,@body)))
		 (let ((binding (car bindings)))
		    (loop (cdr bindings)
			  (+fx i 1)
			  (cons* (if (pair? (cdr bindings)) "," ")")
				 (cadr binding)
				 str)
			  (cons `(,(car binding) (vector-ref ,vec ,i))
				nbindings)))))))))

;*---------------------------------------------------------------------*/
;*    with-hop ...                                                     */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (with-hop arg0 . args)
   (define (%invoke-service form)
      (match-case form
	 ((?svc . ?args)
	  `((hop-service-proc ,svc) ,@args))
	 (else
	  (error 'with-hop "Illegal service invokation" form))))
   (define (%with-local-host arg0 args)
      (cond
	 ((null? args)
	  `(with-hop-response ,(%invoke-service arg0)
			      (lambda (_) #f)
			      raise))
	 ((null? (cdr args))
	  `(with-hop-response ,(%invoke-service arg0)
			      ,(car args)
			      raise))
	 (else
	  `(with-hop-response ,(%invoke-service arg0)
			      ,(car args)
			      ,(cadr args)))))
   (cond
      ((not (eq? arg0 :host))
       (%with-local-host arg0 args))
      ((null? args)
       (error 'with-hop "Illegal host" arg0))
      ((null? (cdr args))
       (error 'with-hop "Service missing"))
      (else
       (match-case (cadr args)
	  ((?svc . ?opts)
	   `(with-remote-host ,(car args)
			      ,svc (list ,@opts)
			      ,(if (pair? (cddr args))
				   (car (cddr args))
				   raise)
			      ,(if (pair? (cdddr args))
				   (car (cdddr args))
				   raise)))
	  (else
	   (error 'with-hop "Illegal service invokation" (cadr args)))))))
