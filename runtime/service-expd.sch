;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service-expd.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:36:28 2006                          */
;*    Last change :  Tue Jan 30 19:58:27 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This file implements the service expanders. It is used both      */
;*    at compile-time and runtime-time.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    service-js-arguments ...                                         */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    jscript ...                                                      */
;*---------------------------------------------------------------------*/
(define (jscript args path)
   `(format
     (string-append "function() { return hop_service_url( ~s, "
		    ,(service-js-arguments args)
		    ", arguments ) }")
     ,path))

;*---------------------------------------------------------------------*/
;*    jscript-varargs ...                                              */
;*---------------------------------------------------------------------*/
(define (jscript-varargs path)
   `(format "function() { return hop_service_url_varargs( ~s, arguments ) }"
	    ,path))

;*---------------------------------------------------------------------*/
;*    expand-service ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-service url wid timeout ttl args body)
   (let ((proc (if (symbol? wid) wid 'svc))
	 (errid (if (symbol? wid) `',wid wid)))
      `(let* ((,proc (lambda ,args ,@body))
	      (exec (lambda (req)
		       (let* ((ca (http-request-cgi-args req))
			      (enc (cgi-arg "hop-encoding" ca)))
			  (if (and (string? enc) (string=? enc "hop"))
			      (begin
				 (http-request-char-encoding-set! req 'UTF-8)
				 (,proc ,@(map (lambda (a)
						  `(serialized-cgi-arg
						    ,(symbol->string a) ca))
					       args)))
			      (,proc ,@(map (lambda (a)
					       `(cgi-arg
						 ,(symbol->string a) ca))
					    args))))))
	      (path (make-url-name (hop-service-base) ,url))
	      (file (the-loading-file))
	      (svc (instantiate::hop-service
		      (wid ,(if (symbol? wid) `',wid wid))
		      (id (string->symbol ,url))
		      (path path)
		      (args ',args)
		      (%exec exec)
		      (proc ,proc)
		      (javascript ,(jscript args 'path))
		      (creation (date->seconds (current-date)))
		      (timeout ,timeout)
		      (ttl ,ttl)
		      (resource (and (string? file) (dirname file)))
		      (source (and (string? file) (basename file))))))
	  (register-service! svc))))
   
;*---------------------------------------------------------------------*/
;*    hop-define-service-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (hop-define-service-expander x e)
   (match-case x
      ((?- (?id . ?args) . ?body)
       (if (not (and (symbol? id) (and (list? args) (every? symbol? args))))
	   (error 'define-service "Illegal service declaration" x)
	   (let* ((url (symbol->string id))
		  (wid (string->symbol (car (file-name->list url))))
		  (svc (expand-service url wid -1 -1 args body)))
	      `(define ,id ,(e (evepairify svc x) e)))))
      (else
       (error 'define-service "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    hop-service-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-service-expander x e)
   (match-case x
      ((?- . ?args)
       (let loop ((a args)
		  (tmt '(hop-service-default-timeout))
		  (ttl -1))
	  (cond
	     ((or (symbol? (car a))
		  (and (list? (car a)) (every? symbol? (car a))))
	      (cond
		 ((null? (cdr a))
		  (error 'service "Illegal service (empty body)" x))
		 ((not (list? (car a)))
		  (error 'service
			 "Variable arity services not supported yet"
			 x))
		 (else
		  (let ((svc (expand-service
			      '(get-service-url)
			      '(hop-service-weblet-wid)
			      tmt ttl (car a) (cdr a))))
		     (e (evepairify svc x) e)))))
	     ((eq? (car a) :timeout)
	      (if (null? (cdr a))
		  (error 'service
			 "Illegal service declaration (missing timeout)"
			 x)
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
	      (error 'service "Illegal service declaration" x)))))
      (else
       (error 'service "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-roundtrip ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-roundtrip bindings body)
   (if (null? bindings)
       `(begin ,body)
       (let ((vec (gensym)))
	  (let loop ((obindings bindings)
		     (i 0)
		     (nbindings '()))
	     (if (null? obindings)
		 `(%eval (list ,@(map cadr bindings))
			 (current-request)
			 (lambda (,vec) (let ,nbindings ,@body)))
		 (let ((binding (car obindings)))
		    (loop (cdr obindings)
			  (+fx i 1)
			  (cons `(,(car binding) (vector-ref ,vec ,i))
				nbindings))))))))

;*---------------------------------------------------------------------*/
;*    hop-roundtrip-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-roundtrip-expander x e)
   (match-case x
      ((?- ?bindings . ?body)
       (for-each (lambda (binding)
		    (match-case binding
		       (((and (? symbol?) ?id) ?-)
			#t)
		       (else
			(error '$roundtrip "Illegal binding" binding))))
		 bindings)
       (e (evepairify (expand-roundtrip bindings body) x) e))
      (else
       (error '$roundtrip "Illegal form" x))))


;*---------------------------------------------------------------------*/
;*    %invoke-service ...                                              */
;*---------------------------------------------------------------------*/
(define (%invoke-service form)
   (match-case form
      ((?svc . ?args)
       `((hop-service-proc ,svc) ,@args))
      (else
       (error 'with-hop "Illegal service invokation" form))))

;*---------------------------------------------------------------------*/
;*    expand-with-local-host ...                                       */
;*---------------------------------------------------------------------*/
(define (expand-with-local-host arg0 args)
   (cond
      ((null? args)
       `(with-hop-response ,(%invoke-service arg0) (lambda (v) v) #f))
      ((null? (cdr args))
       `(with-hop-response ,(%invoke-service arg0) ,(car args) #f))
      (else
       `(with-hop-response ,(%invoke-service arg0) ,(car args) ,(cadr args)))))

;*---------------------------------------------------------------------*/
;*    expand-with-hop ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-with-hop x arg0 args)
   (cond
      ((not (eq? arg0 :host))
       (expand-with-local-host arg0 args))
      ((null? args)
       (error 'with-hop "Illegal host" x))
      ((null? (cdr args))
       (error 'with-hop "Service missing" x))
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
	   (error 'with-hop "Illegal service invokation" x))))))

;*---------------------------------------------------------------------*/
;*    hop-with-hop-expander ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-with-hop-expander x e)
   (match-case x
      ((?- ?arg0 . ?args)
       (e (evepairify (expand-with-hop x arg0 args) x) e))
      (else
       (error 'with-hop "Illegal form" x))))
   
