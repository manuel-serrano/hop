;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 14:53:24 2005                          */
;*    Last change :  Wed Dec  6 15:06:43 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop macros                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    $service/filter ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($service/filter url wid timeout ttl args . body)
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
   (let (ca req proc exec hop path svc)
      (if (and (pair? wid)
	       (eq? (car wid) 'quote)
	       (symbol? (cadr wid)))
	  (begin
	     (set! ca (gensym (symbol-append (cadr wid) '-ca)))
	     (set! req (gensym (symbol-append (cadr wid) '-req)))
	     (set! proc (gensym (symbol-append (cadr wid) '-proc)))
	     (set! exec (gensym (symbol-append (cadr wid) '-exec)))
	     (set! hop (gensym (symbol-append (cadr wid) '-hop)))
	     (set! path (gensym (symbol-append (cadr wid) '-path)))
	     (set! svc (gensym (symbol-append (cadr wid) 'svc))))
	  (begin
	     (set! ca (gensym 'ca))
	     (set! req (gensym 'req))
	     (set! proc (gensym 'proc))
	     (set! exec (gensym 'exec))
	     (set! hop (gensym 'hop))
	     (set! path (gensym 'path))
	     (set! svc (gensym 'svc))))
      (let ((vargs (if (list? args) (service-js-arguments args) #f)))
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
		 (,path (make-url-name (hop-service-base) ,url))
		 (svc (instantiate::hop-service
			 (wid ,wid)
			 (id (string->symbol ,url))
			 (path ,path)
			 (args ',args)
			 (%exec ,exec)
			 (proc ,proc)
			 (javascript ,(jscript vargs path))
			 (creation (date->seconds (current-date)))
			 (timeout ,timeout)
			 (ttl ,ttl)
			 (resource (and (string? (the-loading-file))
					(dirname (the-loading-file))))
			 (source (and (string? (the-loading-file))
				      (basename (the-loading-file)))))))
	     (register-service! svc)))))

;*---------------------------------------------------------------------*/
;*    define-service ...                                               */
;*---------------------------------------------------------------------*/
(define-expander define-service
   (lambda (x e)
      (match-case x
	 ((?- (?id . ?args) . ?body)
	  (if (not (and (symbol? id) (every? symbol? args)))
	      (error 'define-service "Illegal service declaration" x)
	      (let* ((url (symbol->string id))
		     (wid (string->symbol (car (file-name->list url))))
		     (body `($service/filter ,url ',wid -1 -1 ,args ,@body)))
		 `(define ,id
		     ,(e (evepairify body x) e)))))
	 (else
	  (error 'define-service "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    service ...                                                      */
;*---------------------------------------------------------------------*/
(define-expander service
   (lambda (x e)
      (match-case x
	 ((?- . ?args)
	  (let loop ((a args)
		     (tmt '(hop-service-default-timeout))
		     (ttl -1))
	     (cond
		((or (symbol? (car a))
		     (and (list? (car a)) (every? symbol? (car a))))
		 (if (null? (cdr a))
		     (error 'service "Illegal service (empty body)" x)
		     (let ((body `($service/filter
				   (get-service-url)
				   (string->symbol (hop-service-weblet-name))
				   ,tmt ,ttl ,(car a) ,@(cdr a))))
			(e (evepairify body x) e))))
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
	  (error 'service "Illegal form" x)))))
	      
;*---------------------------------------------------------------------*/
;*    $roundtrip ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro ($roundtrip bindings . body)
   (for-each (lambda (binding)
		(match-case binding
		   (((and (? symbol?) ?id) ?-)
		    #t)
		   (else
		    (error '$roundtrip "Illegal binding" binding))))
	     bindings)
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
;*    with-hop ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (with-hop arg0 . args)
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
			      (lambda (v) v)
			      #f))
	 ((null? (cdr args))
	  `(with-hop-response ,(%invoke-service arg0)
			      ,(car args)
			      #f))
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
