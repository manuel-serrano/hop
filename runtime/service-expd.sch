;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/service-expd.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:36:28 2006                          */
;*    Last change :  Mon Jun 16 11:53:20 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
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
(define (expand-service id wid url timeout ttl args body)
   (let ((proc (if (symbol? wid) wid 'svc))
	 (errid (if (symbol? wid) `',wid wid)))
      `(let* ((,proc ,(if (pair? body)
			 `(lambda ,args ,@body)
			 `(lambda ,args
			     (let ((path (make-hop-service-url ,id ,@args)))
				(instantiate::http-response-remote
				   (path path)
				   (encoded-path path))))))
	      (exec ,(if (pair? body)
			 `(lambda (req)
			     (let* ((ca (http-request-cgi-args req))
				    (enc (cgi-arg "hop-encoding" ca)))
				(if (and (string? enc) (string=? enc "hop"))
				    (begin
				       (http-request-charset-set! req 'UTF-8)
				       (,proc ,@(map (lambda (a)
							`(serialized-cgi-arg
							  ,(symbol->string a) ca))
						     args)))
				    (,proc ,@(map (lambda (a)
						     `(cgi-arg
						       ,(symbol->string a) ca))
						  args)))))
			 `(lambda (req)
			     (error ',id
				    "Illegal service exec (imported service)"
				    ',args))))
	      (path ,url)
	      (file (the-loading-file))
	      (svc (instantiate::hop-service
		      (wid ,(if (symbol? wid) `',wid wid))
		      (id ,(if (symbol? id) `',id `(string->symbol ,url)))
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
	  (tprint "hop-service id=" ',id " wid=" ',wid " ttl=" ,ttl)
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
		  (svc (expand-service id wid `(make-hop-url-name ,url) -1 -1 args body)))
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
		  (ttl -1)
		  (url '(make-hop-url-name (get-service-url))))
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
			      #f
			      '(hop-service-weblet-wid)
			      url tmt ttl (car a) (cdr a))))
		     (e (evepairify svc x) e)))))
	     ((eq? (car a) :timeout)
	      (if (null? (cdr a))
		  (error 'service
			 "Illegal service declaration (missing timeout)"
			 x)
		  (loop (cddr a)
			(cadr a)
			ttl
			url)))
	     ((eq? (car a) :url)
	      (if (null? (cdr a))
		  (error 'service
			 "Illegal service declaration (missing url)"
			 x)
		  (loop (cddr a)
			tmt
			ttl
			(cadr a))))
	     ((eq? (car a) :name)
	      (if (null? (cdr a))
		  (error 'service
			 "Illegal service declaration (missing name)"
			 x)
		  (loop (cddr a)
			tmt
			ttl
			`(make-hop-url-name ,(cadr a)))))
	     ((eq? (car a) :ttl)
	      (if (null? (cdr a))
		  (error 'service
			 "Illegal service declaration (missing ttl)"
			 (cons 'service args))
		  (loop (cddr a)
			tmt
			(cadr a)
			url)))
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
;*    hop-with-hop-expander ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-with-hop-expander x e)
   (match-case x
      ((?- (?svc . ?a) . ?opts)
       (if (any? keyword? opts)
	   ;; a remote call
	   (let loop ((opts opts)
		      (args '())
		      (success #f)
		      (failure #f))
	      (cond
		 ((null? opts)
		  (let ((nx `(with-hop-remote (make-hop-service-url ,svc ,@a)
					      ,success ,failure
					      ,@(reverse! args))))
		     (e (evepairify nx x) e)))
		 ((not (keyword? (car opts)))
		  (cond
		     ((not success)
		      (loop (cdr opts) args (car opts) failure))
		     ((not failure)
		      (loop (cdr opts) args success (car opts)))
		     (else
		      (error 'with-hop
			     (format "Illegal optional argument: ~a" (car opts))
			     x))))
		 ((null? (cdr opts))
		  (error 'with-hop
			 (format "missing value for optional argument: ~a"
				 (car opts))
			 x))
		 (else
		  (loop (cddr opts)
			(cons* (cadr opts) (car opts) args)
			success failure))))
	   ;; a local call
	   (let ((nx `(with-hop-local ((hop-service-proc ,svc) ,@a) ,@opts)))
	      (e (evepairify nx x) e))))
      (else
       (error 'with-hop "Illegal form" x))))
   
