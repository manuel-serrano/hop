;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/service-expd.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:36:28 2006                          */
;*    Last change :  Sun Mar 22 07:28:22 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This file implements the service expanders. It is used both      */
;*    at compile-time and runtime-time.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    jscript-funcall ...                                              */
;*---------------------------------------------------------------------*/
(define (jscript-funcall path args)
   `(format "function() { return hop_apply_url( ~s, arguments ) }" ,path))

;*---------------------------------------------------------------------*/
;*    expand-service ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-service id wid url timeout ttl args body)
   
   (define (pair->list args)
      (if (list? args)
	  args
	  (let loop ((args args))
	     (cond
		((null? args) '())
		((pair? args) (cons (car args) (loop (cdr args))))
		(else (list args))))))

   (define (args->list args)
      (pair->list (dsssl-formals->scheme-formals args error)))
   
   (let ((proc (if (symbol? wid) (symbol-append wid '-proc) 'proc))
	 (hdl (if (symbol? wid) (symbol-append wid '-handler) 'hdl))
	 (svc (if (symbol? wid) (symbol-append wid '-svc) 'svc))
	 (errid (if (symbol? wid) `',wid wid))
	 (id (if (symbol? id) `',id `(string->symbol ,url)))
	 (path (gensym 'path))
	 (fun (gensym 'fun))
	 (vars (args->list args)))
      `(let* ((,path ,url)
	      (,proc ,(if (pair? body)
			  `(lambda ,args ,@body)
			  `(lambda ,args
			      (instantiate::http-response-remote
				 (port (hop-port))
				 (path (,fun ,@vars))))))
	      (file (the-loading-file))
	      (,svc (instantiate::hop-service
		       (wid ,(if (symbol? wid) `',wid wid))
		       (id ,id)
		       (path ,path)
		       (args ',args)
		       (proc ,proc)
		       (javascript ,(jscript-funcall path args))
		       (creation (date->seconds (current-date)))
		       (timeout ,timeout)
		       (ttl ,ttl)
		       (resource (and (string? file) (dirname file)))
		       (source (and (string? file) (basename file)))))
	      (,fun (lambda ,args
		       (service-funcall-url ,svc ,@vars))))

	  ,(when (pair? body)
	     `(register-service! ,svc))
	  
	  (procedure-attr-set! ,fun ,svc)
	  ,fun)))
   
;*---------------------------------------------------------------------*/
;*    hop-define-service-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (hop-define-service-expander x e)
   (match-case x
      ((?- (?id . ?args) . ?body)
       (let* ((url (symbol->string id))
	      (wid (string->symbol (car (file-name->list url))))
	      (svc (expand-service id wid `(make-hop-url-name ,url) -1 -1 args body)))
	  `(define ,id ,(e (evepairify svc x) e))))
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
	     ((or (symbol? (car a)) (null? (car a)) (pair? (car a)))
	      (let ((svc (expand-service
			  #f
			  '(hop-service-weblet-wid)
			  url tmt ttl (car a) (cdr a))))
		 (e (evepairify svc x) e)))
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
		  (let ((nx `(with-hop-remote (,svc ,@a)
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
	   (let ((nx `(with-hop-local ((hop-service-proc (procedure-attr ,svc))
				       ,@a)
				      ,(when (pair? opts)
					  (car opts))
				      ,(when (and (pair? opts)
						  (pair? (cdr opts)))
					  (cadr opts)))))
	      (e (evepairify nx x) e))))
      (else
       (error 'with-hop "Illegal form" x))))
   
