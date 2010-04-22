;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/service-expd.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 16:36:28 2006                          */
;*    Last change :  Thu Apr 22 14:27:36 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
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
      (filter-map (lambda (f)
		     (cond
			((symbol? f) f)
			((and (pair? f) (symbol? (car f))) (car f))
			(else #f)))
		  (pair->list args)))

   (define (call args)
      (let loop ((args args)
		 (state 'plain))
	 (cond
	    ((null? args)
	     '())
	    ((symbol? args)
	     (list args))
	    ((symbol? (car args))
	     (case state
		((plain optional)
		 (cons (car args) (loop (cdr args) state)))
		((key)
		 (cons* (symbol->keyword (car args))
			(car args)
			(loop (cdr args) state)))))
	    ((and (pair? (car args)) (symbol? (caar args)))
	     (case state
		((key)
		 (cons* (symbol->keyword (caar args))
			(caar args)
			(loop (cdr args) state)))
		((optional)
		 (cons (caar args)
		       (loop (cdr args) state)))
		(else
		 (error 'service "Illegal definition" id))))
	    ((eq? (car args) #!key)
	     (loop (cdr args) 'key))
	    ((eq? (car args) #!optional)
	     (loop (cdr args) 'optional))
	    ((eq? (car args) #!rest)
	     (list (cadr args)))
	    (else
	     (error 'service "Illegal definition" id)))))

   (let* ((proc (if (symbol? wid) (symbol-append wid '%) 'proc))
	  (hdl (if (symbol? wid) (symbol-append wid '-handler) 'hdl))
	  (svc (if (symbol? wid) (symbol-append wid '-svc) 'svc))
	  (errid (if (symbol? wid) `',wid wid))
	  (id (if (symbol? id) `',id `(string->symbol ,url)))
	  (path (gensym 'path))
	  (fun (gensym 'fun))
	  (file (gensym 'file))
	  (actuals (call args))
	  (mkurl (if (and (pair? args)
			  (list? args)
			  (eq? (car args) #!key)
			  (every? symbol? (cdr args)))
		     `(hop-apply-nice-url ,path (list ,@actuals))
		     `(hop-apply-url ,path (list ,@actuals)))))
      `(let* ((,path ,url)
	      (,file (the-loading-file))
	      (,fun (lambda ,args ,mkurl))
	      (,proc ,(if (pair? body)
			  `(lambda ,args ,@body)
			  `(lambda ,args
			      (instantiate::http-response-remote
				 (port (hop-port))
				 (path ,mkurl)))))
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
		       (resource (and (string? ,file) (dirname ,file)))
		       (source (and (string? ,file) (basename ,file))))))

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
		  (url '(make-hop-url-name (gen-service-url))))
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
   
   (define (with-hop-local svc args success failure auth)
      `(with-hop-local ((hop-service-proc (procedure-attr ,svc)) ,@args)
		       ,success
		       ,failure
		       ,auth))

   (define (with-hop-remote svc args success fail opts)
      `(with-hop-remote (,svc ,@args) ,success ,fail ,@(reverse! opts)))
   
   (match-case x
      ((?- (?svc . ?a) . ?opts)
       ;; a remote call
       (let loop ((opts opts)
		  (args '())
		  (success #f)
		  (fail #f)
		  (host #f)
		  (port #f)
		  (sync #f)
		  (auth #f))
	  (cond
	     ((null? opts)
	      (let ((nx (if (and (not host) (not port))
			    ;; a local call
			    (let ((wh (with-hop-local svc a success fail auth)))
			       (if sync
				   (let ((v (gensym)))
				      `(let ((,v ,wh))
					  (if ,sync ,v #unspecified)))
				   `(begin ,wh #unspecified)))
			    ;; a remote call
			    (with-hop-remote svc a success fail args))))
		 (e (evepairify nx x) e)))
	     ((not (keyword? (car opts)))
	      (cond
		 ((not success)
		  (loop (cdr opts) args (car opts) fail host port sync auth))
		 ((not fail)
		  (loop (cdr opts) args success (car opts) host port sync auth))
		 (else
		  (error 'with-hop
			 (format "Illegal optional argument: ~a" (car opts))
			 x))))
	     ((null? (cdr opts))
	      (error 'with-hop
		     (format "missing value for optional argument: ~a"
			     (car opts))
		     x))
	     ((eq? (car opts) :host)
	      (loop (cddr opts)
		    (cons* (cadr opts) (car opts) args)
		    success fail
		    (cadr opts)
		    port
		    sync auth))
	     ((eq? (car opts) :port)
	      (loop (cddr opts)
		    (cons* (cadr opts) (car opts) args)
		    success fail
		    host
		    (cadr opts)
		    sync auth))
	     ((eq? (car opts) :sync)
	      (loop (cddr opts)
		    (cons* (cadr opts) (car opts) args)
		    success fail
		    host port
		    (cadr opts)
		    auth))
	     ((eq? (car opts) :authorization)
	      (loop (cddr opts)
		    (cons* (cadr opts) (car opts) args)
		    success fail
		    host port sync
		    (cadr opts)))
	     (else
	      (loop (cddr opts)
		    (cons* (cadr opts) (car opts) args)
		    success fail host port sync auth)))))
      (else
       (error 'with-hop "Illegal form" x))))
   
