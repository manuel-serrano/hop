;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 14:53:24 2005                          */
;*    Last change :  Thu Feb  2 07:04:39 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop macros                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-weblet ...                                                */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-weblet decl . body)
   (define (weblet-body id args req ca url)
      (if (pair? args)
	  `(let* ((,ca (http-request-cgi-args ,req))
		  ,@args)
	      (if (or (eq? (http-request-method ,req) "HOP")
		      (equal? (cgi-arg "hop-encoding" ,ca) "hop"))
		  (begin
		     ,@(map (lambda (a)
			       `(set! ,a 
				      (serialized-cgi-arg
				       ,(symbol->string a) ,ca)))
			    args))
		  (begin
		     ,@(map (lambda (a)
			       `(set! ,a (cgi-arg ,(symbol->string a) ,ca)))
			    args)))
	      ,@body)
	  `(begin ,@body)))
   (if (not (and (list? decl) (every? symbol? decl)))
       (error 'define-weblet "Inccorect declaration" decl)
       (let ((id (car decl))
	     (args (cdr decl)))
	  (cond
	     ((any? (lambda (c) (char=? c #\/))
		    (string->list (symbol->string id)))
	      (error 'define-weblet
		     "Illegal weblet identifier (/ forbidden)"
		     id))
	     ((eq? id 'service)
	      (error 'define-weblet
		     "Reserved weblet identifier"
		     id))
	     (else
	      (let* ((rq (gensym 'req))
		     (url (symbol->string id))
		     (ca (gensym))
		     (def (gensym))
		     (pa (gensym)))
		 `(begin
		     (define ,id
			(instantiate::hop-request-filter
			   (base (hop-filter-base))
			   (name ,(symbol->string id))
			   (url ,url)))
		     (hop-weblets-set! (cons ,id (hop-weblets)))
		     (hop-filter-add!
		      (lambda (,rq)
			 (when (http-request-localhostp ,rq)
			    (let ((,pa (http-request-path ,rq)))
			       (cond
				  ((not (hop-weblet-prefix? ,pa ,url))
				   #f)
				  ((hop-weblet-path? ,pa ,url)
				   ,(if (pair? body)
					`(let ((the-current-request
						(lambda () ,rq)))
					    (scheme->response
					     ,(weblet-body id args rq ca url)
					     ,rq))
					#f))
				  ((hop-filter-path? ,pa ,url)
				   (scheme->response
				    (hop-service ,id ,rq) ,rq))
				  (else
				   #f)))))))))))))

;*---------------------------------------------------------------------*/
;*    $service/filter ...                                              */
;*---------------------------------------------------------------------*/
(define-pervasive-macro ($service/filter filter service args . body)
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
   (if (not (or (symbol? args) (and (list? args) (every? symbol? args))))
       (error '$service/filter "Illegal $service declaration" args)
       (let* ((ca (gensym))
	      (ta (gensym))
	      (url (gensym))
	      (req (gensym))
	      (proc (gensym))
	      (exec (gensym))
	      (hop (gensym))
	      (path (gensym))
	      (vargs (if (list? args)
			 (service-js-arguments args)
			 #f)))
	  `(let* ((,ta (hop-request-filter-table ,filter))
		  (,url (if (string? ,service) ,service (get-service-url)))
		  (,proc (lambda ,args ,@body))
		  (,exec (lambda (,req)
			    (let ((,ca (http-request-cgi-args ,req)))
			       ,(if (or (null? args) (pair? args))
				    `(if (or (eq? (http-request-method ,req) 'HOP)
					     (equal? (cgi-arg "hop-encoding" ,ca) "hop"))
					 (,proc ,@(map (lambda (a)
							  `(serialized-cgi-arg
							    ,(symbol->string a)
							    ,ca))
						       args))
					 (,proc ,@(map (lambda (a)
							  `(cgi-arg
							    ,(symbol->string a)
							    ,ca))
						       args)))
				    `(,proc (error '$service/filter
						   "not implement"
						   "yet"))))))
		  (,path (string-append (hop-filter-base)
					"/"
					(hop-request-filter-url ,filter)
					"/"
					,url))
		  (svc (instantiate::hop-request-service
			  (id (string->symbol ,url))
			  (path ,path)
			  (filter ,filter)
			  (args ',args)
			  (%exec ,exec)
			  (proc ,proc)
			  (javascript ,(jscript vargs path)))))
	      (hop-verb 2 (hop-color 1 1 " REG. SERVICE: ") svc " " ,path "\n")
	      (hashtable-put! ,ta ,path svc)
	      svc))))

;*---------------------------------------------------------------------*/
;*    define-service ...                                               */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-service decl . body)
   (define (service-filter ident)
      (let* ((s (symbol->string ident))
	     (l (string-length s)))
	 (if (char=? (string-ref s (-fx l 1)) #\/)
	     (error 'define-service "Illegal identifier (illegal ending /)"
		    ident)
	     (let loop ((i (-fx l 2)))
		(cond
		   ((=fx i 0)
		    (error 'define-service "Illegal identifier (missing /)"
			   ident))
		   ((char=? (string-ref s i) #\/)
		    (values (string->symbol (substring s 0 i))
			    (substring s (+fx i 1) l)))
		   (else
		    (loop (-fx i 1))))))))
   (if (not (and (pair? decl) (every? symbol? decl)))
       (error 'define-service "Illegal service declaration" decl)
       (let* ((id (car decl))
	      (args (cdr decl)))
	  (multiple-value-bind (filter service)
	      (service-filter id)
	      `(define ,id ($service/filter ,filter ',service ,args ,@body))))))

;*---------------------------------------------------------------------*/
;*    service ...                                                      */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (service args . body)
   `($service/filter anonymous #f ,args ,@body))
	      
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

