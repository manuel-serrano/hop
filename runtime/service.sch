;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/service.sch                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 14:53:24 2005                          */
;*    Last change :  Tue Mar 21 12:12:28 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop macros                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    $service/filter ...                                              */
;*---------------------------------------------------------------------*/
(define-pervasive-macro ($service/filter svcurl args . body)
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
	  (vargs (if (list? args) (service-js-arguments args) #f)))
      `(let* ((,url ,svcurl)
	      (,proc (lambda ,args ,@body))
	      (,exec (lambda (,req)
			,(if (null? args)
			     `(,proc)
			     `(let ((,ca (http-request-cgi-args ,req)))
				 ,(if (pair? args)
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
						    "yet")))))))
	      (,path (make-file-name (hop-service-base) ,url))
	      (svc (instantiate::hop-service
		      (id (string->symbol ,url))
		      (path ,path)
		      (args ',args)
		      (%exec ,exec)
		      (proc ,proc)
		      (javascript ,(jscript vargs path)))))
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
	      ($service/filter ,(symbol->string id) ,args ,@body)))))

;*---------------------------------------------------------------------*/
;*    service ...                                                      */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (service args . body)
   (if (not (or (symbol? args) (every? symbol? args)))
       (error 'service "Illegal service declaration" args)
       `($service/filter (get-service-url) ,args ,@body)))
	      
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
