;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/xml.sch                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 15:24:40 2005                          */
;*    Last change :  Wed Apr 12 17:38:53 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    XML macros                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-xml-element ...                                           */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml element id . exp)
   (define (define-xml-ident id el exp)
      `(define (,id . args)
	  (let loop ((args args)
		     (attr '())
		     (body '())
		     (id   #unspecified))
	     (cond
		((null? args)
		 ,(if (null? exp)
		      `(,(symbol-append 'instantiate:: element)
			(markup ',(string->symbol
				   (string-downcase
				    (symbol->string el))))
			(attributes attr)
			(id (xml-make-id id ',el))
			(body (reverse! body)))
		      `(begin ,@exp)))
		((keyword? (car args))
		 (if (null? (cdr args))
		     (error ',id "attribute value missing" (car args))
		     (if (eq? (car args) :id)
			 (if (string? (cadr args))
			     (loop (cddr args)
				   attr
				   body
				   (cadr args))
			     (bigloo-type-error ',id
						"string"
						(cadr args)))
			 (loop (cddr args)
			       (cons (cons (keyword->string (car args))
					   (cadr args))
				     attr)
			       body
			       id))))
		((null? (car args))
		 (loop (cdr args) attr body id))
		((pair? (car args))
		 (loop (append (car args) (cdr args)) attr body id))
		(else
		 (loop (cdr args) attr (cons (car args) body) id))))))
   (let ((s (symbol->string id)))
      (if (and (>fx (string-length s) 2)
	       (char=? (string-ref s 0) #\<)
	       (char=? (string-ref s (-fx (string-length s) 1)) #\>))
	  (let ((el (string->symbol (substring s 1 (-fx (string-length s) 1))))
		(css (memq :hss-type exp)))
	     (if (and (pair? css) (pair? (cdr css)))
		 (let ((new (cadr css)))
		    (set-cdr! css (cddr css))
		    (set! exp (remq! (car css) exp))
		    `(begin
			(hop-hss-type! ,(symbol->string el) ,new)
			,(define-xml-ident id el exp)))
		 (define-xml-ident id el exp)))
	  (error 'define-xml "Illegal identifier" id))))
	   
;*---------------------------------------------------------------------*/
;*    define-xml-element ...                                           */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml-element id . exp)
   (define (define-element id el exp)
      (if (null? exp)
	  `(define (,id . args)
	      (%make-xml-element ',el args))
	  `(define-xml xml-element ,el ,@exp)))
   (let ((s (symbol->string id)))
      (if (and (>fx (string-length s) 2)
		   (char=? (string-ref s 0) #\<)
		   (char=? (string-ref s (-fx (string-length s) 1)) #\>))
	  (let ((el (string->symbol (substring s 1 (-fx (string-length s) 1))))
		(css (memq :hss-type exp)))
	     (if (and (pair? css) (pair? (cdr css)))
		 (let ((new (cadr css)))
		    (set-cdr! css (cddr css))
		    (set! exp (remq! (car css) exp))
		    `(begin
			(hop-hss-type! ,(symbol->string el) ,new)
			,(define-element id el exp)))
		 (define-element id el exp)))
	 (error 'define-xml-element "Illegal identifier" id))))
	   
;*---------------------------------------------------------------------*/
;*    define-xml-alias ...                                             */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml-alias id alias . opts)
   (define (define-alias id ea opts)
      `(define (,id . args)
	  (%make-xml-element ',ea (append args ',opts))))
   (let ((s (symbol->string id))
	 (t (symbol->string alias)))
      (cond
	 ((not (and (>fx (string-length s) 2)
		    (char=? (string-ref s 0) #\<)
		    (char=? (string-ref s (-fx (string-length s) 1)) #\>)))
	  (error 'define-xml-alias "Illegal identifier" id))
	 ((not (and (>fx (string-length t) 2)
		    (char=? (string-ref t 0) #\<)
		    (char=? (string-ref t (-fx (string-length t) 1)) #\>)))
	  (error 'define-xml-alias "Illegal alias identifier" alias))
	 (else
	  (let ((el (string->symbol (substring s 1 (-fx (string-length s) 1))))
		(ea (string->symbol (substring t 1 (-fx (string-length t) 1))))
		(css (memq :hss-type opts)))
	     (if (and (pair? css) (pair? (cdr css)))
		 (let ((new (cadr css)))
		    (set-cdr! css (cddr css))
		    (set! opts (remq! (car css) opts))
		    `(begin
			(hop-hss-type! ,(symbol->string el) ,new)
			,(define-alias id ea opts)))
		 (define-alias id ea opts)))))))

;*---------------------------------------------------------------------*/
;*    define-xml-compound ...                                          */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml-compound id bindings . body)
   (define (define-compound m el bindings body)
      (let ((args (gensym 'args))
	    (loop (gensym 'loop)))
	 `(define (,m . ,args)
	     (let ,(map (lambda (b)
			   (match-case b
			      (((and ?i (? symbol?)) ?init . ?-)
			       `(,i ,init))
			      (((and ?i (? symbol?)))
			       `(,i '()))
			      (else
			       (if (symbol? b)
				   `(,b '())
				   (error m "Illegal binding" b)))))
			bindings)
		(let ,loop ((,args ,args))
		     (cond
			((null? ,args)
			 ,@(map (lambda (b)
				   (match-case b
				      ((?id ?- (and (? symbol?) ?type))
				       `(unless (or (eq? ,id #unspecified)
						    (,(symbol-append type '?) ,id))
					   (bigloo-type-error
					    ',m
					    (symbol->string ',type)
					    ,id)))
				      (((? symbol?))
				       `(set! ,(car b) (reverse! ,(car b))))
				      ((? symbol?)
				       `(set! ,b (reverse! ,b)))
				      (else
				       #unspecified)))
				bindings)
			 ,@body)
			,@(map (lambda (b)
				  (match-case b
				     (((and (? symbol?) ?id) ?init . ?-)
				      `((eq? (car ,args) ,(symbol->keyword id))
					(when (null? (cdr ,args))
					   (error ',m
						  ,(format "Illegal :~a" id)
						  (car ,args)))
					(set! ,id (cadr ,args))
					(,loop (cddr ,args))))
				     (((and (? symbol?) ?id))
				      `((keyword? (car ,args))
					(when (null? (cdr ,args))
					   (error ',m
						  ,(format "Illegal :~a" id)
						  (car ,args)))
					(set! ,id
					      (cons (cons
						     (keyword->symbol
						      (car ,args))
						     (cadr ,args)) ,id))
					(,loop (cddr ,args))))
				     ((? symbol?)
				      `((not (keyword? (car ,args)))
					(if (pair? (car ,args))
					    (set! ,b (append (reverse (car ,args)) ,b))
					    (set! ,b (cons (car ,args) ,b)))
					(,loop (cdr ,args))))
				     (else
				      `((pair? (car ,args))
					(,loop
					 (append (car ,args) (cdr ,args)))))))
			       bindings)
			(else
			 (error ',m "Illegal argument" (car ,args)))))))))
   (let ((s (symbol->string id)))
      (if (and (>fx (string-length s) 2)
	       (char=? (string-ref s 0) #\<)
	       (char=? (string-ref s (-fx (string-length s) 1)) #\>))
	  (let ((el (string->symbol (substring s 1 (-fx (string-length s) 1))))
		(css (memq :hss-type body)))
	     (if (and (pair? css) (pair? (cdr css)))
		 (let ((new (cadr css)))
		    (set-cdr! css (cddr css))
		    (set! body (remq! (car css) body))
		    `(begin
			(hop-hss-type! ,(symbol->string el) ,new)
			,(define-compound id el bindings body)))
		 (define-compound id el bindings body)))
	  (error 'define-xml-compound "Illegal identifier" id))))
