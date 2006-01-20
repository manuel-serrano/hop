;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/xml.sch                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 17 15:24:40 2005                          */
;*    Last change :  Fri Jan 20 09:18:02 2006 (serrano)                */
;*    Copyright   :  2005-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    XML macros                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-xml-element ...                                           */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml element el . exp)
   (define (define-xml el exp)
      (let ((id (symbol-append '< el '>)))
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
			(error ',el "attribute value missing" (car args))
			(let* ((s (keyword->string (car args)))
			       (l (string-length s))
			       (s2 (substring s 1 l)))
			   (unless (char=? (string-ref s 0) #\:)
			      (error ',el "illegal attribute" (car args)))
			   (if (eq? (car args) :id)
			       (if (string? (cadr args))
				   (loop (cddr args)
					 attr
					 body
					 (cadr args))
				   (bigloo-type-error ',el
						      "string"
						      (cadr args)))
			       (loop (cddr args)
				     (cons (cons s2 (cadr args)) attr)
				     body
				     id)))))
		   ((null? (car args))
		    (loop (cdr args) attr body id))
		   ((pair? (car args))
		    (loop (append (car args) (cdr args)) attr body id))
		   (else
		    (loop (cdr args) attr (cons (car args) body) id)))))))
   (let ((css (memq :hss-type exp)))
      (if (and (pair? css) (pair? (cdr css)))
	  (let ((new (cadr css)))
	     (set-cdr! css (cddr css))
	     (set! exp (remq! (car css) exp))
	     `(begin
		 (hop-hss-type! ,(symbol->string el) ,new)
		 ,(define-xml el exp)))
	  (define-xml el exp))))
	   
;*---------------------------------------------------------------------*/
;*    define-xml-element ...                                           */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml-element el . exp)
   (define (define-element el exp)
      (if (null? exp)
	  `(define (,(symbol-append '< el '>) . args)
	      (%make-xml-element ',el args))
	  `(define-xml xml-element ,el ,@exp)))
   (let ((css (memq :hss-type exp)))
      (if (and (pair? css) (pair? (cdr css)))
	  (let ((new (cadr css)))
	     (set-cdr! css (cddr css))
	     (set! exp (remq! (car css) exp))
	     `(begin
		 (hop-hss-type! ,(symbol->string el) ,new)
		 ,(define-element el exp)))
	  (define-element el exp))))
	   
;*---------------------------------------------------------------------*/
;*    define-xml-alias ...                                             */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml-alias el alias . opts)
   (define (define-alias el opts)
      `(define (,(symbol-append '< el '>) . args)
	  (%make-xml-element ',alias (append args ',opts))))
   (let ((css (memq :hss-type opts)))
      (if (and (pair? css) (pair? (cdr css)))
	  (let ((new (cadr css)))
	     (set-cdr! css (cddr css))
	     (set! opts (remq! (car css) opts))
	     `(begin
		 (hop-hss-type! ,(symbol->string el) ,new)
		 ,(define-alias el opts)))
	  (define-alias el opts))))

;*---------------------------------------------------------------------*/
;*    define-xml-compound ...                                          */
;*---------------------------------------------------------------------*/
(define-pervasive-macro (define-xml-compound el bindings . body)
   (define (define-compound el bindings body)
      (let ((m (symbol-append '< el '>))
	    (args (gensym 'args))
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
				      `((eq? (car ,args)
					     ,(string->keyword
					       (string-append
						":" (symbol->string id))))
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
   (let ((css (memq :hss-type body)))
      (if (and (pair? css) (pair? (cdr css)))
	  (let ((new (cadr css)))
	     (set-cdr! css (cddr css))
	     (set! body (remq! (car css) body))
	     `(begin
		 (hop-hss-type! ,(symbol->string el) ,new)
		 ,(define-compound el bindings body)))
	  (define-compound el bindings body))))
