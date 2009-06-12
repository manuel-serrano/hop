;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/xml-expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:27:30 2006                          */
;*    Last change :  Fri Jun 12 15:34:04 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    XML expanders                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    define-xml-constructor ...                                       */
;*---------------------------------------------------------------------*/
(define (define-xml-constructor type name attr el exp)
   `(define (,name . args)
       (let loop ((args args)
		  (attr ',attr)
		  (init '())
		  (body '()))
	  (cond
	     ((null? args)
	      ,(if (null? exp)
		   `(,(symbol-append 'instantiate:: type)
		     (markup ',el)
		     (attributes (reverse! attr))
		     (initializations (reverse! init))
		     (body (reverse! body)))
		   `(begin ,@exp)))
	     ((keyword? (car args))
	      (cond
		 ((null? (cdr args))
		  (loop '() attr
			(cons* #t (car args) init)
			body))
		 ((and (xml-tilde? (cadr args))
		       (not (xml-event-handler-attribute? (car args))))
		  (loop (cddr args)
			attr
			(cons* (cadr args) (car args) init)
			body))
		 (else
		  (loop (cddr args)
			(cons* (cadr args) (car args) attr)
			init body))))
	     ((null? (car args))
	      (loop (cdr args) attr init body))
	     ((pair? (car args))
	      (loop (append (car args) (cdr args)) attr init body))
	     (else
	      (loop (cdr args) attr init (cons (car args) body)))))))

;*---------------------------------------------------------------------*/
;*    define-xml-constructor-with-id ...                               */
;*---------------------------------------------------------------------*/
(define (define-xml-constructor-with-id type name attr el exp)
   `(define (,name . args)
       (let loop ((args args)
		  (attr ',attr)
		  (init '())
		  (body '())
		  (id   #unspecified))
	  (cond
	     ((null? args)
	      ,(if (null? exp)
		   `(,(symbol-append 'instantiate:: type)
		     (markup ',el)
		     (id (xml-make-id id ',el))
		     (attributes (reverse! attr))
		     (initializations (reverse! init))
		     (body (reverse! body)))
		   `(begin ,@exp)))
	     ((keyword? (car args))
	      (cond
		 ((null? (cdr args))
		  (if (eq? (car args) :id)
		      (error ',name ":id value missing" (car args))
		      (loop '() attr
			    (cons* #t (car args) init)
			    body id)))
		 ((eq? (car args) :id)
		  (if (string? (cadr args))
		      (loop (cddr args) attr init body (cadr args))
		      (bigloo-type-error ',name "string" (cadr args))))
		 ((and (xml-tilde? (cadr args))
		       (not (xml-event-handler-attribute? (car args))))
		  (loop (cddr args) attr
			(cons* (cadr args) (car args) init)
			body id))
		 (else
		  (loop (cddr args)
			(cons* (cadr args) (car args) attr)
			init body id))))
	     ((null? (car args))
	      (loop (cdr args) attr init body id))
	     ((pair? (car args))
	      (loop (append (car args) (cdr args)) attr init body id))
	     (else
	      (loop (cdr args) attr init (cons (car args) body) id))))))

;*---------------------------------------------------------------------*/
;*    expand-define-xml ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-define-xml type with-id name exp)
   (let ((s (symbol->string name)))
      (cond
	 ((and (>fx (string-length s) 2)
	       (char=? (string-ref s 0) #\<)
	       (char=? (string-ref s (-fx (string-length s) 1)) #\>))
	  (let ((el (string->symbol
		     (string-downcase!
		      (substring s 1 (-fx (string-length s) 1)))))
		(css (memq :hss-type exp))
		(markup (memq :markup exp))
		(at (memq :attributes exp))
		(attr '()))
	     (when (and (pair? markup) (pair? (cdr markup)))
		(set! el (cadr markup))
		(set-cdr! markup (cddr markup))
		(set! exp (remq! (car markup) exp)))
	     (when (and (pair? at) (pair? (cdr at)))
		(set! attr (cadr at))
		(set-cdr! at (cddr at))
		(set! exp (remq! (car at) exp)))
	     (if (and (pair? css) (pair? (cdr css)))
		 (let ((new (cadr css)))
		    (set-cdr! css (cddr css))
		    (set! exp (remq! (car css) exp))
		    `(begin
			(hop-hss-type! ,(symbol->string el) ,new)
			,(if with-id
			     (define-xml-constructor-with-id type name attr el exp)
			     (define-xml-constructor type name attr el exp))))
		 (if with-id
		     (define-xml-constructor-with-id type name attr el exp)
		     (define-xml-constructor type name attr el exp)))))
	 (else
	  (error 'define-xml "Illegal identifier" name)))))

;*---------------------------------------------------------------------*/
;*    hop-define-xml-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-expander x e)
   (match-case x
      ((?- ?type ?with-id ?name . ?exp)
       (e (evepairify (expand-define-xml type with-id name exp) x) e))
      (else
       (error 'define-xml "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    hop-define-xml-markup-expander ...                               */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-markup-expander x e)
   (match-case x
      ((?- ?name . ?exp)
       (e (evepairify (expand-define-xml 'xml-markup #f name exp) x) e))
      (else
       (error 'define-xml-markup "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    define-element ...                                               */
;*---------------------------------------------------------------------*/
(define (define-element id el exp)
   (if (null? exp)
       `(define (,id . args)
	   (%make-xml-element ',el args))
       `(define-xml xml-element #t ,el ,@exp)))

;*---------------------------------------------------------------------*/
;*    expand-define-xml-element ...                                    */
;*---------------------------------------------------------------------*/
(define (expand-define-xml-element id exp)
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
;*    hop-define-xml-el-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-el-expander x e)
   (match-case x
      ((?- ?id . ?exp)
       (e (evepairify (expand-define-xml-element id exp) x) e))
      (else
       (error 'define-xml-element "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    define-alias ...                                                 */
;*---------------------------------------------------------------------*/
(define (define-alias id ea opts)
   `(define (,id . args)
       (%make-xml-element ',ea (append args ',opts))))

;*---------------------------------------------------------------------*/
;*    expand-define-xml-alias ...                                      */
;*---------------------------------------------------------------------*/
(define (expand-define-xml-alias id alias opts)
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
;*    hop-define-xml-alias-expander ...                                */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-alias-expander x e)
   (match-case x
      ((?- ?id ?alias . ?opts)
       (e (evepairify (expand-define-xml-alias id alias opts) x) e))
      (error 'define-xml-alias "Illegal form" x)))

;*---------------------------------------------------------------------*/
;*    define-compound ...                                              */
;*---------------------------------------------------------------------*/
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
		      (let () ,@body))
		     ,@(map (lambda (b)
			       (match-case b
				  (((and (? symbol?) ?id) ?init . ?-)
				   `((eq? (car ,args) ,(symbol->keyword id))
				     (if (null? (cdr ,args))
					 (begin
					    (set! ,id #t)
					    (,loop '()))
					 (begin
					    (set! ,id (cadr ,args))
					    (,loop (cddr ,args))))))
				  (((and (? symbol?) ?id))
				   `((keyword? (car ,args))
				     (if (null? (cdr ,args))
					 (begin
					    (set! ,id #t)
					    (,loop '()))
					 (begin
					    (set! ,id (cons* (cadr ,args) (car ,args) ,id))
					    (,loop (cddr ,args))))))
				  ((? symbol?)
				   `((not (keyword? (car ,args)))
				     (if (pair? (car ,args))
					 (set! ,b (append (reverse (car ,args)) ,b))
					 (set! ,b (cons (car ,args) ,b)))
				     (,loop (cdr ,args))))
				  (else
				   `((pair? (car ,args))
				     (,loop (append (car ,args) (cdr ,args)))))))
			    bindings)
		     (else
		      (error ',m "Illegal argument" (car ,args)))))))))

;*---------------------------------------------------------------------*/
;*    expand-define-xml-compound ...                                   */
;*---------------------------------------------------------------------*/
(define (expand-define-xml-compound id bindings body)
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

;*---------------------------------------------------------------------*/
;*    hop-define-xml-cpd-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-cpd-expander x e)
   (match-case x
      ((?- ?id ?bindings . ?body)
       (e (evepairify (expand-define-xml-compound id bindings body) x) e))
      (else
       (error 'define-xml-compound "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-define-markup ...                                         */
;*---------------------------------------------------------------------*/
(define (expand-define-markup id bindings body)
   (let ((s (symbol->string id)))
      (if (and (>fx (string-length s) 2)
	       (char=? (string-ref s 0) #\<)
	       (char=? (string-ref s (-fx (string-length s) 1)) #\>))
	  (let ((el (string->symbol (substring s 1 (-fx (string-length s) 1)))))
	     (define-compound id el bindings body))
	  (error 'define-markup "Illegal identifier" id))))

;*---------------------------------------------------------------------*/
;*    hop-server-define-markup ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-server-define-markup x e)
   (match-case x
      ((?- ?id ?bindings . ?body)
       (e (evepairify (expand-define-markup id bindings body) x) e))
      (else
       (error 'define-markup "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    hop-client-define-markup ...                                     */
;*---------------------------------------------------------------------*/
(define (hop-client-define-markup x e)
   (match-case x
      ((?- ?id ?bindings . ?body)
       (let* ((id2 (symbol-append '< id '>))
	      (nf (expand-define-markup id2 bindings body))
	      (nm `(define-macro (,id . args)
		      (list 'quasiquote
			    (,id2 ,(list 'unquote-splicing 'args)))))
	      (nx `(begin ,nf ,nm))
	      (nx nm))
	  (pp nx)
	  (e (evepairify nx x) e)))
      (else
       (error 'define-markup "Illegal form" x))))
