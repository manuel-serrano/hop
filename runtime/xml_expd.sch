;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/xml_expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:27:30 2006                          */
;*    Last change :  Wed Jun 11 19:32:13 2014 (serrano)                */
;*    Copyright   :  2006-14 Manuel Serrano                            */
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
		  (body '()))
	  (cond
	     ((null? args)
	      (let ((attr (reverse attr)))
		 ,(if (null? exp)
		      `(,(symbol-append 'instantiate:: type)
			(tag ',el)
			(attributes attr)
			(body (reverse! body)))
		      `(let ((body (reverse! body))) ,@exp))))
	     ((keyword? (car args))
	      (if (null? (cdr args))
		  (loop '() (cons* #t (car args) attr) body)
		  (loop (cddr args) (cons* (cadr args) (car args) attr) body)))
	     ((null? (car args))
	      (loop (cdr args) attr body))
	     ((pair? (car args))
	      (loop (append (car args) (cdr args)) attr body))
	     (else
	      (loop (cdr args) attr (cons (car args) body)))))))

;*---------------------------------------------------------------------*/
;*    define-xml-constructor-with-id ...                               */
;*---------------------------------------------------------------------*/
(define (define-xml-constructor-with-id type name attr el exp)
   `(define (,name . args)
       (let loop ((args args)
		  (attr ',(reverse attr))
		  (body '())
		  (id #unspecified))
	  (cond
	     ((null? args)
	      ,(if (null? exp)
		   `(,(symbol-append 'instantiate:: type)
		     (tag ',el)
		     (id (xml-make-id id ',el))
		     (attributes (reverse attr))
		     (body (reverse! body)))
		   `(begin ,@exp)))
	     ((keyword? (car args))
	      (cond
		 ((null? (cdr args))
		  (if (eq? (car args) :id)
		      (error ,(symbol->string name)
			     ":id value missing"
			     (car args))
		      (loop '() (cons* #t (car args) attr) body id)))
		 ((eq? (car args) :id)
		  (if (string? (cadr args))
		      (loop (cddr args) attr body (cadr args))
		      (bigloo-type-error ,(symbol->string name) "string" (cadr args))))
		 (else
		  (loop (cddr args) (cons* (cadr args) (car args) attr) body id))))
	     ((null? (car args))
	      (loop (cdr args) attr body id))
	     ((pair? (car args))
	      (loop (append (car args) (cdr args)) attr body id))
	     (else
	      (loop (cdr args) attr (cons (car args) body) id))))))

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
		(tag (memq :tag exp))
		(at (memq :attributes exp))
		(attr '()))
	     (when (and (pair? tag) (pair? (cdr tag)))
		(set! el (cadr tag))
		(set-cdr! tag (cddr tag))
		(set! exp (remq! (car tag) exp)))
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
	  (error "define-xml" "Illegal identifier" name)))))

;*---------------------------------------------------------------------*/
;*    hop-define-xml-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-expander x e)
   (match-case x
      ((?- ?type ?with-id ?name . ?exp)
       (e (evepairify (expand-define-xml type with-id name exp) x) e))
      (else
       (error "define-xml" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    hop-define-xml-markup-expander ...                               */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-markup-expander x e)
   (match-case x
      ((?- ?name . ?exp)
       (e (evepairify (expand-define-xml 'xml-markup #f name exp) x) e))
      (else
       (error "define-xml-markup" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    define-element ...                                               */
;*---------------------------------------------------------------------*/
(define (define-element id el exp)
   (let ((el (string->symbol (string-downcase! (symbol->string el)))))
      (if (null? exp)
	  `(define (,id . args)
	      (%make-xml-element ',el args))
	  `(define-xml xml-element #t ,el ,@exp))))

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
	  (error "define-xml-element" "Illegal identifier" id))))

;*---------------------------------------------------------------------*/
;*    hop-define-xml-el-expander ...                                   */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-el-expander x e)
   (match-case x
      ((?- ?id . ?exp)
       (e (evepairify (expand-define-xml-element id exp) x) e))
      (else
       (error "define-xml-element" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    define-alias ...                                                 */
;*---------------------------------------------------------------------*/
(define (define-alias id ea opts)
   (let ((eal (string->symbol (string-downcase! (symbol->string ea)))))
      `(define (,id . args)
	  (%make-xml-element ',eal (append args (list ,@opts))))))

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
	  (error "define-xml-alias" "Illegal identifier" id))
	 ((not (and (>fx (string-length t) 2)
		    (char=? (string-ref t 0) #\<)
		    (char=? (string-ref t (-fx (string-length t) 1)) #\>)))
	  (error "define-xml-alias" "Illegal alias identifier" alias))
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
      (error "define-xml-alias" "Illegal form" x)))

;*---------------------------------------------------------------------*/
;*    define-tag ...                                                   */
;*---------------------------------------------------------------------*/
(define (define-tag m el bindings body loc)
   
   (define (predicate type id)
      (if (memq type '(int long string bstring bool char symbol keword double
		       boolean pair vector procedure integer real number
		       elong llong date list))
	  `(,(symbol-append type '?) ,id)
	  `(isa? ,id ,type)))
   
   (define (untyped-ident id)
      (let* ((string (symbol->string id))
	     (len (string-length string)))
	 (let loop ((walker  0))
	    (cond
	       ((=fx walker len)
		id)
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(string->symbol (substring string 0 walker)))
	       (else
		(loop (+fx walker 1)))))))
   
   (define (attach-loc new old loc)
      (let ((loc (or (get-source-location old) loc)))
	 (if loc
	     (econs (car new) (cdr new) loc)
	     new)))
   
   (let ((args (gensym 'args))
	 (loop (gensym 'loop))
	 (name (symbol->string m)))
      `(define (,m . ,args)
	  (let ,(map (lambda (b)
			(match-case b
			   (((and ?i (? symbol?)) ?init . ?-)
			    (attach-loc `(,i ,init) b loc))
			   (((and ?i (? symbol?)))
			    (attach-loc `(,i '()) b loc))
			   (else
			    (if (symbol? b)
				(attach-loc `(,b '()) b loc)
				(error name "Illegal binding" b)))))
		   bindings)
	     (let ,loop ((,args ,args))
		  (cond
		     ((null? ,args)
		      ,@(map (lambda (b)
				(match-case b
				   ((?id ?init (and (? symbol?) ?type))
				    (let ((id (untyped-ident id)))
				       `(unless (or ,(if (eq? init #f)
							 `(not ,id)
							 `(eq? ,id #unspecified))
						    ,(predicate type id))
					   ,(match-case (get-source-location b)
					       ((at ?fname ?pos)
						`(bigloo-type-error/location ,(symbol->string m)
						    (symbol->string ',type)
						    ,id ,fname ,pos))
					       (else
						`(bigloo-type-error ,(symbol->string m)
						    (symbol->string ',type)
						    ,id))))))
				   (((and ?id (? symbol?)))
				    (let ((id (untyped-ident id)))
				       `(set! ,id (reverse! ,id))))
				   ((and ?id (? symbol?))
				    (let ((id (untyped-ident id)))
				       `(set! ,id (reverse! ,id))))
				   (else
				    #unspecified)))
			   bindings)
		      (let () ,@body))
		     ,@(map (lambda (b)
			       (match-case b
				  (((and (? symbol?) ?id) ?- . ?-)
				   (let ((id (untyped-ident id)))
				      `((eq? (car ,args) ,(symbol->keyword id))
					(if (null? (cdr ,args))
					    (begin
					       (set! ,id #t)
					       (,loop '()))
					    (begin
					       (set! ,id (cadr ,args))
					       (,loop (cddr ,args)))))))
				  (((and (? symbol?) ?id))
				   (let ((id (untyped-ident id)))
				      `((keyword? (car ,args))
					(if (null? (cdr ,args))
					    (begin
					       (set! ,id #t)
					       (,loop '()))
					    (begin
					       (set! ,id (cons* (cadr ,args) (car ,args) ,id))
					       (,loop (cddr ,args)))))))
				  ((and ?id (? symbol?))
				   (let ((id (untyped-ident id)))
				      `((not (keyword? (car ,args)))
					(cond
					   ((xml-unpack (car ,args))
					    =>
					    (lambda (l) (,loop (append l (cdr ,args)))))
					   (else
					    (set! ,id (cons (car ,args) ,id))
					    (,loop (cdr ,args)))))))
				  (else
				   `((xml-unpack (car ,args))
				     =>
				     (lambda (l) (,loop (append l (cdr ,args))))))))
			bindings)
		     (else
		      (error ,name "Illegal argument" (car ,args)))))))))

;*---------------------------------------------------------------------*/
;*    expand-define-xml-compound ...                                   */
;*---------------------------------------------------------------------*/
(define (expand-define-xml-compound id bindings body loc)
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
			,(define-tag id el bindings body loc)))
		 (define-tag id el bindings body loc)))
	  (error "define-xml-compound" "Illegal identifier" id))))

;*---------------------------------------------------------------------*/
;*    hop-define-xml-cpd-expander ...                                  */
;*---------------------------------------------------------------------*/
(define (hop-define-xml-cpd-expander x e)
   (match-case x
      ((?- ?id ?bindings . ?body)
       (e (evepairify (expand-define-xml-compound id bindings body (get-source-location x)) x) e))
      (else
       (error "define-xml-compound" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    hop-client-define-xml-compound ...                               */
;*---------------------------------------------------------------------*/
(define (hop-client-define-xml-compound x e)
   (match-case x
      ((?- ?id ?bindings . ?body)
       (let* ((id2 (symbol-append '< id '>))
	      (nf (expand-define-xml-compound id bindings body (get-source-location x)))
	      (nm `(define-macro (,id . args)
		      (list 'quasiquote
			    (,id2 ,(list 'unquote-splicing 'args)))))
	      (nx nm))
	  (e (evepairify nx x) e)
	  (e (evepairify nf x) e)))
      (else
       (error "define-xml-compound" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-define-tag ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-define-tag id bindings body loc)
   (let ((s (symbol->string id)))
      (if (and (>fx (string-length s) 2)
	       (char=? (string-ref s 0) #\<)
	       (char=? (string-ref s (-fx (string-length s) 1)) #\>))
	  (let ((el (string->symbol (substring s 1 (-fx (string-length s) 1)))))
	     (define-tag id el bindings body loc))
	  (error "define-tag" "Illegal identifier" id))))

;*---------------------------------------------------------------------*/
;*    hop-server-define-tag ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-server-define-tag x e)
   (match-case x
      ((?- ?id ?bindings . ?body)
       (e (evepairify (expand-define-tag id bindings body (get-source-location x)) x) e))
      (else
       (error "define-tag" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    hop-client-define-tag ...                                        */
;*---------------------------------------------------------------------*/
(define (hop-client-define-tag x e)
   (match-case x
      ((?- ?id ?bindings . ?body)
       (let* ((id2 (symbol-append '< id '>))
	      (nf (expand-define-tag id2 bindings body (get-source-location x)))
	      (nm `(define-macro (,id . args)
		      (list 'quasiquote
			    (,id2 ,(list 'unquote-splicing 'args)))))
	      (nx nm))
	  (e (evepairify nx x) e)))
      (else
       (error "define-tag" "Illegal form" x))))
