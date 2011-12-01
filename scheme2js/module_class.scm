;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/scheme2js/module_class.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 23 11:24:26 2011                          */
;*    Last change :  Thu Dec  1 12:30:03 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme2JS class compiler                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module-class
   
   (import module-system
	   export-desc
	   error
	   tools)
   
   (static (class sjsslot
	      (id::symbol read-only)
	      (type::obj (default #f))
	      (class-owner (default #unspecified))
	      (read-only?::bool read-only (default #f))
	      (default-value read-only (default (slot-no-default-value)))
	      (virtual-num (default -1))
	      (getter (default #f))
	      (setter (default #f))
	      (user-info read-only (default #unspecified)))

	   (class sjsclass
	      (id::symbol read-only)
	      (holder::obj read-only)
	      (its-super::sjsclass read-only (default (class-nil sjsclass)))
	      (slots::pair-nil (default '()))
	      (constructor::obj (default #f))))
	   
   (export (parse-module-class! ::WIP-Unit ::pair-nil ::bool ::bool)))

;*---------------------------------------------------------------------*/
;*    class-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (class-error loc proc msg obj)
   (scheme2js-error proc msg obj loc))

;*---------------------------------------------------------------------*/
;*    *object* ...                                                     */
;*---------------------------------------------------------------------*/
(define *object*
   (instantiate::sjsclass
      (id 'object)
      (holder '(@ sc_Object js))))

;*---------------------------------------------------------------------*/
;*    scheme2js-find-class ...                                         */
;*---------------------------------------------------------------------*/
(define (scheme2js-find-class id env)
   (if (eq? id 'object)
       *object*
       (find (lambda (c)
		(with-access::sjsclass c ((cid id))
		   (eq? cid id)))
	  env)))

;*---------------------------------------------------------------------*/
;*    scheme2js-class-exists ...                                       */
;*---------------------------------------------------------------------*/
(define (scheme2js-class-exists id env)
   (scheme2js-find-class id env))

;*---------------------------------------------------------------------*/
;*    slot-no-default-value ...                                        */
;*---------------------------------------------------------------------*/
(define (slot-no-default-value)
   (cons 1 2))

;*---------------------------------------------------------------------*/
;*    slot-default? ...                                                */
;*---------------------------------------------------------------------*/
(define (slot-default? slot)
   (with-access::sjsslot slot (default-value)
      (not (equal? default-value (slot-no-default-value)))))

;*---------------------------------------------------------------------*/
;*    slot-virtual? ...                                                */
;*---------------------------------------------------------------------*/
(define (slot-virtual? slot)
   (with-access::sjsslot slot (getter)
      getter))

;*---------------------------------------------------------------------*/
;*    find-slot-offset ...                                             */
;*---------------------------------------------------------------------*/
(define (find-slot-offset slots::pair-nil name::symbol form sexp)
   (let loop ((slots slots)
	      (i 0))
      (cond
	 ((null? slots)
	  (error form (format "Field unknown \"~a\"" name) sexp))
	 ((with-access::sjsslot (car slots) (id) (eq? id name))
	  i)
	 (else   
	  (loop (cdr slots) (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    decompose-ident ...                                              */
;*---------------------------------------------------------------------*/
(define (decompose-ident id::symbol)
   (let* ((string (symbol->string! id))
	  (len (string-length string)))
      (let loop ((walker  0))
	 (cond
	    ((=fx walker len)
	     (values id #f))
	    ((and (char=? (string-ref string walker) #\:)
		  (<fx walker (-fx len 1))
		  (char=? (string-ref string (+fx walker 1)) #\:))
	     (values (string->symbol (substring string 0 walker))
		     (string->symbol (substring string (+fx walker 2) len))))
	    (else
	     (loop (+fx walker 1)))))))

;*---------------------------------------------------------------------*/
;*    localize ...                                                     */
;*---------------------------------------------------------------------*/
(define (localize loc p)
   (if (or (not loc) (not (epair? loc)))
       p
       (let loop ((p p))
	  (if (or (epair? p) (not (pair? p)))
	      p
	      (econs (loop (car p)) (loop (cdr p)) (cer loc))))))

;*---------------------------------------------------------------------*/
;*    parse-class-slot ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-class-slot loc f classes)
   (cond
      ((symbol? f)
       (multiple-value-bind (id type)
	  (decompose-ident f)
	  (list (instantiate::sjsslot
		   (id id)
		   (type (scheme2js-class-exists type classes))
		   (virtual-num 0)))))
      ((not (and (list? f) (symbol? (car f))))
       (class-error (or (get-source-location f) loc)
	  "scheme2js" "Illegal slot declaration" f))
      (else
       (let ((id (car f))
	     (attrs (cdr f)))
	  (multiple-value-bind (id type)
	     (decompose-ident id)
	     (let ((def #f)
		   (get #f)
		   (set #f)
		   (info #f)
		   (ronly #f))
		(for-each (lambda (attr)
			     (cond
				((eq? attr 'read-only)
				 (set! ronly #t))
				(else
				 (match-case attr
				    ((info ?value)
				     (set! info value))
				    ((get ?expr)
				     (if (symbol? expr)
					 (let ((o (gensym)))
					    (set! get `(lambda (,o)
							  (,expr ,o))))
					 (set! get expr)))
				    ((set ?expr)
				     (if (symbol? expr)
					 (let ((o (gensym))
					       (v (gensym)))
					    (set! set `(lambda (,o ,v)
							  (,expr ,o ,v))))
					 (set! set expr)))
				    ((default ?expr)
				     (set! def expr))
				    (else
				     (class-error
					(or (get-source-location f) loc)
					"scheme2js" "Illegal slot declaration" f))))))
		   attrs)
		(cond
		   ((and get (not ronly) (not set))
		    (class-error
		       (or (get-source-location f) loc)
		       "scheme2js" "Missing virtual set" f))
		   ((and set (not get))
		    (class-error
		       (or (get-source-location f) loc)
		       "scheme2js" "Missing virtual get" f))
		   (else
		    (let ((s (instantiate::sjsslot
				(id id)
				(type (scheme2js-class-exists type classes))
				(read-only? ronly)
				(default-value def)
				(virtual-num 0)
				(getter get)
				(setter set)
				(user-info info))))
		       (list s))))))))))

;*---------------------------------------------------------------------*/
;*    parse-class ...                                                  */
;*    -------------------------------------------------------------    */
;*    Parse the class clauses, returning the constructor and           */
;*    the new slots.                                                   */
;*---------------------------------------------------------------------*/
(define (parse-class loc clauses classes)
   (let ((loc (or (get-source-location clauses) loc)))
      (cond
	 ((null? clauses)
	  (values #f '()))
	 ((not (list? clauses))
	  (class-error (or (get-source-location clauses) loc)
			   "scheme2js" "Illegal class declaration" clauses))
	 ((match-case (car clauses) (((? symbol?)) #t) (else #f))
	  ;; the constructor must be protected under a lambda because
	  ;; may be still uninitialized
	  (values `(lambda (o) (,(caar clauses) o))
		  (append-map (lambda (f)
				 (parse-class-slot loc f classes))
			      (cdr clauses))))
	 (else
	  (values #f
		  (append-map (lambda (f)
				 (parse-class-slot loc f classes))
			      clauses))))))

;*---------------------------------------------------------------------*/
;*    get-class-hash ...                                               */
;*---------------------------------------------------------------------*/
(define (get-class-hash def)
   
   (define (gethash v)
      (bit-and (get-hashnumber v) #xffff))
   
   (let loop ((def def)
	      (hash 1705))
      (cond
	 ((null? def)
	  hash)
	 ((not (pair? def))
	  (bit-xor (gethash def) hash))
	 (else
	  (loop (cdr def)
	     (loop (car def)
		(bit-xor 1966 hash)))))))

;*---------------------------------------------------------------------*/
;*    sjs-instantiate-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (sjs-instantiate-expander class)
   (lambda (x e)
      (match-case x
	 ((?- . ?-)
	  (localize (get-source-location x)
	     (instantiate->make x class e)))
	 (else
	  (error "instantiate" "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    sjs-duplicate-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (sjs-duplicate-expander class)
   (lambda (x e)
      (match-case x
	 ((?- ?dup . ?-)
	  (localize (get-source-location x)
	     (duplicate->make x class dup e)))
	 (else
	  (error "instantiate" "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    allocate-expr ...                                                */
;*---------------------------------------------------------------------*/
(define (allocate-expr class::sjsclass)
   (with-access::sjsclass class (holder)
      `(js-new ,holder)))

;*---------------------------------------------------------------------*/
;*    instantiate->make ...                                            */
;*---------------------------------------------------------------------*/
(define (instantiate->make x class::sjsclass e)
   (let ((o (allocate-expr class)))
      (instantiate-fill (car x) (cdr x) class
	 (with-access::sjsclass class (slots) slots)
	 o x e)))

;*---------------------------------------------------------------------*/
;*    instantiate-fill ...                                             */
;*---------------------------------------------------------------------*/
(define (instantiate-fill op provided class slots init x e)
   
   (define (literal? n)
      (or (number? n)
	  (string? n)
	  (null? n)
	  (char? n)
	  (boolean? n)
	  (eq? n #unspecified)
	  (match-case n
	     ((quote . ?-) #t)
	     (else #f))))
   
   (define (slot-default-expr s)
      (with-access::sjsclass class (holder)
	 (with-access::sjsslot s (id default-value)
	    (if (literal? default-value)
		default-value
		`(class-field-default-value
		    (find-class-field ,holder ',id))))))
   
   (define (collect-slot-values slots)
      (let ((vargs (make-vector (length slots))))
	 ;; collect the default values
	 (let loop ((i 0)
		    (slots slots))
	    (when (pair? slots)
	       (let ((s (car slots)))
		  (cond
		     ((slot-default? s)
		      (vector-set! vargs i (cons #t (slot-default-expr s))))
		     (else
		      (vector-set! vargs i (cons #f #unspecified))))
		  (loop (+fx i 1) (cdr slots)))))
	 ;; collect the provided values
	 (let loop ((provided provided))
	    (when (pair? provided)
	       (let ((p (car provided)))
		  (match-case p
		     (((and (? symbol?) ?s-name) ?value)
		      ;; plain slot
		      (let ((pval (vector-ref
				     vargs
				     (find-slot-offset slots s-name op p))))
			 (set-car! pval #t)
			 (set-cdr! pval (localize (get-source-location p)
					   value))))
		     (else
		      (error op "Illegal argument \"~a\"" x)))
		  (loop (cdr provided)))))
	 ;; build the result
	 (vector->list vargs)))

   (with-access::sjsclass class (id)
      (let* ((new (gensym 'new))
	     (tnew (make-typed-ident new id))
	     (args (collect-slot-values slots)))
	 ;; check that there is enough values
	 (for-each (lambda (a s)
		      (unless (or (car a) (slot-virtual? s))
			 ;; value missin
			 (error op
			    (format "Missing value for field \"~a\""
			       (with-access::sjsslot s (id) id))
			    x)))
	    args slots)
	 ;; allocate the object and set the fields,
	 ;; first the actual fields, second the virtual fields
	 `(let ((,tnew ,init))
	     ;; actual fields
	     ,@(filter-map (lambda (slot val)
			      (unless (slot-virtual? slot)
				 (let ((v (e (cdr val) e)))
				    (with-access::sjsslot slot (id)
				       `(set! (-> ,new ,id) ,v)))))
		  slots args)
	     ;; constructors
	     ,@(map (lambda (c) (e `(,c ,new) e))
		  (find-class-constructors class))	
	     ;; virtual fields
	     ,@(filter-map (lambda (slot val)
			      (with-access::sjsslot slot (read-only? id)
				 (when (and (slot-virtual? slot)
					    (not read-only?)
					    (car val))
				    (let ((v (e (cdr val) e)))
				       `(set! (-> ,new ,id) ,v)))))
		  slots args)
	     ;; return the new instance
	     ,new))))

;*---------------------------------------------------------------------*/
;*    duplicate->make ...                                              */
;*---------------------------------------------------------------------*/
(define (duplicate->make x class::sjsclass duplicated e)
   
   (define (collect-slot-values slots dupvar)
      (let ((vargs (make-vector (length slots))))
	 ;; collect the provided values
	 (let loop ((provided (cddr x)))
	    (when (pair? provided)
	       (let ((p (car provided)))
		  (match-case p
		     (((and (? symbol?) ?s-name) ?value)
		      ;; plain slot
		      (vector-set! vargs
			 (find-slot-offset slots s-name (car x) p)
			 (cons #t (localize (get-source-location p) value))))
		     (else
		      (error (car x) "Illegal argument \"~a\"" x)))
		  (loop (cdr provided)))))
	 ;; collect the duplicated values
	 (let loop ((i 0)
		    (slots slots))
	    (when (pair? slots)
	       (let ((value (vector-ref vargs i)))
		  (unless (pair? value)
		     (let ((slot (car slots)))
			(with-access::sjsslot slot (id) 
			   ;; no value is provided for this object we pick
			   ;; one from this duplicated object.
			   (vector-set! vargs
			      i
			      (cons #t `(-> ,dupvar ,id))))))
		  (loop (+fx i 1) (cdr slots)))))
	 ;; build the result
	 (vector->list vargs)))
   
   (with-access::sjsclass class (id slots)
      (let* ((new (gensym 'new))
	     (dup (gensym 'duplicated))
	     (tnew (make-typed-ident new id))
	     (tdup (make-typed-ident dup id))
	     (args (collect-slot-values slots dup))
	     (init (allocate-expr class)))
	 ;; check that there is enough values
	 (for-each (lambda (a s)
		      (unless (or (car a) (slot-virtual? s))
			 ;; value missing
			 (error (car x)
			    (format "Missing value for field \"~a\""
			       (with-access::sjsslot s (id) id))
			    x)))
	    args slots)
	 ;; allocate the object and set the fields,
	 ;; first the actual fields, second the virtual fields
	 `(let ((,tnew ,init)
		(,tdup ,(e duplicated e)))
	     ;; actual fields
	     ,@(filter-map (lambda (slot val)
			      (unless (slot-virtual? slot)
				 (let ((v (e (cdr val) e)))
				    (with-access::sjsslot slot (id)
				       `(set! (-> ,new ,id) ,v)))))
		  slots args)
	     ;; constructors
	     ,@(map (lambda (c) (e `(,c ,new) e))
		  (find-class-constructors class))	
	     ;; virtual fields
	     ,@(filter-map (lambda (slot val)
			      (with-access::sjsslot slot (read-only? id)
				 (when (and (slot-virtual? slot)
					    (not read-only?)
					    (car val))
				    (let ((v (e (cdr val) e)))
				       `(set! (-> ,new ,id) ,v)))))
		  slots args)
	     ;; return the new instance
	     ,new))))
 
;*---------------------------------------------------------------------*/
;*    find-class-constructors ...                                      */
;*    -------------------------------------------------------------    */
;*    I just don't know what to do here. i) Shall we invoke the        */
;*    all constructors (a la C++). ii) Shall we call the first         */
;*    constructor defined? iii) Shall we call the constructor          */
;*    if it exists? For now I have chosen ii) because it fits the need */
;*    for all the code I have currently that make use of constructors. */
;*---------------------------------------------------------------------*/
(define (find-class-constructors class::sjsclass)
   (let loop ((class class))
      (with-access::sjsclass class (constructor its-super)
	 (cond
	    ((eq? class its-super) '())
	    (constructor (list constructor))
	    (else (loop its-super))))))

;*---------------------------------------------------------------------*/
;*    sjs-with-access-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (sjs-with-access-expander class::sjsclass)
   (lambda (x e)
      (match-case x
	 ((?- ?instance (and (? pair?) ?slots) . (and (? pair?) ?body))
	  (let loop ((s slots)
		     (nslots '()))
	     (cond
		((null? s)
		 (let* ((aux (gensym 'i))
			(instance (e instance e))
			(class-id (with-access::sjsclass class (id) id))
			(taux (make-typed-ident aux class-id)))
		    (localize
		       (get-source-location x)
		       (let ((e (with-access-expander e aux class nslots x)))
			  `(let ((,aux ,instance))
			      ,(e `(begin ,@body) e))))))
		((not (pair? s))
		 (error s "Illegal field" x))
		((symbol? (car s))
		 (loop (cdr s) (cons (list (car s) (car s)) nslots)))
		((and (pair? (car s))
		      (symbol? (car (car s)))
		      (pair? (cdr (car s)))
		      (symbol? (cadr (car s)))
		      (null? (cddr (car s))))
		 (loop (cdr s) (cons (car s) nslots)))
		(else
		 (error (car s) "Illegal form" x)))))
	 (else
	  (error "with-access" "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    with-access-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (with-access-expander olde i class slots form)
   
   (define (id var) (cadr (assq var slots)))
   
   (let ((ids (map car slots)))
      (lambda (x e)
	 (match-case x
	    ((and ?var (? symbol?))
	     (if (memq var ids)
		 `(-> ,i ,(id var))
		 (olde var olde)))
	    ((set! (and (? symbol?) ?var) ?val)
	     (let ((val (e val e)))
		(if (memq var ids)
		    (localize (get-source-location x)
		       (let ((id `(-> ,i ,(id var))))
			  `(set! ,id ,val)))
		    (begin
		       (set-car! (cddr x) val)
		       (olde x olde)))))
	    (else
	     (olde x e))))))

;*---------------------------------------------------------------------*/
;*    export-desc ...                                                  */
;*---------------------------------------------------------------------*/
(define (export-desc cid m::WIP-Unit)
   (with-access::WIP-Unit m (name)
      (create-Export-Desc `(,cid (type obj) (arity 0) (constant? #t)) name #f)))

;*---------------------------------------------------------------------*/
;*    slot->field ...                                                  */
;*---------------------------------------------------------------------*/
(define (slot->field slot::sjsslot)

   (define (slot-default-expr s)
      (if (slot-default? s)
	  `(lambda ()
	      ,(with-access::sjsslot s (default-value) default-value))
	  #f))
   
   (with-access::sjsslot slot (id getter setter read-only?
				 virtual user-info type)
      `(js-new (@ sc_Field js) ',id
	  ,(or getter `(lambda (o) (-> o ,id)))
	  ,(or setter `(lambda (o v) (set! (-> o ,id) v)))
	  ,read-only?
	  #f ,user-info ,(slot-default-expr slot)
	  ,(if (isa? type sjsclass)
	       (with-access::sjsclass type (id) `,id)
	       type))))

;*---------------------------------------------------------------------*/
;*    make-class-fields ...                                            */
;*---------------------------------------------------------------------*/
(define (make-class-fields slots class)
   (filter-map (lambda (s)
		  (with-access::sjsslot s (class-owner)
		     (when (eq? class-owner class)
			(slot->field s))))
      slots))

;*---------------------------------------------------------------------*/
;*    parse-module-class! ...                                          */
;*---------------------------------------------------------------------*/
(define (parse-module-class! m::WIP-Unit x definep::bool exportp::bool)
   (match-case x
      ((?class ?id . ?clauses)
       (with-access::WIP-Unit m (classes class-expr exports name)
	  (multiple-value-bind (cid sid)
	     (decompose-ident id)
	     (let* ((loc (get-source-location x))
		    (sid (or sid 'object))
		    (super (scheme2js-find-class sid classes)))
		(if (not super)
		    (class-error loc "scheme2js" "Cannot find super class" sid)
		    (multiple-value-bind (constructor slots)
		       (parse-class loc clauses classes)
		       (with-access::sjsclass super ((superholder holder)
						     (superslots slots))
			  (let ((nclass (instantiate::sjsclass
					   (id cid)
					   (holder cid)
					   (its-super super)
					   (constructor constructor)
					   (slots (append superslots slots)))))
			     (for-each (lambda (s)
					  (with-access::sjsslot s (class-owner)
					     (set! class-owner nclass)))
				slots)
			     (set! classes (cons nclass classes))
			     (when definep
				(set! class-expr
				   (append
				      class-expr
				      (list
					 `(define (,cid) (pragma ""))
					 `((@ sc_register_class js)
					      ;;; class
					      ,cid
				              ;; class name
					      ',cid
					      ;; super class
					      ,superholder
					      ;; hash number
					      ,(get-class-hash x)
					      ;; allocator
					      (lambda () (js-new ,cid))
					      ;; constructor
					      ,constructor
					      ;; class fields
					      (vector
						 ,@(make-class-fields slots nclass))))))
				(when exportp
				   (set! exports
				      (cons (export-desc cid m) exports))))
			     (list
				`(define-expander
				    ,(symbol-append 'instantiate:: cid)
				    ,(sjs-instantiate-expander nclass))
				`(define-expander
				    ,(symbol-append 'duplicate:: cid)
				    ,(sjs-duplicate-expander nclass))
				`(define-expander
				    ,(symbol-append 'with-access:: cid)
				    ,(sjs-with-access-expander nclass)))))))))))
      (else
       (error "scheme2js" "Illegal class" x))))
