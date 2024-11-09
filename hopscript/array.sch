;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/array.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 18 08:02:30 2016                          */
;*    Last change :  Sat Nov  9 09:25:13 2024 (serrano)                */
;*    Copyright   :  2016-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Array macros for js2scheme                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    utility macros ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (define-%make-ref)
   (eval '(define (%make-ref idx ref)
	   (if (or (symbol? idx) (number? idx))
	       (ref idx)
	       (let ((i (gensym 'idx)))
		  `(let ((,i ,idx))
		      ,(ref i))))))
   #unspecified)

(define-macro (define-%make-set)

   (eval '(define (%make-set idx val set)
	   (define (set/idx i)
	      (if (symbol? val)
		  (set i val)
		  (let ((v (gensym 'val)))
		     `(let ((,v ,val))
			 ,(set i v)))))
	   (if (or (symbol? idx) (number? idx))
	       (set/idx idx)
	       (let ((i (gensym 'idx)))
		  `(let ((,i ,idx))
		      ,(set/idx i))))))

   #unspecified)

(define-macro (define-%update-deps)
   (eval '(define (%update-deps deps)
	   (append-map (lambda (dep)
			  (let ((arr (car dep))
				(vec (cadr dep))
				(ilen (caddr dep)))
			     `((set! ,vec (js-array-vec ,arr))
			       (set! ,ilen (js-array-ilen ,arr)))))
	      deps)))
   #unspecified)

(define-%make-ref)
(define-%make-set)
(define-%update-deps)

;*---------------------------------------------------------------------*/
;*    js-array-XXX-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-macro (JS-ARRAY-MARK-REF arr idx avec alen mark %this)

   (define (ref i)
      `(if (and (fixnum? ,i)
		(>=fx ,i 0)
		(<u32 (fixnum->uint32 ,i) ,alen)
		(eq? ,mark (js-array-mark)))
	   (vector-ref ,avec ,i)
	   (js-array-ref ,arr ,i ,%this)))
   
   (%make-ref idx ref))

(define-macro (JS-ARRAY-MARK-SET! arr idx val avec alen mark throw %this)

   (define (set i v)
      (let ((tmp (gensym 'tmp)))
	 `(if (and (fixnum? ,i)
		   (>=fx ,i 0)
		   (<u32 (fixnum->uint32 ,i) ,alen)
		   (eq? ,mark (js-array-mark)))
	      (vector-set! ,avec ,i ,v)
	      (let ((,tmp (js-array-set! ,arr ,i ,v ,throw ,%this)))
		 (set! ,alen (js-array-ilen ,arr))
		 (set! ,avec (js-array-vec ,arr))
		 ,tmp))))

   (%make-set idx val set))

(define-macro (JS-ARRAY-FAST-REF arr idx avec alen deps %this)
   
   (define (ref i)
      (let ((tmp (gensym 'tmp)))
	 `(if (and (fixnum? ,i)
		   (>=fx ,i 0)
		   (<u32 (fixnum->uint32 ,i) ,alen))
	      (vector-ref ,avec ,i)
	      (let ((,tmp (js-array-ref-ur ,arr (fixnum->uint32 ,i) ,%this)))
		 ,@(%update-deps deps)
		 ,tmp))))
   
   (%make-ref idx ref))

(define-macro (JS-ARRAY-FAST-SET! arr idx val avec alen deps throw %this)

   (define (set i v)
      (let ((tmp (gensym 'tmp)))
	 `(if (and (fixnum? ,i)
		   (>=fx ,i 0)
		   (<u32 (fixnum->uint32 ,i) ,alen))
	      (vector-set! ,avec ,i ,v)
	      (let ((,tmp (js-array-set-ur! ,arr (fixnum->uint32 ,i)
			     ,v ,throw ,%this)))
		 ,@(%update-deps deps)
		 ,tmp))))

   (%make-set idx val set))

;*---------------------------------------------------------------------*/
;*    js-array-fixnum-XXX-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-macro (JS-ARRAY-FIXNUM-MARK-REF arr idx avec alen mark %this)
   
   (define (ref i)
      `(if (and (>=fx ,i 0)
		(<u32 (fixnum->uint32 ,i) ,alen)
		(eq? ,mark (js-array-mark)))
	   (vector-ref ,avec ,i)
	   (js-array-ref ,arr ,i ,%this)))
   
   (%make-ref idx ref))

(define-macro (JS-ARRAY-FIXNUM-MARK-SET! arr idx val avec alen mark throw %this)
   
   (define (set i v)
      `(if (and (>=fx ,i 0)
		(<u32 (fixnum->uint32 ,i) ,alen)
		(eq? ,mark (js-array-mark)))
	   (vector-set! ,avec ,i ,v)
	   (js-array-set-ur! ,arr (fixnum->uint32 ,i) ,v ,throw ,%this)))

   (%make-set idx val set))

(define-macro (JS-ARRAY-FIXNUM-FAST-REF arr idx avec alen deps %this)
   
   (define (ref i)
      (let ((tmp (gensym 'tmp)))
	 `(if (and (>=fx ,i 0) (<u32 (fixnum->uint32 ,i) ,alen))
	      (vector-ref ,avec ,i)
	      (let ((,tmp (js-array-ref-ur ,arr (fixnum->uint32 ,i) ,%this)))
		 ,@(%update-deps deps)
		 ,tmp))))
   
   (%make-ref idx ref))

(define-macro (JS-ARRAY-FIXNUM-FAST-SET! arr idx val avec alen deps throw %this)
   
   (define (set i v)
      (let ((tmp (gensym 'tmp)))
	 `(if (and (>=fx ,i 0) (<u32 (fixnum->uint32 ,i) ,alen))
	      (vector-set! ,avec ,i ,v)
	      (let ((,tmp (js-array-set-ur! ,arr (fixnum->uint32 ,i)
			     ,v ,throw ,%this)))
		 ,@(%update-deps deps)
		 ,tmp))))
   
   (%make-set idx val set))

;*---------------------------------------------------------------------*/
;*    js-array-index-XXX-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-macro (JS-ARRAY-INDEX-MARK-REF arr idx avec alen mark %this)
   
   (define (ref i)
      `(if (and (<u32 ,i ,alen) (eq? ,mark (js-array-mark)))
	   (vector-ref ,avec (uint32->fixnum ,i))
	   (js-array-index-ref ,arr ,i ,%this)))

   (%make-ref idx ref))

(define-macro (JS-ARRAY-INDEX-MARK-SET! arr idx val avec alen mark throw %this)
   
   (define (set i v)
      `(if (and (<u32 ,i ,alen) (eq? ,mark (js-array-mark)))
	   (vector-set! ,avec (uint32->fixnum ,i) ,v)
	   (js-array-index-set! ,arr ,i ,v ,throw ,%this)))
   
   (%make-set idx val set))

(define-macro (JS-ARRAY-INDEX-FAST-REF arr idx avec alen deps %this)
   
   (define (ref i)
      (let ((tmp (gensym 'tmp)))
	 `(if (<u32 ,i ,alen)
	      (vector-ref ,avec (uint32->fixnum ,i))
	      (let ((,tmp (js-array-index-ref ,arr ,i ,%this)))
		 ,@(%update-deps deps)
		 ,tmp))))
   
   (%make-ref idx ref))
   
(define-macro (JS-ARRAY-INDEX-FAST-SET! arr idx val avec alen deps throw %this)
   
   (define (set i v)
      (let ((tmp (gensym 'tmp)))
	 `(if (<u32 ,i ,alen)
	      (vector-set! ,avec (uint32->fixnum ,i) ,v)
	      (let ((,tmp (js-array-index-set! ,arr ,i ,v ,throw ,%this)))
		 ,@(%update-deps deps)
		 ,tmp))))

   (%make-set idx val set))

;*---------------------------------------------------------------------*/
;*    js-make-vector ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (js-make-vector len init)
   `($js-init-vector ($alloca ($js-vector-bytesize ,len)) ,len ,init))

;*---------------------------------------------------------------------*/
;*    js-call-with-with-stack-vector ...                               */
;*---------------------------------------------------------------------*/
(define-macro (js-call-with-stack-vector vec proc)
   
   (define (decl-vector p len)
      (if (fixnum? len)
	  `(pragma
	      ,(format "extern obj_t bgl_init_vector_sans_fill(); char ~a[VECTOR_SIZE + ((~a - 1) * OBJ_SIZE)]"
		  p len))
	  `(pragma
	      ,(format "extern obj_t bgl_init_vector_sans_fill(); char ~a[VECTOR_SIZE + ((($1) - 1) * OBJ_SIZE)]" p) ,len)))

   (define (init-vector p len)
      (cond-expand
	 (bigloo-compile
	  (if (fixnum? len)
	      `(pragma::vector ,(format "BGL_INIT_VECTOR_SANS_FILL(&(~a), ~a)" p len))
	      `(pragma::vector ,(format "BGL_INIT_VECTOR_SANS_FILL(&(~a), $1)" p) ,len)))
	 (else
	  (if (fixnum? len)
	      `(pragma::vector ,(format "bgl_init_vector_sans_fill(&(~a), ~a)" p len))
	      `(pragma::vector ,(format "bgl_init_vector_sans_fill(&(~a), $1)" p) ,len)))))
   
   (match-case vec
      ((vector)
       (match-case proc
	  ((lambda (?v) . ?body)
	   `(,proc '#()))
	  (else
	   (error "js-call-with-stack-vector" "bad form"
	      `(js-call-with-stack-vector ,vec ,proc)))))
      ((vector . ?args)
       (match-case proc
	  ((lambda (?v) . ?body)
	   (cond-expand
	      ((and bigloo-c (config nan-tagging #f) (not bigloo-saw) (config have-c99-stack-alloc #t) (not devel) (not debug))
	       (let ((p (gensym 'p))
		     (len (length args)))
		  `(let ()
		      ,(decl-vector p (length args))
		      (let ((,v ,(init-vector p (length args))))
			 ,@(map (lambda (i o)
				   `(vector-set! ,v ,i ,o))
			      (iota len) args)
			 ,@body))))
	      (else
	       `((@ js-call-with-stack-vector __hopscript_array) ,vec ,proc))))
	  (else
	   (error "js-call-with-stack-vector" "bad form"
	      `(js-call-with-stack-vector ,vec ,proc)))))
      ((make-vector (and ?len (? fixnum?)))
       (match-case proc
	  ((lambda (?v) . ?body)
	   (cond-expand
	      ((and bigloo-c (config nan-tagging #f) (not bigloo-saw) (config have-c99-stack-alloc #t) (not devel) (not debug))
	       (let ((p (gensym 'p)))
		  `(let ()
		      ,(decl-vector p len)
		      (let ((,v ,(init-vector p len)))
			 ,@body))))
	      (else
	       `((@ js-call-with-stack-vector __hopscript_array) ,vec ,proc))))
	  (else
	   (error "js-call-with-stack-vector" "bad form"
	      `(js-call-with-stack-vector ,vec ,proc)))))
      ((make-vector ?len)
       (match-case proc
	  ((lambda (?v) . ?body)
	   (cond-expand
	      ((and bigloo-c (config nan-tagging #f) (not bigloo-saw) (config have-c99-stack-alloc #t) (not devel) (not debug))
	       (let ((p (gensym 'p))
		     (l (gensym 'l)))
		  `(let ((,l ,len))
		      ,(decl-vector p l)
		      (let ((,v ,(init-vector p l)))
			 ,@body))))
	      (else
	       `((@ js-call-with-stack-vector __hopscript_array) ,vec ,proc))))
	  (else
	   (error "js-call-with-stack-vector" "bad form"
	      `(js-call-with-stack-vector ,vec ,proc)))))
      ((vector-copy (and (? symbol?) ?vec)
	  (and (or (? integer?) (? symbol?)) ?start)
	  (and (or (? integer?) (? symbol?)) ?end))
       (match-case proc
	  ((lambda (?v) . ?body)
	   (cond-expand
	      ((and bigloo-c (config nan-tagging #f) (not bigloo-saw) (config have-c99-stack-alloc #t) (not devel) (not debug))
	       (let* ((p (gensym 'p))
		      (i (gensym 'i))
		      (len (gensym 'l))
		      (tlen (symbol-append len '|::long|)))
		  `(let ((,tlen (-fx ,end ,start)))
		      ,(decl-vector p len)
		      (let ((,v ,(init-vector p len)))
			 (let loop ((,i ,start))
			    (when (<fx ,i ,end)
			       (vector-set! ,v (-fx ,i ,start) (vector-ref ,vec ,i))
			       (loop (+fx ,i 1))))
			 ,@body))))
	      (else
	       `(,proc ,vec))))
	  (else
	   (error "js-call-with-stack-vector" "bad form"
	      `(js-call-with-stack-vector ,vec ,proc)))))
      (else
       `((@ js-call-with-stack-vector __hopscript_array) ,vec ,proc))))

;*---------------------------------------------------------------------*/
;*    js-vector->jsarray                                               */
;*---------------------------------------------------------------------*/
(define-expander js-vector->jsarray
   (lambda (x e)
      (match-case x
	 ((js-vector->jsarray (vector ?a0) ?%this)
	  (e `(js-vector1->jsarray ,a0 ,%this) e))
	 ((js-vector->jsarray (copy-vector '#(?a0) 1) ?%this)
	  (e `(js-vector1->jsarray ,a0 ,%this) e))
	 ((js-vector->jsarray (vector ?a0 ?a1) ?%this)
	  (e `(js-vector2->jsarray ,a0 ,a1 ,%this) e))
	 ((js-vector->jsarray (copy-vector '#(?a0 ?a1) 2) ?%this)
	  (e `(js-vector2->jsarray ,a0 ,a1 ,%this) e))
	 ((js-vector->jsarray (vector ?a0 ?a1 ?a2) ?%this)
	  (e `(js-vector3->jsarray ,a0 ,a1 ,a2 ,%this) e))
	 ((js-vector->jsarray (copy-vector '#(?a0 ?a1 ?a2) 3) ?%this)
	  (e `(js-vector3->jsarray ,a0 ,a1 ,a2 ,%this) e))
	 ((js-vector->jsarray (vector ?a0 ?a1 ?a2 ?a3) ?%this)
	  (e `(js-vector4->jsarray ,a0 ,a1 ,a2 ,a3 ,%this) e))
	 ((js-vector->jsarray (copy-vector '#(?a0 ?a1 ?a2 ?a3) 4) ?%this)
	  (e `(js-vector4->jsarray ,a0 ,a1 ,a2 ,a3 ,%this) e))
	 (else
	  (e `((@ js-vector->jsarray __hopscript_array) ,@(cdr x)) e)))))
