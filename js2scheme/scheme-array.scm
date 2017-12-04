;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-array.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Mon Dec  4 14:12:32 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Array functions.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-array

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_array
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)

   (export (j2s-new-array ::J2SNew mode return conf hint totype)
	   (j2s-array-ref ::J2SAccess mode return conf hint totype)
	   (j2s-vector-ref ::J2SAccess mode return conf hint totype)
	   (j2s-vector-set! ::J2SAssig mode return conf hint totype)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArray mode return conf hint totype)
   
   (define (unique? conf vec)
      (let ((vectors (config-get conf :%vectors)))
	 (unless (member vec vectors)
	    (config-put! conf :%vectors (cons vec vectors))
	    #t)))
   
   (with-access::J2SArray this (loc exprs type)
      (let ((sexprs (j2s-scheme exprs mode return conf hint totype)))
	 (cond
	    ((null? sexprs)
	     (if (eq? type 'vector)
		 ''#()
		 `(js-empty-vector->jsarray %this)))
	    ((and (every (lambda (x)
			    (or (number? x) (string? x) (boolean? x)))
		     sexprs)
		  (unique? conf sexprs))
	     (let ((vec `',(list->vector sexprs)))
		(epairify loc
		   (if (eq? type 'vector)
		       vec
		       `(js-vector->jsarray ,vec %this)))))
	    ((any (lambda (x) (isa? x J2SArrayAbsent)) exprs)
	     (let ((vec `(vector ,@sexprs)))
		(epairify loc
		   (if (eq? type 'vector)
		       vec
		       `(js-vector->sparse-jsarray ,vec %this)))))
	    (else
	     (let ((vec `(vector ,@sexprs)))
		(epairify loc
		   (if (eq? type 'vector)
		       vec
		       `(js-vector->jsarray ,vec %this)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-new-array ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-new-array this::J2SNew mode return conf hint totype)

   (define (smaller-than? o k)
      (when (isa? o J2SNumber)
	 (with-access::J2SNumber o (val)
	    (and (fixnum? val) (>=fx val 0) (<fx val k)))))

   (with-access::J2SNew this (loc cache clazz args type)
      (cond
	 ((null? args)
	  '(js-empty-vector->jsarray %this))
	 ((eq? type 'vector)
	  `(make-vector
	      ,(j2s-scheme (car args) mode return conf hint totype)
	      (js-undefined)))
	 ((and (is-integer? (car args)) (null? (cdr args)))
	  (if (smaller-than? (car args) 16)
	      `(js-array-construct-alloc-small %this 
		  ,(j2s-scheme (car args) mode return conf hint totype))
	      `(js-array-construct/length %this (js-array-alloc %this)
		  ,(j2s-scheme (car args) mode return conf hint totype))))
	 ((null? (cdr args))
	  `(js-array-construct %this (js-array-alloc %this)
	      (list ,@(j2s-scheme args mode return conf hint totype))))
	 (else
	  `(js-vector->jsarray
	      (vector ,@(j2s-scheme args mode return conf hint totype))
	      %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-array-index-ref ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-array-index-ref this::J2SAccess mode return conf hint totype)

   (define (uint32 type expr)
      (if (memq type '(uint29 uint32 index))
	  expr
	  `(fixnum->uint32 ,expr)))
   
   (define (aref/cache this::J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (with-access::J2SAref obj (array alen amark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return conf hint (j2s-type-ref field)))
		  (scmobj (j2s-scheme obj mode return conf hint 'array))
		  (tyfield (j2s-type-ref field)))
	       (case tyfield
		  ((index uint29 ufixnum)
		   (let ((idx (j2s-scheme field mode return conf hint 'uint32)))
		      (if amark
			  `(JS-ARRAY-INDEX-MARK-REF ,scmobj
			      ,(uint32 tyfield idx)
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id amark)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-REF ,scmobj
			      ,(uint32 tyfield idx)
			      ,scmarray ,scmalen 
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      %this))))
		  ((fixnum)
		   (if amark
		       `(JS-ARRAY-FIXNUM-MARK-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   %this)
		       `(JS-ARRAY-FIXNUM-FAST-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   %this)))
		  (else
		   (if amark
		       `(JS-ARRAY-MARK-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   %this)
		       `(JS-ARRAY-FAST-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   %this))))))))

   (define (aref/w-cache this::J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (case (j2s-type-ref field)
	    ((index)
	     `(js-array-index-ref ,(j2s-scheme obj mode return conf hint 'array)
		 ,(j2s-scheme field mode return conf hint 'uint32)
		 %this))
	    ((ufixnum uint29)
	     `(js-array-index-ref ,(j2s-scheme obj mode return conf hint 'array)
		 ,(js-fixnum->uint32 (j2s-scheme field mode return conf hint 'uint32))
		 %this))
	    ((fixnum)
	     `(js-array-fixnum-ref ,(j2s-scheme obj mode return conf hint totype)
		 ,(j2s-scheme field mode return conf hint totype)
		 %this))
	    (else
	     `(js-array-ref ,(j2s-scheme obj mode return conf hint totype)
		 ,(j2s-scheme field mode return conf hint totype)
		 %this)))))

   (define (aref this::J2SAccess)
      (if *j2s-array-cache*
	  (aref/cache this)
	  (aref/w-cache this)))

   (with-access::J2SAccess this (obj field)
      (cond
	 ((isa? obj J2SAref)
	  (aref this))
	 ((is-fixnum? field conf)
	  (aref/w-cache this))
	 ((eq? (j2s-type-ref field) 'index)
	  `(js-array-ref-ur ,(j2s-scheme obj mode return conf hint totype)
	      ,(js-uint32->fixnum
		  (j2s-scheme field mode return conf hint totype) conf)
	      %this))
	 (else
	  `(js-array-ref ,(j2s-scheme obj mode return conf hint totype)
	      ,(j2s-scheme field mode return conf hint totype)
	      %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-array-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-array-ref this::J2SAccess mode return conf hint totype)
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((maybe-number? field)
	  (j2s-array-index-ref this mode return conf hint totype))
	 ((j2s-field-length? field)
	  (let ((x `(js-array-length
		       ,(j2s-scheme obj mode return conf hint 'array))))
	     (if (memq type '(index uint32 length))
		 x
		 (js-uint32->jsnum x conf))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-vector-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-vector-ref this::J2SAccess mode return conf hint totype)
   (with-access::J2SAccess this (obj field)
      (with-access::J2SRef obj (decl)
	 (with-access::J2SDecl decl ((h hint))
	    (with-access::J2SExpr field (range)
	       (let ((h (car h)))
		  (cond
		     ((and (>= (interval-min range) h)
			   (< (interval-max range) h))
		      `(vector-ref ,(j2s-scheme obj
				       mode return conf hint totype)
			  ,(j2s-scheme field mode return conf hint totype)))
		     ((or (eq? (j2s-type-ref field) 'index)
			  (eq? (j2s-type-ref field) 'uint29)
			  (>=fx (interval-min range) 0))
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme field
					mode return conf hint totype)))
			     (if (<fx ,i ,h)
				 (vector-ref
				    ,(j2s-scheme obj
					mode return conf hint totype) ,i)
				 (js-undefined)))))
		     (else
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme field
					mode return conf hint totype)))
			     (if (and (<fx ,i ,h) (>=fx ,i 0))
				 (vector-ref
				    ,(j2s-scheme obj
					mode return conf hint totype) ,i)
				 (js-undefined))))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-vector-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-vector-set! this::J2SAssig mode return conf hint totype)
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field)
	 `(vector-set! ,(j2s-scheme obj mode return conf hint totype)
	     ,(j2s-scheme field mode return conf hint totype)
	     ,(j2s-scheme rhs mode return conf hint totype)))))
	     
