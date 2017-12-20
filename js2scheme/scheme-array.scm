;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-array.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Wed Dec 20 18:18:22 2017 (serrano)                */
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
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)

   (export (j2s-new-array ::J2SNew mode return conf hint)
	   (j2s-array-ref ::J2SAccess mode return conf hint)
	   (j2s-array-set! ::J2SAssig mode return conf hint)
	   (j2s-vector-ref ::J2SAccess mode return conf hint)
	   (j2s-vector-set! ::J2SAssig mode return conf hint)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArray mode return conf hint)
   
   (define (unique? conf vec)
      (let ((vectors (config-get conf :%vectors)))
	 (unless (member vec vectors)
	    (config-put! conf :%vectors (cons vec vectors))
	    #t)))
   
   (with-access::J2SArray this (loc exprs type)
      (let ((sexprs (j2s-scheme exprs mode return conf hint)))
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
(define (j2s-new-array this::J2SNew mode return conf hint)
   
   (define (smaller-than? o k)
      (if (isa? o J2SNumber)
	  (with-access::J2SNumber o (val)
	     (cond
		((fixnum? val) (>=fx val 0) (<fx val k))
		((int32? val) (>=s32 val #s32:0) (<s32 val (fixnum->int32 k)))
		((uint32? val) (<u32 val (fixnum->uint32 k)))
		(else #f)))
	  (with-access::J2SExpr o (range)
	     (when range
		(and (>=llong (interval-min range) 0)
		     (<=llong (interval-max range) (fixnum->llong k)))))))
   
   (define (alloc-length arg k)
      (if (smaller-than? arg 16)
	  `(js-array-construct-alloc-small %this 
	      ,(k (j2s-scheme arg mode return conf hint)))
	  
	  `(js-array-construct/lengthu32 %this
	      (js-array-alloc %this)
	      ,(k (j2s-scheme arg mode return conf hint)))))
   
   (with-access::J2SNew this (loc cache clazz args type)
      (cond
	 ((null? args)
	  '(js-empty-vector->jsarray %this))
	 ((pair? (cdr args))
	  `(js-vector->jsarray
	      (vector ,@(j2s-scheme args mode return conf hint))
	      %this))
	 ((eq? type 'vector)
	  `(make-vector
	      ,(j2s-scheme (car args) mode return conf hint)
	      (js-undefined)))
	 (else
	  (let loop ((arg (car args)))
	     (let ((t (j2s-type arg)))
		(cond
		   ((eq? t 'int32)
		    (alloc-length arg (lambda (v) `(int32->uint32 ,v))))
		   ((eq? t 'uint32)
		    (alloc-length arg (lambda (v) v)))
		   ((memq t '(integer bint int53))
		    (alloc-length arg (lambda (v) `(fixnum->uint32 ,v))))
		   ((isa? arg J2SCast)
		    (with-access::J2SCast arg (type expr)
		       (if (eq? type 'any)
			   (loop expr)
			   `(js-array-construct %this (js-array-alloc %this)
			       (list ,(j2s-scheme arg mode return conf hint))))))
		   (else
		    `(js-array-construct %this (js-array-alloc %this)
			(list ,(j2s-scheme arg mode return conf hint)))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-array-index-ref ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-array-index-ref this::J2SAccess mode return conf hint)

   (define (aref/cache this::J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (with-access::J2SAref obj (array alen amark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return conf hint))
		  (scmobj (j2s-scheme obj mode return conf hint))
		  (tyfield (j2s-type-ref field)))
	       (case tyfield
		  ((uint32)
		   (let ((idx (j2s-scheme field mode return conf hint)))
		      (if amark
			  `(JS-ARRAY-INDEX-MARK-REF ,scmobj
			      ,idx
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id amark)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-REF ,scmobj
			      ,idx
			      ,scmarray ,scmalen 
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      %this))))
		  ((int32)
		   (if amark
		       `(JS-ARRAY-FIXNUM-MARK-REF ,scmobj
			   (int32->fixnum ,scmfield)
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   %this)
		       `(JS-ARRAY-FIXNUM-FAST-REF ,scmobj
			   (int32->fixnum ,scmfield)
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   %this)))
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
	    ((uint32)
	     `(js-array-index-ref ,(j2s-scheme obj mode return conf hint)
		 ,(j2s-scheme field mode return conf hint)
		 %this))
	    ((int32)
	     `(js-array-fixnum-ref ,(j2s-scheme obj mode return conf hint)
		 (int32->fixnum
		    ,(j2s-scheme field mode return conf hint))
		 %this))
	    ((fixnum)
	     `(js-array-fixnum-ref ,(j2s-scheme obj mode return conf hint)
		 ,(j2s-scheme field mode return conf hint)
		 %this))
	    (else
	     `(js-array-ref ,(j2s-scheme obj mode return conf hint)
		 ,(j2s-scheme field mode return conf hint)
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
	 ((eq? (j2s-type-ref field) 'uint32)
	  `(js-array-index-ref ,(j2s-scheme obj mode return conf hint)
	      ,(j2s-scheme field mode return conf hint)
	      %this))
	 ((eq? (j2s-type-ref field) 'int32)
	  `(js-array-ref ,(j2s-scheme obj mode return conf hint)
	      (int32->fixnum ,(j2s-scheme field mode return conf hint))
	      %this))
	 (else
	  `(js-array-ref ,(j2s-scheme obj mode return conf hint)
	      ,(j2s-scheme field mode return conf hint)
	      %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-array-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-array-ref this::J2SAccess mode return conf hint)
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((maybe-number? field)
	  (j2s-array-index-ref this mode return conf hint))
	 ((j2s-field-length? field)
	  (let ((x `(js-array-length
		       ,(j2s-scheme obj mode return conf hint))))
	     (if (eq? type 'uint32)
		 x
		 (js-uint32->jsnum x conf))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-array-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-array-set! this::J2SAssig mode return conf hint)

   (define (aset/cache lhs rhs)
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (with-access::J2SAref obj (array alen amark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return conf hint))
		  (scmobj (j2s-scheme obj mode return conf hint))
		  (scmrhs (j2s-scheme rhs mode return conf hint))
		  (tyfield (j2s-type-ref field)))
	       (case tyfield
		  ((uint32)
		   (let ((idx (j2s-scheme field mode return conf hint)))
		      (if amark
			  `(JS-ARRAY-INDEX-MARK-SET! ,scmobj
			      ,idx ,scmrhs
			      ,scmarray ,scmalen
			      ,(j2s-decl-scheme-id amark)
			      ,(strict-mode? mode)
			      %this)
			  `(JS-ARRAY-INDEX-FAST-SET! ,scmobj
			      ,idx ,scmrhs
			      ,scmarray ,scmalen
			      ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			      ,(strict-mode? mode)
			      %this))))
		  ((int32)
		   (if amark
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj
			   (int32->fixnum ,scmfield) ,scmrhs
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   ,(strict-mode? mode)
			   %this)
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj
			   (int32->fixnum ,scmfield) ,scmrhs
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   ,(strict-mode? mode)
			   %this)))
		  ((int53)
		   (if amark
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   ,(strict-mode? mode)
			   %this)
		       `(JS-ARRAY-FIXNUM-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   ,(strict-mode? mode)
			   %this)))
		  (else
		   (if amark
		       `(JS-ARRAY-MARK-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   ,(strict-mode? mode)
			   %this)
		       `(JS-ARRAY-FAST-SET! ,scmobj ,scmfield ,scmrhs
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   ,(strict-mode? mode)
			   %this))))))))

   (define (aset/w-cache lhs rhs)
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (with-access::J2SAref obj (array alen)
	    (case (j2s-type-ref field)
	       ((uint32)
		`(js-array-index-set! ,(j2s-scheme obj mode return conf hint)
		    ,(j2s-scheme field mode return conf hint)
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this))
	       ((int32)
		`(js-array-fixnum-set! ,(j2s-scheme obj mode return conf hint)
		    (int32->fixnum
		       ,(j2s-scheme field mode return conf hint))
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this))
	       ((fixnum)
		`(js-array-fixnum-set! ,(j2s-scheme obj mode return conf hint)
		    ,(j2s-scheme field mode return conf hint)
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this))
	       (else
		`(js-array-set! ,(j2s-scheme obj mode return conf hint)
		    ,(j2s-scheme field mode return conf hint)
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this))))))
   
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field cache (loca loc))
	 (let ((tyf (j2s-type-ref field)))
	    (cond
	       ((isa? obj J2SAref)
		(if *j2s-array-cache*
		    (aset/cache lhs rhs)
		    (aset/w-cache lhs rhs)))
	       ((eq? tyf 'int53)
		`(js-array-fixnum-set!
		    ,(j2s-scheme obj mode return conf hint)
		    ,(j2s-scheme field mode return conf hint)
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this))
	       ((eq? tyf 'uint32)
		`(js-array-index-set!
		    ,(j2s-scheme obj mode return conf hint)
		    ,(j2s-scheme field mode return conf hint)
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this))
	       ((eq? tyf 'int32)
		`(js-array-fixnum-set!
		    ,(j2s-scheme obj mode return conf hint)
		    (int32->fixnum
		       ,(j2s-scheme field mode return conf hint))
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this))
	       (else
		`(js-array-set!
		    ,(j2s-scheme obj mode return conf hint)
		    ,(j2s-scheme field mode return conf hint)
		    ,(j2s-scheme rhs mode return conf hint)
		    ,(strict-mode? mode)
		    %this)))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-vector-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-vector-ref this::J2SAccess mode return conf hint)
   (with-access::J2SAccess this (obj field)
      (with-access::J2SRef obj (decl)
	 (with-access::J2SDecl decl ((h hint))
	    (with-access::J2SExpr field (range)
	       (let ((h (car h)))
		  (cond
		     ((eq? (j2s-type-ref field) 'uint32)
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme field mode return conf hint)))
			     (if (<u32 ,i ,(fixnum->uint32 h))
				 (vector-ref
				    ,(j2s-scheme obj mode return conf hint)
				    (uint32->fixnum ,i))
				 (js-undefined)))))
		     ((and (>= (interval-min range) h)
			   (< (interval-max range) h))
		      `(vector-ref ,(j2s-scheme obj mode return conf hint)
			  (uint32->fixum
			     ,(j2s-scheme-as-uint32 field mode return conf hint))))
		     ((>=fx (interval-min range) 0)
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme-as-uint32 field mode
					return conf hint)))
			     (if (<u32 ,i ,(fixnum->uint32 h))
				 (vector-ref
				    ,(j2s-scheme obj
					mode return conf hint)
				    (uint32->fixnum ,i))
				 (js-undefined)))))
		     (else
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme-as-integer field
					mode return conf hint)))
			     (if (and (<fx ,i ,h) (>=fx ,i 0))
				 (vector-ref
				    ,(j2s-scheme obj mode return conf hint) ,i)
				 (js-undefined))))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-vector-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-vector-set! this::J2SAssig mode return conf hint)
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field)
	 `(vector-set! ,(j2s-scheme obj mode return conf hint)
	     (uint32->fixnum
		,(j2s-scheme-as-uint32 field mode return conf hint))
	     ,(j2s-scheme rhs mode return conf hint)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-as-uint32 ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-as-uint32 field mode return conf hint)
   (j2s-cast
      (j2s-scheme field mode return conf hint)
      field
      (j2s-type-ref field) 'uint32 conf))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-as-integer ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-as-integer field mode return conf hint)
   (j2s-cast
      (j2s-scheme field mode return conf hint)
      field
      (j2s-type-ref field) 'integer conf))

      
	     
