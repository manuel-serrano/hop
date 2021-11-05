;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-array.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Fri Nov  5 07:54:22 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Array functions.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-array
   
   (include "ast.sch" "context.sch")
   
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
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-spread)
   
   (export (j2s-array-builtin-method fun::J2SAccess args
	      expr mode return::procedure conf)
	   (j2s-new-array ::J2SNew mode return conf)
	   (j2s-new-jsvector ::J2SNew mode return conf)
	   (j2s-array-ref ::J2SAccess mode return conf)
	   (j2s-array-set! ::J2SAssig mode return conf)
	   (j2s-vector-ref ::J2SAccess mode return conf)
	   (j2s-vector-set! ::J2SAssig mode return conf)
	   (j2s-jsvector-ref ::J2SAccess mode return conf)
	   (j2s-jsvector-set! ::J2SAssig mode return conf)
	   (j2s-array-foreach obj args mode return conf)
	   (j2s-array-maybe-foreach obj args mode return conf)
	   (j2s-array-map obj args mode return conf)
	   (j2s-array-maybe-map obj args mode return conf)
	   (j2s-array-filter obj args mode return conf)
	   (j2s-array-maybe-filter obj args mode return conf)
	   (j2s-array-filter-map obj args mode return conf)
	   (j2s-array-maybe-filter-map obj args mode return conf)
	   (j2s-array-flatmap obj args mode return conf)
	   (j2s-array-maybe-flatmap obj args mode return conf)
	   (j2s-array-maybe-join obj args mode return conf)
	   (j2s-array-concat0 obj args mode return conf)
	   (j2s-array-maybe-concat0 obj args mode return conf)
	   (j2s-array-concat1 obj args mode return conf)
	   (j2s-array-maybe-concat1 obj args mode return conf)
	   (j2s-array-fill1 obj args mode return conf)
	   (j2s-array-sort obj args mode return conf)
	   (j2s-array-maybe-sort obj args mode return conf)
	   (j2s-array-reduce obj args mode return conf)
	   (j2s-array-maybe-reduce obj args mode return conf)
	   (j2s-array-maybe-splice2 ::J2SCall obj args mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-array-builtin-method ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-array-builtin-method fun::J2SAccess args expr mode return ctx)
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "isArray")
		(when (=fx (length args) 1)
		   (let ((tmp (gensym 'tmp)))
		      `(let ((,tmp ,(j2s-scheme (car args) mode return ctx)))
			  (or (js-array? ,tmp) (js-proxy-array? ,tmp))))))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SArray ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SArray mode return ctx)
   
   (define (unique? ctx vec)
      (let ((vectors (context-%vectors ctx)))
	 (unless (member vec vectors)
	    (context-%vectors-set! ctx (cons vec vectors))
	    #t)))
   
   (with-access::J2SArray this (loc exprs type)
      (let ((sexprs (j2s-scheme exprs mode return ctx)))
	 (cond
	    ((null? sexprs)
	     (case type
		((vector)
		 ''#())
		((jsvector)
		 '(js-vector-empty))
		(else
		 `(js-empty-vector->jsarray %this))))
	    ((and (every (lambda (x)
			    (or (number? x) (string? x) (boolean? x)))
		     sexprs)
		  (unique? ctx sexprs))
	     (epairify loc
		(if (eq? type 'jsvector)
		    `(js-vector->jsvector ',(list->vector sexprs) %this)
		    (let ((vec `(copy-vector ',(list->vector sexprs)
				   ,(length sexprs))))
		       (if (eq? type 'vector)
			   vec
			   `(js-vector->jsarray ,vec %this))))))
	    ((any (lambda (x) (isa? x J2SSpread)) exprs)
	     (j2s-scheme (spread->array-expr loc exprs #t) mode return ctx))
	    ((any (lambda (x) (isa? x J2SArrayAbsent)) exprs)
	     (let ((vec `(vector ,@sexprs)))
		(epairify loc
		   (if (eq? type 'vector)
		       vec
		       `(js-vector->sparse-jsarray ,vec %this)))))
	    (else
	     (epairify loc
		(if (eq? type 'jsvector)
		    (let ((vec (gensym 'vec)))
		       `(let ((,vec (js-vector-alloc
				       ,(fixnum->uint32 (length sexprs))
				       %this)))
			   ,@(map (lambda (i v)
				     `(js-vector-inline-set! ,vec ,i ,v))
				(iota (length sexprs)) sexprs)
			   ,vec))
		    (let ((vec `(vector ,@sexprs)))
		       (if (eq? type 'vector)
			   vec
			   `(js-vector->jsarray ,vec %this))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-new-array ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-new-array this::J2SNew mode return ctx)
   
   (define (smaller-than? o k)
      (if (isa? o J2SNumber)
	  (with-access::J2SNumber o (val)
	     (cond
		((fixnum? val)
		 (and (>=fx val 0) (<fx val k)))
		((int32? val)
		 (and (>=s32 val #s32:0) (<s32 val (fixnum->int32 k))))
		((uint32? val)
		 (<u32 val (fixnum->uint32 k)))
		(else
		 #f)))
	  (with-access::J2SExpr o (range)
	     (when (interval? range)
		(and (>=llong (interval-min range) 0)
		     (<=llong (interval-max range) (fixnum->llong k)))))))

   (define (alloc-length arg k)
      (cond
	 ((smaller-than? arg 1024)
	  `(js-array-construct-alloc-small %this 
	      ,(k (j2s-scheme arg mode return ctx))))
	 ((smaller-than? arg (bit-lsh 1 29))
	  `(js-array-construct-alloc/lengthu32 %this
	      ,(k (j2s-scheme arg mode return ctx))))
	 (else
	  `(js-array-construct-alloc/length %this
	      ,(box (j2s-scheme arg mode return ctx)
		  (j2s-type arg) ctx)))))

   (define (uncast a)
      (if (isa? a J2SCast)
	  (with-access::J2SCast a (expr type)
	     (if (eq? type 'any)
		 expr
		 a))
	  a))
   
   (with-access::J2SNew this (loc cache clazz args type)
      (cond
	 ((null? args)
	  '(js-empty-vector->jsarray %this))
	 ((pair? (cdr args))
	  `(js-vector->jsarray
	      (vector ,@(j2s-scheme args mode return ctx))
	      %this))
	 ((eq? type 'vector)
	  `(js-make-vector
	      ,(j2s-scheme (car args) mode return ctx)
	      (js-undefined)))
	 (else
	  (let loop ((arg (uncast (car args))))
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
			   `(js-array-construct-alloc %this
			       ,(box (j2s-scheme arg mode return ctx) type ctx)))))
		   (else
		    `(js-array-construct-alloc %this 
			,(j2s-scheme arg mode return ctx))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-new-jsvector ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-new-jsvector this::J2SNew mode return ctx)
   
   (define (uncast a)
      (if (isa? a J2SCast)
	  (with-access::J2SCast a (expr type)
	     (if (eq? type 'any)
		 expr
		 a))
	  a))
   
   (with-access::J2SNew this (loc cache clazz args type)
      (cond
	 ((null? args)
	  (error/location "hopscript" "new Vector(), missing argument" this
	     (cadr loc) (caddr loc)))
	 ((pair? (cdr args))
	  (error/location "hopscript" "new Vector(...), too many arguments" this
	     (cadr loc) (caddr loc)))
	 ((eq? type 'vector)
	  `(js-make-vector
	      ,(j2s-scheme (car args) mode return ctx)
	      (js-undefined)))
	 (else
	  (let loop ((arg (uncast (car args))))
	     (let ((t (j2s-type arg)))
		(cond
		   ((eq? t 'int32)
		    `(js-vector-alloc
			(int32->uint32 ,(j2s-scheme arg mode return ctx))
			%this))
		   ((eq? t 'uint32)
		    `(js-vector-alloc
			,(j2s-scheme arg mode return ctx)
			%this))
		   ((memq t '(integer bint int53))
		    `(js-vector-alloc
			(fixnum->uint32 ,(j2s-scheme arg mode return ctx))
			%this))
		   (else
		    `(js-vector-new1
			,(box (j2s-scheme arg mode return ctx) type ctx)
			%this)))))))))

;*---------------------------------------------------------------------*/
;*    j2s-array-index-ref ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-array-index-ref this::J2SAccess mode return ctx)

   (define (j2s-decl-scheme-id d::J2SDecl)
      (j2s-decl-scm-id d ctx))
   
   (define (aref/cache this::J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (with-access::J2SAref obj (array alen amark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return ctx))
		  (scmobj (j2s-scheme obj mode return ctx))
		  (tyfield (j2s-type field)))
	       (case tyfield
		  ((uint32)
		   (let ((idx (j2s-scheme field mode return ctx)))
		      (if amark
			  `(JS-ARRAY-INDEX-MARK-REF ,scmobj
			      ,idx
			      ,scmarray ,scmalen
			      ,(j2s-decl-scm-id amark ctx)
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
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   %this)))
		  ((int53)
		   (if amark
		       `(JS-ARRAY-FIXNUM-MARK-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   %this)
		       `(JS-ARRAY-FIXNUM-FAST-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
			   %this)))
		  ((fixnum)
		   (if amark
		       `(JS-ARRAY-FIXNUM-MARK-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(j2s-decl-scheme-id amark)
			   %this)
		       `(JS-ARRAY-FIXNUM-FAST-REF ,scmobj ,scmfield
			   ,scmarray ,scmalen
			   ,(map (lambda (d) (map j2s-decl-scheme-id d)) deps)
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
	 (case (j2s-type field)
	    ((uint32)
	     `(js-array-index-ref ,(j2s-scheme obj mode return ctx)
		 ,(j2s-scheme field mode return ctx)
		 %this))
	    ((int32)
	     `(js-array-fixnum-ref ,(j2s-scheme obj mode return ctx)
		 (int32->fixnum
		    ,(j2s-scheme field mode return ctx))
		 %this))
	    ((fixnum int53)
	     `(js-array-fixnum-ref ,(j2s-scheme obj mode return ctx)
		 ,(j2s-scheme field mode return ctx)
		 %this))
	    (else
	     (case (j2s-etype field (context-conf ctx))
		((string)
		 `(js-array-string-ref ,(j2s-scheme obj mode return ctx)
		     ,(j2s-scheme field mode return ctx)
		     %this))
		((fixnum int53)
		 `(js-array-fixnum-ref ,(j2s-scheme obj mode return ctx)
		     ,(j2s-scheme field mode return ctx)
		     %this))
		(else
		 `(js-array-ref ,(j2s-scheme obj mode return ctx)
		     ,(j2s-scheme field mode return ctx)
		     %this)))))))

   (define (aref this::J2SAccess)
      (if *j2s-array-cache*
	  (aref/cache this)
	  (aref/w-cache this)))

   (with-access::J2SAccess this (obj field)
      (if (isa? obj J2SAref)
	  (aref this)
	  (aref/w-cache this))))

;*---------------------------------------------------------------------*/
;*    j2s-array-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-array-ref this::J2SAccess mode return ctx)
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((maybe-number? field)
	  (j2s-array-index-ref this mode return ctx))
	 ((j2s-field-length? field)
	  (let ((x `(js-array-length
		       ,(j2s-scheme obj mode return ctx))))
	     (if (eq? type 'uint32)
		 x
		 (js-uint32-tointeger x (context-conf ctx)))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-array-set! ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-array-set! this::J2SAssig mode return ctx)
   
   (define (j2s-decl-scheme-id d::J2SDecl)
      (j2s-decl-scm-id d ctx))
   
   (define (aset/cache this lhs rhs) 
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (with-access::J2SAref obj (array alen amark deps)
	    (let ((scmarray (j2s-decl-scheme-id array))
		  (scmalen (j2s-decl-scheme-id alen))
		  (scmfield (j2s-scheme field mode return ctx))
		  (scmobj (j2s-scheme obj mode return ctx))
		  (scmrhs (j2s-scheme rhs mode return ctx))
		  (tyfield (j2s-type field)))
	       (case tyfield
		  ((uint32)
		   (let ((idx (j2s-scheme field mode return ctx)))
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
		       `(JS-ARRAY-FIXNUM-FAST-SET! ,scmobj
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
		       `(JS-ARRAY-FIXNUM-FAST-SET! ,scmobj ,scmfield ,scmrhs
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
   
   (define (aset/w-cache this lhs rhs)
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (with-access::J2SAref obj (array alen)
	    (case (j2s-type field)
	       ((uint32)
		`(js-array-index-set! ,(j2s-scheme obj mode return ctx)
		    ,(j2s-scheme field mode return ctx)
		    ,(j2s-scheme rhs mode return ctx)
		    ,(strict-mode? mode)
		    %this))
	       ((int32)
		`(js-array-fixnum-set! ,(j2s-scheme obj mode return ctx)
		    (int32->fixnum
		       ,(j2s-scheme field mode return ctx))
		    ,(j2s-scheme rhs mode return ctx)
		    ,(strict-mode? mode)
		    %this))
	       ((fixnum int53)
		`(js-array-fixnum-set! ,(j2s-scheme obj mode return ctx)
		    ,(j2s-scheme field mode return ctx)
		    ,(j2s-scheme rhs mode return ctx)
		    ,(strict-mode? mode)
		    %this))
	       (else
		(case (j2s-type field)
		   ((string)
		    (if (eq? (j2s-type rhs) 'length)
			(with-access::J2SString field (val loc)
			   (if (string=? val "length")
			       `(js-array-length-set!
				   ,(j2s-scheme obj mode return ctx)
				   ,(j2s-scheme rhs mode return ctx)
				   ,(strict-mode? mode)
				   %this)
			       (error/location "hopc"
				  "Illegal assignment type \"length\""
				  val
				  (cadr loc) (caddr loc))))
			`(js-array-string-set! ,(j2s-scheme obj mode return ctx)
			    ,(j2s-scheme field mode return ctx)
			    ,(j2s-scheme rhs mode return ctx)
			    ,(strict-mode? mode)
			    %this)))
		   ((fixnum int53)
		    `(js-array-fixnum-set! ,(j2s-scheme obj mode return ctx)
			,(j2s-scheme field mode return ctx)
			,(j2s-scheme rhs mode return ctx)
			,(strict-mode? mode)
			%this))
		   (else
		    `(js-array-set! ,(j2s-scheme obj mode return ctx)
			,(j2s-scheme field mode return ctx)
			,(j2s-scheme rhs mode return ctx)
			,(strict-mode? mode)
			%this))))))))
   
   (define (aset this::J2SAssig lhs rhs)
      (if *j2s-array-cache*
	  (aset/cache this lhs rhs)
	  (aset/w-cache this lhs rhs)))
   
   (with-access::J2SAssig this (lhs rhs)
      (let loop ((rhs rhs))
	 (if (boxed-type? (j2s-type rhs))
	     (with-access::J2SExpr rhs (loc type)
		(let ((tmp (gensym)))
		   `(let ((,tmp ,(j2s-scheme rhs mode return ctx)))
		       ,(loop (J2SCast 'any (J2SHopRef/type tmp type)))
		       ,tmp)))
	     (with-access::J2SAccess lhs (obj field cache (loca loc))
		(if (isa? obj J2SAref)
		    (aset this lhs rhs)
		    (aset/w-cache this lhs rhs)))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-vector-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-vector-ref this::J2SAccess mode return ctx)
   (with-access::J2SAccess this (obj field)
      (with-access::J2SRef obj (decl)
	 (with-access::J2SDecl decl ((h hint))
	    (with-access::J2SExpr field (range)
	       (let ((h (car h)))
		  (cond
		     ((eq? (j2s-type field) 'uint32)
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme field mode return ctx)))
			     (if (<u32 ,i ,(fixnum->uint32 h))
				 (vector-ref
				    ,(j2s-scheme obj mode return ctx)
				    (uint32->fixnum ,i))
				 (js-undefined)))))
		     ((and (interval? range)
			   (>= (interval-min range) h)
			   (< (interval-max range) h))
		      `(vector-ref ,(j2s-scheme obj mode return ctx)
			  (uint32->fixum
			     ,(j2s-scheme-as-uint32 field mode return ctx))))
		     ((and (interval? range) (>=llong (interval-min range) 0))
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme-as-uint32 field mode
					return ctx)))
			     (if (<u32 ,i ,(fixnum->uint32 h))
				 (vector-ref
				    ,(j2s-scheme obj
					mode return ctx)
				    (uint32->fixnum ,i))
				 (js-undefined)))))
		     (else
		      (let ((i (gensym 'idx)))
			 `(let ((,i ,(j2s-scheme-as-integer field
					mode return ctx)))
			     (if (and (<fx ,i ,h) (>=fx ,i 0))
				 (vector-ref
				    ,(j2s-scheme obj mode return ctx) ,i)
				 (js-undefined))))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-vector-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-vector-set! this::J2SAssig mode return ctx)
   (with-access::J2SAssig this (lhs rhs)
      (let loop ((rhs rhs))
	 (if (boxed-type? (j2s-type rhs))
	     (with-access::J2SExpr rhs (loc type)
		(let ((tmp (gensym)))
		   `(let ((,tmp ,(j2s-scheme rhs mode return ctx)))
		       ,(loop (J2SCast 'any (J2SHopRef/type tmp type)))
		       ,tmp)))
	     (with-access::J2SAccess lhs (obj field)
		`(vector-set! ,(j2s-scheme obj mode return ctx)
		    (uint32->fixnum
		       ,(j2s-scheme-as-uint32 field mode return ctx))
		    ,(j2s-scheme rhs mode return ctx)))))))

;*---------------------------------------------------------------------*/
;*    j2s-jsvector-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-jsvector-ref this::J2SAccess mode return ctx)
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((eq? (j2s-type field) 'uint32)
	  `(js-vector-index-ref ,(j2s-scheme obj mode return ctx)
	      ,(j2s-scheme-as-uint32 field mode return ctx)
	      %this))
	 ((eq? (j2s-type field) 'int32)
	  `(js-vector-fixnum-ref ,(j2s-scheme obj mode return ctx)
	      (int32->fixnum ,(j2s-scheme field mode return ctx))
	      %this))
	 ((or (memq (j2s-type field) '(fixnum int53))
	      (memq (j2s-etype field (context-conf ctx)) '(fixnum int53)))
	  `(js-vector-fixnum-ref ,(j2s-scheme obj mode return ctx)
	      ,(j2s-scheme field mode return ctx)
	      %this))
	 ((j2s-field-length? field)
	  (let ((x `(js-vector-length
		       ,(j2s-scheme obj mode return ctx))))
	     (if (eq? type 'uint32)
		 x
		 `(uint32->fixnum ,x))))
	 (else
	  `(js-vector-get ,(j2s-scheme obj mode return ctx)
	      ,(j2s-scheme field mode return ctx)
	      %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-jsvector-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-jsvector-set! this::J2SAssig mode return ctx)
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field)
	 (let loop ((rhs rhs))
	    (cond
	       ((boxed-type? (j2s-type rhs))
		(with-access::J2SExpr rhs (loc)
		   (loop (J2SCast 'any rhs))))
	       ((eq? (j2s-type field) 'uint32)
		`(js-vector-index-set! ,(j2s-scheme obj mode return ctx)
		    ,(j2s-scheme-as-uint32 field mode return ctx)
		    ,(j2s-scheme rhs mode return ctx)
		    %this))
	       ((eq? (j2s-type field) 'int32)
		`(js-vector-index-set! ,(j2s-scheme obj mode return ctx)
		    (int32->fixnum ,(j2s-scheme field mode return ctx))
		    ,(j2s-scheme rhs mode return ctx)
		    %this))
	       ((or (memq (j2s-type field) '(fixnum int53))
		    (memq (j2s-etype field (context-conf ctx)) '(fixnum int53)))
		`(js-vector-fixnum-set! ,(j2s-scheme obj mode return ctx)
		    ,(j2s-scheme field mode return ctx)
		    ,(j2s-scheme rhs mode return ctx)
		    %this))
	       (else
		`(js-vector-put! ,(j2s-scheme obj mode return ctx)
		    ,(j2s-scheme field mode return ctx)
		    ,(j2s-scheme rhs mode return ctx)
		    %this)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-as-uint32 ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-as-uint32 field mode return ctx)
   (j2s-cast
      (j2s-scheme field mode return ctx)
      field
      (j2s-type field) 'uint32 ctx))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-as-integer ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-as-integer field mode return ctx)
   (j2s-cast
      (j2s-scheme field mode return ctx)
      field
      (j2s-type field) 'integer ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-iterator ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-array-iterator iterator obj args mode return ctx)
   
   (define (j2s-iterator js-iterator obj proc thisarg %this cache)
      `(,js-iterator
	  ,(j2s-scheme obj mode return ctx)
	  ,proc
	  ,thisarg
	  ,%this ,cache))
   
   (define (iterator/thisarg obj fun thisarg %this cache)
      (cond
	 ((and (isa? fun J2SFun) (not (isa? fun J2SSvc)))
	  (with-access::J2SFun fun (generator vararg)
	     (unless (or generator vararg)
		(let ((proc (jsfun->lambda fun mode return ctx #f))
		      (iterator (symbol-append iterator '-procedure)))
		   (match-case proc
		      ((?lambda (?this ?v) ?body)
		       (j2s-iterator iterator obj
			  `(lambda (,this ,v %n %arr %this::JsGlobalObject) ,body)
			  thisarg %this cache))
		      ((labels ((?id (?this ?v) ?body)) ?id)
		       (j2s-iterator iterator obj
			  `(labels ((,id (,this ,v %n %arr %this::JsGlobalObject) ,body)) ,id)
			  thisarg %this cache))
		      ((?lambda (?this ?v ?n) ?body)
		       (j2s-iterator iterator obj
			  `(lambda (,this ,v ,n %arr %this::JsGlobalObject) ,body)
			  thisarg %this cache))
		      ((labels ((?id (?this ?v ?n) ?body)) ?id)
		       (j2s-iterator iterator obj
			  `(labels ((,id (,this ,v ,n %arr %this::JsGlobalObject) ,body)) ,id)
			  thisarg %this cache))
		      ((?lambda (?this ?v ?n ?arr) ?body)
		       (j2s-iterator iterator obj
			  `(lambda (,this ,v ,n ,arr %this::JsGlobalObject) ,body)
			  thisarg %this cache))
		      ((labels ((?id (?this ?v ?n ?arr) ?body)) ?id)
		       (j2s-iterator iterator obj
			  `(labels ((,id (,this ,v ,n ,arr %this::JsGlobalObject) ,body)) ,id)
			  thisarg %this cache))
		      (else
		       #f))))))
	 (else
	  (j2s-iterator iterator
	     obj (j2s-scheme fun mode return ctx) thisarg %this cache))))

   (match-case args
      ((?fun ?thisarg ?%this ?cache)
       (iterator/thisarg obj fun (j2s-scheme thisarg mode return ctx) %this cache))
      ((?fun ?%this ?cache)
       (iterator/thisarg obj fun '(js-undefined) %this cache))
      (else
       #f)))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-foreach ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-array-foreach obj args mode return ctx)
   (j2s-array-iterator 'js-array-foreach
      obj args mode return ctx))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-foreach ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-foreach obj args mode return ctx)
   (j2s-array-iterator 'js-array-maybe-foreach
      obj args mode return ctx))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-map ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-array-map obj args mode return ctx)
   (let ((r (j2s-array-iterator 'js-array-map
	       obj args mode return ctx)))
      (match-case r
	 ((js-array-map-procedure
	     (js-array-filter-procedure ?obj ?procf ?thisargs ?%this ?cachef)
	     ?procm ?thisargs ?%this ?cachem)
	  `(js-array-filter-map2-procedure ,obj ,procf ,procm
	      ,thisargs ,%this ,cachef ,cachem))
	 (else
	  r))))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-map ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-map obj args mode return ctx)
   (j2s-array-iterator 'js-array-maybe-map
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-filter ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-array-filter obj args mode return ctx)
   (j2s-array-iterator 'js-array-filter
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-filter ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-filter obj args mode return ctx)
   (j2s-array-iterator 'js-array-maybe-filter
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-filter-map ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-array-filter-map obj args mode return ctx)
   (j2s-array-iterator 'js-array-filter-map
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-filter-map ...                                   */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-filter-map obj args mode return ctx)
   (j2s-array-iterator 'js-array-maybe-filter-map
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-flatmap ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-array-flatmap obj args mode return ctx)
   (j2s-array-iterator 'js-array-flatmap
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-flatmap ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-flatmap obj args mode return ctx)
   (j2s-array-iterator 'js-array-maybe-flatmap
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-join ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-join obj args mode return ctx)
   (match-case args
      ((?sep ?%this ?cachej)
       (let ((obj (j2s-scheme obj mode return ctx))
	     (sep (j2s-scheme sep mode return ctx)))
	  (match-case obj
	     ((js-array-maybe-map-procedure ?obj ?proc ?arg %this ?cachem)
	      `(js-array-maybe-map-join ,obj ,proc ,arg ,sep ,%this ,cachem ,cachej))
	     (else
	      `(js-array-maybe-join ,obj ,sep ,%this ,cachej)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-array-concat0 ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-array-concat0 obj args mode return ctx)
   (cond
      ((isa? obj J2SArray)
       (with-access::J2SArray obj (exprs)
	  (cond
	     ((null? exprs)
	      `(js-array-concat0-empty
		  ,@args))
	     ((null? (cdr exprs))
	      `(js-array-concat0-create
		  ,(j2s-scheme (car exprs) mode return ctx)
		  ,@args))
	     (else
	      `(js-array-concat0 ,(j2s-scheme obj mode return ctx)
		  ,@args)))))
      (else
       `(js-array-concat0 ,(j2s-scheme obj mode return ctx)
	   ,@args))))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-concat0 ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-concat0 obj args mode return ctx)
   (let ((arg (j2s-scheme (car args) mode return ctx)))
      (cond
	 ((isa? obj J2SArray)
	  (j2s-array-concat0 obj args mode return ctx))
	 (else
	  `(js-array-maybe-concat0 ,(j2s-scheme obj mode return ctx)
	      ,@args)))))

;*---------------------------------------------------------------------*/
;*    j2s-array-concat1 ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-array-concat1 obj args mode return ctx)
   (let ((arg (j2s-scheme (car args) mode return ctx)))
      (cond
	 ((isa? obj J2SArray)
	  (with-access::J2SArray obj (exprs)
	     (cond
		((null? exprs)
		 `(js-array-concat1-empty
		     ,arg ,@(cdr args)))
		((null? (cdr exprs))
		 `(js-array-concat1-create
		     ,(j2s-scheme (car exprs) mode return ctx)
		     ,arg ,@(cdr args)))
		(else
		 `(js-array-concat1 ,(j2s-scheme obj mode return ctx)
		     ,arg ,@(cdr args))))))
	 (else
	  `(js-array-concat1 ,(j2s-scheme obj mode return ctx)
	      ,arg ,@(cdr args))))))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-concat1 ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-concat1 obj args mode return ctx)
   (let ((arg (j2s-scheme (car args) mode return ctx)))
      (cond
	 ((isa? obj J2SArray)
	  (with-access::J2SArray obj (exprs)
	     (cond
		((null? exprs)
		 `(js-array-maybe-concat1-empty
		     ,arg ,@(cdr args)))
		((null? (cdr exprs))
		 `(js-array-maybe-concat1-create
		     ,(j2s-scheme (car exprs) mode return ctx)
		     ,arg ,@(cdr args)))
		(else
		 `(js-array-maybe-concat1 ,(j2s-scheme obj mode return ctx)
		     ,arg ,@(cdr args))))))
	 (else
	  `(js-array-maybe-concat1 ,(j2s-scheme obj mode return ctx)
	      ,arg ,@(cdr args))))))

;*---------------------------------------------------------------------*/
;*    j2s-array-fill1 ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-array-fill1 obj args mode return ctx)
   (let ((obj (j2s-scheme obj mode return ctx))
	 (arg (j2s-scheme (car args) mode return ctx)))
      (match-case obj
	 ((js-array-construct-alloc/length ?%this ?len)
	  `(js-array-construct-alloc-init/length ,%this ,len ,arg))
	 ((js-array-construct-alloc ?%this ?len)
	  `(js-array-construct-alloc-init/length ,%this ,len ,arg))
	 ((js-array-construct-alloc-small ?%this ?len)
	  `(js-array-construct-alloc-init/length ,%this (uint32->fixnum ,len) ,arg))
	 (else
	  `(js-array-fill1 ,obj ,arg ,@(cdr args))))))

;*---------------------------------------------------------------------*/
;*    array-sort ...                                                   */
;*---------------------------------------------------------------------*/
(define (array-sort sortfn obj args mode return ctx)
   
   (define (j2s-sort js-sort obj proc %this cache)
      `(,js-sort ,(j2s-scheme obj mode return ctx) ,proc ,%this ,cache))
   
   (define (sort obj fun %this cache)
      (cond
	 ((and (isa? fun J2SFun) (not (isa? fun J2SSvc)))
	  (with-access::J2SFun fun (generator vararg)
	     (unless (or generator vararg)
		(let ((proc (jsfun->lambda fun mode return ctx #f))
		      (sortfn (symbol-append sortfn '-procedure)))
		   (match-case proc
		      ((?lambda (?this ?x ?y) ?body)
		       (j2s-sort sortfn obj
			  `(lambda (,this ,x ,y %this::JsGlobalObject) ,body)
			  %this cache))
		      ((labels ((?id (?this ?x ?y) ?body)) ?id)
		       (j2s-sort sortfn obj
			  `(labels ((,id (,this ,x ,y %this::JsGlobalObject) ,body)) ,id)
			  %this cache))
		      (else
		       #f))))))
	 (else
	  (j2s-sort sortfn
	     obj (j2s-scheme fun mode return ctx) %this cache))))

   (match-case args
      ((?fun ?%this ?cache) (sort obj fun %this cache))
      (else #f)))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-sort ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-array-sort obj args mode return ctx)
   (array-sort 'js-array-sort
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-sort ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-sort obj args mode return ctx)
   (array-sort 'js-array-maybe-sort
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    array-reduce ...                                                 */
;*---------------------------------------------------------------------*/
(define (array-reduce reducefn obj args mode return ctx)
   
   (define (j2s-reduce js-reduce obj proc init %this cache)
      `(,js-reduce ,(j2s-scheme obj mode return ctx) ,proc ,init ,%this ,cache))
   
   (define (reduce obj fun init %this cache)
      (cond
	 ((and (isa? fun J2SFun) (not (isa? fun J2SSvc)))
	  (with-access::J2SFun fun (generator vararg)
	     (unless (or generator vararg)
		(let ((proc (jsfun->lambda fun mode return ctx #f))
		      (reducefn (symbol-append reducefn '-procedure)))
		   (match-case proc
		      ((?lambda (?this ?x ?y) ?body)
		       (j2s-reduce reducefn obj
			  `(lambda (,this ,x ,y) ,body)
			  (j2s-scheme init mode return ctx)
			  %this cache))
		      ((labels ((?id (?this ?x ?y) ?body)) ?id)
		       (j2s-reduce reducefn obj
			  `(labels ((,id (,this ,x ,y) ,body)) ,id)
			  (j2s-scheme init mode return ctx)
			  %this cache))
		      (else
		       #f))))))
	 (else
	  (j2s-reduce reducefn
	     obj
	     (j2s-scheme fun mode return ctx)
	     (j2s-scheme init mode return ctx) %this cache))))

   (match-case args
      ((?fun ?init ?%this ?cache) (reduce obj fun init %this cache))
      (else #f)))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-reduce ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-array-reduce obj args mode return ctx)
   (array-reduce 'js-array-reduce
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-reduce ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-reduce obj args mode return ctx)
   (array-reduce 'js-array-maybe-reduce
      obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-splice2 ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-splice2 this::J2SCall obj args mode return ctx)
   (with-access::J2SCall this (%info)
      `(,(if (eq? %info 'void)
	     'js-array-maybe-splice2-sans-result
	     'js-array-maybe-splice2)
	,(j2s-scheme obj mode return ctx)
	,(j2s-scheme (car args) mode return ctx)
	,(j2s-scheme (cadr args) mode return ctx)
	,(j2s-scheme (caddr args) mode return ctx)
	,(j2s-scheme (cadddr args) mode return ctx))))
       

