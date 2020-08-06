;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-array.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Thu Apr 16 18:45:17 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
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
	   (j2s-array-ref ::J2SAccess mode return conf)
	   (j2s-array-set! ::J2SAssig mode return conf)
	   (j2s-vector-ref ::J2SAccess mode return conf)
	   (j2s-vector-set! ::J2SAssig mode return conf)
	   (j2s-array-foreach obj args mode return conf)
	   (j2s-array-maybe-foreach obj args mode return conf)
	   (j2s-array-map obj args mode return conf)
	   (j2s-array-maybe-map obj args mode return conf)
	   (j2s-array-maybe-join obj args mode return conf)))

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
	     (if (eq? type 'vector)
		 ''#()
		 `(js-empty-vector->jsarray %this)))
	    ((and (every (lambda (x)
			    (or (number? x) (string? x) (boolean? x)))
		     sexprs)
		  (unique? ctx sexprs))
	     (let ((vec `(copy-vector ',(list->vector sexprs) ,(length sexprs))))
		(epairify loc
		   (if (eq? type 'vector)
		       vec
		       `(js-vector->jsarray ,vec %this)))))
	    ((any (lambda (x) (isa? x J2SSpread)) exprs)
	     (j2s-scheme (spread->array-expr loc exprs #t) mode return ctx))
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
	  `(js-array-construct-alloc/lengthu32 %this (js-array-alloc %this)
	      ,(k (j2s-scheme arg mode return ctx))))
	 (else
	  `(js-array-construct-alloc/length %this
	      ,(box (j2s-scheme arg mode return ctx)
		  (j2s-vtype arg) ctx)))))
   
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
			   `(js-array-construct-alloc %this
			       ,(box (j2s-scheme arg mode return ctx) type ctx)))))
		   (else
		    `(js-array-construct-alloc %this 
			,(j2s-scheme arg mode return ctx))))))))))

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
		  (tyfield (j2s-vtype field)))
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
	 (case (j2s-vtype field)
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
   
   (define (aset/cache this)
      (with-access::J2SAssig this (lhs rhs)
	 ;; an optimized array set in a loop (see array.scm)
	 (with-access::J2SAccess lhs (obj field)
	    (with-access::J2SAref obj (array alen amark deps)
	       (let ((scmarray (j2s-decl-scheme-id array))
		     (scmalen (j2s-decl-scheme-id alen))
		     (scmfield (j2s-scheme field mode return ctx))
		     (scmobj (j2s-scheme obj mode return ctx))
		     (scmrhs (j2s-scheme rhs mode return ctx))
		     (tyfield (j2s-vtype field)))
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
			      %this)))))))))

   (define (aset/w-cache this)
      (with-access::J2SAssig this (lhs rhs)
	 ;; an optimized array set in a loop (see array.scm)
	 (with-access::J2SAccess lhs (obj field)
	    (with-access::J2SAref obj (array alen)
	       (case (j2s-vtype field)
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
		       `(js-array-string-set! ,(j2s-scheme obj mode return ctx)
			   ,(j2s-scheme field mode return ctx)
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
		       `(js-array-set! ,(j2s-scheme obj mode return ctx)
			   ,(j2s-scheme field mode return ctx)
			   ,(j2s-scheme rhs mode return ctx)
			   ,(strict-mode? mode)
			   %this)))))))))

   (define (aset this::J2SAssig)
      (if *j2s-array-cache*
	  (aset/cache this)
	  (aset/w-cache this)))
   
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field cache (loca loc))
	 (if (isa? obj J2SAref)
	     (aset this)
	     (aset/w-cache this)))))
   
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
		     ((eq? (j2s-vtype field) 'uint32)
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
      (with-access::J2SAccess lhs (obj field)
	 `(vector-set! ,(j2s-scheme obj mode return ctx)
	     (uint32->fixnum
		,(j2s-scheme-as-uint32 field mode return ctx))
	     ,(j2s-scheme rhs mode return ctx)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-as-uint32 ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-as-uint32 field mode return ctx)
   (j2s-cast
      (j2s-scheme field mode return ctx)
      field
      (j2s-vtype field) 'uint32 ctx))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-as-integer ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-as-integer field mode return ctx)
   (j2s-cast
      (j2s-scheme field mode return ctx)
      field
      (j2s-vtype field) 'integer ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-foreach-map ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-array-foreach-map js-foreach-or-map obj args mode return ctx)
   
   (define (foreach obj proc thisarg %this cache)
      `(,js-foreach-or-map
	  ,(j2s-scheme obj mode return ctx)
	  ,proc
	  ,thisarg
	  ,%this ,cache))
   
   (define (foreach/thisarg obj fun thisarg %this cache)
      (cond
	 ((and (isa? fun J2SFun) (not (isa? fun J2SSvc)))
	  (with-access::J2SFun fun (generator vararg)
	     (unless (or generator vararg)
		(let ((proc (jsfun->lambda fun mode return ctx #f #f)))
		   (match-case proc
		      ((?lambda (?this ?v) ?body)
		       (foreach obj
			  `(lambda (,this ,v %n %arr %this::JsGlobalObject) ,body)
			  thisarg %this cache))
		      ((labels ((?id (?this ?v) ?body)) ?id)
		       (foreach obj
			  `(labels ((,id (,this ,v %n %arr %this::JsGlobalObject) ,body)) ,id)
			  thisarg %this cache))
		      ((?lambda (?this ?v ?n) ?body)
		       (foreach obj
			  `(lambda (,this ,v ,n %arr %this::JsGlobalObject) ,body)
			  thisarg %this cache))
		      ((labels ((?id (?this ?v ?n) ?body)) ?id)
		       (foreach obj
			  `(labels ((,id (,this ,v ,n %arr %this::JsGlobalObject) ,body)) ,id)
			  thisarg %this cache))
		      ((?lambda (?this ?v ?n ?arr) ?body)
		       (foreach obj
			  `(lambda (,this ,v ,n ,arr %this::JsGlobalObject) ,body)
			  thisarg %this cache))
		      ((labels ((?id (?this ?v ?n ?arr) ?body)) ?id)
		       (foreach obj
			  `(labels ((,id (,this ,v ,n ,arr %this::JsGlobalObject) ,body)) ,id)
			  thisarg %this cache))
		      (else
		       #f))))))
	 (else
	  #f)))

   (match-case args
      ((?fun ?thisarg ?%this ?cache)
       (foreach/thisarg obj fun (j2s-scheme thisarg mode return ctx) %this cache))
      ((?fun ?%this ?cache)
       (foreach/thisarg obj fun '(js-undefined) %this cache))
      (else
       #f)))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-foreach ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-array-foreach obj args mode return ctx)
   (j2s-array-foreach-map 'js-array-foreach-procedure obj args mode return ctx))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-foreach ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-foreach obj args mode return ctx)
   (j2s-array-foreach-map 'js-array-maybe-foreach-procedure obj args mode return ctx))
	   
;*---------------------------------------------------------------------*/
;*    j2s-array-map ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-array-map obj args mode return ctx)
   (j2s-array-foreach-map 'js-array-map-procedure obj args mode return ctx))

;*---------------------------------------------------------------------*/
;*    j2s-array-maybe-map ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-array-maybe-map obj args mode return ctx)
   (j2s-array-foreach-map 'js-array-maybe-map-procedure obj args mode return ctx))

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
