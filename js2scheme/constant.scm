;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/constant.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Thu Jul 15 10:33:55 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Preallocate constant objects (regexps, literal cmaps,            */
;*    closed functions, ronly literal objects, ...)                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_constant

   (include "ast.sch"
	    "usage.sch")
   
   (import  __js2scheme_ast
	    __js2scheme_dump
	    __js2scheme_compile
	    __js2scheme_stage
	    __js2scheme_utils)

   (export  j2s-constant-stage))

;*---------------------------------------------------------------------*/
;*    j2s-constant-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-constant-stage
   (instantiate::J2SStageProc
      (name "constant")
      (comment "Pre-allocated constants")
      (proc j2s-constant)))

;*---------------------------------------------------------------------*/
;*    j2s-constant ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-constant this conf)
   
   (define (keys-hashnumber v)
      (cond
	 ((not (vector? v))
	  (get-hashnumber v))
	 ((=fx (vector-length v) 0)
	  0)
	 ((=fx (vector-length v) 1)
	  (get-hashnumber (vector-ref v 0)))
	 (else
	  (let loop ((i (-fx (vector-length v) 2))
		     (n (get-hashnumber
			   (vector-ref v (-fx (vector-length v) 1)))))
	     (if (=fx i 0)
		 n
		 (bit-xor (get-hashnumber (vector-ref v i)) n))))))
   
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls loc pcache-size cnsts)
	 (let ((env (env 0 '() (create-hashtable)
		       (create-hashtable :eqtest equal?
			  :hash keys-hashnumber)
		       '())))
	    (for-each (lambda (n) (constant! n env 0 conf)) headers)
	    (for-each (lambda (n) (constant! n env 0 conf)) decls)
	    (for-each (lambda (n) (constant! n env 0 conf)) nodes)
	    (set! cnsts (reverse! (env-list env)))
	    (set! decls (append (env-vars env) decls)))))
   this)

;*---------------------------------------------------------------------*/
;*    env ...                                                          */
;*---------------------------------------------------------------------*/
(define-struct env cnt list table inits-table vars)

;*---------------------------------------------------------------------*/
;*    env-list-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (env-list-ref env n)
   (list-ref (env-list env) (-fx (env-cnt env) (+fx n 1))))

;*---------------------------------------------------------------------*/
;*    add-env! ...                                                     */
;*---------------------------------------------------------------------*/
(define (add-env! this::J2SExpr val env::struct sharep)
   (if sharep
       (let* ((t (env-table env))
	      (k val)
	      (old (hashtable-get t k)))
	  (or old
	      (let ((n (env-cnt env)))
		 (hashtable-put! t k n)
		 (env-cnt-set! env (+fx 1 n))
		 (env-list-set! env (cons this (env-list env)))
		 n)))
       (let ((n (env-cnt env)))
	  (env-cnt-set! env (+fx 1 n))
	  (env-list-set! env (cons this (env-list env)))
	  n)))

;*---------------------------------------------------------------------*/
;*    add-literal! ...                                                 */
;*---------------------------------------------------------------------*/
(define (add-literal! this env::struct type sharep)
   (with-access::J2SLiteralValue this (val)
      (let ((index (add-env! this val env sharep)))
	 (with-access::J2SExpr this (loc)
	    (instantiate::J2SLiteralCnst
	       (loc loc)
	       (type type)
	       (index index)
	       (val this))))))

;*---------------------------------------------------------------------*/
;*    add-expr! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-expr! this env::struct sharep)
   (with-access::J2SExpr this (loc)
      (let ((g (J2SLetOpt '(ref) (gensym (typeof this)) this)))
	 (with-access::J2SDeclInit g (scope)
	    (set! scope '%scope))
	 (env-vars-set! env (cons g (env-vars env)))
	 (J2SRef g))))

;*---------------------------------------------------------------------*/
;*    add-cmap! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-cmap! loc keys env::struct)
   (let* ((t (env-inits-table env))
	  (k keys)
	  (old (when (vector? keys) (hashtable-get t k))))
      ;; don't store empty cmap in the hash table in order to get
      ;; separated cmap for all empty object create sites
      (or old
	  (let ((n (env-cnt env)))
	     (hashtable-put! t k n)
	     (env-cnt-set! env (+fx 1 n))
	     (let ((cnst (instantiate::J2SCmap
			    (type 'pair)
			    (loc loc)
			    (val keys))))
		(env-list-set! env (cons cnst (env-list env)))
		n)))))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SNode env::struct nesting conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SRegExp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SRegExp env nesting conf)
   (with-access::J2SRegExp this (val flags)
      (if (=fx nesting 0)
	  (add-literal! this env 'regexp #f)
	  this)))

;*---------------------------------------------------------------------*/
;*    constant! ::J2STilde ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2STilde env nesting conf)
   (with-access::J2STilde this (stmt)
      (set! stmt (constant! stmt env (+fx nesting 1) conf))
      this))
   
;*---------------------------------------------------------------------*/
;*    constant! ::J2SDollar ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SDollar env nesting conf)
   (with-access::J2SDollar this (node)
      (set! node (constant! node env (-fx nesting 1) conf))
      this))
   
;*---------------------------------------------------------------------*/
;*    constant! ::J2SObjInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SObjInit env nesting conf)
   (with-access::J2SObjInit this (inits cmap loc ronly)
      (let ((keys (map (lambda (i)
			  (when (isa? i J2SDataPropertyInit)
			     (with-access::J2SDataPropertyInit i (name)
				(cond
				   ((isa? name J2SString)
				    (with-access::J2SString name (val)
				       (unless (string=? val "__proto__")
					  (string->symbol val))))
				   ((isa? name J2SNumber)
				    (with-access::J2SNumber name (val)
				       (string->symbol
					  (number->string val))))
				   (else
				    #f)))))
		     inits)))
	 (call-default-walker)
	 (cond
	    ((>fx (config-get conf :debug 0) 0)
	     ;; WARNING: Constant cmap are only computed in non-debug mode
	     ;; because the debug initialization does not support
	     ;; recursivity between the objects that use cmaps and
	     ;; cmaps themselves (see js-constant-init@hopscript/lib.scm
	     ;; and j2sscheme/scheme-program.scm)
	     this)
	    ((null? keys)
	     (let ((n (add-cmap! loc '#() env)))
		(set! cmap
		   (instantiate::J2SLiteralCnst
		      (loc loc)
		      (index n)
		      (val (env-list-ref env n)))))
	     (if ronly
		 (let ((index (add-env! this this env #t)))
		    (with-access::J2SExpr this (loc)
		       (instantiate::J2SLiteralCnst
			  (loc loc)
			  (type 'object)
			  (index index)
			  (val this))))
		 this))
	    ((and (pair? keys) (every (lambda (x) x) keys))
	     (let ((n (add-cmap! loc (list->vector keys) env)))
		(set! cmap
		   (instantiate::J2SLiteralCnst
		      (loc loc)
		      (index n)
		      (val (env-list-ref env n)))))
	     (if (and ronly
		      (every (lambda (init)
				(with-access::J2SDataPropertyInit init (val)
				   (or (isa? val J2SLiteralCnst)
				       (isa? val J2SString)
				       (isa? val J2SNumber)
				       (isa? val J2SBool)
				       (isa? val J2SUndefined))))
			 inits))
		 (let ((index (add-env! this this env #t)))
		    (with-access::J2SExpr this (loc)
		       (instantiate::J2SLiteralCnst
			  (loc loc)
			  (type 'object)
			  (index index)
			  (val this))))
		 this))
	    (else
	     this)))))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SDeclInit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SDeclInit env nesting conf)
   (with-access::J2SDeclInit this (val vtype vtype itype loc scope)
      (if (and (not (decl-usage-has? this '(assig set ref method)))
	       (constant-array? val)
	       (not (eq? scope 'global)))
	  (begin
	     ;; MS CARE UTYPE
	     ;; (set! utype 'array)
	     (set! vtype 'array)
	     (set! itype 'array)
	     (set! val (add-expr! val env #t)))
	  (call-default-walker))
      this))

;*---------------------------------------------------------------------*/
;*    constant-array? ...                                              */
;*---------------------------------------------------------------------*/
(define (constant-array? this::J2SExpr)
   (when (isa? this J2SArray)
      (with-access::J2SArray this (exprs)
	 (every (lambda (e)
		   (or (isa? e J2SLiteralCnst)
		       (isa? e J2SString)
		       (isa? e J2SNumber)
		       (isa? e J2SBool)
		       (isa? e J2SUndefined)
		       (constant-array? e)))
	    exprs))))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SAccess env nesting conf)
   (with-access::J2SAccess this (obj field)
      (set! obj (constant! obj env nesting conf))
      (unless (isa? field J2SString)
	 ;; MS 15mar19: otherwise, no hidden class test would ever be emitted
	 ;; (see "constant! ::J2SString" and Scheme code generation
	 ;; (put and get)
	 (set! field (constant! field env nesting conf)))
      this))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SUnary env nesting conf)
   (call-default-walker)
   (with-access::J2SUnary this (op expr expr loc type)
      (if (isa? expr J2SNumber)
	  (with-access::J2SNumber expr (val)
	     (case op
		((+)
		 (if (= val 0)
		     (J2SNumber/type 'real +0.0)
		     (J2SNumber/type type val)))
		((-)
		 (if (= val 0)
		     (J2SNumber/type 'real -0.0)
		     (J2SNumber/type type (- val))))
		((~)
		 (if (fixnum? val)
		     (let ((~val (bit-nots32 (fixnum->int32 val))))
			(if (and (>=fx (int32->fixnum ~val) (minvalfx))
				 (<=fx (int32->fixnum ~val) (maxvalfx)))
			    (J2SNumber/type 'integer
			       (int32->fixnum (bit-nots32 (fixnum->int32 val))))
			    this))
		     this))
		(else
		 this)))
	  this)))

;*---------------------------------------------------------------------*/
;*    arithmetic bounds ...                                            */
;*---------------------------------------------------------------------*/
(define *max-int30* (-llong (bit-lshllong #l1 29) #l1))
(define *min-int30* (negllong (bit-lshllong #l1 29)))
   
(define *max-int53* (bit-lshllong #l1 53))
(define *min-int53* (negllong *max-int53*))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SBinary ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SBinary env nesting conf)
   
   (define (in-range-53? v)
      (let ((lv (fixnum->llong v)))
	 (and (<=llong lv *max-int53*) (>=llong lv *min-int53*))))

   (define (in-range-30? v)
      (let ((lv (fixnum->llong v)))
	 (and (<llong lv *max-int30*) (>=llong lv *min-int30*))))

   (define (evaluate this op l r)
      (with-access::J2SBinary this (loc)
	 (cond
	    ((and (flonum? l) (flonum? r))
	     (J2SNumber/type 'real (op l r)))
	    ((flonum? l)
	     (J2SNumber/type 'real (op l (fixnum->flonum r))))
	    ((flonum? r)
	     (J2SNumber/type 'real (op (fixnum->flonum l) r)))
	    ((and (fixnum? l) (fixnum? l))
	     (let ((v (op l r)))
		(cond
		   ((not (fixnum? v))
		    (J2SNumber/type 'real v))
		   ((and (in-range-53? v) (>=fx (config-get conf :int-size 0) 53))
		    (J2SNumber/type 'integer v))
		   ((in-range-30? v)
		    (J2SNumber/type 'integer v))
		   (else
		    this))))
	    (else
	     this))))
   
   (define (unparen expr)
      (if (isa? expr J2SParen)
	  (with-access::J2SParen expr (expr) (unparen expr))
	  expr))
   
   (call-default-walker)
   (with-access::J2SBinary this (op expr lhs rhs loc type)
      (cond
	 ((and (isa? (unparen lhs) J2SNumber) (isa? (unparen rhs) J2SNumber))
	  (with-access::J2SNumber (unparen lhs) ((lval val))
	     (with-access::J2SNumber (unparen rhs) ((rval val))
		(case op
		   ((+) (evaluate this + lval rval))
		   ((-) (evaluate this - lval rval))
		   ((*) (evaluate this * lval rval))
		   ((/)
		    (if (= rval 0)
			(cond
			   ((flonum? rval)
			    ;; get the correct infinity sign
			    (J2SNumber (/ lval rval)))
			   (else
			    (J2SNumber (/ lval 0.0))))
			(evaluate this / lval rval)))
		   ((BIT_OR & ^)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->int32 lval))
			       (y (fixnum->int32 rval))
			       (r (case op
				     ((BIT_OR) (bit-ors32 x y))
				     ((&) (bit-ands32 x y))
				     (else (bit-xors32 x y)))))
			   (if (and (>=s32 r (fixnum->int32 (minvalfx)))
				    (<=s32 r (fixnum->int32 (maxvalfx))))
			       (J2SNumber/type 'integer (int32->fixnum r))
			       this))
			this))
		   ((<<)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->int32 lval))
			       (y (bit-andu32 (fixnum->uint32 rval) #u32:31))
			       (r (bit-lshu32 x (uint32->fixnum y))))
			   (if (and (>=s32 r (fixnum->int32 (minvalfx)))
				    (<=s32 r (fixnum->int32 (maxvalfx))))
			       (J2SNumber/type 'integer (int32->fixnum r))
			       this))
			this))
		   ((>>)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->int32 lval))
			       (y (bit-andu32 (fixnum->uint32 rval) #u32:31))
			       (r (bit-rshs32 x (uint32->fixnum y))))
			   (if (and (>=s32 r (fixnum->int32 (minvalfx)))
				    (<=s32 r (fixnum->int32 (maxvalfx))))
			       (J2SNumber/type 'integer (int32->fixnum r))
			       this))
			this))
		   ((>>>)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->uint32 lval))
			       (y (bit-andu32 (fixnum->uint32 rval) #u32:31))
			       (r (bit-rshu32 x (uint32->fixnum y))))
			   (if (<=u32 r (fixnum->uint32 (maxvalfx)))
			       (J2SNumber/type 'integer (uint32->fixnum r))
			       this))
			this))
		   ((%)
		    (if (and (fixnum? lval) (fixnum? rval)
			     (>=fx lval 0) (>fx rval 0))
			(J2SNumber/type 'integer (remainder lval rval))
			this))
		   (else this)))))
	 ((and (isa? (unparen lhs) J2SBool) (isa? (unparen rhs) J2SBool))
	  (with-access::J2SBool (unparen lhs) ((lval val))
	     (with-access::J2SBool (unparen rhs) ((rval val))
		(case op
		   ((&&) (J2SBool (and lval rval)))
		   ((OR OR*) (J2SBool (or lval rval)))
		   (else this)))))
	 ((and (isa? (unparen lhs) J2SString) (isa? (unparen rhs) J2SString))
	  (with-access::J2SString (unparen lhs) ((lval val))
	     (with-access::J2SString (unparen rhs) ((rval val))
		(case op
		   ((== ===)
		    (instantiate::J2SBool
		       (loc loc)
		       (val (string=? lval rval))))
		   ((!= !==)
		    (instantiate::J2SBool
		       (loc loc)
		       (val (not (string=? lval rval)))))
		   ((+)
		    (if (eq? (string-minimal-charset lval) 'ascii )
			(let ((ns (duplicate::J2SString (unparen lhs)
				     (val (string-append lval rval)))))
			   (constant! ns env nesting conf))
			this))
		   (else
		    this)))))
	 (else
	  this))))
       
;*---------------------------------------------------------------------*/
;*    constant! ::J2SDeclFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SDeclFun env nesting conf)
   (with-access::J2SDeclFun this (val)
      (constant! val env nesting conf))
   this)

;*---------------------------------------------------------------------*/
;*    constant! ::J2SRecord ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SRecord env nesting conf)
   (call-default-walker)
   (with-access::J2SRecord this (cmap loc)
      (let ((n (add-cmap! loc
		  (list->vector
		     (map! (lambda (prop)
			      (with-access::J2SPropertyInit prop (name)
				 (with-access::J2SString name (val)
				    (string->symbol val))))
			(j2s-class-instance-properties this)))
		  env)))
	 (set! cmap
	    (instantiate::J2SLiteralCnst
	       (loc loc)
	       (index n)
	       (val (env-list-ref env n))))))
   this)
