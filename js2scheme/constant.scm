;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/constant.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Wed Jun 20 17:49:49 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Preallocate constant objects (regexps, literal cmaps,            */
;*    closed functions, ...)                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_constant

   (include "ast.sch")
   
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
      (proc j2s-constant)
      (optional 2)))

;*---------------------------------------------------------------------*/
;*    j2s-constant ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-constant this args)
   
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
	    (for-each (lambda (n) (constant! n env 0)) headers)
	    (for-each (lambda (n) (constant! n env 0)) decls)
	    (for-each (lambda (n) (constant! n env 0)) nodes)
	    (set! cnsts (reverse! (env-list env)))
	    (set! decls (append decls (env-vars env))))))
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
(define (add-literal! this env::struct sharep)
   (with-access::J2SLiteralValue this (val)
      (let ((index (add-env! this val env sharep)))
	 (with-access::J2SExpr this (loc)
	    (instantiate::J2SLiteralCnst
	       (loc loc)
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
	  (old (hashtable-get t k)))
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
(define-walk-method (constant! this::J2SNode env::struct nesting)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SRegExp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SRegExp env nesting)
   (with-access::J2SRegExp this (val flags)
      (if (=fx nesting 0)
	  (add-literal! this env #f)
	  this)))

;*---------------------------------------------------------------------*/
;*    constant! ::J2STilde ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2STilde env nesting)
   (with-access::J2STilde this (stmt)
      (set! stmt (constant! stmt env (+fx nesting 1)))
      this))
   
;*---------------------------------------------------------------------*/
;*    constant! ::J2SDollar ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SDollar env nesting)
   (with-access::J2SDollar this (node)
      (set! node (constant! node env (-fx nesting 1)))
      this))
   
;*---------------------------------------------------------------------*/
;*    constant! ::J2SObjInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SObjInit env nesting)
   (with-access::J2SObjInit this (inits cmap loc)
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
	 (if (and (pair? keys) (every (lambda (x) x) keys))
	     ;; constant cmap
	     (let ((n (add-cmap! loc (list->vector keys) env)))
		(set! cmap
		   (instantiate::J2SLiteralCnst
		      (loc loc)
		      (index n)
		      (val (env-list-ref env n))))
		this)
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    constant! ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SUnary env nesting)
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
;*    constant! ::J2SBinary ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SBinary env nesting)
   
   (define (evaluate this op l r)
      (with-access::J2SBinary this (loc)
	 (cond
	    ((and (flonum? l) (flonum? r)) (J2SNumber/type 'real (op l r)))
	    ((flonum? l) (J2SNumber/type 'real (op l (fixnum->flonum r))))
	    ((flonum? r) (J2SNumber/type 'real (op (fixnum->flonum l) r)))
	    (else this))))
   
   (call-default-walker)
   (with-access::J2SBinary this (op expr lhs rhs loc type)
      (if (and (isa? lhs J2SNumber) (isa? rhs J2SNumber))
	  (with-access::J2SNumber lhs ((lval val))
	     (with-access::J2SNumber rhs ((rval val))
		(case op
		   ((+) (evaluate this + lval rval))
		   ((-) (evaluate this - lval rval))
		   ((*) (evaluate this * lval rval))
		   ((/) (evaluate this / lval rval))
		   ((BIT_OR & ^)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->int32 lval))
			       (y (fixnum->int32 rval))
			       (r (case op
				     ((BIT_OR) (bit-ors32 x y))
				     ((&) (bit-ands32 x y))
				     (else (bit-xors32 x y)))))
			   (if (and (>=s32 (fixnum->int32 (minvalfx)) r)
				    (<=s32 (fixnum->int32 (maxvalfx)) r))
			       (J2SNumber (int32->fixnum r))
			       this))
			this))
		   ((<<)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->int32 lval))
			       (y (bit-andu32 (fixnum->uint32 rval) #u32:31))
			       (r (bit-lshu32 x (uint32->fixnum y))))
			   (if (and (>=s32 (fixnum->int32 (minvalfx)) r)
				    (<=s32 (fixnum->int32 (maxvalfx)) r))
			       (J2SNumber (int32->fixnum r))
			       this))
			this))
		   ((>>)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->int32 lval))
			       (y (bit-andu32 (fixnum->uint32 rval) #u32:31))
			       (r (bit-rshs32 x (uint32->fixnum y))))
			   (if (and (>=s32 (fixnum->int32 (minvalfx)) r)
				    (<=s32 (fixnum->int32 (maxvalfx)) r))
			       (J2SNumber (int32->fixnum r))
			       this))
			this))
		   ((>>>)
		    (if (and (fixnum? lval) (fixnum? rval))
			(let* ((x (fixnum->uint32 lval))
			       (y (bit-andu32 (fixnum->uint32 rval) #u32:31))
			       (r (bit-rshu32 x (uint32->fixnum y))))
			   (if (<=u32 r (fixnum->uint32 (maxvalfx)))
			       (J2SNumber (uint32->fixnum r))
			       this))
			this))
		   ((%)
		    (tprint "TODO.constant! " (j2s->list this))
		    this)
		   (else this))))
	  this)))
       
;*---------------------------------------------------------------------*/
;*    constant! ::J2SDeclFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SDeclFun env nesting)
   (with-access::J2SDeclFun this (val)
      (constant! val env nesting))
   this)

;*---------------------------------------------------------------------*/
;*    constant! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (constant! this::J2SFun env nesting)
   (call-default-walker)
   (with-access::J2SFun this (body params)
      ;; in order to activate this optimization, it must be proved
      ;; that no field is added to the function
      (if (and #f (closed? body params))
	  (add-expr! this env #f)
	  this)))

;*---------------------------------------------------------------------*/
;*    closed? ...                                                      */
;*---------------------------------------------------------------------*/
(define (closed? this::J2SNode env)
   (let ((cell (make-cell #t)))
      (node-closed this env cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    node-closed ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-closed this::J2SNode env cell)
   (when (cell-ref cell)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    node-closed ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-closed this::J2SRef env cell)
   (when (cell-ref cell)
      (with-access::J2SRef this (decl)
	 (if (memq decl env)
	     (call-default-walker)
	     (with-access::J2SDecl decl (scope)
		(unless (eq? scope '%scope)
		   (cell-set! cell #f)))))))

;*---------------------------------------------------------------------*/
;*    node-closed ::J2SThis ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-closed this::J2SThis env cell)
   (cell-ref cell))

;*---------------------------------------------------------------------*/
;*    node-closed ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-closed this::J2SFun env cell)
   #t)

