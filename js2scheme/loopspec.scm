;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/loopspec.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  3 07:00:40 2019                          */
;*    Last change :  Wed Dec 29 08:52:51 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Loop specialization                                              */
;*    -------------------------------------------------------------    */
;*    This optimization duplicates (small) loops in order to use       */
;*    precise types for the loop index.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_loopspec

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_node-size
	   __js2scheme_alpha)

   (export j2s-loopspec-stage))

;*---------------------------------------------------------------------*/
;*    j2s-loopspec-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-loopspec-stage
   (instantiate::J2SStageProc
      (name "loopspec")
      (comment "Loop type specialization")
      (optional :optim-loopspec)
      (proc j2s-loopspec!)))

;*---------------------------------------------------------------------*/
;*    j2s-loopspec! ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-loopspec! this conf)
   (when (isa? this J2SProgram)
      (j2s-loopspec-program! this conf))
   this)

;*---------------------------------------------------------------------*/
;*    loopspec-threshold ...                                           */
;*---------------------------------------------------------------------*/
(define loopspec-threshold 1000)
(define loopspec-uint29 (-fx (exptfx 2 29) 1))

;*---------------------------------------------------------------------*/
;*    j2s-loopspec-program! ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-loopspec-program! this::J2SProgram conf)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each (lambda (n) (loopspec! n conf)) decls)
      (for-each (lambda (n) (loopspec! n conf)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    loopspec! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (loopspec! this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    loopspec! ::J2SMeta ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (loopspec! this::J2SMeta conf)
   (with-access::J2SMeta this (optim)
      (if (>=fx optim 2)
	  (call-default-walker)
	  this)))
   
;*---------------------------------------------------------------------*/
;*    incr-index ...                                                   */
;*---------------------------------------------------------------------*/
(define (incr-index incr::J2SExpr)
   (when (isa? incr J2SAssig)
      (with-access::J2SAssig incr (lhs)
	 (when (isa? lhs J2SRef)
	    lhs))))

;*---------------------------------------------------------------------*/
;*    test-constant? ...                                               */
;*    -------------------------------------------------------------    */
;*    Does the range test involve a constant value?                    */
;*---------------------------------------------------------------------*/
(define (test-constant? test::J2SExpr ref)
   (with-access::J2SRef ref (decl)
      (let ((cnt (let loop ((armed 0)
			    (test test))
		    (cond
		       ((isa? test J2SNumber)
			armed)
		       ((isa? test J2SBinary)
			(with-access::J2SBinary test (lhs rhs)
			   (loop (loop armed lhs) rhs)))
		       ((isa? test J2SRef)
			(if (>=fx armed 0)
			    (with-access::J2SRef test ((rdecl decl))
			       (cond
				  ((eq? rdecl decl)
				   armed)
				  ((decl-usage-has? rdecl '(assig))
				   -1)
				  (else
				   (+ armed 1))))
			    armed))
		       (else
			-1)))))
	 (>fx cnt 0))))

;*---------------------------------------------------------------------*/
;*    incr-integer? ...                                                */
;*    -------------------------------------------------------------    */
;*    Is the incr a possible integer increment?                        */
;*---------------------------------------------------------------------*/
(define (incr-integer? incr::J2SExpr)
   (cond
      ((isa? incr J2SPostfix)
       (with-access::J2SPostfix incr (rhs)
	  (when (isa? rhs J2SBinary)
	     (with-access::J2SBinary rhs (op)
		(memq op '(+ -))))))
      ((isa? incr J2SPrefix)
       (with-access::J2SPrefix incr (rhs)
	  (when (isa? rhs J2SBinary)
	     (with-access::J2SBinary rhs (op)
		(memq op '(+ -))))))
      ((isa? incr J2SAssigOp)
       (with-access::J2SAssigOp incr (op)
	  (memq op '(+ -))))
      ((isa? incr J2SAssig)
       (with-access::J2SAssig incr (rhs)
	  (when (isa? rhs J2SBinary)
	     (with-access::J2SBinary rhs (op)
		(memq op '(+ -))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    incr-sign ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is the index increases or decreases?                             */
;*---------------------------------------------------------------------*/
(define (incr-sign incr::J2SExpr)
   (let loop ((incr incr))
      (cond
	 ((isa? incr J2SPostfix)
	  (with-access::J2SPostfix incr (rhs)
	     (loop rhs)))
	 ((isa? incr J2SPrefix)
	  (with-access::J2SPrefix incr (rhs)
	     (loop rhs)))
	 ((isa? incr J2SAssigOp)
	  (with-access::J2SAssigOp incr (rhs)
	     (loop rhs)))
	 ((isa? incr J2SAssig)
	  (with-access::J2SAssig incr (rhs)
	     (loop rhs)))
	 ((isa? incr J2SRef)
	  1)
	 ((isa? incr J2SBinary)
	  (with-access::J2SBinary incr (op rhs lhs)
	     (if (eq? op '+)
		 (* (incr-sign rhs) (incr-sign lhs))
		 (- (* (incr-sign rhs) (incr-sign lhs))))))
	 ((isa? incr J2SNumber)
	  (with-access::J2SNumber incr (val)
	     (if (> val 0) 1 -1)))
	 (else
	  0))))

;*---------------------------------------------------------------------*/
;*    range-test? ...                                                  */
;*    -------------------------------------------------------------    */
;*    Does the test checks a upper (resp. lower) bound (range test)?   */
;*---------------------------------------------------------------------*/
(define (range-test? test::J2SExpr ref::J2SRef)
   
   (define (ref-equal? expr::J2SExpr ref::J2SRef)
      (when (isa? expr J2SRef)
	 (with-access::J2SRef expr (decl)
	    (with-access::J2SRef ref ((var decl))
	       (eq? decl var)))))
   
   (define (constant-range? expr::J2SExpr)
      (when (isa? expr J2SRef)
	 (with-access::J2SRef expr (decl)
	    (not (decl-usage-has? decl '(assig eval))))))
   
   (when (isa? test J2SBinary)
      (with-access::J2SBinary test (lhs rhs op)
	 (when (memq op '(== != === !== < <= > >=))
	    (cond
	       ((ref-equal? lhs ref) (constant-range? rhs))
	       ((ref-equal? rhs ref) (constant-range? lhs))
	       (else #f))))))

;*---------------------------------------------------------------------*/
;*    test-rhs ...                                                     */
;*    -------------------------------------------------------------    */
;*    Does the range test involve a constant value?                    */
;*---------------------------------------------------------------------*/
(define (test-rhs test::J2SExpr ref)
   (with-access::J2SRef ref (decl)
      (with-access::J2SBinary test (lhs rhs)
	 (if (isa? lhs J2SRef)
	     (with-access::J2SRef lhs ((lhsdecl decl))
		(if (eq? lhsdecl decl)
		    rhs
		    lhs))
	     lhs))))

;*---------------------------------------------------------------------*/
;*    test-rhs-decl ...                                                */
;*---------------------------------------------------------------------*/
(define (test-rhs-decl test::J2SExpr ref)
   (with-access::J2SRef ref (decl)
      (with-access::J2SBinary test (lhs rhs)
	 (when (isa? lhs J2SRef)
	    (with-access::J2SRef lhs ((lhsdecl decl))
	       (if (eq? lhsdecl decl)
		   (when (isa? rhs J2SRef)
		      (with-access::J2SRef rhs (decl) decl))
		   lhsdecl))))))

;*---------------------------------------------------------------------*/
;*    for-decl ...                                                     */
;*---------------------------------------------------------------------*/
(define (for-decl decl::J2SDecl loc type::symbol)
   (with-access::J2SDecl decl (id)
      (let ((ref (J2SRef decl)))
	 (with-access::J2SRef ref ((rtype type))
	    (set! rtype type))
	 (J2SLetOptVtype type '(ref get assig)
	    (symbol-append '%F id)
	    ref))))

;*---------------------------------------------------------------------*/
;*    loopspec! ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (loopspec! this::J2SFor conf)
   (with-access::J2SFor this (init test incr body loc)
      (let ((ref (incr-index incr)))
	 ;; optimize the loop iff...
	 (cond
	    ((not ref)
	     ;; ... 1) there is an index,
	     (call-default-walker))
	    ((not (memq (j2s-type ref) '(any number integer unknown)))
	     ;; ... 2) it is not already specialized
	     (call-default-walker))
	    ((not (incr-integer? incr))
	     ;; ... 3) the incr is possibly integer
	     (call-default-walker))
	    ((memq (j2s-type ref) '(int32 uint32))
	     ;; ... 4) already specifialized
	     (call-default-walker))
	    ((not (range-test? test ref))
	     ;; ... 5) the test is a range test
	     (call-default-walker))
	    ((not (test-constant? test ref))
	     ;; ... 6) the upper bound is not a constant
	     (call-default-walker))
	    ((>fx (node-size this) loopspec-threshold)
	     ;; ... 7) body too large
	     (call-default-walker))
	    (else
	     (with-access::J2SRef ref (decl (refloc loc))
		(with-access::J2SDecl decl (escape)
		   (if escape
		       (call-default-walker)
		       (let ((testrhs (test-rhs test ref))
			     (endloc (node-endloc this)))
			  (if (memq (j2s-type testrhs) '(int32 uint32))
			      (call-default-walker)
			      (let ((fordecl (for-decl decl refloc 'int32))
				    (tmp (gensym 'tmp)))
				 (when (>=fx (config-get conf :verbose 0) 3)
				    (display " [" (current-error-port))
				    (display (cadr loc) (current-error-port))
				    (display ":" (current-error-port))
				    (display (caddr loc) (current-error-port))
				    (display "]" (current-error-port)))
				 (J2SIf (J2SBinary/type '&& 'bool
					       (J2SPragma/bindings 'bool
						  (list tmp) (list testrhs)
						  `(fixnum? ,tmp))
					   (if (>fx (incr-sign incr) 0)
					       (J2SBinary/type '< 'bool
						  (as-int testrhs)
						  (J2SNumber loopspec-uint29))
					       (J2SBinary/type '>= 'bool
						  (as-int testrhs)
						  (J2SNumber 0))))
				    (J2SLetRecBlock #f (list fordecl)
				       (force-type-bint!
					  (j2s-alpha this
					     (list decl) (list fordecl))
					  (list (test-rhs-decl test ref) fordecl)))
				    (J2SMeta 'loopspec 0 0
				       (uncache! this))))))))))))))

;*---------------------------------------------------------------------*/
;*    as-int ...                                                       */
;*---------------------------------------------------------------------*/
(define (as-int expr::J2SExpr)
   (with-access::J2SExpr expr (type)
      (set! type 'bint)
      expr))

;*---------------------------------------------------------------------*/
;*    force-type-bint! ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type-bint! this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    force-type-bint! ::J2SRef ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type-bint! this::J2SRef env::pair-nil)
   (with-access::J2SRef this (decl type)
      (when (memq type '(integer number any))
	 (when (memq decl env)
	    (with-access::J2SDecl decl (vtype)
	       (if (eq? vtype 'int32)
		   (set! type 'int32)
		   (set! type 'bint))))))
   this)

;*---------------------------------------------------------------------*/
;*    force-type-bint! ::J2SDeclInit ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type-bint! this::J2SDeclInit env::pair-nil)
   (with-access::J2SDeclInit this (val vtype)
      (when (and (isa? val J2SRef) (not (decl-usage-has? this '(assig))))
	 (with-access::J2SRef val (decl)
	    (when (memq decl env)
	       (set-cdr! (last-pair env) (list this))
	       (set! vtype 'bint)))))
   this)

;*---------------------------------------------------------------------*/
;*    force-type-bint! ::J2SLetBlock ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (force-type-bint! this::J2SLetBlock env::pair-nil)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d) (force-type-bint! d env)) decls)
      (for-each (lambda (n) (force-type-bint! n env)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SNode ...                                           */
;*    -------------------------------------------------------------    */
;*    Remove property cache to save code size.                         */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SNode)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SPrefix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SPrefix)
   (call-default-walker)
   (with-access::J2SPrefix this (cache)
      (set! cache #f))
   this)

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SPostfix ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SPostfix)
   (call-default-walker)
   (with-access::J2SPostfix this (cache)
      (set! cache #f))
   this)

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SAssigOp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SAssigOp)
   (call-default-walker)
   (with-access::J2SAssigOp this (cache)
      (set! cache #f))
   this)

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SAccess)
   (call-default-walker)
   (with-access::J2SAccess this (cache)
      (set! cache #f))
   this)

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SCall)
   (call-default-walker)
   (with-access::J2SCall this (cache)
      (set! cache #f))
   this)

;*---------------------------------------------------------------------*/
;*    uncache! ::J2SOPTInitSeq ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (uncache! this::J2SOPTInitSeq)
   (call-default-walker)
   (with-access::J2SOPTInitSeq this (cache)
      (set! cache #f))
   this)
