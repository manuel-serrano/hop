;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/return.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Wed Dec  9 08:33:42 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript Return -> bind-exit                                   */
;*    -------------------------------------------------------------    */
;*    This module implements the JavaScript return removal. After      */
;*    this pass, return statement are replaced with either bind-exit   */
;*    calls or by tail returns.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_return

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

   (export j2s-return-stage
	   (generic j2s-return ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-return-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-return-stage
   (instantiate::J2SStageProc
      (name "return")
      (comment "Mark functions that need a Bigloo bind-exit for return")
      (proc j2s-return)))

;*---------------------------------------------------------------------*/
;*    j2s-return ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (j2s-return this args)
   this)

;*---------------------------------------------------------------------*/
;*    trim-nop ...                                                     */
;*---------------------------------------------------------------------*/
(define (trim-nop nodes)
   (filter (lambda (x) (not (isa? x J2SNop))) nodes))

;*---------------------------------------------------------------------*/
;*    j2s-return ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s-return this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each (lambda (o) (unreturn! o #f #t args)) headers)
      (for-each (lambda (o) (unreturn! o #f #t args)) decls)
      (for-each (lambda (o) (unreturn! o #f #t args)) nodes)
      (set! headers (trim-nop headers))
      (set! decls (trim-nop decls))
      (set! nodes (trim-nop nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SNode target tail? args)
   (default-walk! this target tail? args))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SSeq ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SSeq target tail? args)
   (with-access::J2SSeq this (nodes loc)
      (let loop ((n nodes))
	 (when (pair? n)
	    (let ((t? (and tail? (null? (cdr n)))))
	       (set-car! n (walk! (car n) target t? args))
	       (loop (cdr n)))))
      ;; remove all useful nop (for readability)
      (set! nodes (trim-nop nodes))
      ;; force a return when needed
      (when (and target tail? (not (return? this)))
	 ;; this is a function tail statement that misses a return
	 ;; statement and we add one.
	 ;; we could do better here, instead of transformaing
	 ;;    (if test (return XXX))
	 ;; into
	 ;;    (seq (if test (return XXXX)) (return (js-undefined)))
	 ;; we could tranform it into
	 ;;    (if test (return XXX) (return (js-undefined)))
	 (let ((ret (instantiate::J2SReturn
		       (loc loc)
		       (tail #t)
		       (expr (instantiate::J2SUndefined
				(loc loc))))))
	    (if (null? nodes)
		(set! nodes (list ret))
		(begin
		   (for-each (lambda (n) (untail-return! n target)) nodes)
		   (set-cdr! (last-pair nodes) (list ret)))))))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SLetBlock target tail? args)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d) (unreturn! d target #f args)) decls)
      (call-next-method)))
      
;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SSwitch ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SSwitch target tail? args)
   (with-access::J2SSwitch this (key cases)
      (set! key (unreturn! key target tail? args))
      (cond
	 ((not tail?)
	  (for-each (lambda (kase::J2SCase)
		       (with-access::J2SCase kase (expr body)
			  (set! expr (unreturn! expr target #f args))
			  (set! body (unreturn! body target #f args))))
	     cases))
	 ((pair? cases)
	  (let ((sesac (reverse cases)))
	     (for-each (lambda (kase::J2SCase)
			  (with-access::J2SCase kase (expr body)
			     (set! expr (unreturn! expr target #f args))
			     (set! body (unreturn! body target #f args))))
		(cdr sesac))
	     (with-access::J2SCase (car sesac) (expr body)
		(unless (isa? (car sesac) J2SDefault)
		   (set! expr (unreturn! expr target #f args)))
		(set! body (unreturn! body target #f args))))))
      this))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2STry...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2STry target tail? args)
   (with-access::J2STry this (body catch finally)
      (set! body (walk! body target tail? args))
      (set! catch (walk! catch target tail? args))
      (set! finally (walk! finally target #f args)))
   this)
   
;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SFun target tail? args)
   (with-access::J2SFun this (body)
      (set! body (walk! body this #t args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SCatch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SCatch target tail? args)
   (with-access::J2SCatch this (body)
      (set! body (walk! body target tail? args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SWhile target tail? args)
   (with-access::J2SWhile this (test body)
      (set! test (walk! test target #f args))
      (set! body (walk! body target #f args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SAssig ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SAssig target tail? args)
   (with-access::J2SAssig this (lhs rhs)
      (set! lhs (walk! lhs target #f args))
      (set! rhs (walk! rhs target #f args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SReturn target tail? args)
   (with-access::J2SReturn this (tail loc expr exit)
      (unless target
	 (if (config-get args :return-as-exit)
	     (begin
		(set! exit #t)
		(set! tail #t)
		(set! tail? #t)
		(set! expr (walk! expr target #f args)))
	     (syntax-error this "Illegal \"return\" statement")))
      (unless tail?
	 ;; mark the return as non-tail
	 (set! tail #f)
	 ;; mark the function as needing a bind-exit
	 (with-access::J2SFun target (need-bind-exit-return)
	    (set! need-bind-exit-return #t)))
      (set! expr (walk! expr target #f args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SLabel ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SLabel target tail? args)
   (with-access::J2SLabel this (body)
      (set! body (walk! body target tail? args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SIf target tail? args)
   (with-access::J2SIf this (test then else)
      (set! test (walk! test target #f args))
      (set! then (walk! then target tail? args))
      (set! else (walk! else target tail? args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SCond ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SCond target tail? args)
   (with-access::J2SCond this (test then else)
      (set! test (walk! test target #f args))
      (set! then (walk! then target tail? args))
      (set! else (walk! else target tail? args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SFor target tail? args)
   (with-access::J2SFor this (init test incr body)
      (set! init (walk! init target #f args))
      (set! test (walk! test target #f args))
      (set! incr (walk! incr target #f args))
      (set! body (walk! body target #f args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SForIn ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SForIn target tail? args)
   (with-access::J2SForIn this (lhs obj body)
      (set! lhs (walk! lhs target #f args))
      (set! obj (walk! obj target #f args))
      (set! body (walk! body target #f args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SLetInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SLetInit target tail? args)
   (with-access::J2SLetInit this (val)
      (set! val (walk! val target #f args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SLetOpt ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SLetOpt target tail? args)
   (with-access::J2SLetOpt this (val)
      (set! val (walk! val target #f args)))
   this)

;*---------------------------------------------------------------------*/
;*    untail-return! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (untail-return! this::J2SNode target)
   (default-walk! this target))

;*---------------------------------------------------------------------*/
;*    untail-return! ::J2SReturn ...                                   */
;*---------------------------------------------------------------------*/
(define-method (untail-return! this::J2SReturn target)
   (with-access::J2SReturn this (tail)
      (with-access::J2SFun target (need-bind-exit-return)
	 (set! need-bind-exit-return #t))
      (set! tail #f)
      this))

;*---------------------------------------------------------------------*/
;*    untail-return! ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (untail-return! this::J2SFun target)
   this)

;*---------------------------------------------------------------------*/
;*    return? ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SNode)
   #f)

;*---------------------------------------------------------------------*/
;*    return? ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (return? this::J2SFun)
   #f)

;*---------------------------------------------------------------------*/
;*    return? ::J2SSeq ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SSeq)
   (with-access::J2SSeq this (nodes)
      (when (pair? nodes)
	 (return? (car (last-pair nodes))))))

;*---------------------------------------------------------------------*/
;*    return? ::J2SIf ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SIf)
   (with-access::J2SIf this (then else)
      (and (return? then) (return? else))))

;*---------------------------------------------------------------------*/
;*    return? ::J2SStmtExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SStmtExpr)
   (with-access::J2SStmtExpr this (expr)
      (return? expr)))

;*---------------------------------------------------------------------*/
;*    return? ::J2SSwitch ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SSwitch)

   (define (empty-block? this)
      (with-access::J2SCase this (body)
	 (and (isa? body J2SBlock)
	      (with-access::J2SBlock body (nodes)
		 (null? nodes)))))
      
   (with-access::J2SSwitch this (cases)
      (let loop ((cases cases))
	 (cond
	    ((null? cases)
	     #t)
	    ((or (return? (car cases))
		 (and (empty-block? (car cases)) (pair? (cdr cases))))
	     (loop (cdr cases)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    return? ::J2SDo ...                                              */
;*    -------------------------------------------------------------    */
;*    Only the Do loop is considered as other loops are not            */
;*    guarantied to execute once.                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SDo)
   (with-access::J2SDo this (body)
      (return? body)))

;*---------------------------------------------------------------------*/
;*    unreturn? ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SReturn)
   #t)

