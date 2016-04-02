;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/return.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Fri Mar 25 07:44:17 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
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
      (for-each (lambda (o) (unreturn! o #f #t #f args)) headers)
      (for-each (lambda (o) (unreturn! o #f #t #f args)) decls)
      (for-each (lambda (o) (unreturn! o #f #t #f args)) nodes)
      (set! headers (trim-nop headers))
      (set! decls (trim-nop decls))
      (set! nodes (trim-nop nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SNode target tail? in-handler args)
   (default-walk! this target tail? in-handler args))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2STry ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2STry target tail? in-handler args)
   (with-access::J2STry this (body catch finally)
      (unreturn! body target (and tail? (isa? finally J2SNop)) #t args)
      (unreturn! catch target (and tail? (isa? finally J2SNop)) #f args)
      (unreturn! finally target tail? in-handler args))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SSeq ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SSeq target tail? in-handler args)
   (with-access::J2SSeq this (nodes loc)
      ;; remove all useful nop (for readability)
      (set! nodes (trim-nop nodes))
      (let loop ((n nodes))
	 (when (pair? n)
	    (let ((t? (and tail? (null? (cdr n)))))
	       (set-car! n (walk! (car n) target t? in-handler args))
	       (loop (cdr n)))))
      ;; force a return when needed
      (when (and target tail? (not (return? this in-handler)))
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
(define-walk-method (unreturn! this::J2SLetBlock target tail? in-handler args)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d) (unreturn! d target #f in-handler args)) decls)
      (call-next-method)))
      
;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SSwitch ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SSwitch target tail? in-handler args)
   (with-access::J2SSwitch this (key cases)
      (set! key (unreturn! key target tail? in-handler args))
      (cond
	 ((not tail?)
	  (for-each (lambda (kase::J2SCase)
		       ;; compute the cascade
		       (let ((c (return? kase in-handler)))
			  (with-access::J2SCase kase (expr body)
			     (set! expr (unreturn! expr target #f in-handler args))
			     (set! body (unreturn! body target #f in-handler args)))))
	     cases))
	 ((pair? cases)
	  (let loop ((cases cases))
	     (cond
		((null? (cdr cases))
		 (with-access::J2SCase (car cases) (expr body)
		    (set! expr (unreturn! expr target #f in-handler args))
		    (set! body (unreturn! body target tail? in-handler args))))
		(else
		 ;; compute the cascade
		 (let ((c (return? (car cases) in-handler)))
		    (with-access::J2SCase (car cases) (expr body cascade)
		       (set! expr (unreturn! expr target #f in-handler args))
		       (set! body (unreturn! body target (and tail? (not cascade)) in-handler args))
		       (loop (cdr cases)))))))))
      this))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2STry...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2STry target tail? in-handler args)
   (with-access::J2STry this (body catch finally)
      (set! body (walk! body target tail? in-handler args))
      (set! catch (walk! catch target tail? in-handler args))
      (set! finally (walk! finally target #f in-handler args)))
   this)
   
;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SFun target tail? in-handler args)
   (with-access::J2SFun this (body)
      (set! body (walk! body this #t in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SCatch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SCatch target tail? in-handler args)
   (with-access::J2SCatch this (body)
      (set! body (walk! body target tail? in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SWhile target tail? in-handler args)
   (with-access::J2SWhile this (test body)
      (set! test (walk! test target #f in-handler args))
      (set! body (walk! body target #f in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SAssig ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SAssig target tail? in-handler args)
   (with-access::J2SAssig this (lhs rhs)
      (set! lhs (walk! lhs target #f in-handler args))
      (set! rhs (walk! rhs target #f in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SReturn target tail? in-handler args)
   (with-access::J2SReturn this (tail loc expr exit)
      (unless target
	 (if (config-get args :return-as-exit)
	     (begin
		(set! exit #t)
		(set! tail #t)
		(set! tail? #t)
		(set! expr (walk! expr target #f in-handler args)))
	     (syntax-error this "Illegal \"return\" statement")))
      (unless tail?
	 ;; mark the return as non-tail
	 (set! tail #f)
	 ;; mark the function as needing a bind-exit
	 (with-access::J2SFun target (need-bind-exit-return)
	    (set! need-bind-exit-return #t)))
      (set! expr (walk! expr target #f in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SLabel ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SLabel target tail? in-handler args)
   (with-access::J2SLabel this (body)
      (set! body (walk! body target tail? in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SIf target tail? in-handler args)
   (with-access::J2SIf this (test then else)
      (set! test (walk! test target #f in-handler args))
      (set! then (walk! then target tail? in-handler args))
      (set! else (walk! else target tail? in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SCond ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SCond target tail? in-handler args)
   (with-access::J2SCond this (test then else)
      (set! test (walk! test target #f in-handler args))
      (set! then (walk! then target tail? in-handler args))
      (set! else (walk! else target tail? in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SFor target tail? in-handler args)
   (with-access::J2SFor this (init test incr body)
      (set! init (walk! init target #f in-handler args))
      (set! test (walk! test target #f in-handler args))
      (set! incr (walk! incr target #f in-handler args))
      (set! body (walk! body target #f in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SForIn ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SForIn target tail? in-handler args)
   (with-access::J2SForIn this (lhs obj body)
      (set! lhs (walk! lhs target #f in-handler args))
      (set! obj (walk! obj target #f in-handler args))
      (set! body (walk! body target #f in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SDeclInit ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SDeclInit target tail? in-handler args)
   (with-access::J2SDeclInit this (val)
      (set! val (walk! val target #f in-handler args)))
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SYield ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SYield target tail? in-handler args)
   (with-access::J2SYield this (expr)
      (cond
	 ((not target)
	  (syntax-error this "Illegal \"yield\" statement"))
	 ((with-access::J2SFun target (generator) (not generator))
	  (syntax-error this "Illegal \"yield\" statement outside generator" )))
      (set! expr (walk! expr target #f in-handler args)))
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
;*    -------------------------------------------------------------    */
;*    Does a node _always_ return                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SNode in-handler)
   #f)

;*---------------------------------------------------------------------*/
;*    return? ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SFun in-handler)
   #f)

;*---------------------------------------------------------------------*/
;*    return? ::J2SSeq ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SSeq in-handler)
   (with-access::J2SSeq this (nodes)
      (when (pair? nodes)
	 (return? (car (last-pair nodes)) in-handler))))

;*---------------------------------------------------------------------*/
;*    return? ::J2SIf ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SIf in-handler)
   (with-access::J2SIf this (then else)
      (and (return? then in-handler) (return? else in-handler))))

;*---------------------------------------------------------------------*/
;*    return? ::J2SStmtExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SStmtExpr in-handler)
   (with-access::J2SStmtExpr this (expr)
      (return? expr in-handler)))

;*---------------------------------------------------------------------*/
;*    return? ::J2SSwitch ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SSwitch in-handler)
   
   (define (empty-block? this)
      (with-access::J2SCase this (body)
	 (and (isa? body J2SBlock)
	      (with-access::J2SBlock body (nodes)
		 (null? nodes)))))
   
   (with-access::J2SSwitch this (cases)
      (let loop ((cases cases)
		 (hasdefault #f))
	 (cond
	    ((null? cases)
	     hasdefault)
	    ((return? (car cases) in-handler)
	     (loop (cdr cases) (or hasdefault (isa? (car cases) J2SDefault))))
	    ((empty-block? (car cases))
	     (loop (cdr cases) hasdefault))
	    (else
	     (for-each (lambda (c)
			  (with-access::J2SCase c (cascade body)
			     (set! cascade
				(not (return-or-break? body in-handler)))))
		cases)
	     #f)))))

;*---------------------------------------------------------------------*/
;*    return? ::J2SCase ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SCase in-handler)
   (with-access::J2SCase this (expr body cascade)
      (let ((ret (and (not (return? expr in-handler))
		      (return? body in-handler))))
	 (set! cascade (not ret))
	 ret)))

;*---------------------------------------------------------------------*/
;*    return? ::J2SDefault ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SDefault in-handler)
   (with-access::J2SCase this (expr body cascade)
      (let ((ret (and (not (return? expr in-handler))
		      (return? body in-handler))))
	 (set! cascade (not ret))
	 ret)))

;*---------------------------------------------------------------------*/
;*    return? ::J2SDo ...                                              */
;*    -------------------------------------------------------------    */
;*    Only the Do loop is considered as other loops are not            */
;*    guarantied to execute once.                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SDo in-handler)
   (with-access::J2SDo this (body)
      (return? body in-handler)))

;*---------------------------------------------------------------------*/
;*    return? ::J2SReturn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SReturn in-handler)
   #t)

;*---------------------------------------------------------------------*/
;*    return? ::J2SThrow ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2SThrow in-handler)
   (not in-handler))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2STry ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (return? this::J2STry in-handler)
   (with-access::J2STry this (body)
      (return? body #t)))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SNode ...                                   */
;*    -------------------------------------------------------------    */
;*    Does a node always breaks and returns.                           */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SNode in-handler)
   #f)

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SSeq ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SSeq in-handler)
   (with-access::J2SSeq this (nodes)
      (when (pair? nodes)
	 (return-or-break? (car (last-pair nodes)) in-handler))))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SIf ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SIf in-handler)
   (with-access::J2SIf this (then else)
      (and (return-or-break? then in-handler)
	   (return-or-break? else in-handler))))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SStmtExpr ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SStmtExpr in-handler)
   (with-access::J2SStmtExpr this (expr)
      (return-or-break? expr in-handler)))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SSwitch ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SSwitch in-handler)
   
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
	    ((return-or-break? (car cases) in-handler)
	     (loop (cdr cases)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SCase ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SCase in-handler)
   (with-access::J2SCase this (expr body)
      (and (not (return-or-break? expr in-handler))
	   (return-or-break? body in-handler))))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SDo ...                                     */
;*    -------------------------------------------------------------    */
;*    Only the Do loop is considered as other loops are not            */
;*    guarantied to execute once.                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SDo in-handler)
   (with-access::J2SDo this (body)
      (return-or-break? body in-handler)))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SReturn ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SReturn in-handler)
   #t)

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2SReturn ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2SBreak in-handler)
   (with-access::J2SBreak this (target)
      (not target)))

;*---------------------------------------------------------------------*/
;*    return-or-break? ::J2STry ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (return-or-break? this::J2STry in-handler)
   (with-access::J2STry this (body)
      (return-or-break? body #t)))
