;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/hintnum.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  1 16:06:44 2018                          */
;*    Last change :  Wed Sep  1 09:59:02 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    hint typing of numerical values.                                 */
;*    -------------------------------------------------------------    */
;*    This optimization consists in propagating expression and         */
;*    declaration hints that will be used by the code generator.       */
;*    -------------------------------------------------------------    */
;*    Two top-down hints are propagated on unary op, binary op, and    */
;*    assignments.                                                     */
;*                                                                     */
;*      1- if only one argument of binary expression is typed/hinted,  */
;*         add its type/hint as a hint of the second argument.         */
;*      2- if the result is typed/hinted, propagate that hint to the   */
;*         arguments.                                                  */
;*      3- in a variable assignment (aliasing), if one of the vars     */
;*         is hinted/typed, the hint is propagated.                    */
;*                                                                     */
;*    This propagation iterates until the fix point is reached.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_hintnum

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage)

   (export j2s-hintnum-stage))

;*---------------------------------------------------------------------*/
;*    j2s-hintnum-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-hintnum-stage
   (instantiate::J2SStageProc
      (name "hintnum")
      (comment "Numerical hint typing")
      (optional :optim-hintnum)
      (proc j2s-hintnum)))

;*---------------------------------------------------------------------*/
;*    j2s-hintnum ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-hintnum this conf)
   
   (define j2s-verbose (config-get conf :verbose 0))
   
   (when (isa? this J2SProgram)
      (when (>=fx j2s-verbose 3) (display " " (current-error-port)))
      (let global-loop ()
	 (let ((fix (make-cell #t)))
	    (let loop ((i 1))
	       (when (>=fx j2s-verbose 3)
		  (fprintf (current-error-port) "~a." i)
		  (flush-output-port (current-error-port)))
	       (cell-set! fix #t)
	       (hintnum this #f fix)
	       (unless (cell-ref fix)
		  (loop (+fx i 1)))))
	 (when (>=fx j2s-verbose 3) (fprintf (current-error-port) "/"))
	 (let ((fix (make-cell #t)))
	    (let loop ((i 1))
	       (when (>=fx j2s-verbose 3)
		  (fprintf (current-error-port) "~a." i)
		  (flush-output-port (current-error-port)))
	       (cell-set! fix #t)
	       (propagate-types this fix)
	       (cond
		  ((not (cell-ref fix))
		   (loop (+fx i 1)))
		  ((>fx i 1)
		   (global-loop)))))))
   (propagate-real! this)
   this)

;*---------------------------------------------------------------------*/
;*    expr-hint ...                                                    */
;*---------------------------------------------------------------------*/
(define (expr-hint::pair-nil this::J2SExpr)
   (with-access::J2SExpr this (type hint)
      (cond
	 ((not (memq type '(number any integer real))) (list (cons type 100)))
	 ((pair? hint) hint)
	 (else '()))))

;*---------------------------------------------------------------------*/
;*    add-expr-hint! ...                                               */
;*---------------------------------------------------------------------*/
(define (add-expr-hint! this::J2SExpr newhint propagate::bool fix)
   
   (define (add-hint!::pair-nil hint::pair-nil newhint::pair-nil)
      (if (null? hint)
	  (begin
	     (cell-set! fix #f)
	     newhint)
	  (let loop ((hint hint)
		     (newhint newhint))
	     (if (null? newhint)
		 hint
		 (let* ((h (car newhint))
			(o (assq (car h) hint)))
		    (let ((nh (cond
				 ((not (pair? o))
				  (cell-set! fix #f)
				  (cons h hint))
				 ((<fx (cdr o) 0)
				  hint)
				 ((<fx (cdr h) 0)
				  (if (>=fx (cdr o) 0)
				      (begin
					 (cell-set! fix #f)
					 (cons h (remq o hint)))
				      hint))
				 ((<fx (cdr o) (cdr h))
				  (cell-set! fix #f)
				  (cons h (remq o hint)))
				 (else
				  hint))))
		       (loop nh (cdr newhint))))))))
   
   (when (pair? newhint)
      (with-access::J2SExpr this (hint)
	 (set! hint (add-hint! hint newhint)))
      (when (and propagate (isa? this J2SRef))
	 (with-access::J2SRef this (decl)
	    (let loop ((decl decl))
	       (with-access::J2SDecl decl (hint id loc %info)
		  (unless (isa? decl J2SThis)
		     (set! hint (add-hint! hint newhint))
		     (when (and (pair? %info) (eq? (car %info) 'hintnum-alias))
			(loop (cdr %info))))))))))

;*---------------------------------------------------------------------*/
;*    union-hint! ...                                                  */
;*---------------------------------------------------------------------*/
(define (union-hint! x y)
   (for-each (lambda (x)
		(let ((c (assq (car x) y)))
		   (cond
		      ((not (pair? c))
		       (set! y (cons x y)))
		      ((<fx (cdr x) 0)
		       (set-cdr! c (cdr x)))
		      ((>=fx (cdr c) 0)
		       (set-cdr! c (max (cdr x) (cdr c)))))))
      x)
   y)

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SNode assig::bool fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SReturn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SReturn assig::bool fix::cell)
   (with-access::J2SReturn this (from expr)
      (when (isa? from J2SBindExit)
	 (with-access::J2SBindExit from (hint)
	    (add-expr-hint! expr hint #t fix)))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    hintnum-binary ...                                               */
;*---------------------------------------------------------------------*/
(define (hintnum-binary this op lhs rhs fix)
   (when (memq (j2s-type lhs) '(any number))
      (add-expr-hint! lhs (expr-hint this) #t fix))
   (when (memq (j2s-type rhs) '(any number))
      (add-expr-hint! rhs (expr-hint this) #t fix))
   (case op
      ((+ ++)
       (when (memq (j2s-type lhs) '(any number))
	  (let ((hint (union-hint! (expr-hint this) (expr-hint rhs))))
	     (add-expr-hint! lhs hint #t fix)
	     (add-expr-hint! this hint #f fix))))
      ((-)
       (unless (eq? (j2s-type lhs) 'any)
	  (unhint-string-ref lhs fix))
       (unless (eq? (j2s-type rhs) 'any)
	  (unhint-string-ref rhs fix))
       (when (memq (j2s-type lhs) '(any number))
	  (let ((hint (union-hint! (expr-hint this) (expr-hint rhs))))
	     (unless (eq? (j2s-type lhs) 'any)
		(add-expr-hint! lhs (cons `(string . ,(minvalfx)) hint) #t fix))
	     (add-expr-hint! this hint #f fix)))
       (when (memq (j2s-type rhs) '(any number))
	  (let ((hint (union-hint! (expr-hint this) (expr-hint lhs))))
	     (unless (eq? (j2s-type rhs) 'any)
		(add-expr-hint! rhs (cons `(string . ,(minvalfx)) hint) #t fix))
	     (add-expr-hint! this hint #f fix))))
      ((-- *)
       (unhint-string-ref lhs fix)
       (unhint-string-ref rhs fix)
       (when (memq (j2s-type lhs) '(any number))
	  (let ((hint (union-hint! (expr-hint this) (expr-hint rhs))))
	     (add-expr-hint! lhs (cons `(string . ,(minvalfx)) hint) #t fix)
	     (add-expr-hint! this hint #f fix)))
       (when (memq (j2s-type rhs) '(any number))
	  (let ((hint (union-hint! (expr-hint this) (expr-hint lhs))))
	     (add-expr-hint! rhs (cons `(string . ,(minvalfx)) hint) #t fix)
	     (add-expr-hint! this hint #f fix))))
      ((/ %)
       (unhint-string-ref lhs fix)
       (unhint-string-ref rhs fix)
       (when (memq (j2s-type lhs) '(any number))
	  (let* ((hint (union-hint! (expr-hint this) (expr-hint rhs)))
		 (noreal (filter (lambda (c) (not (eq? (car c) 'real))) hint)))
	     (add-expr-hint! lhs (cons `(string . ,(minvalfx)) noreal) #t fix)
	     (add-expr-hint! this hint #f fix)))
       (when (memq (j2s-type rhs) '(any number))
	  (let* ((hint (union-hint! (expr-hint this) (expr-hint lhs)))
		 (noreal (filter (lambda (c) (not (eq? (car c) 'real))) hint)))
	     (add-expr-hint! rhs (cons `(string . ,(minvalfx)) noreal) #t fix)
	     (add-expr-hint! this hint #f fix))))
      ((< > <= >= == === != !==)
       (when (memq (j2s-type lhs) '(any number))
	  (unless (eq? (j2s-type lhs) 'any)
	     (unhint-string-ref lhs fix))
	  (let ((hint (expr-hint rhs)))
	     (add-expr-hint! lhs hint #t fix)))
       (when (memq (j2s-type rhs) '(any number))
	  (unless (eq? (j2s-type rhs) 'any)
	     (unhint-string-ref rhs fix))
	  (let ((hint (expr-hint lhs)))
	     (add-expr-hint! rhs hint #t fix))))
      ((>> >>> << BIT_OR & ^)
       (unhint-string-ref lhs fix)
       (unhint-string-ref rhs fix))))

;*---------------------------------------------------------------------*/
;*    unhint-string-ref ...                                            */
;*---------------------------------------------------------------------*/
(define (unhint-string-ref expr fix)
   (when (isa? expr J2SAccess)
      (with-access::J2SAccess expr (obj)
	 (add-expr-hint! obj (list `(string . ,(minvalfx))) #t fix))))
      
;*---------------------------------------------------------------------*/
;*    hintnum ::J2SBinary ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SBinary assig fix::cell)
   (call-default-walker)
   (with-access::J2SBinary this (lhs rhs op loc hint)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SUnary ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SUnary assig fix::cell)
   (call-default-walker)
   (with-access::J2SUnary this (expr)
      (when (memq (j2s-type expr) '(any number))
	 (add-expr-hint! expr (expr-hint this) #t fix))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SParen ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SParen assig fix::cell)
   (call-default-walker)
   (with-access::J2SParen this (expr)
      (when (memq (j2s-type expr) '(any number))
	 (add-expr-hint! expr (expr-hint this) #t fix))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SAssigOp ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SAssigOp assig fix::cell)
   (with-access::J2SAssigOp this (op lhs rhs)
      (call-default-walker)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SPostfix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SPostfix assig fix::cell)
   (with-access::J2SPostfix this (op lhs rhs)
      (call-default-walker)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SPrefix ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SPrefix assig fix::cell)
   (with-access::J2SPrefix this (op lhs rhs)
      (call-default-walker)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SRef ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SRef assig fix::cell)
   (unless assig
      (with-access::J2SRef this (decl loc)
	 (with-access::J2SDecl decl (hint (loc2 loc))
	    (add-expr-hint! this hint #f fix)))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SDeclInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SDeclInit assig fix::cell)
   (call-default-walker)
   (with-access::J2SDeclInit this (hint val vtype loc %info)
      (when (isa? val J2SRef)
	 (with-access::J2SRef val (decl)
	    (set! %info (cons 'hintnum-alias decl))))
      (unless (isa? this J2SDeclFun)
	 (add-expr-hint! val hint #f fix)
	 (when (and (is-hint? val 'real) (memq vtype '(integer number)))
	    (set! vtype 'real)
	    (when (isa? val J2SUndefined)
	       (with-access::J2SUndefined val (loc)
	       (set! val (J2SNumber 0.0))))))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SAssig ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SAssig assig fix::cell)
   (with-access::J2SAssig this (lhs rhs)
      (hintnum lhs #t fix)
      (hintnum rhs #f fix)
      (with-access::J2SAssig this (lhs rhs)
	 (cond
	    ((is-hint? lhs 'real)
	     (add-expr-hint! rhs (expr-hint lhs) #f fix))
	    ((is-hint? rhs 'real)
	     (add-expr-hint! this (expr-hint rhs) #f fix)
	     (when (isa? lhs J2SRef)
		(add-expr-hint! lhs (expr-hint rhs) #t fix)))
	    ((eq? (j2s-type rhs) 'real)
	     (add-expr-hint! this '((real . 20)) #f fix)
	     (when (isa? lhs J2SRef)
		(add-expr-hint! lhs '((real . 20)) #t fix)))))))

;*---------------------------------------------------------------------*/
;*    hinthum ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SCall assig fix::cell)
   
   (define (math-call? fun args)
      (when (and (pair? args) (isa? fun J2SAccess))
	 (with-access::J2SAccess fun (obj field)
	    (when (and (is-builtin-ref? obj 'Math) (isa? field J2SString))
	       (with-access::J2SString field (val)
		  (member val '("floor" "round" "ceil")))))))

   (define (math-callfl? fun args)
      (when (and (pair? args) (isa? fun J2SAccess))
	 (with-access::J2SAccess fun (obj field)
	    (when (and (is-builtin-ref? obj 'Math) (isa? field J2SString))
	       (with-access::J2SString field (val)
		  (member val '("asin" "sin" "acos" "cos" "sqrt" "log")))))))
   
   (call-default-walker)
   (with-access::J2SCall this (fun args)
      (cond
	 ((math-callfl? fun args)
	  (add-expr-hint! (car args) '((real . 40)) #f fix)
	  (add-expr-hint! this '((real . 40)) #f fix))
	 ((math-call? fun args)
	  (add-expr-hint! (car args) '((real . 40)) #f fix)))))

;*---------------------------------------------------------------------*/
;*    type<? ...                                                       */
;*---------------------------------------------------------------------*/
(define (type<? t1 t2)
   (unless (eq? t1 t2)
      (eq? t1 'real)))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SNode fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SRef fix::cell)
   (with-access::J2SRef this (decl type)
      (with-access::J2SDecl decl (vtype)
	 (when (type<? vtype type)
	    (set! type vtype)
	    (cell-set! fix #f)))))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SBinary ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SBinary fix::cell)
   (call-default-walker)
   (with-access::J2SBinary this (op lhs rhs type loc)
      (when (memq op '(- -- / * + ++))
	 (when (or (eq? (j2s-vtype lhs) 'real) (eq? (j2s-vtype rhs) 'real))
	    (when (memq (j2s-vtype lhs) '(number integer))
	       (set! lhs (J2SCast 'real lhs)))
	    (when (memq (j2s-vtype rhs) '(number integer))
	       (set! rhs (J2SCast 'real rhs)))
	    (when (memq type '(number integer))
	       (set! type 'real)
	       (cell-set! fix #f))))))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SPrefix ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SPrefix fix::cell)
   (call-default-walker)
   (with-access::J2SPrefix this (rhs type)
      (when (eq? (j2s-type rhs) 'real)
	 (when (memq type '(number integer))
	    (set! type 'real)
	    (cell-set! fix #f)))))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SPostfix ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SPostfix fix::cell)
   (call-default-walker)
   (with-access::J2SPostfix this (rhs type)
      (when (eq? (j2s-type rhs) 'real)
	 (when (memq type '(number integer))
	    (set! type 'real)
	    (cell-set! fix #f)))))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SUnary ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SUnary fix::cell)
   (call-default-walker)
   (with-access::J2SUnary this (op expr type)
      (when (memq op '(- +))
	 (when (eq? (j2s-type expr) 'real)
	    (when (memq type '(number integer))
	       (set! type 'real)
	       (cell-set! fix #f))))))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SParen ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SParen fix::cell)
   (call-default-walker)
   (with-access::J2SParen this (expr type loc)
      (when (eq? (j2s-type expr) 'real)
	 (when (memq type '(number integer))
	    (set! type 'real)
	    (cell-set! fix #f)))))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SDeclInit ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SDeclInit fix::cell)
   (call-default-walker)
   (with-access::J2SDeclInit this (val vtype)
      (when (and (memq vtype '(integer number)) (eq? (j2s-vtype val) 'real))
	 (set! vtype 'real)
	 (cell-set! fix #f))))

;*---------------------------------------------------------------------*/
;*    propagate-real! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-real! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propagate-real! ::J2SExpr ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-real! this::J2SExpr)
   (call-default-walker)
   (with-access::J2SExpr this (type loc)
      (if (and (is-hint? this 'real) (not (eq? type 'real))
	       (memq type '(number integer)))
	  (as-real! this)
	  this)))

;*---------------------------------------------------------------------*/
;*    propagate-real! ::J2SParen ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-real! this::J2SParen)
   (call-default-walker)
   (with-access::J2SParen this (expr type loc)
      (when (eq? (j2s-type expr) 'real)
	 (when (memq type '(number integer))
	    (set! type 'real))))
   this)
   
;*---------------------------------------------------------------------*/
;*    propagate-real! ::J2SCast ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-real! this::J2SCast)
   (with-access::J2SCast this (type)
      (when (and (is-hint? this 'real) (not (eq? type 'real)))
	 (set! type 'real)))
   this)

;*---------------------------------------------------------------------*/
;*    as-real! ...                                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (as-real! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    as-real! ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (as-real! this::J2SExpr)
   (if (eq? (j2s-vtype this) 'real)
       this
       (with-access::J2SExpr this (loc)
	  (J2SCast 'real this))))

;*---------------------------------------------------------------------*/
;*    as-real! ::J2SBinary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (as-real! this::J2SBinary)
   (call-default-walker)
   (with-access::J2SBinary this (type)
      (set! type 'real))
   this)
   
;*---------------------------------------------------------------------*/
;*    as-real! ::J2SUnary ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (as-real! this::J2SUnary)
   (call-default-walker)
   (with-access::J2SUnary this (type)
      (set! type 'real))
   this)
   
;*---------------------------------------------------------------------*/
;*    as-real! ::J2SCast ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (as-real! this::J2SCast)
   (with-access::J2SCast this (type)
      (set! type 'real)
      this))

;*---------------------------------------------------------------------*/
;*    as-real! ::J2SParen ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (as-real! this::J2SParen)
   (with-access::J2SParen this (expr type loc)
      (if (eq? (j2s-type expr) 'real)
	  (begin
	     (set! type 'real)
	     this)
	  (J2SCast 'real this))))
