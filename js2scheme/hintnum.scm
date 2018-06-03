;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/hintnum.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  1 16:06:44 2018                          */
;*    Last change :  Sun Jun  3 06:11:13 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    hint typing of numerical values.                                 */
;*    -------------------------------------------------------------    */
;*    This optimization consists in propagating expressions and        */
;*    declarations hints that will be used by the code generator.      */
;*    -------------------------------------------------------------    */
;*    Two top-down hints are propagated on unary op, binary op, and    */
;*    assignments.                                                     */
;*                                                                     */
;*      1- if only one argument of binary expression is typed/hinted,  */
;*         add its type/hint as a hint of the second argument.         */
;*      2- if the result is typed/hinted propagate that hint to the    */
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
      (when (>=fx j2s-verbose 4) (display " " (current-error-port)))
      (let ((fix (make-cell #t)))
	 (let loop ((i 1))
	    (when (>=fx j2s-verbose 4)
	       (fprintf (current-error-port) "~a." i)
	       (flush-output-port (current-error-port)))
	    (cell-set! fix #t)
	    (hintnum this fix)
	    (unless (cell-ref fix)
	       (loop (+fx i 1)))))
      (when (>=fx j2s-verbose 4) "/")
      (let ((fix (make-cell #t)))
	 (let loop ((i 1))
	    (when (>=fx j2s-verbose 4)
	       (fprintf (current-error-port) "~a." i)
	       (flush-output-port (current-error-port)))
	    (cell-set! fix #t)
	    (propagate-types this fix)
	    (unless (cell-ref fix)
	       (loop (+fx i 1))))))
   this)

;*---------------------------------------------------------------------*/
;*    expr-hint ...                                                    */
;*---------------------------------------------------------------------*/
(define (expr-hint::pair-nil this::J2SExpr)
   (with-access::J2SExpr this (type hint)
      (cond
	 ((not (memq type '(number any))) (list (cons type 100)))
	 ((pair? hint) hint)
	 (else '()))))

;*---------------------------------------------------------------------*/
;*    add-expr-hint! ...                                               */
;*---------------------------------------------------------------------*/
(define (add-expr-hint! this::J2SExpr newhint propagate::bool fix)
   
   (define (add-hint! hint newhint)
      (cond
	 ((null? hint)
	  (cell-set! fix #f)
	  newhint)
	 ((not (every (lambda (h) (pair? (assq (car h) hint))) newhint))
	  (cell-set! fix #f)
	  (for-each (lambda (h)
		       (let ((c (assq (car h) hint)))
			  (if (pair? c)
			      (when (<fx (cdr c) (cdr h))
				 (cell-set! fix #f)
				 (set-cdr! c (cdr h)))
			      (begin
				 (cell-set! fix #f)
				 (set! hint (cons h hint))))))
	     newhint)
	  hint)
	 (else
	  hint)))

   (when (pair? newhint)
      (with-access::J2SExpr this (hint)
	 (set! hint (add-hint! hint newhint)))
      (when (and propagate (isa? this J2SRef))
	 (with-access::J2SRef this (decl)
	    (with-access::J2SDecl decl (hint)
	       (set! hint (add-hint! hint newhint)))))))

;*---------------------------------------------------------------------*/
;*    union-hint! ...                                                  */
;*---------------------------------------------------------------------*/
(define (union-hint! x y)
   (for-each (lambda (x)
		(let ((c (assq (car x) y)))
		   (if (pair? c)
		       (set-cdr! c (max (cdr x) (cdr c)))
		       (set! y (cons x y)))))
      x)
   y)

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SNode fix::cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    hintnum-binary ...                                               */
;*---------------------------------------------------------------------*/
(define (hintnum-binary this op lhs rhs fix)
   (case op
      ((+ - * / %)
       (when (memq (j2s-type lhs) '(any number))
	  (let ((hint (union-hint! (expr-hint this) (expr-hint rhs))))
	     (add-expr-hint! lhs hint #t fix)))
       (when (memq (j2s-type rhs) '(any number))
	  (let ((hint (union-hint! (expr-hint this) (expr-hint lhs))))
	     (add-expr-hint! rhs hint #t fix))))
      ((< > <= >= == === != !==)
       (when (memq (j2s-type lhs) '(any number))
	  (let ((hint (expr-hint rhs)))
	     (add-expr-hint! lhs hint #t fix)))
       (when (memq (j2s-type rhs) '(any number))
	  (let ((hint (expr-hint lhs)))
	     (add-expr-hint! rhs hint #t fix))))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SBinary ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SBinary fix::cell)
   (call-default-walker)
   (with-access::J2SBinary this (lhs rhs op)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SUnary ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SUnary fix::cell)
   (call-default-walker)
   (with-access::J2SUnary this (expr)
      (when (memq (j2s-type expr) '(any number))
	 (add-expr-hint! expr (expr-hint this) #t fix))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SAssigOp ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SAssigOp fix::cell)
   (with-access::J2SAssigOp this (op lhs rhs)
      (call-default-walker)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SPostfix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SPostfix fix::cell)
   (with-access::J2SPostfix this (op lhs rhs)
      (call-default-walker)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SPrefix ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SPrefix fix::cell)
   (with-access::J2SPrefix this (op lhs rhs)
      (call-default-walker)
      (hintnum-binary this op lhs rhs fix)))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SRef ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SRef fix::cell)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (hint)
	 (add-expr-hint! this hint #f fix))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SDeclInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SDeclInit fix::cell)
   (call-default-walker)
   (with-access::J2SDeclInit this (hint val vtype itype)
      (unless (isa? this J2SDeclFun)
	 (add-expr-hint! val hint  #f fix)
	 (when (is-hint? val 'real)
	    (set! vtype 'real)
	    (set! itype 'real)))))

;*---------------------------------------------------------------------*/
;*    hintnum ::J2SAssig ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (hintnum this::J2SAssig fix::cell)
   (call-default-walker)
   (with-access::J2SAssig this (lhs rhs)
      (when (and (isa? lhs J2SRef) (isa? rhs J2SRef))
	 (cond
	    ((is-hint? lhs 'real)
	     (add-expr-hint! rhs (expr-hint lhs) #f fix))
	    ((is-hint? rhs 'real)
	     (add-expr-hint! lhs (expr-hint rhs) #f fix)
	     (add-expr-hint! this (expr-hint rhs) #f fix))))))

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
;*    propagate-types! ::J2SRef ...                                    */
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
   (call-next-method)
   (with-access::J2SBinary this (op lhs rhs type)
      (case op
	 ((- / *)
	  (when (or (eq? (j2s-vtype lhs) 'real) (eq? (j2s-vtype rhs) 'real))
	     (unless (eq? type 'real)
		(set! type 'real)
		(cell-set! fix #f))))
	 ((+)
	  (when (and (eq? (j2s-vtype lhs) 'real) (eq? (j2s-vtype rhs) 'real))
	     (unless (eq? type 'real)
		(set! type 'real)
		(cell-set! fix #f)))))))

;*---------------------------------------------------------------------*/
;*    propagate-types ::J2SUnary ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (propagate-types this::J2SUnary fix::cell)
   (call-next-method)
   (with-access::J2SUnary this (op expr type)
      (when (memq op '(- +))
	 (when (eq? (j2s-vtype expr) 'real)
	    (unless (eq? type 'real)
	       (set! type 'real)
	       (cell-set! fix #f))))))

