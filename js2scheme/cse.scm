;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/cse.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep 10 10:07:35 2020                          */
;*    Last change :  Fri Apr  9 11:37:37 2021 (serrano)                */
;*    Copyright   :  2020-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Common subexpressions elimination optimization                   */
;*    -------------------------------------------------------------    */
;*    This optimization removes common subexpressions inside blocks    */
;*    for expression that only contains literals, global immutable     */
;*    variables or local variable, unary, binary, or parenthesis       */
;*    expressions. Assigments to local variables invalidate the        */
;*    subexpressions candidates that depend on the variable.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_cse

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-cse-stage))

;*---------------------------------------------------------------------*/
;*    j2s-cse-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-cse-stage
   (instantiate::J2SStageProc
      (name "cse")
      (comment "Common Subexpression Elimination")
      (proc j2s-cse!)
      (optional :optim-cse)))

;*---------------------------------------------------------------------*/
;*    j2s-cse! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-cse! this conf)
   (when (isa? this J2SProgram)
      ;; mark all the possible reduction, and replace CS with var references
      (cse! this (bag '()) #f conf)
      ;; insert the temporaries declarations and assignments
      (temp! this))
   this)

;*---------------------------------------------------------------------*/
;*    reduction-benefit-threshold ...                                  */
;*---------------------------------------------------------------------*/
(define (reduction-benefit-threshold) 6)

;*---------------------------------------------------------------------*/
;*    ce ...                                                           */
;*---------------------------------------------------------------------*/
(define-struct ce loc expr decls decl block)

;*---------------------------------------------------------------------*/
;*    bag                                                              */
;*---------------------------------------------------------------------*/
(define-struct bag ces)

;*---------------------------------------------------------------------*/
;*    reducible ::J2SExpr ...                                          */
;*    -------------------------------------------------------------    */
;*    This function return -1 is the expression cannot be reduced.     */
;*    If it can be recuded, the function returns an integer standing   */
;*    for the reduction benefit.                                       */
;*---------------------------------------------------------------------*/
(define-generic (reducible::long this::J2SExpr)
   -1)

;*---------------------------------------------------------------------*/
;*    reducible ::J2SLiteral ...                                       */
;*---------------------------------------------------------------------*/
(define-method (reducible::long this::J2SLiteral)
   0)

;*---------------------------------------------------------------------*/
;*    reducible ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (reducible this::J2SRef)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (scope escape)
	 (cond
	    ((memq scope '(local letblock)) 0)
	    ((not (decl-usage-has? decl '(assig))) 0)
	    (escape 0)
	    (else -1)))))

;*---------------------------------------------------------------------*/
;*    reducible ::J2SParen ...                                         */
;*---------------------------------------------------------------------*/
(define-method (reducible this::J2SParen)
   (with-access::J2SParen this (expr)
      (reducible expr)))

;*---------------------------------------------------------------------*/
;*    reducible ::J2SUnary ...                                         */
;*---------------------------------------------------------------------*/
(define-method (reducible this::J2SUnary)
   (with-access::J2SUnary this (expr op)
      (let ((r (reducible expr)))
	 (if (>=fx r 0) (+fx r 1) -1))))

;*---------------------------------------------------------------------*/
;*    reducible ::J2SBinary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (reducible this::J2SBinary)
   (with-access::J2SBinary this (lhs rhs type op)
      (let ((l (reducible lhs))
	    (r (reducible rhs)))
	 (if (and (>=fx l 0) (>=fx r 0))
	     (+fx (if (eq? type 'string) 10 1) (+fx l r))
	     -1))))
   
;*---------------------------------------------------------------------*/
;*    cse* ...                                                         */
;*---------------------------------------------------------------------*/
(define (cse* nodes bag block conf)
   (let loop ((nodes nodes))
      (if (null? nodes)
	  '()
	  (cons (cse! (car nodes) bag block conf) (loop (cdr nodes))))))

;*---------------------------------------------------------------------*/
;*    cse! ::J2SNode ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SNode bag::struct block conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cse! ::J2SSeq ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SSeq bag block conf)
   (with-access::J2SSeq this (nodes %info)
      (set! %info '())
      (set! nodes (cse* nodes bag block conf))
      this))
      
;*---------------------------------------------------------------------*/
;*    cse! ::J2SBlock ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SBlock bag block conf)
   (with-access::J2SSeq this (nodes %info)
      (set! %info '())
      (set! nodes (cse* nodes bag this conf))
      this))
      
;*---------------------------------------------------------------------*/
;*    cse! ::J2SLetBlock ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SLetBlock bag block conf)
   (with-access::J2SLetBlock this (nodes %info decls)
      (set! %info '())
      (set! decls (cse* decls bag this conf))
      (set! nodes (cse* nodes bag this conf))
      this))
      
;*---------------------------------------------------------------------*/
;*    cse! ::J2SIf ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SIf bag block conf)
   (with-access::J2SIf this (test then else)
      (set! test (cse! test bag block conf))
      (let ((tbag (bag-copy bag))
	    (ebag (bag-copy bag)))
	 (set! then (cse! then tbag block conf))
	 (set! else (cse! else ebag block conf))
	 (bag-ces-set! bag (bag-join tbag ebag))
	 this)))

;*---------------------------------------------------------------------*/
;*    cse! ::J2SExpr ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SExpr bag block conf)
   (call-default-walker)
   (when (and block (>fx (reducible this) (reduction-benefit-threshold)))
      (bag-add-expr! bag this block))
   this)

;*---------------------------------------------------------------------*/
;*    cse-reduce ...                                                   */
;*---------------------------------------------------------------------*/
(define (cse-reduce this ce conf)
   (with-access::J2SExpr this (loc)
      (when (>=fx (config-get conf :verbose 0) 3)
	 (fprintf (current-error-port) " [~a:~a]"
	    (basename (cadr loc)) (caddr loc)))
      (J2SRef (ce-get-decl ce))))

;*---------------------------------------------------------------------*/
;*    cse! ::J2SBinary ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SBinary bag block conf)
   (with-access::J2SBinary this (loc left right)
      (let ((ce (bag-find-expr bag this)))
	 (if ce
	     (cse-reduce this ce conf)
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    cse! ::J2SUnary ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SUnary bag block conf)
   (with-access::J2SUnary this (loc expr)
      (let ((ce (bag-find-expr bag this)))
	 (if ce
	     (cse-reduce this ce conf)
	     (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    cse! ::J2SParen ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SParen bag block conf)
   (with-access::J2SParen this (loc expr)
      (let ((ce (bag-find-expr bag this)))
	 (if ce
	     (cse-reduce this ce conf)
	     (call-next-method)))))
	    
;*---------------------------------------------------------------------*/
;*    cse! ::J2Assig ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (cse! this::J2SAssig bag block conf)
   (with-access::J2SAssig this (lhs rhs)
      (call-default-walker)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (bag-invalidate! bag decl)))
      this))

;*---------------------------------------------------------------------*/
;*    temp! ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (temp! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    temp! ::J2SBlock ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (temp! this::J2SBlock)
   (with-access::J2SBlock this (nodes %info loc endloc)
      (if (pair? %info)
	  (set! nodes (list (J2SLetBlock* %info (map temp! nodes))))
	  (set! nodes (map temp! nodes)))
      this))

;*---------------------------------------------------------------------*/
;*    temp! ::J2SLetBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (temp! this::J2SLetBlock)
   (with-access::J2SLetBlock this (nodes %info decls)
      (set! decls (append %info (map temp! decls)))
      (set! nodes (map temp! nodes))
      this))

;*---------------------------------------------------------------------*/
;*    temp! ::J2SExpr ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (temp! this::J2SExpr)
   (with-access::J2SExpr this (%info loc)
      (if (and (ce? %info) (ce-decl %info))
	  (J2SSequence
	     (J2SAssig (J2SRef (ce-decl %info)) this)
	     (J2SRef (ce-decl %info)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    bag-join ...                                                     */
;*---------------------------------------------------------------------*/
(define (bag-join bag1 bag2)
   (let loop ((ces1 (bag-ces bag1))
	      (ces2 (bag-ces bag2))
	      (res '()))
      (cond
	 ((or (null? ces1) (null? ces2))
	  (reverse! res))
	 ((=fx (caddr (ce-loc (car ces1))) (caddr (ce-loc (car ces2))))
	  (loop (cdr ces1) (cdr ces2) (cons (car ces1) res)))
	 ((<fx (caddr (ce-loc (car ces1))) (caddr (ce-loc (car ces2))))
	  (loop (cdr ces1) ces2 res))
	 (else
	  (loop ces1 (cdr ces2) res)))))
	  
;*---------------------------------------------------------------------*/
;*    bag-invalidate! ...                                              */
;*    -------------------------------------------------------------    */
;*    Remove from bag all the CE that use DECL                         */
;*---------------------------------------------------------------------*/
(define (bag-invalidate! bag decl)
   (bag-ces-set! bag
      (filter! (lambda (ce) (not (memq decl (ce-decls ce)))) (bag-ces bag))))

;*---------------------------------------------------------------------*/
;*    bag-copy ...                                                     */
;*---------------------------------------------------------------------*/
(define (bag-copy b)
   (bag (list-copy (bag-ces b))))

;*---------------------------------------------------------------------*/
;*    bag-find-expr ...                                                */
;*---------------------------------------------------------------------*/
(define (bag-find-expr bag expr)
   (find (lambda (ce) (j2s-expr-equal? (ce-expr ce) expr)) (bag-ces bag)))

;*---------------------------------------------------------------------*/
;*    bag-add-expr! ...                                                */
;*    -------------------------------------------------------------    */
;*    Build a CE from the expression and insert it in the order CES    */
;*    list.                                                            */
;*---------------------------------------------------------------------*/
(define (bag-add-expr! bag expr block)
   (with-access::J2SExpr expr (loc)
      (let ((ces (bag-ces bag))
	    (ce (expr->ce expr block)))
	 (cond
	    ((null? ces)
	     (bag-ces-set! bag (list ce)))
	    ((<fx (caddr loc) (caddr (ce-loc (car ces))))
	     (bag-ces-set! bag (cons ce ces)))
	    (else
	     (let loop ((ces (cdr ces))
			(prev ces))
		(cond
		   ((null? ces)
		    (set-cdr! prev (list ce)))
		   ((<fx (caddr loc) (caddr (ce-loc (car ces))))
		    (set-cdr! prev (list ce)))
		   (else
		    (loop (cdr ces) ces)))))))))

;*---------------------------------------------------------------------*/
;*    unparen ...                                                      */
;*---------------------------------------------------------------------*/
(define (unparen expr)
   (if (isa? expr J2SParen)
       (with-access::J2SParen expr (expr) (unparen expr))
       expr))

;*---------------------------------------------------------------------*/
;*    j2s-expr-equal? ::J2SExpr ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (j2s-expr-equal? this::J2SExpr e)
   (tprint "eq " (typeof this) " " (typeof e))
   #f)

;*---------------------------------------------------------------------*/
;*    j2s-expr-equal? ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-expr-equal? this::J2SRef e)
   (let ((e (unparen e)))
      (when (eq? (object-class this) (object-class e))
	 (with-access::J2SRef this ((thisdecl decl))
	    (with-access::J2SRef e (decl)
	       (eq? thisdecl decl))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-expr-equal? ::J2SLiteralValue ...                            */
;*---------------------------------------------------------------------*/
(define-method (j2s-expr-equal? this::J2SLiteralValue e)
   (let ((e (unparen e)))
      (when (eq? (object-class this) (object-class e))
	 (with-access::J2SLiteralValue this ((thisval val))
	    (with-access::J2SLiteralValue e (val)
	       (equal? thisval val))))))

;*---------------------------------------------------------------------*/
;*    j2s-expr-equal? ::J2SUnary ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-expr-equal? this::J2SUnary e)
   (let ((e (unparen e)))
      (when (eq? (object-class this) (object-class e))
	 (with-access::J2SUnary this ((thisexpr expr))
	    (with-access::J2SUnary e (expr)
	       (j2s-expr-equal? thisexpr expr))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-expr-equal? ::J2SParen ...                                   */
;*---------------------------------------------------------------------*/
(define-method (j2s-expr-equal? this::J2SParen e)
   (j2s-expr-equal? (unparen this) (unparen e)))
   
;*---------------------------------------------------------------------*/
;*    j2s-expr-equal? ::J2SBinary ...                                  */
;*---------------------------------------------------------------------*/
(define-method (j2s-expr-equal? this::J2SBinary e)
   (let ((e (unparen e)))
      (when (eq? (object-class this) (object-class e))
	 (with-access::J2SBinary this ((thislhs lhs) (thisrhs rhs))
	    (with-access::J2SBinary e (lhs rhs)
	       (and (j2s-expr-equal? thislhs lhs)
		    (j2s-expr-equal? thisrhs rhs)))))))

;*---------------------------------------------------------------------*/
;*    j2s-expr-vars* ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-expr-vars* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-expr-vars ::J2SRef ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-expr-vars* this::J2SRef)
   (with-access::J2SRef this (decl)
      (list decl)))

;*---------------------------------------------------------------------*/
;*    expr->ce ...                                                     */
;*---------------------------------------------------------------------*/
(define (expr->ce this::J2SExpr block)
   (with-access::J2SExpr this (loc %info)
      (set! %info (ce loc this (j2s-expr-vars* this) #f block))
      %info))

;*---------------------------------------------------------------------*/
;*    ce-get-decl ...                                                  */
;*---------------------------------------------------------------------*/
(define (ce-get-decl ce)
   (if (isa? (ce-decl ce) J2SDecl)
       (ce-decl ce)
       (let ((expr (ce-expr ce)))
	  (with-access::J2SExpr expr (loc)
	     ;; MS CARE UTYPE
	     ;; (let ((decl (J2SLetOptVUtype (j2s-type expr) '(ref assig)
	     (let ((decl (J2SLetOptVtype (j2s-type expr) '(ref assig)
			    (gensym '%cse) (default-expr expr))))
		(ce-decl-set! ce decl)
		(with-access::J2SBlock (ce-block ce) (%info)
		   (set! %info (cons decl %info)))
		decl)))))

;*---------------------------------------------------------------------*/
;*    default-expr ...                                                 */
;*---------------------------------------------------------------------*/
(define (default-expr expr::J2SExpr)
   (with-access::J2SExpr expr (loc)
      (case (j2s-type expr)
	 ((string) (J2SString ""))
	 ((number integer) (J2SNumber/type (j2s-type expr) 0))
	 ((real) (J2SNumber/type (j2s-type expr) 0.0))
	 (else (J2SUndefined)))))
