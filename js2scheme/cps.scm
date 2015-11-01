;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/cps.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Sun Nov  1 16:09:32 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript CPS transformation                                    */
;*    -------------------------------------------------------------    */
;*    This module implements the JavaScript CPS transformation needed  */
;*    to implement generators. Only generator function are modified.   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_cps

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

   (export j2s-cps-stage
	   (generic j2s-cps ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-cps-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-cps-stage
   (instantiate::J2SStageProc
      (name "cps")
      (comment "transform generator in CPS")
      (proc j2s-cps)))

;*---------------------------------------------------------------------*/
;*    j2s-cps ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (j2s-cps this args)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-cps ::J2SProgram ...                                         */
;*---------------------------------------------------------------------*/
(define-method (j2s-cps this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each (lambda (o) (cps-fun o)) headers)
      (for-each (lambda (o) (cps-fun o)) decls)
      (for-each (lambda (o) (cps-fun o)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    cps-fun ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (cps-fun this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cps-fun ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (cps-fun this::J2SFun)
   (with-access::J2SFun this (generator body)
      (when generator
	 (set! body (cps body (lambda (n::J2SNode) n))))
      this))

;*---------------------------------------------------------------------*/
;*    cps* ...                                                         */
;*---------------------------------------------------------------------*/
(define (cps* nodes::pair k)
   (let loop ((nodes nodes)
	      (knodes '()))
      (if (null? nodes)
	  (k (reverse! knodes))
	  (cps (car nodes)
	     (lambda (knode::J2SNode)
		(loop (cdr nodes)
		   (cons knode knodes)))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNode ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (cps this::J2SNode k)
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SLiteral ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLiteral k)
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNop ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SNop k)
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SParen ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SParen k)
   (with-access::J2SParen this (expr)
      (if (yield-expr? expr)
	  (cps expr
	     (lambda (kexpr::J2SNode)
		(k (duplicate::J2SParen this
		      (expr kexpr)))))
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SUnary ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SUnary k)
   (with-access::J2SUnary this (expr)
      (if (yield-expr? expr)
	  (cps expr
	     (lambda (kexpr::J2SNode)
		(k (duplicate::J2SUnary this
		      (expr kexpr)))))
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SBinary ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBinary k)
   (with-access::J2SBinary this (lhs rhs loc)
      (cond
	 ((yield-expr? lhs)
	  (cps lhs
	     (lambda (klhs::J2SNode)
		(cps (duplicate::J2SBinary this
			 (lhs klhs))
		   k))))
	 ((yield-expr? rhs)
	  (cps rhs
	     (lambda (krhs::J2SNode)
		(cps (duplicate::J2SBinary this
			 (rhs krhs))
		   k))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SYield ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SYield k)
   
   (define (make-yield-kont k id loc)
      (instantiate::J2SKont
	 (loc loc)
	 (param id)
	 (body (k (instantiate::J2SHopRef
		     (loc loc)
		     (id id))))))
   
   (with-access::J2SYield this (expr kont loc)
      (cps expr
	 (lambda (kexpr::J2SExpr)
	    (set! expr kexpr)
	    (set! kont (make-yield-kont k (gensym '%arg) loc))
	    this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SReturn ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SReturn k)
   (with-access::J2SReturn this (loc expr)
      (cps expr
	 (lambda (kexpr::J2SExpr)
	    (instantiate::J2SStmtExpr
	       (loc loc)
	       (expr (instantiate::J2SYield
			(loc loc)
			(done #t)
			(expr kexpr))))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSeq ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSeq k)
   (with-access::J2SSeq this (loc nodes)
      (let loop ((nodes nodes))
	 (cond
	    ((null? nodes)
	     (k this))
	    ((not (yield-expr? (car nodes)))
	     (loop (cdr nodes)))
	    (else
	     (let ((knodes (cdr nodes)))
		(set-cdr! nodes '())
		(set-car! nodes
		   (cps (car nodes)
		      (lambda (n::J2SNode)
			 (let ((kseq (duplicate::J2SSeq this
					(nodes (cons n knodes)))))
			    (cps kseq k)))))
		this))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCond ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCond k)
   (with-access::J2SCond this (loc test then else)
      (cond
	 ((yield-expr? test)
	  (cps test
	     (lambda (ktest::J2SNode)
		(cps (duplicate::J2SCond this
			(test ktest))
		   k))))
	 ((yield-expr? then)
	  (cps then
	     (lambda (kthen::J2SNode)
		(cps (duplicate::J2SCond this
			(then kthen))
		   k))))
	 ((yield-expr? else)
	  (cps else
	     (lambda (kthen::J2SNode)
		(cps (duplicate::J2SCond this
			(else else))
		   k))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SIf ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SIf k)
   (with-access::J2SIf this (loc test then else)
      (cond
	 ((yield-expr? test)
	  (cps test
	     (lambda (ktest::J2SNode)
		(cps (duplicate::J2SIf this
			(test ktest))
		   k))))
	 ((or (yield-expr? then) (yield-expr? else))
	  (let* ((name (gensym '%kif))
		 (kfun (instantiate::J2SFun
			  (name name)
			  (loc loc)
			  (params '())
			  (mode 'hopscript)
			  (body (instantiate::J2SBlock
				   (loc loc)
				   (endloc loc)
				   (nodes
				      (list (k (instantiate::J2SUndefined
						  (loc loc)))))))))
		 (decl (instantiate::J2SLetOpt
			  (usage '(call))
			  (loc loc)
			  (isconst #t)
			  (val kfun)
			  (id name)))
		 (callt (instantiate::J2SCall
			  (loc loc)
			  (fun (instantiate::J2SRef
				  (loc loc)
				  (decl decl)))
			  (args (list (instantiate::J2SUndefined
					 (loc loc))))))
		 (calle (instantiate::J2SCall
			  (loc loc)
			  (fun (instantiate::J2SRef
				  (loc loc)
				  (decl decl)))
			  (args (list (instantiate::J2SUndefined
					 (loc loc))))))
		 (kthen (instantiate::J2SBlock
			   (loc loc)
			   (endloc loc)
			   (nodes (list then callt))))
		 (kelse (if (isa? else J2SNop)
			    (with-access::J2SNop else (loc)
			       (instantiate::J2SStmtExpr
				  (loc loc)
				  (expr calle)))
			    (instantiate::J2SBlock
			       (loc loc)
			       (endloc loc)
			       (nodes (list else calle)))))
		 (kif (duplicate::J2SIf this
			 (then (cps kthen (lambda (n) n)))
			 (else (cps kelse (lambda (n) n))))))
	     (instantiate::J2SLetBlock
		(loc loc)
		(endloc loc)
		(decls (list decl))
		(nodes (list kif)))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SStmtExpr ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SStmtExpr k)
   (with-access::J2SStmtExpr this (expr)
      (if (yield-expr? expr)
	  (cps expr
	     (lambda (kexpr::J2SNode)
		(set! expr kexpr)
		(k this)))
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SAssig ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SAssig k)
   (with-access::J2SAssig this (lhs rhs)
      (cond
	 ((yield-expr? lhs)
	  (cps lhs
	     (lambda (klhs::J2SNode)
		(set! lhs klhs)
		(cps this k))))
	 ((yield-expr? rhs)
	  (cps rhs
	     (lambda (krhs::J2SNode)
		(set! rhs krhs)
		(cps this k))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCall ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCall k)
   (with-access::J2SCall this (fun args)
      (cond
	 ((yield-expr? fun)
	  (cps fun
	     (lambda (kfun::J2SNode)
		(set! fun kfun)
		(cps this k))))
	 ((any yield-expr? args)
	  (cps* args
	     (lambda (kargs::pair-nil)
		(set! args kargs)
		(cps this k))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSequence k)
   (with-access::J2SSequence this (exprs)
      (if (any yield-expr? exprs)
	  (cps* exprs
	     (lambda (kexprs::pair-nil)
		(set! exprs kexprs)
		(cps this k)))
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCond ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCond k)
   (with-access::J2SCond this (test then else)
      (cond
	 ((yield-expr? test)
	  (cps test
	     (lambda (ktest::J2SExpr)
		(set! test ktest)
		(cps this k))))
	 ((yield-expr? then)
	  (cps then
	     (lambda (kthen::J2SExpr)
		(set! then kthen)
		(cps this k))))
	 ((yield-expr? else)
	  (cps else
	     (lambda (kelse::J2SExpr)
		(set! else kelse)
		(cps this k))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    yield-expr? ...                                                  */
;*---------------------------------------------------------------------*/
(define (yield-expr? this::J2SNode)
   (let ((v (yield-expr* this)))
      (find (lambda (v) v) v)))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SNode ...                                        */
;*    -------------------------------------------------------------    */
;*    Returns #t iff a statement contains a YIELD. Otherwise           */
;*    returns #f.                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SYield ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SYield)
   (list this))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SReturn ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SReturn)
   (list this))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SFun)
   (list #f))





