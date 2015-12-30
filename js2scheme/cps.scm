;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/cps.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Wed Dec 30 16:30:02 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript CPS transformation                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#sec-14.4         */
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

   (static (final-class %J2STail::J2SReturn))

   (cond-expand
      (bigloo-debug
       (static (abstract-class Kont
		  (node::obj read-only)
		  (link::obj read-only)
		  (proc::procedure read-only)
		  (name::bstring read-only (default "")))
	       (class KontStmt::Kont)
	       (class KontExpr::Kont)
	       (class KontExpr*::Kont))))

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
;*    Small macro-based API for helping creating J2SNode               */
;*---------------------------------------------------------------------*/
(define-macro (J2SUndefined)
   `(instantiate::J2SUndefined
       (loc loc)))

(define-macro (J2SBool val)
   `(instantiate::J2SBool
       (loc loc)
       (val ,val)))

(define-macro (J2SNumber val)
   `(instantiate::J2SNumber
       (loc loc)
       (val ,val)))

(define-macro (J2SString val)
   `(instantiate::J2SString
       (loc loc)
       (val ,val)))

(define-macro (J2SNop)
   `(instantiate::J2SNop
       (loc loc)))

(define-macro (J2SPragma expr)
   `(instantiate::J2SPragma
       (loc loc)
       (expr ,expr)))

(define-macro (J2SParen expr)
   `(instantiate::J2SParen
       (loc loc)
       (expr ,expr)))

(define-macro (J2SBinary op lhs rhs)
   `(instantiate::J2SBinary
       (loc loc)
       (op ,op)
       (lhs ,lhs)
       (rhs ,rhs)))

(define-macro (J2SPostfix op lhs rhs)
   `(instantiate::J2SPostfix
       (loc loc)
       (op ,op)
       (lhs ,lhs)
       (rhs ,rhs)))

(define-macro (J2SCall fun . args)
   `(instantiate::J2SCall
       (loc loc)
       (fun ,fun)
       (args ,(if (pair? args) `(list ,(car args)) ''()))))

(define-macro (J2SAccess obj field)
   `(instantiate::J2SAccess
       (loc loc)
       (obj ,obj)
       (field ,field)))

(define-macro (J2SRef decl)
   `(instantiate::J2SRef
       (loc loc)
       (decl ,decl)))

(define-macro (J2SUnresolvedRef id)
   `(instantiate::J2SUnresolvedRef
       (loc loc)
       (id ,id)))

(define-macro (J2SHopRef id)
   `(instantiate::J2SHopRef
       (loc loc)
       (id ,id)))

(define-macro (J2SFun name params body)
   `(instantiate::J2SFun
       (loc loc)
       (mode 'hopscript)
       (name ,name)
       (params ,params)
       (body ,body)))

(define-macro (J2SBlock . nodes)
   `(instantiate::J2SBlock
       (loc loc)
       (endloc loc)
       (nodes ,(if (pair? nodes) `(list ,@nodes) ''()))))

(define-macro (J2SSeq . nodes)
   `(instantiate::J2SSeq
       (loc loc)
       (nodes ,(if (pair? nodes) `(list ,@nodes) ''()))))

(define-macro (J2SSeq* nodes)
   `(instantiate::J2SSeq
       (loc loc)
       (nodes ,nodes)))

(define-macro (J2SSequence . exprs)
   `(instantiate::J2SSequence
       (loc loc)
       (exprs ,(if (pair? exprs) `(list ,@exprs) ''()))))

(define-macro (J2SSequence* exprs)
   `(instantiate::J2SSequence
       (loc loc)
       (exprs ,exprs)))

(define-macro (J2SLetBlock decls . nodes)
   `(instantiate::J2SLetBlock
       (loc loc)
       (endloc loc)
       (decls ,decls)
       (nodes ,(if (pair? nodes) `(list ,@nodes) ''()))))

(define-macro (J2STry body catch . finally)
   `(instantiate::J2STry
       (loc loc)
       (body ,body)
       (catch ,catch)
       (finally ,(if (pair? finally) (car finally) '(J2SNop)))))

(define-macro (J2SCatch param body)
   `(instantiate::J2SCatch
       (loc loc)
       (param ,param)
       (body ,body)))

(define-macro (J2SKont param exn body)
   `(instantiate::J2SKont
       (loc loc)
       (param ,param)
       (exn ,exn)
       (body ,body)))

(define-macro (J2SYield expr kont gen)
   `(instantiate::J2SYield
       (loc loc)
       (expr ,expr)
       (kont ,kont)
       (generator ,gen)))

(define-macro (J2SStmtExpr expr)
   `(instantiate::J2SStmtExpr
       (loc loc)
       (expr ,expr)))

(define-macro (J2SExprStmt stmt)
   `(instantiate::J2SExprStmt
       (loc loc)
       (stmt ,stmt)))

(define-macro (J2SDecl binder usage id)
   `(instantiate::J2SDecl
       (loc loc)
       (binder ,binder)
       (usage ,usage)
       (id ,id)))

(define-macro (J2SParam usage id)
   `(instantiate::J2SDecl
       (loc loc)
       (binder 'param)
       (usage ,usage)
       (id ,id)))

(define-macro (J2SDeclInit usage id val)
   `(instantiate::J2SDeclInit
       (loc loc)
       (binder 'var)
       (usage ,usage)
       (val ,val)
       (id ,id)))

(define-macro (J2SLetOpt usage id val)
   `(instantiate::J2SDeclInit
       (loc loc)
       (writable #f)
       (binder 'let-opt)
       (usage ,usage)
       (val ,val)
       (id ,id)))

(define-macro (%J2STail expr)
   `(instantiate::%J2STail
       (loc loc)
       (expr ,expr)))

(define-macro (J2SIf test then else)
   `(instantiate::J2SIf
       (loc loc)
       (test ,test)
       (then ,then)
       (else ,else)))

(define-macro (J2SCond test then else)
   `(instantiate::J2SCond
       (loc loc)
       (test ,test)
       (then ,then)
       (else ,else)))

(define-macro (J2SReturn tail expr)
   `(instantiate::J2SReturn
       (loc loc)
       (tail ,tail)
       (expr ,expr)))

(define-macro (J2SAssig lhs rhs)
   `(instantiate::J2SAssig
       (loc loc)
       (lhs ,lhs)
       (rhs ,rhs)))

(define-macro (J2SThrow expr)
   `(instantiate::J2SThrow
       (loc loc)
       (expr ,expr)))

(define-macro (J2SSwitch key cases)
   `(instantiate::J2SSwitch
       (loc loc)
       (key ,key)
       (cases ,cases)))

(define-macro (J2SDefault body)
   `(instantiate::J2SDefault
       (loc loc)
       (expr (J2SUndefined))
       (body ,body)))

(define-macro (J2SFor init test incr body)
   `(instantiate::J2SFor
       (loc loc)
       (init ,init)
       (test ,test)
       (incr ,incr)
       (body ,body)))

(define-macro (J2SWhile test body)
   `(instantiate::J2SWhile
       (loc loc)
       (test ,test)
       (body ,body)))

;*---------------------------------------------------------------------*/
;*    kid ...                                                          */
;*---------------------------------------------------------------------*/
(define (kid n::J2SNode)
   n)

;*---------------------------------------------------------------------*/
;*    kid? ...                                                         */
;*---------------------------------------------------------------------*/
(define (kid? obj)
   (cond-expand
      (bigloo-debug
       (with-access::Kont obj (proc)
	  (eq? proc kid)))
      (else
       (eq? obj kid))))

;*---------------------------------------------------------------------*/
;*    KontStmt ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (KontStmt proc node link . name)
   (cond-expand
      (bigloo-debug
       `(instantiate::KontStmt
	   (proc ,proc)
	   (node ,node)
	   (link ,link)
	   (name ,(if (pair? name) (string-append "[" (car name) "]") ""))))
      (else
       proc)))

;*---------------------------------------------------------------------*/
;*    KontExpr ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (KontExpr proc node link . name)
   (cond-expand
      (bigloo-debug
       `(instantiate::KontExpr
	   (proc ,proc)
	   (node ,node)
	   (link ,link)
	   (name ,(if (pair? name) (string-append "[" (car name) "]") ""))))
      (else
       proc)))

;*---------------------------------------------------------------------*/
;*    KontExpr* ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (KontExpr* proc node link . name)
   (cond-expand
      (bigloo-debug
       `(instantiate::KontExpr*
	   (proc ,proc)
	   (node ,node)
	   (link ,link)
	   (name ,(if (pair? name) (string-append "[" (car name) "]") ""))))
      (else
       proc)))

;*---------------------------------------------------------------------*/
;*    kcall ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (kcall kont arg)
   (cond-expand
      (bigloo-debug
       `(with-access::Kont ,kont (proc)
	   (proc ,arg)))
      (else
       `(,kont ,arg))))

;*---------------------------------------------------------------------*/
;*    assert-kont ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander assert-kont
   (lambda (x e)
      (cond-expand
	 (bigloo-debug
	  (match-case x
	     ((?- ?kont ?type ?node)
	      (e `(unless (isa? ,kont ,type)
		     (kont-error ,kont ,type ,node ',(cer x)))
		 e))
	     (else
	      (error "assert-kont" "bad form" x)
	      #f)))
	 (else
	  #t))))

;*---------------------------------------------------------------------*/
;*    kont-error ...                                                   */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo-debug
;;;    
(define (kont-error kont type node loc)
   (raise
      (instantiate::&error
	 (obj (typeof kont))
	 (msg (format "Not a `~s' continuation" (class-name type)))
	 (proc (format "cps ::~a" (typeof node)))
	 (fname (cadr loc))
	 (location (caddr loc))
	 (stack (if (isa? kont Kont)
		    (with-access::Kont kont (node link)
		      (let loop ((link link)
				 (stack '()))
			 (if link
			     (with-access::Kont link (link name node)
				(loop link
				   (cons
				      (format "~a kont: ~a ~a ~a"
					 (typeof node)
					 (typeof link)
					 name
					 (if (>fx (bigloo-debug) 2)
					     (call-with-output-string
						(lambda (p)
						   (display "  ")
						   (display (j2s->list node) p)
						   (newline p)))
					     ""))
				      stack)))
			     (reverse! stack))))
		    (get-trace-stack))))))))

;*---------------------------------------------------------------------*/
;*    empty-stmt? ...                                                  */
;*---------------------------------------------------------------------*/
(define (empty-stmt? stmt)
   (when (isa? stmt J2SSeq)
      (with-access::J2SSeq stmt (nodes)
	 (null? nodes))))

;*---------------------------------------------------------------------*/
;*    make-stmt-kont ...                                               */
;*---------------------------------------------------------------------*/
(define (make-stmt-kont loc stmt::J2SStmt)
   (let* ((name (gensym '%kstmt))
	  (arg (J2SParam '(ref) (gensym '%karg)))
	  (kfun (J2SFun #f (list arg) (J2SBlock stmt))))
      (J2SLetOpt '(call) name kfun)))
   
;*---------------------------------------------------------------------*/
;*    cps-fun ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (cps-fun this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cps-fun ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (cps-fun this::J2SFun)
   (with-access::J2SFun this (generator body name)
      (if generator
	  (set! body (cps body (KontStmt kid this #f) kid '() '()))
	  (cps-fun body))
      this))

;*---------------------------------------------------------------------*/
;*    cps* ...                                                         */
;*---------------------------------------------------------------------*/
(define (cps* nodes::pair-nil k* pack kbreaks kcontinues)
   (assert-kont k* KontExpr* nodes)
   (let loop ((nodes nodes)
	      (knodes '()))
      (if (null? nodes)
	  (kcall k* (reverse! knodes))
	  (cps (car nodes)
	     (KontExpr (lambda (kexpr::J2SExpr)
			  (loop (cdr nodes)
			     (cons kexpr knodes)))
		nodes k*)
	     pack kbreaks kcontinues))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNode ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (cps this::J2SNode k pack::procedure kbreaks kcontinues)
   (warning "cps: should not be here " (typeof this))
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SLiteral ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLiteral k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNop ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SNop k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SParen ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SParen k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (with-access::J2SParen this (loc expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (KontExpr (lambda (kexpr::J2SExpr)
			  (kcall k (J2SParen kexpr)))
		this k "expr")
	     pack kbreaks kcontinues)
	  (kcall k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SRef ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SRef k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SUnary ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SUnary k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (with-access::J2SUnary this (expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (KontExpr (lambda (kexpr::J2SNode)
			  (kcall k (duplicate::J2SUnary this
				      (expr kexpr))))
		this k)
	     pack kbreaks kcontinues)
	  (kcall k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SBinary ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBinary k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (with-access::J2SBinary this (lhs rhs loc)
      (cond
	 ((yield-expr? lhs kbreaks kcontinues)
	  (cps lhs
	     (KontExpr (lambda (klhs::J2SNode)
			  (cps (duplicate::J2SBinary this
				  (lhs klhs))
			     k pack kbreaks kcontinues))
		this k)
	     pack kbreaks kcontinues))
	 ((yield-expr? rhs kbreaks kcontinues)
	  (cps rhs
	     (KontExpr (lambda (krhs::J2SNode)
			  (cps (duplicate::J2SBinary this
				  (rhs krhs))
			     k pack kbreaks kcontinues))
		this k)
	     pack kbreaks kcontinues))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SYield ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SYield k pack kbreaks kcontinues)
   
   (define (make-yield-kont k loc)
      (let* ((arg (J2SParam '(call ref) (gensym '%arg)))
	     (exn (J2SParam '(ref) (gensym '%exn)))
	     (body (kcall k (J2SRef arg)))
	     (cont (J2SIf (J2SBinary '=== (J2SRef exn) (J2SBool #t))
		      (J2SThrow (J2SRef arg))
		      (if (isa? body J2SExpr)
			  (J2SStmtExpr body)
			  body))))
	 (J2SKont arg exn (pack cont))))
   
   (with-access::J2SYield this (loc expr generator)
      (let ((kont (make-yield-kont k loc)))
	 (if (yield-expr? expr kbreaks kcontinues)
	     (cps expr
		(KontExpr (lambda (kexpr::J2SExpr)
			     (J2SYield kexpr kont generator))
		   this k)
		pack kbreaks kcontinues)
	     (begin
		(cps-fun expr)
		(J2SYield expr kont generator))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SReturn ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SReturn k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SReturn this (loc expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (KontExpr (lambda (kexpr::J2SExpr)
			  (J2SStmtExpr (J2SYield kexpr #t #f)))
		this k)
	     pack kbreaks kcontinues)
	  (J2SStmtExpr (J2SYield expr #t #f)))))

;*---------------------------------------------------------------------*/
;*    cps ::%J2STail ...                                               */
;*    -------------------------------------------------------------    */
;*    J2STail are introduced by the CPS conversion of loops.           */
;*---------------------------------------------------------------------*/
(define-method (cps this::%J2STail k pack kbreaks kcontinues)
   (with-access::%J2STail this (loc expr)
      this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SStmtExpr ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SStmtExpr k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SStmtExpr this (loc expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (KontExpr (lambda (ke::J2SExpr)
			  (kcall k (J2SStmtExpr ke)))
		this k "expr")
	     pack kbreaks kcontinues)
	  (begin
	     (cps-fun expr)
	     (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSeq ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSeq k pack kbreaks kcontinues)
   
   (define (pack-seq::J2SSeq this::J2SSeq prev res)
      (if (pair? prev)
	  (set-cdr! prev (list res))
	  (with-access::J2SSeq this (nodes)
	     (set! nodes (list res))))
      this)

   (assert-kont k KontStmt this)
   (with-access::J2SSeq this (loc nodes)
      (let loop ((walk nodes)
		 (pnodes '()))
	 (cond
	    ((null? walk)
	     (kcall k this))
	    ((not (yield-expr? (car walk) kbreaks kcontinues))
	     (cps-fun (car walk))
	     (loop (cdr walk) walk))
	    ((null? (cdr walk))
	     (pack-seq this pnodes
		(cps (car walk)
		   k pack kbreaks kcontinues)))
	    (else
	     (pack-seq this pnodes
		(cps (car walk)
		   (KontStmt (lambda (khead::J2SStmt)
				(cps (J2SSeq* (cons khead (cdr walk)))
				   k pack kbreaks kcontinues))
		      this k)
		   pack kbreaks kcontinues)))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSequence k pack kbreaks kcontinues)
   
   (define (pack-sequence::J2SSequence this::J2SSequence prev res)
      (if (pair? prev)
	  (set-cdr! prev (list res))
	  (with-access::J2SSequence this (exprs)
	     (set! exprs (list res))))
      this)
   
   (assert-kont k KontExpr this)
   (with-access::J2SSequence this (loc exprs)
      (let loop ((walk exprs)
		 (pexprs '()))
	 (cond
	    ((null? walk)
	     (kcall k this))
	    ((not (yield-expr? (car walk) kbreaks kcontinues))
	     (cps-fun (car walk))
	     (loop (cdr walk) walk))
	    ((null? (cdr walk))
	     (pack-sequence this pexprs
		(cps (car walk)
		   k pack kbreaks kcontinues)))
	    (else
	     (pack-sequence this pexprs
		(cps (car walk)
		   (KontExpr (lambda (khead::J2SExpr)
				(cps (J2SSequence* (cons khead (cdr walk)))
				   k pack kbreaks kcontinues))
		      this k)
		   pack kbreaks kcontinues)))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDecl ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDecl k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDeclInit ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDeclInit k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SDeclInit this (val)
      (cond
	 ((not (j2s-let-opt? this))
	  (kcall k this))
	 ((yield-expr? val kbreaks kcontinues)
	  (cps val
	     (KontExpr (lambda (v::J2SExpr)
			  (set! val v)
			  (kcall k this))
		this k)
	     pack kbreaks kcontinues))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDeclFun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDeclFun k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SDeclFun this (val)
      (cps-fun val)
      (kcall k this)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SVarDecls ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SVarDecls k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SVarDecls this (decls)
      (map! (lambda (decl)
	       (cps decl k pack kbreaks kcontinues))
	 decls)
      (kcall k this)))

;*---------------------------------------------------------------------*/
;*    cps ::j2SLetBlock ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLetBlock k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SLetBlock this (loc decls nodes)
      (let loop ((wdecls decls)
		 (pdecls '())
		 (sdecls '()))
	 (cond
	    ((null? wdecls)
	     (call-next-method))
	    ((not (yield-expr? (car wdecls) kbreaks kcontinues))
	     (cps-fun (car wdecls))
	     (loop (cdr wdecls) wdecls (if (pair? sdecls) sdecls wdecls)))
	    (else
	     (with-access::J2SDecl (car wdecls) (scope)
		(set! scope 'kont))
	     (let* ((y (cps (car wdecls)
			  (KontStmt (lambda (ndecl::J2SStmt)
				       (cond
					  ((null? (cdr decls))
					   (set-car! decls ndecl)
					   (call-next-method))
					  ((null? (cdr wdecls))
					   (let ((block (instantiate::J2SBlock
							   (loc loc)
							   (endloc loc)
							   (nodes nodes))))
					      (J2SLetBlock
						 (list ndecl)
						 (cps block
						    k pack kbreaks kcontinues))))
					  (else
					   (set! decls (cdr wdecls))
					   (J2SLetBlock
					      (list ndecl)
					      (cps this
						 k pack kbreaks kcontinues)))))
			     this k)
			  pack kbreaks kcontinues))
		    (b (if (isa? y J2SExpr) (J2SStmtExpr y) y)))
		(if (null? pdecls)
		    (J2SBlock b)
		    (begin
		       (set-cdr! pdecls '())
		       (J2SLetBlock sdecls b)))))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SIf ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SIf k pack kbreaks kcontinues)
   
   (define (make-kont-decl loc k)
      (let* ((name (gensym '%kif))
	     (kfun (J2SFun name '()
		      (J2SBlock (kcall k (J2SStmtExpr (J2SUndefined)))))))
	 (J2SLetOpt '(call) name kfun)))
   
   (define (make-kont-fun-call loc decl)
      (J2SCall (J2SRef decl)))
   
   (assert-kont k KontStmt this)
   (with-access::J2SIf this (loc test then else)
      (cond
	 ((yield-expr? test kbreaks kcontinues)
	  (cps test
	     (KontExpr (lambda (ktest::J2SExpr)
			  (cps (duplicate::J2SIf this
				  (test ktest))
			     k pack kbreaks kcontinues))
		this k)
	     pack kbreaks kcontinues))
	 ((or (yield-expr? then kbreaks kcontinues)
	      (yield-expr? else kbreaks kcontinues))
	  (if (kid? k)
	      ;; no continuation to the if
	      (duplicate::J2SIf this
		 (then (cps then k pack kbreaks kcontinues))
		 (else (cps else k pack kbreaks kcontinues)))
	      ;; full if
	      (let* ((decl (make-kont-decl loc k))
		     (callt (make-kont-fun-call loc decl))
		     (calle (make-kont-fun-call loc decl))
		     (kthen (J2SBlock then callt))
		     (kelse (if (isa? else J2SNop)
				(with-access::J2SNop else (loc)
				   (J2SStmtExpr calle))
				(J2SBlock else calle)))
		     (kif (duplicate::J2SIf this
			     (then (cps kthen
				      (KontStmt kid this k)
				      pack kbreaks kcontinues))
			     (else (cps kelse
				      (KontStmt kid this k)
				      pack kbreaks kcontinues)))))
		 (J2SLetBlock (list decl) kif))))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SExprStmt ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SExprStmt k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (with-access::J2SExprStmt this (stmt)
      (if (yield-expr? stmt kbreaks kcontinues)
	  (cps stmt
	     (KontStmt (lambda (kstmt::J2SStmt)
			  (set! stmt kstmt)
			  (kcall k this))
		this k)
	     pack kbreaks kcontinues)
	  (kcall k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SFor ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SFor k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SFor this (loc init test incr body id)
      (cond
	 ((yield-expr? init kbreaks kcontinues)
	  (let ((i init))
	     (set! init (J2SNop))
	     (cps (J2SSeq (J2SStmtExpr i) this) k pack kbreaks kcontinues)))
	 ((let* ((cell (cons this #t))
		 (kbreaks+ (cons cell kbreaks))
		 (kcontinues+ (cons cell kcontinues)))
	     (or (yield-expr? test kbreaks kcontinues)
		 (yield-expr? incr kbreaks kcontinues)
		 (yield-expr? body kbreaks+ kcontinues+)))
	  ;; K( for( ; test; incr ) body )
	  ;;   -->
	  ;;  (letrec ((for (lambda () (K (if test (begin body incr (for)))))))
	  ;;      (for))
	  ;; If break is used
	  ;;   (let ((kfun (function () (K (js-undefined)))))
	  ;;     (letrec ((for (lambda ()
	  ;;                      (kfun (if test (begin body incr (for)))))))
	  ;;        (for)))
	  ;; If continue is used
	  ;;   (let ((kfun (function () (K (js-undefined)))))
	  ;;     (letrec ((for (lambda ()
	  ;;                      (kfun (if test (begin body incr (for)))))))
	  ;;        (for)))
	  ;;
	  (let* ((name (gensym '%kfor))
		 (bname (gensym '%kbreak))
		 (cname (gensym '%kcontinue))
		 (block (J2SBlock))
		 (for (J2SFun name '() block))
		 (decl (J2SLetOpt '(call) name for))
		 (break (J2SFun name '()
			   (J2SBlock (kcall k (J2SStmtExpr (J2SUndefined))))))
		 (fbody (J2SBlock
			   (J2SSeq
			      (J2SStmtExpr incr)
			      (%J2STail (J2SCall (J2SRef decl))))))
		 (conti (J2SFun name '()
			   (cps fbody
			      (KontStmt kid this k)
			      pack kbreaks
			      kcontinues)))
		 (bdecl (J2SLetOpt '(call) bname break))
		 (cdecl (J2SLetOpt '(call) cname conti))
		 (then (J2SBlock body (%J2STail (J2SCall (J2SRef cdecl)))))
		 (stop (J2SBlock (%J2STail (J2SCall (J2SRef bdecl)))))
		 (node (J2SIf test then stop)))
	     (with-access::J2SBlock block (nodes)
		(set! nodes
		   (list (cps node (KontStmt kid this k) kid
			    (cons (cons this bdecl) kbreaks)
			    (cons (cons this cdecl) kcontinues)))))
	     (J2SLetBlock (list decl bdecl cdecl)
		init
		(%J2STail (J2SCall (J2SRef decl))))))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SForIn ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SForIn k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SForIn this (loc lhs obj body)
      (cond
	 ((yield-expr? obj kbreaks kcontinues)
	  (cps obj
	     (KontExpr (lambda (kobj)
			  (cps (duplicate::J2SForIn this (obj kobj))
			     k pack kbreaks kcontinues))
		this k)
	     pack kbreaks kcontinues))
	 ((let* ((cell (cons this #t))
		 (kbreaks+ (cons cell kbreaks))
		 (kcontinues+ (cons cell kcontinues)))
	     (yield-expr? body kbreaks+ kcontinues+))
	  (let* ((v (gensym '%kkeys))
		 (l (gensym '%klen))
		 (i (gensym '%ki))
		 (keys (J2SLetOpt '(ref) v
			  (J2SCall
			     (J2SAccess
				(J2SUnresolvedRef 'Object)
				(J2SString "keys"))
			     obj)))
		 (len (J2SLetOpt '(ref) l
			 (J2SAccess (J2SRef keys) (J2SString "length"))))
		 (idx (J2SDeclInit '(ref write) i (J2SNumber 0)))
		 (for (J2SFor idx
			 (J2SBinary '< (J2SRef idx) (J2SRef len))
			 (J2SPostfix '++ (J2SRef idx) (J2SUndefined))
			 (J2SBlock
			    (J2SStmtExpr
			       (J2SAssig lhs
				  (J2SAccess (J2SRef keys) (J2SRef idx))))
			    body))))
	     (J2SLetBlock (list keys len)
		(cps for k pack kbreaks kcontinues))))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SWhile ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SWhile k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SWhile this (test body loc)
      (let* ((cell (cons this #t))
	     (kbreaks+ (cons cell kbreaks))
	     (kcontinues+ (cons cell kcontinues)))
	 (if (or (yield-expr? test kbreaks kcontinues)
		 (yield-expr? body kbreaks+ kcontinues+))
	     (let* ((name (gensym '%kwhile))
		    (bname (gensym '%kbreak))
		    (cname (gensym '%kcontinue))
		    (block (J2SBlock))
		    (while (J2SFun name '() block))
		    (decl (J2SLetOpt '(call) name while))
		    (break (J2SFun name '()
			      (J2SBlock (kcall k (J2SStmtExpr (J2SUndefined))))))
		    (fbody (J2SBlock
			      (J2SStmtExpr (J2SCall (J2SRef decl)))))
		    (conti (J2SFun name '()
			      (cps fbody
				 (KontStmt kid this k)
				 pack kbreaks
				 kcontinues)))
		    (bdecl (J2SLetOpt '(call) bname break))
		    (cdecl (J2SLetOpt '(call) cname conti))
		    (then (J2SBlock body (%J2STail (J2SCall (J2SRef cdecl)))))
		    (else (J2SBlock (%J2STail (J2SCall (J2SRef bdecl)))))
		    (node (J2SIf test then else)))
		(with-access::J2SBlock block (nodes)
		   (set! nodes
		      (list (cps node (KontStmt kid this k) kid
			       (cons (cons this bdecl) kbreaks)
			       (cons (cons this cdecl) kcontinues)))))
		(J2SLetBlock (list decl bdecl cdecl)
		   (J2SStmtExpr (J2SCall (J2SRef decl)))))
	     (kcall k this)))))
      
;*---------------------------------------------------------------------*/
;*    cps ::J2SDo ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDo k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SDo this (test body loc)
      (let* ((cell (cons this #t))
	     (kbreaks+ (cons cell kbreaks))
	     (kcontinues+ (cons cell kcontinues)))
	 (if (or (yield-expr? test kbreaks kcontinues)
		 (yield-expr? body kbreaks+ kcontinues+))
	     (let* ((name (gensym '%kdo))
		    (bname (gensym '%kbreak))
		    (cname (gensym '%kcontinue))
		    (tname (gensym '%ktmp))
		    (block (J2SBlock))
		    (while (J2SFun name '() block))
		    (decl (J2SLetOpt '(call) name while))
		    (declv (J2SLetOpt '(ref) tname (J2SBool #t)))
		    (conti (J2SFun name '()
			      (cps (J2SBlock
				      (J2SIf (J2SCond test
						(J2SRef declv)
						(J2SBool #f))
					 (%J2STail (J2SCall (J2SRef decl)))
					 (J2SNop)))
				 k
				 pack kbreaks kcontinues)))
		    (cdecl (J2SLetOpt '(call) cname conti))
		    (break (J2SFun name '()
			      (J2SBlock
				 (J2SStmtExpr
				    (J2SAssig (J2SRef declv) (J2SBool #f)))
				 (%J2STail (J2SCall (J2SRef cdecl))))))
		    (bdecl (J2SLetOpt '(call) bname break))
		    (else (J2SBlock (%J2STail (J2SCall (J2SRef bdecl)))))
		    (sbody (J2SSeq body (%J2STail (J2SCall (J2SRef cdecl)))))
		    (body (cps sbody
			     (KontStmt kid this k) kid
			     (cons (cons this bdecl) kbreaks)
			     (cons (cons this cdecl) kcontinues))))
		(with-access::J2SBlock block (nodes)
		   (set! nodes (list body)))
		(J2SLetBlock (list decl bdecl cdecl declv)
		   (J2SStmtExpr (J2SCall (J2SRef decl)))))
	     (kcall k this)))))
      
;*---------------------------------------------------------------------*/
;*    cps ::J2SBreak ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBreak k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SBreak this (loc target)
      (cond
	 ((assq target kbreaks)
	  =>
	  (lambda (c)
	     (let ((kont (cdr c)))
		(J2SCall (J2SRef kont)))))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SContinue ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SContinue k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SContinue this (loc target)
      (cond
	 ((assq target kcontinues)
	  =>
	  (lambda (c)
	     (let ((kont (cdr c)))
		(J2SCall (J2SRef kont)))))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SAssig ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SAssig k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (with-access::J2SAssig this (lhs rhs)
      (cond
	 ((yield-expr? lhs kbreaks kcontinues)
	  (cps lhs
	     (KontExpr (lambda (klhs::J2SExpr)
			  (set! lhs klhs)
			  (cps this k pack kbreaks kcontinues))
		this k)
	     pack kbreaks kcontinues))
	 ((yield-expr? rhs kbreaks kcontinues)
	  (cps-fun lhs)
	  (cps rhs
	     (KontExpr (lambda (krhs::J2SExpr)
			  (set! rhs krhs)
			  (kcall k this))
		this k)
	     pack kbreaks kcontinues))
	 (else
	  (cps-fun lhs)
	  (cps-fun rhs)
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCall ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCall k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (with-access::J2SCall this (fun args)
      (cond
	 ((yield-expr? fun kbreaks kcontinues)
	  (cps fun
	     (KontExpr (lambda (kfun::J2SExpr)
			  (set! fun kfun)
			  (cps this k pack kbreaks kcontinues))
		this k)
	     pack kbreaks kcontinues))
	 ((any (lambda (e) (yield-expr? e kbreaks kcontinues)) args)
	  (cps-fun fun)
	  (cps* args
	     (KontExpr* (lambda (kargs::pair-nil)
			   (set! args kargs)
			   (cps this k pack kbreaks kcontinues))
		args k)
	     pack kbreaks kcontinues))
	 (else
	  (cps-fun fun)
	  (for-each cps-fun args)
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SArray ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SArray k pack kbreaks kcontinues)
   (assert-kont k KontExpr this)
   (with-access::J2SArray this (exprs)
      (if (any (lambda (e) (yield-expr? e kbreaks kcontinues)) exprs)
	  (cps* exprs
	     (KontExpr* (lambda (kexprs::pair-nil)
			   (set! exprs kexprs)
			   (cps this k pack kbreaks kcontinues))
		exprs k)
	     pack kbreaks kcontinues)
	  (kcall k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCond ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCond k pack kbreaks kcontinues)
   
   (define (make-kont-fun k param loc)
      (J2SFun (gensym '%kont) (list param)
	 (J2SBlock (kcall k (J2SRef param)))))
   
   (define (make-kont-fun-call kfun arg loc)
      (J2SCall (J2SRef kfun) arg))
   
   (assert-kont k KontExpr this)
   (with-access::J2SCond this (test then else loc)
      (cond
	 ((yield-expr? test kbreaks kcontinues)
	  (cps test
	     (KontExpr (lambda (ktest::J2SExpr)
			  (set! test ktest)
			  (cps this k pack kbreaks kcontinues))
		this k "test")
	     pack kbreaks kcontinues))
	 ((or (yield-expr? then kbreaks kcontinues)
	      (yield-expr? else kbreaks kcontinues))
	  ;; (K (test ? then : else ))
	  ;;    -->
	  ;; (let ((kfun (function (karg) (k karg))))
	  ;;    (test ? kfun(then) : kfun(else)))
	  ;;    ===
	  ;; ((function (kfun) test ? kfun(then) : kfun(else))
	  ;;  (function (karg) (K karg)))
	  (let* ((kfun (J2SParam '(call)  (gensym '%kfun)))
		 (karg (J2SParam '(ref)  (gensym '%karg)))
		 (kont (make-kont-fun k karg loc))
		 (kthen (cps then
			   (KontExpr (lambda (n)
					(make-kont-fun-call kfun n loc))
			      this k)
			   pack kbreaks kcontinues))
		 (kelse (cps else
			   (KontExpr (lambda (n)
					(make-kont-fun-call kfun n loc))
			      this k)
			   pack kbreaks kcontinues))
		 (kcond (duplicate::J2SCond this
			   (then kthen)
			   (else kelse)))
		 (binder (J2SFun (gensym '%fun) (list kfun)
			    (J2SBlock (J2SReturn #t kcond)))))
	     (J2SCall binder kont)))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2STry ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2STry k pack kbreaks kcontinues)
   
   (define (Catch loc param declc)
      (J2SCatch param
	 (J2SBlock
	    (%J2STail
	       (J2SCall (J2SRef declc) (J2SRef param))))))
   
   (define (cps-try-body this k pack kbreaks kcontinues param declc)
      (with-access::J2SSeq this (loc)
	 (J2SBlock
	    (cps this k (lambda (n)
			   (J2STry (J2SBlock (pack n))
			      (Catch loc param declc)))
	       kbreaks kcontinues))))
   
   (assert-kont k KontStmt this)
   (with-access::J2STry this (loc body catch finally)
      (cond
	 ((not (or (yield-expr? body kbreaks kcontinues)
		   (yield-expr? catch kbreaks kcontinues)
		   (yield-expr? finally kbreaks kcontinues)))
	  (kcall k this))
	 ((isa? finally J2SNop)
	  (if (isa? catch J2SNop)
	      (cps body k pack kbreaks kcontinues)
	      (let* ((cname (gensym '%kcatch))
		     (catch (with-access::J2SCatch catch (param body)
			       (J2SFun cname (list param)
				  (J2SBlock
				     (cps body
					k pack kbreaks kcontinues)))))
		     (declc (J2SLetOpt '(call) cname catch))
		     (eparam (J2SParam '(ref)  (gensym '%exc))))
		 (J2SLetBlock (list declc)
		    (J2STry
		       (cps-try-body body k pack kbreaks kcontinues eparam declc)
		       (Catch loc eparam declc))))))
	 ((isa? catch J2SNop)
	  (let* ((fname (gensym '%kfinally))
		 (paramf (J2SParam '(ref)  (gensym '%excf)))
		 (okname (gensym '%kok))
		 (declok (J2SLetOpt '(call ref) okname (J2SPragma ''(0 . 0))))
		 (final (J2SFun fname (list paramf)
			   (J2SBlock
			      (cps (J2SSeq
				      finally
				      (J2SIf (J2SBinary '===
						(J2SRef paramf)
						(J2SRef declok))
					 (J2SNop)
					 (J2SThrow (J2SRef paramf))))
				 k pack kbreaks kcontinues))))
		 (declf (J2SLetOpt '(call ref) fname final))
		 (eparam (J2SParam '(ref)  (gensym '%exc))))
	     (J2SLetBlock (list declf declok)
		(J2STry
		   (cps-try-body
		      (J2SSeq
			 body
			 (%J2STail (J2SCall (J2SRef declf) (J2SRef declok))))
		      k pack kbreaks kcontinues eparam declf)
		   (Catch loc eparam declf)))))
	 (else
	  (with-access::J2SCatch catch ((cbody body) param)
	     (let* ((fname (gensym '%kfinally))
		    (paramf (J2SParam '(ref)  (gensym '%excf)))
		    (okname (gensym '%kok))
		    (declok (J2SLetOpt '(call ref) okname (J2SPragma ''(1 . 1))))
		    (final (J2SFun fname (list paramf)
			      (J2SBlock 
				 (cps (J2SSeq
					 finally
					 (J2SIf (J2SBinary '===
						   (J2SRef paramf)
						   (J2SRef declok))
					    (J2SNop)
					    (J2SThrow (J2SRef paramf))))
				    k pack kbreaks kcontinues))))
		    (declf (J2SLetOpt '(call ref) fname final))
		    (cname (gensym '%kcatch))
		    (eparam (J2SParam '(ref)  (gensym '%exc)))
		    (catch (J2SFun cname (list param)
			      (cps (J2SBlock
				      (J2STry cbody (J2SNop)
					 (%J2STail
					    (J2SCall (J2SRef declf) (J2SRef declok)))))
				 k pack kbreaks kcontinues)))
		    (declc (J2SLetOpt '(call) cname catch)))
		(with-access::J2SDecl param (usage)
		   (set! usage (cons 'ref usage)))
		(J2SLetBlock (list declf declc declok)
		   (J2STry
		      (cps-try-body
			 (J2SSeq
			    body
			    (%J2STail (J2SCall (J2SRef declf) (J2SRef declok))))
			 k pack kbreaks kcontinues param declc)
		      (Catch loc eparam declc)))))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SThrow ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SThrow k pack kbreaks kcontinues)
   (assert-kont k KontStmt this)
   (with-access::J2SThrow this (loc expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (KontExpr (lambda (kexpr::J2SExpr)
			  (kcall k (J2SThrow kexpr)))
		this k)
	     pack kbreaks kcontinues)
	  (kcall k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSwitch ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSwitch k pack kbreaks kcontinues)
   
   (define (switch->if key tmp clause)
      (with-access::J2SCase clause (loc expr body)
	 (if (isa? clause J2SDefault)
	     body
	     (J2SIf (J2SBinary 'OR
		       (J2SRef tmp) (J2SBinary '=== (J2SRef key) expr))
		(J2SSeq
		   (J2SAssig (J2SRef tmp) (J2SBool #t))
		   body)
		(J2SNop)))))
   
   (assert-kont k KontStmt this)
   (with-access::J2SSwitch this (loc key cases need-bind-exit-break)
      (cond
	 ((yield-expr? key kbreaks kcontinues)
	  (cps key
	     (KontExpr (lambda (kkey::J2SExpr)
			  (cps (duplicate::J2SSwitch this (key kkey))
			     k pack kbreaks kcontinues))
		this k)
	     pack kbreaks kcontinues))
	 ((not (any (lambda (c) (yield-expr? c kbreaks kcontinues)) cases))
	  (kcall k this))
	 (else
	  (let* ((v (gensym '%kkey))
		 (t (gensym '%ktmp))
		 (key (J2SLetOpt '(ref) v key))
		 (tmp (J2SLetOpt '(write ref) t (J2SBool #f)))
		 (seq (J2SSeq*
			 (map (lambda (clause)
				 (switch->if key tmp clause))
			    cases))))
	     (if need-bind-exit-break
		 (let* ((bname (gensym '%kbreak))
			(break (J2SFun bname '()
				  (J2SBlock
				     (J2SBlock
					(kcall k (J2SStmtExpr (J2SUndefined)))))))
			(bdecl (J2SLetOpt '(call) bname break)))
		    (J2SLetBlock (list key tmp bdecl)
		       (cps seq k pack
			  (cons (cons this bdecl) kbreaks)
			  kcontinues)))
		 (J2SLetBlock (list key tmp)
		    (cps seq k pack kbreaks kcontinues))))))))
	 
;*---------------------------------------------------------------------*/
;*    yield-expr? ...                                                  */
;*---------------------------------------------------------------------*/
(define (yield-expr? this kbreaks kcontinues)
   (let ((v (yield-expr* this kbreaks kcontinues)))
      (find (lambda (v) v) v)))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SNode ...                                        */
;*    -------------------------------------------------------------    */
;*    Returns #t iff a statement contains a YIELD. Otherwise           */
;*    returns #f.                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SNode kbreaks kcontinues)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SYield ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SYield kbreaks kcontinues)
   (list this))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SReturn ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SReturn kbreaks kcontinues)
   (list this))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SBreak ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SBreak kbreaks kcontinues)
   (with-access::J2SBreak this (target)
      (if (assq target kbreaks)
	  (list this)
	  '())))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SContinue ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SContinue kbreaks kcontinues)
   (with-access::J2SContinue this (target)
      (if (assq target kcontinues)
	  (list this)
	  '())))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SFun kbreaks kcontinues)
   '())

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SCase ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SCase kbreaks kcontinues)
   (with-access::J2SCase this (expr body)
      (append (yield-expr* expr kbreaks kcontinues)
	 (yield-expr* body kbreaks kcontinues))))
