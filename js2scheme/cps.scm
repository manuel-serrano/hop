;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/cps.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Tue Nov 10 09:25:44 2015 (serrano)                */
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

   (static (final-class %J2STail::J2SReturn))
   
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
	 (set! body (cps body (lambda (n::J2SNode) n) '() '())))
      this))

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

(define-macro (J2SNop)
   `(instantiate::J2SNop
       (loc loc)))

(define-macro (J2SCall fun . args)
   `(instantiate::J2SCall
       (loc loc)
       (fun ,fun)
       (args ,(if (pair? args) `(list ,(car args)) ''()))))

(define-macro (J2SRef decl)
   `(instantiate::J2SRef
       (loc loc)
       (decl ,decl)))

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

(define-macro (J2SKont param body)
   `(instantiate::J2SKont
       (loc loc)
       (param ,param)
       (body ,body)))

(define-macro (J2SYield expr done)
   `(instantiate::J2SYield
       (loc loc)
       (done ,done)
       (expr ,expr)))

(define-macro (J2SStmtExpr expr)
   `(instantiate::J2SStmtExpr
       (loc loc)
       (expr ,expr)))

(define-macro (J2SExprStmt stmt)
   `(instantiate::J2SExprStmt
       (loc loc)
       (stmt ,stmt)))

(define-macro (J2SLetOpt usage id val)
   `(instantiate::J2SLetOpt
       (loc loc)
       (isconst #t)
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

(define-macro (J2SParam id usage)
   `(instantiate::J2SParam
       (loc loc)
       (usage ,usage)
       (id ,id)))

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

;*---------------------------------------------------------------------*/
;*    cps* ...                                                         */
;*---------------------------------------------------------------------*/
(define (cps* nodes::pair k kbreaks kcontinues)
   (let loop ((nodes nodes)
	      (knodes '()))
      (if (null? nodes)
	  (k (reverse! knodes))
	  (cps (car nodes)
	     (lambda (knode::J2SNode)
		(loop (cdr nodes)
		   (cons knode knodes)))
	     kbreaks kcontinues))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNode ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (cps this::J2SNode k kbreaks kcontinues)
   (warning "cps: should not be here " (typeof this))
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SLiteral ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLiteral k kbreaks kcontinues)
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNop ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SNop k kbreaks kcontinues)
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SParen ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SParen k kbreaks kcontinues)
   (with-access::J2SParen this (expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (lambda (kexpr::J2SNode)
		(k (duplicate::J2SParen this
		      (expr kexpr))))
	     kbreaks kcontinues)
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SRef ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SRef k kbreaks kcontinues)
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SUnary ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SUnary k kbreaks kcontinues)
   (with-access::J2SUnary this (expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (lambda (kexpr::J2SNode)
		(k (duplicate::J2SUnary this
		      (expr kexpr))))
	     kbreaks kcontinues)
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SBinary ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBinary k kbreaks kcontinues)
   (with-access::J2SBinary this (lhs rhs loc)
      (cond
	 ((yield-expr? lhs kbreaks kcontinues)
	  (cps lhs
	     (lambda (klhs::J2SNode)
		(cps (duplicate::J2SBinary this
			 (lhs klhs))
		   k kbreaks kcontinues))
	     kbreaks kcontinues))
	 ((yield-expr? rhs kbreaks kcontinues)
	  (cps rhs
	     (lambda (krhs::J2SNode)
		(cps (duplicate::J2SBinary this
			 (rhs krhs))
		   k kbreaks kcontinues))
	     kbreaks kcontinues))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SYield ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SYield k kbreaks kcontinues)
   
   (define (make-yield-kont k id loc)
      (J2SKont id (k (J2SHopRef id))))
   
   (with-access::J2SYield this (expr kont loc)
      (cps expr
	 (lambda (kexpr::J2SExpr)
	    (set! expr kexpr)
	    (set! kont (make-yield-kont k (gensym '%arg) loc))
	    this)
	 kbreaks kcontinues)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SReturn ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SReturn k kbreaks kcontinues)
   (with-access::J2SReturn this (loc expr)
      (cps expr
	 (lambda (kexpr::J2SExpr)
	    (J2SStmtExpr (J2SYield kexpr #t)))
	 kbreaks kcontinues)))

;*---------------------------------------------------------------------*/
;*    cps ::%J2STail ...                                               */
;*    -------------------------------------------------------------    */
;*    J2STail are introduced by the CPS conversion of loops.           */
;*---------------------------------------------------------------------*/
(define-method (cps this::%J2STail k kbreaks kcontinues)
   (with-access::%J2STail this (loc expr)
      this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSeq ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSeq k kbreaks kcontinues)
   (with-access::J2SSeq this (loc nodes)
      (let loop ((nodes nodes))
	 (cond
	    ((null? nodes)
	     (k this))
	    ((not (yield-expr? (car nodes) kbreaks kcontinues))
	     (loop (cdr nodes)))
	    (else
	     (let ((knodes (cdr nodes)))
		(set-cdr! nodes '())
		(set-car! nodes
		   (cps (car nodes)
		      (lambda (n::J2SNode)
			 (let ((kseq (duplicate::J2SSeq this
					(nodes knodes))))
			    (let ((res (cps kseq k kbreaks kcontinues)))
			       (with-access::J2SSeq res (nodes)
				  (set! nodes (cons n nodes)))
			       res)))
		      kbreaks kcontinues))
		this))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDecl ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDecl k kbreaks kcontinues)
   (k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDeclInit ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDeclInit k kbreaks kcontinues)
   (with-access::J2SDeclInit this (val)
      (if (yield-expr? val kbreaks kcontinues)
	 (cps val
	    (lambda (v::J2SExpr)
	       (set! val v)
	       (k this))
	    kbreaks kcontinues)
	 (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SLetInit ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLetInit k kbreaks kcontinues)
   (with-access::J2SLetInit this (val)
      (if (yield-expr? val kbreaks kcontinues)
	 (cps val
	    (lambda (v::J2SExpr)
	       (set! val v)
	       (k this))
	    kbreaks kcontinues)
	 (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SLetOpt ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLetOpt k kbreaks kcontinues)
   (with-access::J2SLetOpt this (val)
      (if (yield-expr? val kbreaks kcontinues)
	 (cps val
	    (lambda (v::J2SExpr)
	       (set! val v)
	       (k this))
	    kbreaks kcontinues)
	 (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SVarDecls ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SVarDecls k kbreaks kcontinues)
   (with-access::J2SVarDecls this (decls)
      (map! (lambda (decl)
	       (cps decl k kbreaks kcontinues))
	 decls)
      (k this)))

;*---------------------------------------------------------------------*/
;*    cps ::j2sletblock ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLetBlock k kbreaks kcontinues)
   (with-access::J2SLetBlock this (decls)
      (map! (lambda (decl)
	       (cps decl (lambda (n) n) kbreaks kcontinues))
	 decls)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SIf ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SIf k kbreaks kcontinues)

   (define (make-kont-decl loc k)
      (let* ((name (gensym '%kif))
	     (kfun (J2SFun name '() (J2SBlock (k (J2SUndefined))))))
	 (J2SLetOpt '(call) name kfun)))
   
   (define (make-kont-fun-call loc decl)
      (J2SCall (J2SRef decl)))
   
   (with-access::J2SIf this (loc test then else)
      (cond
	 ((yield-expr? test kbreaks kcontinues)
	  (cps test
	     (lambda (ktest::J2SNode)
		(cps (duplicate::J2SIf this
			(test ktest))
		   k kbreaks kcontinues))
	     kbreaks kcontinues))
	 ((or (yield-expr? then kbreaks kcontinues)
	      (yield-expr? else kbreaks kcontinues))
	  (let* ((decl (make-kont-decl loc k))
		 (callt (make-kont-fun-call loc decl))
		 (calle (make-kont-fun-call loc decl))
		 (kthen (J2SBlock then callt))
		 (kelse (if (isa? else J2SNop)
			    (with-access::J2SNop else (loc)
			       (J2SStmtExpr calle))
			    (J2SBlock else calle)))
		 (kif (duplicate::J2SIf this
			 (then (cps kthen (lambda (n) n) kbreaks kcontinues))
			 (else (cps kelse (lambda (n) n) kbreaks kcontinues)))))
	     (J2SLetBlock (list decl) kif)))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SStmtExpr ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SStmtExpr k kbreaks kcontinues)
   (with-access::J2SStmtExpr this (expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (lambda (kexpr::J2SNode)
		(set! expr kexpr)
		(k this))
	     kbreaks kcontinues)
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SExprStmt ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SExprStmt k kbreaks kcontinues)
   (with-access::J2SExprStmt this (stmt)
      (if (yield-expr? stmt kbreaks kcontinues)
	  (cps stmt
	     (lambda (kstmt::J2SNode)
		(set! stmt kstmt)
		(k this))
	     kbreaks kcontinues)
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SFor ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SFor k kbreaks kcontinues)
   (with-access::J2SFor this (loc init test incr body id)
      (cond
	 ((yield-expr? init kbreaks kcontinues)
	  (let ((i init))
	     (set! init (J2SNop))
	     (cps (J2SSeq i this) k kbreaks kcontinues)))
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
		 (break (J2SFun name '() (J2SBlock (k (J2SUndefined)))))
		 (conti (J2SFun name '()
			   (cps (J2SBlock
				   (J2SSeq
				      (J2SStmtExpr incr)
				      (J2SCall (J2SRef decl))))
			      (lambda (n) n)
			      kbreaks
			      kcontinues)))
		 (bdecl (J2SLetOpt '(call) bname break))
		 (cdecl (J2SLetOpt '(call) cname conti))
		 (then (J2SBlock body (%J2STail (J2SCall (J2SRef cdecl)))))
		 (else (J2SBlock (%J2STail (J2SCall (J2SRef bdecl)))))
		 (node (J2SIf test then else)))
	     (with-access::J2SBlock block (nodes)
		(set! nodes
		   (list (cps node (lambda (n) n)
			    (cons (cons this bdecl) kbreaks)
			    (cons (cons this cdecl) kcontinues)))))
	     (J2SLetBlock (list decl bdecl cdecl)
		init
		(J2SStmtExpr (J2SCall (J2SRef decl))))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SWhile ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SWhile k kbreaks kcontinues)
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
		    (break (J2SFun name '() (J2SBlock (k (J2SUndefined)))))
		    (conti (J2SFun name '()
			      (cps (J2SBlock
				      (J2SStmtExpr
					 (J2SCall (J2SRef decl))))
				 (lambda (n) n)
				 kbreaks
				 kcontinues)))
		    (bdecl (J2SLetOpt '(call) bname break))
		    (cdecl (J2SLetOpt '(call) cname conti))
		    (then (J2SBlock body (%J2STail (J2SCall (J2SRef cdecl)))))
		    (else (J2SBlock (%J2STail (J2SCall (J2SRef bdecl)))))
		    (node (J2SIf test then else)))
		(with-access::J2SBlock block (nodes)
		   (set! nodes
		      (list (cps node (lambda (n) n)
			       (cons (cons this bdecl) kbreaks)
			       (cons (cons this cdecl) kcontinues)))))
		(J2SLetBlock (list decl bdecl cdecl)
		   (J2SStmtExpr (J2SCall (J2SRef decl)))))
	     (k this)))))
      
;*---------------------------------------------------------------------*/
;*    cps ::J2SDo ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDo k kbreaks kcontinues)
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
				 kbreaks kcontinues)))
		    (cdecl (J2SLetOpt '(call) cname conti))
		    (break (J2SFun name '()
			      (J2SBlock
				 (J2SAssig (J2SRef declv) (J2SBool #f))
				 (%J2STail (J2SCall (J2SRef cdecl))))))
		    (bdecl (J2SLetOpt '(call) bname break))
		    (else (J2SBlock (%J2STail (J2SCall (J2SRef bdecl)))))
		    (body (cps (J2SSeq body (%J2STail (J2SCall (J2SRef cdecl))))
			     (lambda (n) n)
			     (cons (cons this bdecl) kbreaks)
			     (cons (cons this cdecl) kcontinues))))
		(with-access::J2SBlock block (nodes)
		   (set! nodes (list body)))
		(J2SLetBlock (list decl bdecl cdecl declv)
		   (J2SStmtExpr (J2SCall (J2SRef decl)))))
	     (k this)))))
      
;*---------------------------------------------------------------------*/
;*    cps ::J2SBreak ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBreak k kbreaks kcontinues)
   (with-access::J2SBreak this (loc target)
      (cond
	 ((assq target kbreaks)
	  =>
	  (lambda (c)
	     (let ((kont (cdr c)))
		(J2SCall (J2SRef kont)))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SContinue ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SContinue k kbreaks kcontinues)
   (with-access::J2SContinue this (loc target)
      (cond
	 ((assq target kcontinues)
	  =>
	  (lambda (c)
	     (let ((kont (cdr c)))
		(J2SCall (J2SRef kont)))))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SAssig ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SAssig k kbreaks kcontinues)
   (with-access::J2SAssig this (lhs rhs)
      (cond
	 ((yield-expr? lhs kbreaks kcontinues)
	  (cps lhs
	     (lambda (klhs::J2SNode)
		(set! lhs klhs)
		(cps this k kbreaks kcontinues))
	     kbreaks kcontinues))
	 ((yield-expr? rhs kbreaks kcontinues)
	  (cps rhs
	     (lambda (krhs::J2SNode)
		(set! rhs krhs)
		(cps this k kbreaks kcontinues))
	     kbreaks kcontinues))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCall ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCall k kbreaks kcontinues)
   (with-access::J2SCall this (fun args)
      (cond
	 ((yield-expr? fun kbreaks kcontinues)
	  (cps fun
	     (lambda (kfun::J2SNode)
		(set! fun kfun)
		(cps this k kbreaks kcontinues))
	     kbreaks kcontinues))
	 ((any (lambda (e) (yield-expr? e kbreaks kcontinues)) args)
	  (cps* args
	     (lambda (kargs::pair-nil)
		(set! args kargs)
		(cps this k kbreaks kcontinues))
	     kbreaks kcontinues))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSequence k kbreaks kcontinues)
   (with-access::J2SSequence this (exprs)
      (if (any (lambda (e) (yield-expr? e kbreaks kcontinues)) exprs)
	  (cps* exprs
	     (lambda (kexprs::pair-nil)
		(set! exprs kexprs)
		(cps this k kbreaks kcontinues))
	     kbreaks kcontinues)
	  (k this))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCond ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCond k kbreaks kcontinues)
   
   (define (make-kont-fun k param loc)
      (J2SFun (gensym '%kont) (list param)
	 (J2SBlock (k (J2SRef param)))))
   
   (define (make-kont-fun-call kfun arg loc)
      (J2SCall (J2SRef kfun) arg))
   
   (with-access::J2SCond this (test then else loc)
      (cond
	 ((yield-expr? test kbreaks kcontinues)
	  (cps test
	     (lambda (ktest::J2SExpr)
		(set! test ktest)
		(cps this k kbreaks kcontinues))
	     kbreaks kcontinues))
	 ((or (yield-expr? then kbreaks kcontinues)
	      (yield-expr? else kbreaks kcontinues))
	  ;; (K (test ? then : else ))
	  ;;    -->
	  ;; (let ((kfun (function (karg) (k karg))))
	  ;;    (test ? kfun(then) : kfun(else)))
	  ;;    ===
	  ;; ((function (kfun) test ? kfun(then) : kfun(else))
	  ;;  (function (karg) (K karg)))
	  (let* ((kfun (J2SParam (gensym '%kfun) '(call)))
		 (karg (J2SParam (gensym 'k%arg) '(ref)))
		 (kont (make-kont-fun k karg loc))
		 (kthen (cps then
			   (lambda (n)
			      (make-kont-fun-call kfun n loc))
			   kbreaks kcontinues))
		 (kelse (cps else
			   (lambda (n)
			      (make-kont-fun-call kfun n loc))
			   kbreaks kcontinues))
		 (kcond (duplicate::J2SCond this
			   (then kthen)
			   (else kelse)))
		 (binder (J2SFun (gensym '%fun) (list kfun)
			    (J2SBlock (J2SReturn #t kcond)))))
	     (J2SCall binder kont)))
	 (else
	  (k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2STry ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2STry k kbreaks kcontinues)
   (with-access::J2STry this (loc body catch finally)
      (cond
	 ((not (or (yield-expr? body kbreaks kcontinues)
		   (yield-expr? catch kbreaks kcontinues)
		   (yield-expr? finally kbreaks kcontinues)))
	  (k this))
	 ((isa? finally J2SNop)
	  (if (isa? catch J2SNop)
	      (cps body k kbreaks kcontinues)
	      (k this)))
	 (else
	  (let* ((tname (gensym '%ktem))
		 (declv (J2SLetOpt '(ref) tname
			   (J2SExprStmt (J2STry body catch)))))
	     (cps (J2SLetBlock (list declv)
		     (J2SSeq finally (J2SRef declv)))
		k kbreaks kcontinues))))))
   
;*---------------------------------------------------------------------*/
;*    yield-expr? ...                                                  */
;*---------------------------------------------------------------------*/
(define (yield-expr? this::J2SNode kbreaks kcontinues)
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
