;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/cps.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 11 14:30:38 2013                          */
;*    Last change :  Mon Oct 25 11:05:42 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript CPS transformation                                    */
;*    -------------------------------------------------------------    */
;*    http://www.ecma-international.org/ecma-262/6.0/#sec-14.4         */
;*    -------------------------------------------------------------    */
;*    This module implements the JavaScript CPS transformation needed  */
;*    for generators. Only generator function are modified.            */
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
	   __js2scheme_alpha
	   __js2scheme_utils)

   (include "ast.sch" "usage.sch")

   (static (final-class %J2STail::J2SReturn)
	   (final-class KDeclInfo
	      (free::bool (default #f))
	      (def::bool (default #f))
	      (color::int (default -1))))

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

   (export j2s-cps-stage))

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
(define (j2s-cps this conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (headers decls nodes)
	 (for-each (lambda (o) (cps-fun! o (lambda (n) n) conf)) headers)
	 (for-each (lambda (o) (cps-fun! o (lambda (n) n) conf)) decls)
	 (for-each (lambda (o) (cps-fun! o (lambda (n) n) conf)) nodes)))
   this)

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
;*    J2SKontCall ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (J2SKontCall kdecl . args)
   `(J2SMethodCall (J2SRef ,kdecl)
       (list ,(if (pair? args) (car args) `(J2SHopRef '%gen)))
       (J2SHopRef '%yield)
       (J2SHopRef '%this)
       ,@(if (pair? args) (cdr args) '())))
   
;*---------------------------------------------------------------------*/
;*    kcall ...                                                        */
;*---------------------------------------------------------------------*/
(define-expander kcall
   (lambda (x e)
      (match-case x
	 ((?kcall ?kont ?arg)
	  (cond-expand
	     (bigloo-debug
	      (e `(with-access::Kont ,kont (proc name)
		     (let ((res (proc ,arg)))
			(if (or (isa? res J2SStmt)
				(and (isa? res J2SExpr) (isa? ,kont KontExpr)))
			    res
			    (kont-call-error ,kont res ',(cer x)))))
		 e))
	     (else
	      (e `(,kont ,arg) e))))
	 (else
	  (error "kcall" "bad form" x)))))

;*---------------------------------------------------------------------*/
;*    ktrampoline ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function is only to reduce the number of generated          */
;*    continuations. Using it is optional.                             */
;*    -------------------------------------------------------------    */
;*    If a continuation merely jump to another continuation, return    */
;*    that trampoline continuation.                                    */
;*---------------------------------------------------------------------*/
(define (ktrampoline body::J2SStmt)
   
   (define (ishopref? expr i)
      (when (isa? expr J2SHopRef)
	 (with-access::J2SHopRef expr (id)
	    (eq? id id))))

   (define (iskontcall? expr)
      (when (isa? expr J2SCall)
	 (with-access::J2SCall expr (fun thisargs args)
	    (when (and (pair? thisargs)
		       (null? (cdr thisargs))
		       (pair? args)
		       (null? (cdr args)))
	       (and (ishopref? (car thisargs) '%gen)
		    (ishopref? (car args) '%this))))))
   
   (when (isa? body J2SSeq)
      (with-access::J2SSeq body (nodes)
	 (when (and (pair? nodes) (pair? (cdr nodes)) (null? (cddr nodes)))
	    (when (isa? (car nodes) J2SNop)
	       (when (isa? (cadr nodes) J2SReturn)
		  (with-access::J2SReturn (cadr nodes) (expr)
		     (when (iskontcall? expr)
			(with-access::J2SCall expr (fun thisargs)
			   (when (isa? fun J2SRef)
			      expr))))))))))

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
;*    kont-debug                                                       */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo-debug
    (define (kont-debug kont #!optional (max-depth 20))
       (if (isa? kont Kont)
	   (with-access::Kont kont (node link)
	      (let loop ((link link)
			 (stack '())
			 (depth max-depth))
		 (if (and link (>fx depth 0))
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
					   (display (j2s->sexp node) p)
					   (newline p)))
				     ""))
			      stack)
			   (-fx depth 1)))
		     (reverse! stack))))))))

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
						   (display (j2s->sexp node) p)
						   (newline p)))
					     ""))
				      stack)))
			     (reverse! stack))))
		    (get-trace-stack))))))))

(cond-expand
   (bigloo-debug
;;;    
(define (kont-call-error kont node loc)
   (raise
      (instantiate::&error
	 (obj (with-access::Kont kont (node) (typeof node)))
	 (msg (format "~a returns `~a' expression" (typeof kont) (typeof node)))
	 (proc (with-access::Kont kont (name) name))
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
						   (display (j2s->sexp node) p)
						   (newline p)))
					     ""))
				      stack)))
			     (reverse! stack))))
		    (get-trace-stack))))))))

;*---------------------------------------------------------------------*/
;*    cps-fun! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (cps-fun! this::J2SNode r::procedure conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cps-fun! ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (cps-fun! this::J2SFun r::procedure conf)
   (with-access::J2SFun this (generator body name constrsize loc
				decl params thisp argumentsp %info)
      (if generator
	  (let ((k (KontStmt kid this #f "J2SFun"))
		(ydstar (has-yield*? body)))
	     (set! body (blockify body (cps body k r '() '() this conf)))
	     (if (config-get conf :optim-cps-closure-alloc #f)
		 (let ((temps (delete-duplicates! (collect-temps* this) eq?)))
		    ;; collect all locals used in the generator
		    (for-each (lambda (d)
				 (with-access::J2SDecl d (%info)
				    (set! %info (instantiate::KDeclInfo))))
		       temps)
		    (tprint name " used: "
		       (map (lambda (d)
			       (with-access::J2SDecl d (%info id)
				  id))
			  temps))
		    ;; mark all locals appearing free in a closure
		    (mark-free body '())
		    ;; mark the generator arguments
		    (for-each (lambda (d)
				 (when (isa? d J2SDecl)
				    (mark-def! d)))
		       (cons* decl thisp argumentsp params))
		    ;; retain free variables
		    (let ((frees (filter is-free? temps)))
		       (tprint name " free: "
			  (map (lambda (d)
				  (with-access::J2SDecl d (%info id)
				     id))
			     frees))
		       (let* ((temps (filter (lambda (d)
						(or (is-def? d)
						    (if (decl-usage-has? d '(assig))
							(begin
							   (mark-ignored! d)
							   #f)
							#t)))
					frees))
			      (sz (length temps)))
			  (tprint name " temps: "
			     (map (lambda (d)
				     (with-access::J2SDecl d (%info id)
					id))
				temps))
			  ;; color the continuation variables
			  (set! constrsize (kont-color temps ydstar))
			  ;; allocate temporaries
			  (kont-alloc-temp! this 
			     (filter (lambda (d)
					(with-access::J2SDecl d (%info)
					   (with-access::KDeclInfo %info (def)
					      (not def))))
				temps))
			  this)))
		 (begin
		    (set! constrsize 0)
		    this)))
	  (begin
	     (cps-fun! body r conf)
	     this))))

;*---------------------------------------------------------------------*/
;*    SeqBlock ...                                                     */
;*---------------------------------------------------------------------*/
(define (SeqBlock this::J2SSeq n m)
   (let ((res (dup this)))
      (with-access::J2SSeq res (nodes)
	 (if (or (eq? (object-class m) J2SSeq)
		 (eq? (object-class m) J2SBlock))
	     (with-access::J2SSeq m ((seq nodes))
		(set! nodes (cons n seq)))
	     (set! nodes (list n m))))
      res))

;*---------------------------------------------------------------------*/
;*    dup ...                                                          */
;*---------------------------------------------------------------------*/
(define (dup this::J2SNode)
   (let* ((clazz (object-class this))
	  (ctor (class-constructor clazz))
	  (inst ((class-allocator clazz)))
	  (fields (class-all-fields clazz)))
      ;; instance fields
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref-ur fields i))
		   (v ((class-field-accessor f) this)))
	       ((class-field-mutator f) inst v)
	       (loop (-fx i 1)))))
      ;; constructor
      (when (procedure? ctor) ctor inst)
      inst))

;*---------------------------------------------------------------------*/
;*    dup-assig ...                                                    */
;*---------------------------------------------------------------------*/
(define (dup-assig this l r)
   (let ((n (dup this)))
      (with-access::J2SAssig n (lhs rhs)
	 (set! lhs l)
	 (set! rhs r)
	 n)))

;*---------------------------------------------------------------------*/
;*    blockify ...                                                     */
;*---------------------------------------------------------------------*/
(define (blockify::J2SBlock block::J2SBlock stmt::J2SStmt)
   (if (isa? stmt J2SBlock)
       stmt
       (duplicate::J2SBlock block
	  (nodes (list stmt)))))

;*---------------------------------------------------------------------*/
;*    cps* ...                                                         */
;*---------------------------------------------------------------------*/
(define (cps*::J2SNode nodes::pair-nil k* r* kbreaks kcontinues fun conf)
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
	     r* kbreaks kcontinues fun conf))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNode ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (cps::J2SNode this::J2SNode k r
		   kbreaks::pair-nil kcontinues::pair-nil fun::J2SFun conf)
   (warning "cps: should not be here " (typeof this))
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SYield ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SYield k r kbreaks kcontinues fun conf)

   (define (stmtify n)
      (if (isa? n J2SStmt)
	  n
	  (with-access::J2SExpr n (loc)
	     (J2SStmtExpr n))))
   
   (define (make-yield-kont k loc)
      (let ((arg (J2SParam '(call ref) (gensym '%arg) :vtype 'any))
	    (exn (J2SParam '(ref) (gensym '%exn) :vtype 'bool)))
	 (J2SKont arg exn
	    (r (J2SIf (J2SBinary 'eq? (J2SRef exn) (J2SBool #t))
		  (J2SThrow (J2SRef arg))
		  (stmtify (kcall k (J2SRef arg))))))))
   
   (with-access::J2SYield this (loc expr generator)
      (let ((kont (make-yield-kont k loc)))
	 (cps expr
	    (KontExpr (lambda (kexpr::J2SExpr)
			 (J2SReturnYield kexpr kont generator))
	       this k)
	    r kbreaks kcontinues fun conf))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SReturn ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SReturn k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SReturn this (loc expr)
      (cps expr
	 (KontExpr (lambda (kexpr::J2SExpr)
		      (J2SReturnYield kexpr (J2SUndefined) #f))
	    this k)
	 r kbreaks kcontinues fun conf)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SExpr ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SExpr k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (kcall k this))
   
;*---------------------------------------------------------------------*/
;*    cps ::J2SLiteral ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLiteral k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SFun ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SFun k r breaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (kcall k (cps-fun! this r conf)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SRef ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SRef k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SParen ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SParen k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (with-access::J2SParen this (expr)
      (cps expr k r kbreaks kcontinues fun conf)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SUnary ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SUnary k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (with-access::J2SUnary this (expr)
      (cps expr
	 (KontExpr (lambda (kexpr::J2SExpr)
		      (kcall k
			 (duplicate::J2SUnary this
			    (expr kexpr))))
	    this k)
	 r kbreaks kcontinues fun conf)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SBinary ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBinary k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (with-access::J2SBinary this (lhs rhs loc)
      (cps lhs
	 (KontExpr (lambda (klhs::J2SExpr)
		      (cps rhs
			 (KontExpr (lambda (krhs::J2SExpr)
				      (kcall k
					 (duplicate::J2SBinary this
					    (lhs klhs)
					    (rhs krhs))))
			    this k)
			 r kbreaks kcontinues fun conf))
	    this k)
	 r kbreaks kcontinues fun conf)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSequence k r kbreaks kcontinues fun conf)
   
   (define (seqify this kar kdr)
      (if (isa? kdr J2SSequence)
	  (with-access::J2SSequence kdr (exprs)
	     (duplicate::J2SSequence this
		(exprs (cons kar exprs))))
	  (duplicate::J2SSequence this
	     (exprs (list kar kdr)))))
   
   (assert-kont k KontExpr this)
   (with-access::J2SSequence this (exprs loc)
      (cond
	 ((null? exprs)
	  (kcall k this))
	 ((null? (cdr exprs))
	  (cps (car exprs) k r kbreaks kcontinues fun conf))
	 (else
	  (cps (car exprs)
	     (KontExpr (lambda (kar::J2SExpr)
			  (cps (duplicate::J2SSequence this
				  (exprs (cdr exprs)))
			     (KontExpr (lambda (kdr::J2SExpr)
					  (kcall k
					     (seqify this kar kdr)))
				this k)
			     r kbreaks kcontinues fun conf))
		this k)
	     r kbreaks kcontinues fun conf)))))
		
;*---------------------------------------------------------------------*/
;*    cps ::%J2STail ...                                               */
;*    -------------------------------------------------------------    */
;*    J2STail are introduced by the CPS conversion of loops.           */
;*---------------------------------------------------------------------*/
(define-method (cps this::%J2STail k r kbreaks kcontinues fun conf)
   (with-access::%J2STail this (loc expr)
      this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SNop ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SNop k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SStmtExpr ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SStmtExpr k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SStmtExpr this (loc expr)
      (cps expr
	 (KontExpr (lambda (kexpr::J2SExpr)
		      (kcall k (J2SStmtExpr kexpr)))
	    this k "expr")
	 r kbreaks kcontinues fun conf)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSeq ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSeq k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SSeq this (loc nodes)
      (if (null? nodes)
	  (kcall k this)
	  (let loop ((walk nodes))
	     (if (null? (cdr walk))
		 (cps (car walk)
		    k r kbreaks kcontinues fun conf)
		 (cps (car walk)
		    (KontStmt (lambda (n)
				 (SeqBlock this n (loop (cdr walk))))
		       this k)
		    r kbreaks kcontinues fun conf))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SIf ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SIf k r kbreaks kcontinues fun conf)
   
   (define (cps-if-tramp this tramp)
      (with-access::J2SIf this (loc test then else)
	 (let ((kont (KontStmt (lambda (n)
				  (J2SSeq n (J2SReturn #t tramp fun)))
			this k)))
	    (cps test
	       (KontExpr (lambda (ktest::J2SExpr)
			    (duplicate::J2SIf this
			       (test ktest)
			       (then (cps then kont r kbreaks kcontinues fun conf))
			       (else (cps else kont r kbreaks kcontinues fun conf))))
		  this k)
	       r kbreaks kcontinues fun conf))))
   
   (define (cps-if-cont this kbody)
      (with-access::J2SIf this (loc test then else)
	 (let* ((name (gensym '%kif))
		(kyield (J2SParam '(ref) '%yield :vtype 'any))
		(kthis (J2SParam '(ref) '%this :vtype 'any))
		(kfun (J2SArrowKont name (list kyield kthis)
			 (J2SBlock/w-endloc kbody)))
		(kdecl (J2SLetOpt '(call) name kfun))
		(kont (KontStmt (lambda (n)
				   (J2SSeq
				      n
				      (J2SReturn #t
					 (J2SKontCall kdecl)
					 fun)))
			 this k)))
	    (cps test
	       (KontExpr (lambda (ktest::J2SExpr)
			    (J2SLetRecBlock #f (list kdecl)
			       (duplicate::J2SIf this
				  (test ktest)
				  (then (cps then kont r kbreaks kcontinues fun conf))
				  (else (cps else kont r kbreaks kcontinues fun conf)))))
		  this k)
	       r kbreaks kcontinues fun conf))))
   
   (assert-kont k KontStmt this)
   (with-access::J2SIf this (loc test then else)
      (let* ((kbody (kcall k (J2SNop)))
	     (tramp (ktrampoline kbody)))
	 (if tramp
	     ;; optimized version without new continuation
	     (cps-if-tramp this tramp)
	     ;; normat
	     (cps-if-cont this kbody)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDecl ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDecl k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (kcall k this))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDeclInit ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDeclInit k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SDeclInit this (val)
      (if (not (j2s-let-opt? this))
	  (kcall k this)
	  (cps val
	     (KontExpr (lambda (kval::J2SExpr)
			  ;; must reuse the existing binding
			  ;; in order to preserve the AST inner pointers
			  (set! val kval)
			  (kcall k this))
		this k)
	     r kbreaks kcontinues fun conf))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SDeclFun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDeclFun k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SDeclFun this (val)
      (cps-fun! val r conf)
      (kcall k this)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SVarDecls ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SVarDecls k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SVarDecls this (decls)
      (map! (lambda (decl)
	       (cps decl k r kbreaks kcontinues fun conf))
	 decls)
      (kcall k this)))

;*---------------------------------------------------------------------*/
;*    cps ::j2SLetBlock ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SLetBlock k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SLetBlock this (rec loc endloc decls nodes)
      (cond
	 ((not rec)
	  (let loop ((decls decls))
	     (if (null? decls)
		 (cps (duplicate::J2SBlock this
			 (nodes nodes))
		    k r kbreaks kcontinues fun conf)
		 (with-access::J2SDecl (car decls) (scope)
		    (set! scope 'kont)
		    (cps (car decls)
		       (KontStmt (lambda (ndecl::J2SStmt)
				    (J2SLetRecBlock #f (list ndecl)
				       (loop (cdr decls))))
			  this k)
		       r kbreaks kcontinues fun conf)))))
	 ((not (any (lambda (d)
		       (or (not (isa? d J2SDeclInit))
			   (with-access::J2SDeclInit d (val)
			      (yield-expr? val kbreaks kcontinues))))
		  decls))
	  (J2SLetBlock decls
	     (cps (duplicate::J2SBlock this
		     (nodes nodes))
		k r kbreaks kcontinues fun conf)))
	 (else
	  (let* ((ndecls (map (lambda (d)
				 (if (isa? d J2SDeclInit)
				     (with-access::J2SDeclInit d (loc)
					(duplicate::J2SDeclInit d
					   (key (ast-decl-key))
					   (val (J2SUndefined))))
				     d))
			    decls))
		 (olds (filter (lambda (d) (isa? d J2SDeclInit)) decls))
		 (news (filter (lambda (d) (isa? d J2SDeclInit)) ndecls))
		 (assigs (filter-map (lambda (d nd)
					(when (isa? d J2SDeclInit)
					   (with-access::J2SDeclInit d (loc val)
					      (J2SStmtExpr
						 (J2SAssig (J2SRef nd)
						    (j2s-alpha val olds news))))))
			    decls ndecls))
		 (anodes (map (lambda (n) (j2s-alpha n olds news)) nodes)))
	     (cps (J2SLetRecBlock* #f ndecls (cons (J2SSeq* assigs) anodes))
		k r kbreaks kcontinues fun conf))))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SBindExit ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBindExit k r kbreaks kcontinues fun conf)
   
   (define (make-kont-decl loc k)
      (let* ((name (gensym '%kbind-exit))
	     (kyield (J2SParam '(ref) '%yield :vtype 'any))
	     (kthis (J2SParam '(ref) '%this :vtype 'any))
	     (kfun (J2SArrowKont name (list kyield kthis)
		      (J2SBlock/w-endloc (kcall k (J2SUndefined))))))
	 (J2SLetOpt '(call) name kfun)))
   
   (define (make-kont-fun-call loc decl)
      (J2SKontCall decl))

   (assert-kont k KontExpr this)
   (with-access::J2SBindExit this (stmt lbl loc)
      (cond
	 ((not (yield-expr? this kbreaks kcontinues))
	  (set! stmt (cps-fun! stmt r conf))
	  (kcall k this))
	 (lbl
	  ;; in order to inline code generator, the j2sreturn CPS
	  ;; transformation must know how to map its lbl to a contination
	  (error "cps" "generator cannot use inline expression" loc))
	 (else
	  (cps stmt
	     (KontStmt (lambda (kstmt::J2SStmt)
			  (set! stmt kstmt)
			  (kcall k this))
		this k)
	     r kbreaks kcontinues fun conf)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SFor ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SFor k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SFor this (loc init test incr body id)
      (cond
	 ((yield-expr? init kbreaks kcontinues)
	  (let ((i init))
	     (set! init (J2SNop))
	     (cps (J2SSeq (J2SStmtExpr i) this) k r kbreaks kcontinues fun conf)))
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
	  (let* ((fname (gensym '%kfor))
		 (bname (gensym '%kbreak))
		 (cname (gensym '%kcontinue))
		 (block (J2SBlock/w-endloc))
		 (fyield (J2SParam '(ref) '%yield :vtype 'any))
		 (fthis (J2SParam '(ref) '%this :vtype 'any))
		 (for (J2SArrowKont fname (list fyield fthis) block))
		 (decl (J2SLetOpt '(call) fname for))
		 (byield (J2SParam '(ref) '%yield :vtype 'any))
		 (bthis (J2SParam '(ref) '%this :vtype 'any))
		 (break (J2SArrowKont bname (list byield bthis)
			   (J2SBlock/w-endloc
			      (kcall k (J2SStmtExpr (J2SUndefined))))))
		 (fbody (J2SBlock/w-endloc
			   (J2SStmtExpr incr)
			   (%J2STail
			      (J2SKontCall decl)
			      fun)))
		 (cyield (J2SParam '(ref) '%yield :vtype 'any))
		 (cthis (J2SParam '(ref) '%this :vtype 'any))
		 (conti (J2SArrowKont cname (list cyield cthis)
			   (blockify fbody
			      (cps fbody
				 (KontStmt kid this k)
				 r kbreaks kcontinues fun conf))))
		 (bdecl (J2SLetOpt '(call) bname break))
		 (cdecl (J2SLetOpt '(call) cname conti))
		 (then (J2SBlock/w-endloc body
			  (%J2STail
			     (J2SKontCall cdecl)
			     fun)))
		 (stop (J2SBlock/w-endloc
			  (%J2STail
			     (J2SKontCall bdecl)
			     fun)))
		 (node (J2SIf test then stop)))
	     (with-access::J2SBlock block (nodes)
		(set! nodes
		   (list (cps node (KontStmt kid this k)
			    r
			    (cons (cons this bdecl) kbreaks)
			    (cons (cons this cdecl) kcontinues)
			    fun conf))))
	     (J2SLetBlock (list decl bdecl cdecl)
		(if (isa? init J2SExpr) (J2SStmtExpr init) init)
		(%J2STail
		   (J2SKontCall decl)
		   fun))))
	 (else
	  (set! init (cps-fun! init r conf))
	  (set! test (cps-fun! test r conf))
	  (set! incr (cps-fun! incr r conf))
	  (set! body (cps-fun! body r conf))
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SForIn ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SForIn k r kbreaks kcontinues fun conf)

   (define (cps-for-in this::J2SForIn)
      (with-access::J2SForIn this (loc lhs obj body)
	 (let* ((v (gensym '%kkeys))
		(l (gensym '%klen))
		(i (gensym '%ki))
		(keys (J2SLetOptVtype 'array '(ref) v
			 (J2SCall
			    (J2SAccess
			       (J2SUnresolvedRef 'Object)
			       (J2SString "keys"))
			    obj)))
		(len (J2SLetOpt '(ref) l
			(J2SAccess (J2SRef keys) (J2SString "length"))))
		(idx (J2SLetOpt '(assig ref) i (J2SNumber 0)))
		(for (J2SFor (J2SUndefined)
			(J2SBinary '< (J2SRef idx) (J2SRef len))
			(J2SPostfix '++ (J2SRef idx)
			   (J2SBinary '+ (J2SRef idx) (J2SNumber 1)))
			(J2SBlock/w-endloc
			   (J2SStmtExpr
			      (J2SAssig lhs
				 (J2SAccess (J2SRef keys) (J2SRef idx))))
			   body))))
	    (J2SLetBlock (list keys len idx)
	       (cps for k r kbreaks kcontinues fun conf)))))

   (define (cps-for-of this::J2SForIn)
      (with-access::J2SForIn this (loc op lhs obj body)
	 (let* ((o (gensym '%kobj))
		(f (gensym '%kfun))
		(i (gensym '%kit))
		(n (gensym '%knext))
		(v (gensym '%kval))
		(kobj (J2SLetOpt '(ref) o
			 obj))
		(kfun (J2SLetOpt '(ref) f
			 (J2SAccess (J2SRef kobj)
			    (J2SAccess
			       (J2SUnresolvedRef 'Symbol)
			       (J2SString "iterator")))))
		(kit (J2SLetOpt '(ref) i
			(J2SKontCall kfun (J2SRef kobj))))
		(knext (J2SLetOpt '(ref) n
			  (J2SAccess (J2SRef kit) (J2SString "next"))))
		(kval (J2SLetOpt '(ref assig) v
			 (J2SKontCall knext (J2SRef kit))))
		(for (J2SFor
			(J2SUndefined)
			(J2SUnary '! (J2SAccess (J2SRef kval) (J2SString "done")))
			(J2SAssig (J2SRef kval)
			   (J2SCall (J2SAccess (J2SRef kit) (J2SString "next"))))
			(J2SBlock/w-endloc
			   (J2SStmtExpr
			      (J2SAssig lhs
				 (J2SAccess (J2SRef kval) (J2SString "value"))))
			   body))))
	    (J2SLetBlock (list kobj kfun kit knext kval)
	       (cps for k r kbreaks kcontinues fun conf)))))
   
   (assert-kont k KontStmt this)
   (with-access::J2SForIn this (loc lhs obj body op)
      (cond
	 ((yield-expr? obj kbreaks kcontinues)
	  (cps obj
	     (KontExpr (lambda (kobj)
			  (cps (duplicate::J2SForIn this (obj kobj))
			     k r kbreaks kcontinues fun conf))
		this k)
	     r kbreaks kcontinues fun conf))
	 ((let* ((cell (cons this #t))
		 (kbreaks+ (cons cell kbreaks))
		 (kcontinues+ (cons cell kcontinues)))
	     (yield-expr? body kbreaks+ kcontinues+))
	  (if (eq? op 'in)
	      (cps-for-in this)
	      (cps-for-of this)))
	 (else
	  (set! lhs (cps-fun! lhs r conf))
	  (set! body (cps-fun! body r conf))
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SWhile ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SWhile k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SWhile this (test body loc)
      (let* ((kname (gensym '%kwhile))
	     (bname (gensym '%kbreak))
	     (cname (gensym '%kcontinue))
	     (block (J2SBlock/w-endloc))
	     (wyield (J2SParam '(ref) '%yield :vtype 'any))
	     (warg (J2SParam '(ref) '%this :vtype 'any))
	     (while (J2SArrowKont kname (list wyield warg) block))
	     (wdecl (J2SLetOpt '(call) kname while))
	     (barg (J2SParam '(ref) '%this :vtype 'any))
	     (byield (J2SParam '(ref) '%yield :vtype 'any))
	     (break (J2SArrowKont bname (list byield barg)
		       (J2SBlock/w-endloc
			  (kcall k (J2SStmtExpr (J2SUndefined))))))
	     (fbody (J2SBlock/w-endloc
		       (J2SReturn #t
			  (J2SKontCall wdecl)
			  fun)))
	     (cyield (J2SParam '(ref) '%yield :vtype 'any))
	     (carg (J2SParam '(ref) '%this :vtype 'any))
	     (conti (J2SArrowKont cname (list cyield carg) fbody))
	     (bdecl (J2SLetOpt '(call) bname break))
	     (cdecl (J2SLetOpt '(call) cname conti))
	     (then (J2SBlock/w-endloc body
		      (%J2STail
			 (J2SKontCall wdecl)
			 fun)))
	     (else (J2SBlock/w-endloc
		      (%J2STail
			 (J2SKontCall bdecl)
			 fun)))
	     (node (J2SIf test then else)))
	 (with-access::J2SBlock block (nodes)
	    (set! nodes
	       (list (cps node (KontStmt kid this k) r
			(cons (cons this bdecl) kbreaks)
			(cons (cons this cdecl) kcontinues)
			fun conf))))
	 (J2SLetBlock (list wdecl bdecl cdecl)
	    (J2SReturn #t
	       (J2SKontCall wdecl)
	       fun)))))
      
;*---------------------------------------------------------------------*/
;*    cps ::J2SDo ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SDo k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SDo this (test body loc)
      (let* ((name (gensym '%kdo))
	     (bname (gensym '%kbreak))
	     (cname (gensym '%kcontinue))
	     (tname (gensym '%ktmp))
	     (block (J2SBlock/w-endloc))
	     (wyield (J2SParam '(ref) '%yield :vtype 'any))
	     (wthis (J2SParam '(ref) '%this :vtype 'any))
	     (while (J2SArrowKont name (list wyield wthis) block))
	     (decl (J2SLetOpt '(call) name while))
	     (declv (J2SLetOpt '(assig ref) tname (J2SBool #t)))
	     (cyield (J2SParam '(ref) '%yield :vtype 'any))
	     (cthis (J2SParam '(ref) '%this :vtype 'any))
	     (conti (J2SArrowKont name (list cyield cthis)
		       (J2SBlock/w-endloc
			  (cps (J2SIf (J2SCond test
					 (J2SRef declv)
					 (J2SBool #f))
				  (%J2STail (J2SKontCall decl) fun)
				  (J2SNop))
			     k r kbreaks kcontinues fun conf))))
	     (cdecl (J2SLetOpt '(call) cname conti))
	     (byield (J2SParam '(ref) '%yield :vtype 'any))
	     (bthis (J2SParam '(ref) '%this :vtype 'any))
	     (break (J2SArrowKont name (list byield bthis)
		       (J2SBlock/w-endloc
			  (J2SStmtExpr
			     (J2SAssig (J2SRef declv) (J2SBool #f)))
			  (%J2STail (J2SKontCall cdecl) fun))))
	     (bdecl (J2SLetOpt '(call) bname break))
	     (else (J2SBlock/w-endloc
		      (%J2STail (J2SKontCall bdecl) fun)))
	     (sbody (J2SSeq body
		       (%J2STail (J2SKontCall cdecl) fun))))
	 (with-access::J2SBlock block (nodes)
	    (set! nodes
	       (list (cps sbody (KontStmt kid this k) r
			(cons (cons this bdecl) kbreaks)
			(cons (cons this cdecl) kcontinues)
			fun conf))))
	 (J2SLetBlock (list declv decl bdecl cdecl)
	    (J2SReturn #t (J2SKontCall decl) fun)))))
      
;*---------------------------------------------------------------------*/
;*    cps ::J2SBreak ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SBreak k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SBreak this (loc target)
      (cond
	 ((assq target kbreaks)
	  =>
	  (lambda (c)
	     (let ((kont (cdr c)))
		(J2SReturn #t (J2SKontCall kont) fun))))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SContinue ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SContinue k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SContinue this (loc target)
      (cond
	 ((assq target kcontinues)
	  =>
	  (lambda (c)
	     (let ((kont (cdr c)))
		(J2SReturn #t (J2SKontCall kont) fun))))
	 (else
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SAssig ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SAssig k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (with-access::J2SAssig this (lhs rhs)
      (cps lhs
	 (KontExpr (lambda (klhs::J2SExpr)
		      (cps rhs
			 (KontExpr (lambda (krhs::J2SExpr)
				      (kcall k (dup-assig this klhs krhs)))
			    this k)
			 r kbreaks kcontinues fun conf))
	    this k)
	 r kbreaks kcontinues fun conf)))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCall ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCall k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (with-access::J2SCall this ((callee fun) args)
      (cond
	 ((yield-expr? callee kbreaks kcontinues)
	  (cps callee
	     (KontExpr (lambda (kfun::J2SExpr)
			  (set! callee kfun)
			  (cps this k r kbreaks kcontinues fun conf))
		this k)
	     r kbreaks kcontinues fun conf))
	 ((any (lambda (e) (yield-expr? e kbreaks kcontinues)) args)
	  (cps-fun! callee r conf)
	  (cps* args
	     (KontExpr* (lambda (kargs::pair-nil)
			   (set! args kargs)
			   (cps this k r kbreaks kcontinues fun conf))
		args k)
	     r kbreaks kcontinues fun conf))
	 (else
	  (cps-fun! callee r conf)
	  (for-each (lambda (n) (cps-fun! n r conf)) args)
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SArray ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SArray k r kbreaks kcontinues fun conf)
   (assert-kont k KontExpr this)
   (with-access::J2SArray this (exprs)
      (if (any (lambda (e) (yield-expr? e kbreaks kcontinues)) exprs)
	  (cps* exprs
	     (KontExpr* (lambda (kexprs::pair-nil)
			   (set! exprs kexprs)
			   (cps this k r kbreaks kcontinues fun conf))
		exprs k)
	     r kbreaks kcontinues fun conf)
	  (begin
	     (set! exprs (map! (lambda (n) (cps-fun! n r conf)) exprs ))
	     (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SCond ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SCond k r kbreaks kcontinues fun conf)
   
   (define (make-kont-decl loc k)
      (assert-kont k KontExpr this)
      (let* ((name (gensym '%kcond))
	     (arg (J2SParam '(call ref) (gensym '%arg) :vtype 'any))
	     (kyield (J2SParam '(ref) '%yield :vtype 'any))
	     (kthis (J2SParam '(ref) '%this :vtype 'any))
	     (kfun (J2SArrowKont name (list kyield kthis arg)
		      (J2SBlock/w-endloc (kcall k (J2SRef arg))))))
	 (J2SLetOpt '(call) name kfun)))
   
   (define (make-kont-fun-call loc decl expr)
      (J2SKontCall decl (J2SHopRef '%gen) expr))
   
   (assert-kont k KontExpr this)
   (with-access::J2SCond this (test then else loc)
      (cond
	 ((yield-expr? test kbreaks kcontinues)
	  (cps test
	     (KontExpr (lambda (ktest::J2SExpr)
			  (set! test ktest)
			  (cps this k r kbreaks kcontinues fun conf))
		this k "test")
	     r kbreaks kcontinues fun conf))
	 ((or (yield-expr? then kbreaks kcontinues)
	      (yield-expr? else kbreaks kcontinues))
	  (let* ((decl (make-kont-decl loc k))
		 (kc (lambda (b)
			(J2SReturn #t 
			   (make-kont-fun-call loc decl b)
			   fun)))
		 (kif (J2SIf (cps-fun! test r conf)
			 (cps then
			    (KontExpr kc this k)
			    r kbreaks kcontinues fun conf)
			 (cps else
			    (KontExpr kc this k)
			    r kbreaks kcontinues fun conf))))
	     (J2SLetBlock (list decl) kif)))
	 (else
	  (set! test (cps-fun! test r conf))
	  (set! then (cps-fun! then r conf))
	  (set! else (cps-fun! else r conf))
	  (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2STry ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2STry k r kbreaks kcontinues fun conf)
   
   (define (Catch loc declc::J2SDecl param)
      (J2SCatch param
	 (J2SBlock/w-endloc
	    (%J2STail
	       (J2SKontCall declc (J2SHopRef '%gen) (J2SRef param))
	       fun))))
   
   (define (FinallyCatch finally k r kbreaks kcontinues fun)
      (with-access::J2SNode finally (loc)
	 (let ((eparam (J2SParam '(ref) (gensym '%exc) :vtype 'any)))
	    (J2SCatch eparam
	       (cps finally
		  (KontStmt (lambda (n)
			       (J2SSeq n
				  (J2SThrow (J2SRef eparam))))
		     this k)
		  r kbreaks kcontinues fun conf)))))

   (define (FinallyCatchThrow finally k r kbreaks kcontinues fun)
      (with-access::J2SNode finally (loc)
	 (let ((eparam (J2SParam '(ref) (gensym '%exc) :vtype 'any)))
	    (J2SCatch eparam
	       (cps finally
		  (KontStmt (lambda (n)
			       (J2SSeq n
				  (J2SThrow (J2SRef eparam))))
		     this k)
		  r kbreaks kcontinues fun conf)))))
   
   (define (cps-try-catch this body catch loc)
      ;; try/catch only
      (let* ((cname (gensym '%kcatch))
	     (cyield (J2SParam '(ref) '%yield :vtype 'any))
	     (cthis (J2SParam '(ref) '%this :vtype 'any))
	     (catch (with-access::J2SCatch catch (param (cbody body))
		       (J2SArrowKont cname (list cyield cthis param)
			  (J2SBlock/w-endloc
			     (cps cbody
				k r kbreaks kcontinues fun conf)))))
	     (declc (J2SLetOpt '(call) cname catch))
	     (eparam (J2SParam '(ref) (gensym '%exc) :vtype 'any)))
	 (J2SLetBlock (list declc)
	    (J2STry
	       (blockify body
		  (cps body k
		     (lambda (n)
			(r (J2STry (blockify body n)
			      (Catch loc declc eparam))))
		     kbreaks kcontinues fun conf))
	       (Catch loc declc eparam)))))

   (define (cps-try-finally-gen this body finally loc)
      (let* ((gen (J2SFun* (gensym '%trybody) '() (J2SBlock/w-endloc)))
	     (mark (J2SLetOpt '(ref) (gensym '%mark) (J2SPragma '(cons #f #f))))
	     (gbody (J2SBlock/w-endloc body (J2SReturn #t (J2SRef mark))))
	     (declg (J2SLetOpt '(ref assig) (gensym '%gen) (J2SCall gen)))
	     (gval (J2SLetOpt '(ref assig) (gensym '%yield*)
		      (J2SYield (J2SRef declg) gen))))
	 (with-access::J2SFun gen (body)
	    (set! body gbody))
	 (J2STry
	    (cps (J2SLetRecBlock #f (list mark declg)
		    (J2SLetRecBlock #f (list gval)
		       finally
		       (J2SIf (J2SBinary/type '!== 'bool (J2SRef gval) (J2SRef mark))
			  (J2SReturn #f (J2SRef gval))
			  (J2SNop))))
	       k
	       (lambda (n)
		    (r (J2STry (blockify body n)
			  (FinallyCatch finally k r kbreaks kcontinues fun))))
	       kbreaks kcontinues fun conf)
	    (FinallyCatch finally k r kbreaks kcontinues fun))))
   
   (define (cps-try-finally-explicit this body finally loc)
      (let* ((gen (J2SFun* (gensym '%gen) '() (J2SBlock/w-endloc)))
	     (gbody (J2SBlock/w-endloc body (J2SReturn #t (J2SUndefined) gen)))
	     (declg (J2SLetOpt '(ref assig) (gensym '%gen) (J2SCall gen)))
	     (decln (J2SLetOpt '(ref assig) (gensym '%next)
		       (J2SCall (J2SAccess (J2SRef declg) (J2SString "next"))))))
	 (with-access::J2SFun gen (body)
	    (set! body gbody))
	 (J2STry
	    (cps (J2SLetBlock (list declg)
		    (J2SLetBlock (list decln)
		       (J2SWhile (J2SUnary/type '! 'bool
				    (J2SAccess (J2SRef decln) (J2SString "done")))
			  (J2SSeq
			     (J2SStmtExpr
				(J2SYield
				   (J2SAccess (J2SRef decln) (J2SString "value"))
				   #f))
			     (J2SStmtExpr
				(J2SAssig (J2SRef decln)
				   (J2SCall (J2SAccess (J2SRef declg) (J2SString "next")))))))
		       (J2SSeq
			  finally
			  (J2SAccess (J2SRef decln) (J2SString "value")))))
	       k r kbreaks kcontinues fun conf)
	    (FinallyCatch finally k r kbreaks kcontinues fun))))

   (define (cps-try-finally-tmp this body finally loc)
      (J2STry
	 (blockify body
	    (cps (J2SSeq body finally) k
	       (lambda (n)
		  (r (J2STry (blockify body n)
			(FinallyCatch finally k r kbreaks kcontinues fun)
			(cps finally k r kbreaks kcontinues fun conf))))
	       kbreaks kcontinues fun conf))
	 (FinallyCatch finally k r kbreaks kcontinues fun)
	 (cps finally k r kbreaks kcontinues fun conf)))

   (define (cps-try-finally-throw this body finally loc)
      (let ((eparam (J2SParam '(ref) (gensym '%exc) :vtype 'any))
	    (ret (J2SLetOpt '(ref assig) (gensym '%ret) (J2SBool #f)))
	    (exn (J2SLetOpt '(ref assig) (gensym '%exn) (J2SBool #f)))
	    (val (J2SLetOpt '(ref assig) (gensym '%val) (J2SUndefined))))
	 (cps (J2SLetBlock (list ret exn val)
		 (J2STry
		    (retthrow! body ret '())
		    (J2SCatch eparam
		       (J2SBlock/w-endloc
			  (J2SStmtExpr
			     (J2SAssig (J2SRef val) (J2SRef eparam)))
			  (if (J2SUnary/type '! 'bool (J2SRef ret))
			      (J2SStmtExpr
				 (J2SAssig (J2SRef exn) (J2SBool #t)))
			      (J2SNop)))))
		 finally
		 (J2SIf (J2SRef ret) (J2SReturn #f (J2SRef val) fun) (J2SNop))
		 (J2SIf (J2SRef exn) (J2SThrow (J2SRef val)) (J2SNop)))
	    k r kbreaks kcontinues fun conf)))

   (define (cps-try-catch-finally this body catch finally loc)
      ;; try/catch/finally
      (cps (J2STry
	      (blockify body
		 (J2STry body catch (J2SNop)))
	      (J2SNop)
	      finally)
	 k r kbreaks kcontinues fun conf))
   
   (assert-kont k KontStmt this)
   (with-access::J2STry this (loc body catch finally)
      (cond
	 ((and (isa? catch J2SNop) (isa? finally J2SNop))
	  (cps body k r kbreaks kcontinues fun conf))
	 ((isa? finally J2SNop)
	  (cps-try-catch this body catch loc))
	 ((isa? catch J2SNop)
	  (cps-try-finally-throw this body finally loc))
	 (else
	  (cps-try-catch-finally this body catch finally loc)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SThrow ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SThrow k r kbreaks kcontinues fun conf)
   (assert-kont k KontStmt this)
   (with-access::J2SThrow this (loc expr)
      (if (yield-expr? expr kbreaks kcontinues)
	  (cps expr
	     (KontExpr (lambda (kexpr::J2SExpr)
			  (kcall k (J2SThrow kexpr)))
		this k)
	     r kbreaks kcontinues fun conf)
	  (begin
	     (set! expr (cps-fun! expr r conf))
	     (kcall k this)))))

;*---------------------------------------------------------------------*/
;*    cps ::J2SSwitch ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cps this::J2SSwitch k r kbreaks kcontinues fun conf)
   
   (define (switch->if key tmp clause)
      (with-access::J2SCase clause (loc expr body)
	 (if (isa? clause J2SDefault)
	     body
	     (J2SIf (J2SBinary 'OR
		       (J2SRef tmp) (J2SBinary '=== (J2SRef key) expr))
		(J2SBlock/w-endloc
		   (J2SStmtExpr (J2SAssig (J2SRef tmp) (J2SBool #t)))
		   body)
		(J2SNop)))))
   
   (assert-kont k KontStmt this)
   (with-access::J2SSwitch this (loc key cases need-bind-exit-break)
      (cond
	 ((yield-expr? key kbreaks kcontinues)
	  (cps key
	     (KontExpr (lambda (kkey::J2SExpr)
			  (with-access::J2SSwitch this (key)
			     ;; if here a duplication is preferred to
			     ;; a mutation, don't forget to update the
			     ;; kbreaks set otherwise J2SBreak will be
			     ;; badly compiled
			     (set! key kkey)
			     (cps this k r kbreaks kcontinues fun conf)))
		this k)
	     r kbreaks kcontinues fun conf))
	 ((not (any (lambda (c) (yield-expr? c kbreaks kcontinues)) cases))
	  (set! key (cps-fun! key r conf))
	  (for-each (lambda (clause)
		       (with-access::J2SCase clause (expr body)
			  (set! expr (cps-fun! expr r conf))
			  (set! body (cps-fun! body r conf))))
	     cases)
	  (kcall k this))
	 (else
	  (set! key (cps-fun! key r conf))
	  (let* ((v (gensym '%kkey))
		 (t (gensym '%ktmp))
		 (key (J2SLetOpt '(ref) v key))
		 (tmp (J2SLetOpt '(assig ref) t (J2SBool #f)))
		 (seq (J2SSeq*
			 (map (lambda (clause)
				 (switch->if key tmp clause))
			    cases))))
	     (if need-bind-exit-break
		 (let* ((bname (gensym '%kbreak))
			(byield (J2SParam '(ref) '%yield :vtype 'any))
			(bthis (J2SParam '(ref) '%this :vtype 'any))
			(break (J2SArrowKont bname (list byield bthis)
				  (J2SBlock/w-endloc
				     (J2SBlock/w-endloc
					(kcall k
					   (J2SStmtExpr (J2SUndefined)))))))
			(bdecl (J2SLetOpt '(call) bname break)))
		    (J2SLetBlock (list key tmp bdecl)
		       (cps seq k r
			  (cons (cons this bdecl) kbreaks)
			  kcontinues fun conf)))
		 (J2SLetBlock (list key tmp)
		    (cps seq k r kbreaks kcontinues fun conf))))))))

;*---------------------------------------------------------------------*/
;*    has-yield*? ...                                                  */
;*---------------------------------------------------------------------*/
(define (has-yield*? this)
   (any (lambda (n)
	   (cond
	      ((isa? n J2SYield)
	       (with-access::J2SYield n (generator)
		  generator))
	      ((isa? n J2SReturnYield)
	       (with-access::J2SReturnYield n (generator)
		  generator))))
      (yield-expr* this '() '() '())))
   
;*---------------------------------------------------------------------*/
;*    yield-expr? ...                                                  */
;*---------------------------------------------------------------------*/
(define (yield-expr? this kbreaks kcontinues)
   (pair? (yield-expr* this kbreaks kcontinues '())))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SNode ...                                        */
;*    -------------------------------------------------------------    */
;*    Returns #t iff a statement contains a YIELD. Otherwise           */
;*    returns #f.                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SNode kbreaks kcontinues localrets)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SYield ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SYield kbreaks kcontinues localrets)
   (list this))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SReturn ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SReturn kbreaks kcontinues localrets)
   (with-access::J2SReturn this (from)
      (cond
	 ((memq from localrets)
	  '())
	 (else
	  (list this)))))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SReturnYield ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SReturnYield kbreaks kcontinues localrets)
   (list this))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SBindExit ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SBindExit kbreaks kcontinues localrets)
   (with-access::J2SBindExit this (stmt)
      (yield-expr* stmt kbreaks kcontinues (cons this localrets))))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SBreak ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SBreak kbreaks kcontinues localrets)
   (with-access::J2SBreak this (target)
      (if (assq target kbreaks)
	  (list this)
	  '())))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SContinue ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SContinue kbreaks kcontinues localrets)
   (with-access::J2SContinue this (target)
      (if (assq target kcontinues)
	  (list this)
	  '())))

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SFun kbreaks kcontinues localrets)
   '())

;*---------------------------------------------------------------------*/
;*    yield-expr* ::J2SCase ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (yield-expr* this::J2SCase kbreaks kcontinues localrets)
   (with-access::J2SCase this (expr body)
      (append (yield-expr* expr kbreaks kcontinues localrets)
	 (yield-expr* body kbreaks kcontinues localrets))))

;*---------------------------------------------------------------------*/
;*    retthrow! ::J2SNode ...                                          */
;*    -------------------------------------------------------------    */
;*    Replace untail return (those of the inlined function) with       */
;*    an exit.                                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (retthrow! this::J2SNode decl env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    retthrow! ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (retthrow! this::J2SReturn decl env)
   (with-access::J2SReturn this (tail exit from expr loc from)
      (if (and (not exit) (not (memq from env)))
	  (J2SSeq
	     (J2SStmtExpr (J2SAssig (J2SRef decl) (J2SBool #t)))
	     (J2SThrow (retthrow! expr decl env))
	     this)
	  (begin
	     (set! expr (retthrow! expr decl env))
	     this))))

;*---------------------------------------------------------------------*/
;*    retthrow! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (retthrow! this::J2SFun decl env)
   this)

;*---------------------------------------------------------------------*/
;*    retthrow! ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (retthrow! this::J2SMethod decl env)
   this)


;*---------------------------------------------------------------------*/
;*    collect-temps* ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-temps* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    is-global-or-fun? ...                                            */
;*---------------------------------------------------------------------*/
(define (is-global-or-fun? decl)

   (define (is-fun-decl? decl)
      (when (isa? decl J2SDeclInit)
	 (with-access::J2SDeclInit decl (val)
	    (when (isa? val J2SFun)
	       (not (decl-usage-has? decl '(assig)))))))
   
   (with-access::J2SDecl decl (scope)
      (or (isa? decl J2SDeclExtern)
	  (memq scope '(%scope global tls))
	  (is-fun-decl? decl))))

;*---------------------------------------------------------------------*/
;*    collect-temps* ::J2SRef ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-temps* this::J2SRef)
   (with-access::J2SRef this (decl loc)
      (if (is-global-or-fun? decl)
	  '()
	  (list decl))))

;*---------------------------------------------------------------------*/
;*    mark-def! ...                                                    */
;*---------------------------------------------------------------------*/
(define (mark-def!::J2SDecl d)
   (with-access::J2SDecl d (%info)
      (when (isa? %info KDeclInfo)
	 (with-access::KDeclInfo %info (def)
	    (set! def #t))))
   d)

;*---------------------------------------------------------------------*/
;*    mark-ignored! ...                                                */
;*---------------------------------------------------------------------*/
(define (mark-ignored!::J2SDecl d)
   (with-access::J2SDecl d (%info)
      (set! %info #f))
   d)

;*---------------------------------------------------------------------*/
;*    is-free? ...                                                     */
;*---------------------------------------------------------------------*/
(define (is-free? d)
   (when (isa? d J2SDecl)
      (with-access::J2SDecl d (%info)
	 (when (isa? %info KDeclInfo)
	    (with-access::KDeclInfo %info (free)
	       free)))))
   
;*---------------------------------------------------------------------*/
;*    is-def? ...                                                     */
;*---------------------------------------------------------------------*/
(define (is-def? d)
   (when (isa? d J2SDecl)
      (with-access::J2SDecl d (%info)
	 (when (isa? %info KDeclInfo)
	    (with-access::KDeclInfo %info (def)
	       def)))))
   
;*---------------------------------------------------------------------*/
;*    mark-free ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-free this::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark-free ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-free this::J2SRef env)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (scope %info id)
	 (unless (is-global-or-fun? decl)
	    (with-access::KDeclInfo %info (free)
	       (unless (or free (memq decl env))
		  (set! free #t)))))))

;*---------------------------------------------------------------------*/
;*    mark-free ::J2SKont ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-free this::J2SKont env)
   (with-access::J2SKont this (body param exn loc)
      (let ((nenv (list (mark-def! param) (mark-def! exn))))
	 (mark-free body nenv))))

;*---------------------------------------------------------------------*/
;*    mark-free ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-free this::J2SFun env)
   (with-access::J2SFun this (decl params thisp argumentsp body)
      (let* ((envp (map! mark-def! params))
	     (enva (if argumentsp (cons (mark-def! argumentsp) envp) envp))
	     (envt (if thisp (cons (mark-def! thisp) enva) enva))
	     (nenv (if decl (cons (mark-def! decl) envt) envt)))
	 (mark-free body nenv))))

;*---------------------------------------------------------------------*/
;*    mark-free ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-free this::J2SLetBlock env)
   (with-access::J2SLetBlock this (decls nodes)
      (let ((nenv (append (map! mark-def! decls) env)))
	 (for-each (lambda (d)
		      (when (isa? d J2SDeclInit)
			 (with-access::J2SDeclInit d (val)
			    (mark-free val nenv))))
	    decls)
	 (for-each (lambda (node) (mark-free node nenv)) nodes))))
   
;*---------------------------------------------------------------------*/
;*    mark-free ::J2SBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-free this::J2SBlock env)
   (with-access::J2SBlock this (nodes)
      (let loop ((env env)
		 (nodes nodes))
	 (when (pair? nodes)
	    (let ((n (car nodes)))
	       (cond
		  ((isa? n J2SDeclInit)
		   (with-access::J2SDeclInit n (val)
		      (let ((nenv (cons (mark-def! n) env)))
			 (mark-free val nenv)
			 (loop nenv (cdr nodes)))))
		  ((isa? n J2SDecl)
		   (let ((nenv (cons (mark-def! n) env)))
		      (loop nenv (cdr nodes))))
		  (else
		   (mark-free n env)
		   (loop env (cdr nodes)))))))))

;*---------------------------------------------------------------------*/
;*    mark-free ::J2SCatch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (mark-free this::J2SCatch env)
   (with-access::J2SCatch this (param body)
      (let ((nenv (cons (mark-def! param) env)))
	 (mark-free body nenv))))

;*---------------------------------------------------------------------*/
;*    kont-color ...                                                   */
;*---------------------------------------------------------------------*/
(define (kont-color::long use has-yield*::bool)
   (let ((len (length use)))
      (for-each (lambda (d c)
		   (with-access::J2SDecl d (id %info)
		      (with-access::KDeclInfo %info (color)
			 (set! color c))))
	 use (iota len (if has-yield* 2 0)))
      (+fx len (if has-yield* 2 0))))
		   
;*---------------------------------------------------------------------*/
;*    alloc-temp ...                                                   */
;*---------------------------------------------------------------------*/
(define (alloc-temp p)
   (when (isa? p J2SDecl)
      (with-access::J2SDecl p (%info loc id)
	 (when (isa? %info KDeclInfo)
	    (with-access::KDeclInfo %info (free color)
	       (when free
		  (when (=fx color -1)
		     (error "alloc-temp" (format "wrong color (~s)" id) loc))
		  (J2SStmtExpr
		     (J2SAssig
			(J2SKontRef '%gen color id)
			(J2SRef p)))))))))

;*---------------------------------------------------------------------*/
;*    kont-alloc-temp! ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (kont-alloc-temp! this::J2SNode extra)
   (with-trace 'cps (typeof this)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    kont-alloc-temp! ::J2SKont ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (kont-alloc-temp! this::J2SKont extra)
   (with-access::J2SKont this (param exn body loc)
      (if (or (is-free? param) (is-free? exn))
	  (begin
	     (set! body
		(J2SBlock*/w-endloc
		   (append
		      (filter-map alloc-temp (list param exn))
		      (list (kont-alloc-temp! body extra)))))
	     this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    kont-alloc-temp! ::J2SFun ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (kont-alloc-temp! this::J2SFun extra)
   (with-access::J2SFun this (generator body decl thisp params argumentsp %info loc name)
      (with-trace 'cps "j2sfun"
	 (let ((temps (append (if generator extra '())
			 (cons* decl thisp argumentsp params))))
	    (tprint "   FUN=" name " " generator " tmps="
	       (filter-map (lambda (p)
			      (when (isa? p J2SDecl)
				 (with-access::J2SDecl p (id %info)
				    (when (isa? %info KDeclInfo)
				       id))))
		  temps))
	    (if (any is-free? temps)
		(with-access::J2SBlock body (endloc)
		   (set! body
		      (J2SBlock
			 ;; this block will have to be move before the
			 ;; generator is created, see scheme-fun
			 (J2SBlock* (filter-map alloc-temp temps))
			 (kont-alloc-temp! body extra)))
		   this)
		(call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    kont-alloc-temp! ::J2SRef ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (kont-alloc-temp! this::J2SRef extra)
   (with-trace 'cps (typeof this)
      (with-access::J2SRef this (decl loc)
	 (with-access::J2SDecl decl (%info id)
	    (if (isa? %info KDeclInfo)
		(with-access::KDeclInfo %info (color free)
		   (if free
		       (begin
			  (when (=fx color -1)
			     (error "kont-alloc-temp!"
				(format "wrong color (~s)" id) loc))
			  (J2SKontRef '%gen color id))
		       this))
		this)))))

;*---------------------------------------------------------------------*/
;*    kont-alloc-temp! ::J2SDeclInit ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (kont-alloc-temp! this::J2SDeclInit extra)
   (with-trace 'cps (typeof this)
      (with-access::J2SDeclInit this (val %info loc id)
	 (trace-item "val=" (typeof val))
	 (set! val (kont-alloc-temp! val extra))
	 (when (isa? %info KDeclInfo)
	    (with-access::KDeclInfo %info (color free)
	       (when free
		  (when (=fx color -1)
		     (error "kont-alloc-temp!"
			(format "wrong color (~s)" id) loc))
		  (set! val
		     (J2SSequence
			(J2SAssig (J2SKontRef '%gen color id) val)
			(J2SUndefined))))))
	 this)))
