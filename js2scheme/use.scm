;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/use.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Mon Feb 12 20:41:05 2018 (serrano)                */
;*    Copyright   :  2013-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Count the number of occurrences for all variables                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_use

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-use-stage
	   j2s-dead-stage
	   (generic reset-use-count ::J2SNode)
	   (generic use-count ::J2SNode inc::int inloop::bool)
	   (filter-dead-declarations::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    j2s-use-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-use-stage
   (instantiate::J2SStageProc
      (name "use")
      (comment "Usage property for all variables")
      (proc j2s-use!)
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    j2s-dead-stage ...                                               */
;*---------------------------------------------------------------------*/
(define j2s-dead-stage
   (instantiate::J2SStageProc
      (name "dead")
      (comment "Removed dead variables")
      (proc j2s-dead!)))

;*---------------------------------------------------------------------*/
;*    j2s-use! ::J2SProgram ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-use! this::J2SProgram args)
   
   (define deval
      (make-cell #f))
   
   (define (use-nodes nodes)
      (for-each (lambda (o)
		   (use-count o +1 #f)
		   (usage o 'ref deval #f))
	 nodes))
   
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls direct-eval)
	 (use-nodes headers)
	 (use-nodes decls)
	 (use-nodes nodes)
	 (if (cell-ref deval)
	     (for-each (lambda (d::J2SDecl)
			  (with-access::J2SDecl d (usage)
			     (set! usage (cons 'eval usage))))
		decls)
	     (begin
		;; it might be valuable to count the number of direct
		;; eval calls instead of using a simple boolean.
		(set! direct-eval #f)
		(j2s-dead! this args)))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-dead! ::J2SProgram ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-dead! this::J2SProgram args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls direct-eval)
	 (unless direct-eval
	    (let ((optim (memq :optim args)))
	       (when (and (pair? optim) (>=fx (cadr optim) 1))
		  ;; remove dead declaration
		  (set! decls (filter-dead-declarations decls)))))
	 (for-each dead-code! decls)
	 (for-each dead-code! nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    filter-dead-declarations ...                                     */
;*---------------------------------------------------------------------*/
(define (filter-dead-declarations decls)
   (let ((keep #t))
      (let loop ()
	 (when keep
	    (set! keep #f)
	    (set! decls
	       (filter (lambda (d::J2SDecl)
			  (with-access::J2SDecl d (usecnt)
			     (or (>fx usecnt 0)
				 (and (isa? d J2SDeclInit)
				      (with-access::J2SDeclInit d (val)
					 (or (isa? val J2SSvc)
					     (and (not (isa? val J2SLiteral))
						  (not (isa? val J2SFun)))
					     (isa? val J2SArray))))
				 (begin
				    (set! keep #t)
				    (when (isa? d J2SDeclInit)
				       (with-access::J2SDeclInit d (val)
					  (use-count val -1 #f)))
				    #f))))
		  decls))
	    (loop)))
      decls))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SNode inc inloop)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SFun inc inloop)
   (with-access::J2SFun this (params body decl)
      (use-count body inc #f))
   this)
   
;*---------------------------------------------------------------------*/
;*    use-count ::J2SSvc ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SSvc inc inloop)
   (with-access::J2SSvc this (params body decl)
      (use-count body inc #f))
   this)
   
;*---------------------------------------------------------------------*/
;*    use-count ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SRef inc inloop)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usecnt useinloop)
	 (when inloop (set! useinloop #t))
	 (set! usecnt (+fx inc usecnt))))
   this)

;*---------------------------------------------------------------------*/
;*    use-count ::J2SGlobalRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SGlobalRef inc inloop)
   (with-access::J2SGlobalRef this (decl)
      (with-access::J2SDecl decl (usecnt useinloop)
	 (when inloop (set! useinloop #t))
	 (set! usecnt (+fx inc usecnt))))
   this)

;*---------------------------------------------------------------------*/
;*    use-count ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SFor inc inloop)
   (with-access::J2SFor this (init test incr body)
      (use-count init inc #f)
      (use-count test inc #t)
      (use-count incr inc #t)
      (use-count body inc #t)))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SWhile inc inloop)
   (with-access::J2SWhile this (test body)
      (use-count test inc #t)
      (use-count body inc #t)))
      
;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SRef)
   (with-access::J2SRef this (decl)
      (reset-use-count decl))
   this)

;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SFun ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SFun)
   (with-access::J2SFun this (thisp)
      (when thisp (reset-use-count thisp)))
   (call-default-walker))
      
;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SGlobalRef ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SGlobalRef)
   (with-access::J2SGlobalRef this (decl)
      (reset-use-count decl))
   this)

;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SDecl ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SDecl)
   (with-access::J2SDecl this (usecnt)
      (set! usecnt 0))
   this)

;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SDeclInit ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SDeclInit)
   (with-access::J2SDeclInit this (usecnt val)
      (when (>fx usecnt 0)
	 (set! usecnt 0)
	 (reset-use-count val)))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SNode ctx deval infun)
   (default-walk this ctx deval infun))

;*---------------------------------------------------------------------*/
;*    usage ::J2SExpr ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SExpr ctx deval infun)
   (default-walk this 'ref deval infun))
      
;*---------------------------------------------------------------------*/
;*    usage ::J2SRef ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SRef ctx deval infun)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usage %info useinfun id)
	 (unless (eq? infun %info)
	    (set! useinfun #t))
	 (when ctx
	    (unless (memq ctx usage)
	       (set! usage (cons ctx usage))))))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SUnresolvedRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SUnresolvedRef ctx deval infun)
   (call-default-walker)
   (with-access::J2SUnresolvedRef this (id)
      (when (and (eq? id 'eval) (eq? ctx 'call))
	 (cell-set! deval #t)))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SGlobalRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SGlobalRef ctx deval infun)
   (call-next-method)
   (with-access::J2SGlobalRef this (decl)
      (with-access::J2SDecl decl (usage)
	 (when ctx
	    (unless (memq ctx usage)
	       (set! usage (cons ctx usage))))))
   this)

;*---------------------------------------------------------------------*/
;*    ronly! ::J2SUnary ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SUnary ctx deval infun)
   (with-access::J2SUnary this (op expr)
      (cond
	 ((and (eq? op 'delete) (isa? expr J2SRef))
	  (with-access::J2SRef expr (decl)
	     (with-access::J2SDecl decl (usage)
		(unless (memq 'delete usage)
		   (set! usage (cons 'delete usage))))
	     this))
	 ((and (eq? op 'delete) (isa? expr J2SAccess))
	  (with-access::J2SAccess expr (obj field)
	     (if (isa? obj J2SRef)
		 (with-access::J2SRef obj (decl)
		    (with-access::J2SDecl decl (usage)
		       (unless (memq 'delete usage)
			  (set! usage (cons 'delete usage)))))
		 (usage obj 'ref deval infun))
	     (usage field 'ctx deval infun)
	     this))
	 ((and (eq? op 'delete) (isa? expr J2SGlobalRef))
	  (with-access::J2SGlobalRef expr (decl)
	     (with-access::J2SDecl decl (usage)
		(unless (memq 'delete usage)
		   (set! usage (cons 'delete usage))))
	     this))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    usage ::J2SCall ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SCall ctx deval infun)
   (with-access::J2SCall this (fun args)
      (usage fun 'call deval infun)
      (for-each (lambda (a) (usage a 'ref deval infun)) args))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SParen ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SParen ctx deval infun)
   (with-access::J2SParen this (expr)
      (usage expr ctx deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SAssig ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SAssig ctx deval infun)
   (with-access::J2SAssig this (lhs rhs)
      (usage lhs 'assig deval infun)
      (usage rhs 'ref deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SInit ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SInit ctx deval infun)
   (with-access::J2SInit this (lhs rhs)
      (usage lhs 'init deval infun)
      (usage rhs 'ref deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SAccess ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SAccess ctx deval infun)
   (with-access::J2SAccess this (obj field)
      (usage obj (if (eq? ctx 'assig) 'set 'get) deval infun)
      (usage field 'ref deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SNew ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SNew ctx deval infun)
   (with-access::J2SNew this (clazz args)
      (usage clazz 'new deval infun)
      (for-each (lambda (a) (usage a 'ref deval infun)) args))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SLetBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SLetBlock ctx deval infun)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d)
		   (with-access::J2SDecl d (%info) (set! %info infun)))
	 decls)
      (for-each (lambda (d) (usage d 'init deval infun)) decls)
      (for-each (lambda (n) (usage n 'ref deval infun)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SDeclInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SDeclInit ctx deval infun)
   (with-access::J2SDeclInit this ((u usage) val)
      (usage val 'ref deval infun)
      (set! u (cons 'init u)))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SReturn ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SReturn ctx deval infun)
   (with-access::J2SReturn this (expr)
      (usage expr 'ref deval infun)
      this))

;*---------------------------------------------------------------------*/
;*    usage ::J2SFun ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SFun ctx deval infun)
   (with-access::J2SFun this (params body)
      (for-each (lambda (p)
		   (with-access::J2SDecl p (%info)
		      (set! %info this)))
	 params)
      (usage body 'ref deval this)))

;*---------------------------------------------------------------------*/
;*    dead-code! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-code! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    dead-code! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-code! this::J2SFun)
   (with-access::J2SFun this (params body decl)
      (when (isa? decl J2SDecl)
	 (with-access::J2SDecl decl (usecnt)
	    (when (=fx usecnt 0) (set! decl #f)))))
   this)
   
;*---------------------------------------------------------------------*/
;*    dead-code! ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-code! this::J2SDecl)
   (call-default-walker)
   (with-access::J2SDecl this (usecnt loc binder)
      (if (and (=fx usecnt 0) (not (eq? binder 'param)))
	  (instantiate::J2SNop (loc loc))
	  this)))

;*---------------------------------------------------------------------*/
;*    dead-code! ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-code! this::J2SDeclInit)
   (call-default-walker)
   (with-access::J2SDeclInit this (usecnt loc binder val)
      (if (and (=fx usecnt 0) (isa? val J2SFun))
	  (instantiate::J2SNop (loc loc))
	  this)))

;*---------------------------------------------------------------------*/
;*    dead-code! ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-code! this::J2SLetBlock)
   (call-default-walker)
   (with-access::J2SLetBlock this (decls)
      (set! decls (filter (lambda (n) (isa? n J2SDecl)) decls))
      this))

;*---------------------------------------------------------------------*/
;*    dead-code! ::J2SVarDecls ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-code! this::J2SVarDecls)
   (call-default-walker)
   (with-access::J2SVarDecls this (decls)
      (set! decls (filter (lambda (n) (isa? n J2SDecl)) decls))
      this))

