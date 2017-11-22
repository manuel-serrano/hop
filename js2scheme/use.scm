;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/use.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Wed Nov 22 09:18:44 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
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
	   (generic use-count ::J2SNode inc::int)
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
		   (use-count o +1)
		   (usage o 'ref deval))
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
					  (use-count val -1)))
				    #f))))
		  decls))
	    (loop)))
      decls))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SNode inc)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SFun inc)
   (with-access::J2SFun this (params body decl)
      (use-count body inc))
   this)
   
;*---------------------------------------------------------------------*/
;*    use-count ::J2SSvc ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SSvc inc)
   (with-access::J2SSvc this (params body decl)
      (use-count body inc))
   this)
   
;*---------------------------------------------------------------------*/
;*    use-count ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SRef inc)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usecnt)
	 (set! usecnt (+fx inc usecnt))))
   this)

;*---------------------------------------------------------------------*/
;*    use-count ::J2SGlobalRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SGlobalRef inc)
   (with-access::J2SGlobalRef this (decl)
      (with-access::J2SDecl decl (usecnt)
	 (set! usecnt (+fx inc usecnt))))
   this)

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
;*    usage ::J2SNode ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SNode ctx deval)
   (default-walk this ctx deval))
   
;*---------------------------------------------------------------------*/
;*    usage ::J2SRef ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SRef ctx deval)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usage)
	 (when ctx
	    (unless (memq ctx usage)
	       (set! usage (cons ctx usage))))))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SUnresolvedRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SUnresolvedRef ctx deval)
   (call-default-walker)
   (with-access::J2SUnresolvedRef this (id)
      (when (and (eq? id 'eval) (eq? ctx 'call))
	 (cell-set! deval #t)))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SGlobalRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SGlobalRef ctx deval)
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
(define-walk-method (usage this::J2SUnary ctx deval)
   (with-access::J2SUnary this (op expr)
      (cond
	 ((and (eq? op 'delete) (isa? expr J2SRef))
	  (with-access::J2SRef expr (decl)
	     (with-access::J2SDecl decl (usage)
		(unless (memq 'delete usage)
		   (set! usage (cons 'delete usage))))
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
(define-walk-method (usage this::J2SCall ctx deval)
   (with-access::J2SCall this (fun args)
      (usage fun 'call deval)
      (for-each (lambda (a) (usage a 'ref deval)) args))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SAssig ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SAssig ctx deval)
   (with-access::J2SAssig this (lhs rhs)
      (usage lhs 'assig deval)
      (usage rhs 'ref deval))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SInit ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SInit ctx deval)
   (with-access::J2SInit this (lhs rhs)
      (usage lhs 'init deval)
      (usage rhs 'ref deval))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SAccess ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SAccess ctx deval)
   (with-access::J2SAccess this (obj field)
      (usage obj (if (eq? ctx 'assig) 'set 'get) deval)
      (usage field 'ref deval))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SNew ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SNew ctx deval)
   (with-access::J2SNew this (clazz args)
      (usage clazz 'new deval)
      (for-each (lambda (a) (usage a 'ref deval)) args))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SLetBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SLetBlock ctx deval)
   (with-access::J2SLetBlock this (decls nodes)
      (for-each (lambda (d) (usage d 'init deval)) decls)
      (for-each (lambda (n) (usage n 'ref deval)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SDeclInit ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (usage this::J2SDeclInit ctx deval)
   (with-access::J2SDeclInit this ((u usage) val)
      (usage val 'ref deval)
      (set! u (cons 'init u)))
   this)

;*---------------------------------------------------------------------*/
;*    usage ::J2SReturn ...                                            */
;*---------------------------------------------------------------------*/
(define-method (usage this::J2SReturn ctx deval)
   (with-access::J2SReturn this (expr)
      (usage expr 'ref deval)
      this))

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

