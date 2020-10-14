;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/use.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Thu Jan 16 08:31:50 2020 (serrano)                */
;*    Copyright   :  2013-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Count the number of occurrences for all variables                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_use

   (include "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-use-stage
	   j2s-dead-stage
	   (generic reinit-use-count! ::J2SNode)
	   (generic reset-use-count ::J2SNode)
	   (generic use-count ::J2SNode inc::int looplevel::int)
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
		   (use-count o +1 0)
		   (j2s-use o 'ref deval #f))
	 nodes))
   
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls direct-eval)
	 (use-nodes headers)
	 (use-nodes decls)
	 (use-nodes nodes)
	 (if (cell-ref deval)
	     (for-each (lambda (d::J2SDecl)
 			  (decl-usage-add! d 'eval))
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
;*    decl-usage-add*! ...                                             */
;*---------------------------------------------------------------------*/
(define (decl-usage-add*! decl keys)
   (if (pair? keys)
       (for-each (lambda (key) (decl-usage-add! decl key)) keys)
       (decl-usage-add! decl keys)))

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
			  (with-access::J2SDecl d (usecnt id scope)
			     (or (>fx usecnt 0)
				 (eq? scope 'export)
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
					  (use-count val -1 0)))
				    #f))))
		  decls))
	    (loop)))
      decls))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SNode inc looplevel)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SFun inc looplevel)
   (with-access::J2SFun this (params body decl)
      (use-count body inc looplevel))
   this)
   
;*---------------------------------------------------------------------*/
;*    use-count ::J2SSvc ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SSvc inc looplevel)
   (with-access::J2SSvc this (params body decl)
      (use-count body inc looplevel))
   this)

;*---------------------------------------------------------------------*/
;*    use-count ::J2SDecl ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SDecl inc looplevel)
   (with-access::J2SDecl this (%info)
      (set! %info looplevel)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SRef ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SRef inc looplevel)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (usecnt useinloop %info)
	 (when (and (integer? %info) (<fx %info looplevel))
	    (set! useinloop #t))
	 (set! usecnt (+fx inc usecnt))))
   this)

;*---------------------------------------------------------------------*/
;*    use-count ::J2SGlobalRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SGlobalRef inc looplevel)
   (with-access::J2SGlobalRef this (decl)
      (with-access::J2SDecl decl (usecnt useinloop %info)
	 (when (and (integer? %info) (<fx %info looplevel))
	    (set! useinloop #t))
	 (set! usecnt (+fx inc usecnt))))
   this)

;*---------------------------------------------------------------------*/
;*    use-count ::J2SFor ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SFor inc looplevel)
   (with-access::J2SFor this (init test incr body)
      (use-count init inc looplevel)
      (use-count test inc (+fx 1 looplevel))
      (use-count incr inc (+fx 1 looplevel))
      (use-count body inc (+fx 1 looplevel))))

;*---------------------------------------------------------------------*/
;*    use-count ::J2SWhile ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (use-count this::J2SWhile inc looplevel)
   (with-access::J2SWhile this (test body)
      (use-count test inc (+fx looplevel 1))
      (use-count body inc (+fx looplevel 1))))

;*---------------------------------------------------------------------*/
;*    reinit-use-count! ::J2SNode ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (reinit-use-count! this::J2SNode)
   (reset-use-count this)
   (use-count this +1 0)
   this)

;*---------------------------------------------------------------------*/
;*    reinit-use-count! ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (reinit-use-count! this::J2SProgram)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each reset-use-count headers)
      (for-each reset-use-count decls)
      (for-each reset-use-count nodes)
      (for-each (lambda (n) (use-count n +1 0)) headers)
      (for-each (lambda (n) (use-count n +1 0)) decls)
      (for-each (lambda (n) (use-count n +1 0)) nodes)
      this))

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
   (with-access::J2SDecl this (usecnt scope id)
      (set! usecnt (if (eq? scope 'export) 1000 0)))
   this)

;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SDeclInit ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SDeclInit)
   (with-access::J2SDeclInit this (usecnt val scope %info id)
      (unless (eq? %info this)
	 (set! %info this)
	 (when (or (>fx usecnt 0) (eq? scope 'export))
	    (set! usecnt (if (eq? scope 'export) 1000 0))
	    (reset-use-count val))))
   this)

;*---------------------------------------------------------------------*/
;*    reset-use-count ::J2SDeclSvc ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-use-count this::J2SDeclSvc)
   (with-access::J2SDeclSvc this (val id %info)
      (unless (eq? %info this)
	 (set! %info this)
	 (reset-use-count val)))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SNode ctx deval infun)
   (default-walk this ctx deval infun))

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SExpr ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SExpr ctx deval infun)
   (default-walk this 'ref deval infun))
      
;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SRef ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SRef ctx deval infun)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (%info escape id scope)
	 (when (and infun
		    (not (eq? infun %info))
		    (not (memq scope '(global scope))))
	    (set! escape #t))
	 (when ctx
	    (decl-usage-add*! decl ctx))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SUnresolvedRef ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SUnresolvedRef ctx deval infun)
   (call-default-walker)
   (with-access::J2SUnresolvedRef this (id)
      (when (and (eq? id 'eval) (eq? ctx 'call))
	 (cell-set! deval #t)))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SGlobalRef ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SGlobalRef ctx deval infun)
   (call-next-method)
   (with-access::J2SGlobalRef this (decl)
      (when ctx
	 (decl-usage-add*! decl ctx)))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SUnary ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SUnary ctx deval infun)
   (with-access::J2SUnary this (op expr)
      (cond
	 ((and (eq? op 'delete) (isa? expr J2SRef))
	  (with-access::J2SRef expr (decl)
	     (decl-usage-add! decl 'delete)
	     this))
	 ((and (eq? op 'delete) (isa? expr J2SAccess))
	  (with-access::J2SAccess expr (obj field)
	     (if (isa? obj J2SRef)
		 (with-access::J2SRef obj (decl)
		    (decl-usage-add! decl 'delete))
		 (j2s-use obj 'ref deval infun))
	     (j2s-use field ctx deval infun)
	     this))
	 ((and (eq? op 'delete) (isa? expr J2SGlobalRef))
	  (with-access::J2SGlobalRef expr (decl)
	     (decl-usage-add! decl 'delete)
	     this))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SBinary ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SBinary ctx deval infun)
   (with-access::J2SBinary this (op lhs rhs)
      (if (and (eq? op 'instanceof) (isa? rhs J2SRef))
	  (with-access::J2SRef rhs (decl)
	     (decl-usage-add! decl 'instanceof)
	     (j2s-use lhs 'ref deval infun)
	     this)
	  (call-default-walker))))
      
;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SCall ctx deval infun)
   (with-access::J2SCall this (fun args protocol)
      (let ((ctx (cond
		    ((isa? fun J2SFun) 'call)
		    ((isa? fun J2SAccess) 'method)
		    ((isa? fun J2SRef) 'call)
		    ((isa? fun J2SUnresolvedRef) 'call)
		    (else 'ref))))
	 (j2s-use fun ctx deval infun))
      (when (eq? protocol 'spread)
	 (j2s-use fun 'get deval infun))
      (for-each (lambda (a) (j2s-use a 'ref deval infun)) args))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SParen ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SParen ctx deval infun)
   (with-access::J2SParen this (expr)
      (j2s-use expr ctx deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SAssig ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SAssig ctx deval infun)
   (with-access::J2SAssig this (lhs rhs loc)
      (j2s-use lhs 'assig deval infun)
      (j2s-use rhs 'ref deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SInit ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SInit ctx deval infun)

   (define (already-init? lhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (decl-usage-has? decl '(init)))))
   
   (with-access::J2SInit this (lhs rhs)
      (j2s-use lhs (if (already-init? lhs) 'assig 'init) deval infun)
      (j2s-use rhs 'ref deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SAccess ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SAccess ctx deval infun)
   (with-access::J2SAccess this (obj field)
      (let ((axs (cond
		    ((eq? ctx 'assig) 'set)
		    ((eq? ctx 'method) '(get method))
		    (else 'get))))
	 (j2s-use obj axs deval infun))
      (j2s-use field 'ref deval infun))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SNew ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SNew ctx deval infun)
   (with-access::J2SNew this (clazz args)
      (j2s-use clazz 'new deval infun)
      (for-each (lambda (a) (j2s-use a 'ref deval infun)) args))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SBlock ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SBlock ctx deval infun)
   (with-access::J2SBlock this (nodes)
      (when (isa? infun J2SFun)
	 (for-each (lambda (b)
		      (when (isa? b J2SDecl)
			 (with-access::J2SDecl b (%info)
			    (set! %info infun))))
	    nodes)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SLetBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SLetBlock ctx deval infun)
   (with-access::J2SLetBlock this (decls nodes)
      (when (isa? infun J2SFun)
	 (for-each (lambda (d)
		      (with-access::J2SDecl d (%info) (set! %info infun)))
	    decls))
      (for-each (lambda (d) (j2s-use d 'init deval infun)) decls)
      (for-each (lambda (n) (j2s-use n 'ref deval infun)) nodes))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SDecl ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SDecl ctx deval infun)
   (with-access::J2SDecl this (exports)
      (when (and (pair? exports) (not (decl-usage-has? this '(ref))))
	 (decl-usage-add! this 'ref)))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SDeclInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SDeclInit ctx deval infun)
   (with-access::J2SDeclInit this (val)
      (j2s-use val 'ref deval infun)
      (decl-usage-add! this 'init))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SReturn ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SReturn ctx deval infun)
   (with-access::J2SReturn this (expr)
      (j2s-use expr 'ref deval infun)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SFun ctx deval infun)
   (with-access::J2SFun this (params body generator)
      (unless generator
	 (for-each (lambda (p)
		      (with-access::J2SDecl p (%info)
			 (set! %info this)))
	    params))
      (j2s-use body 'ref deval (if generator 'dummy-to-escape-all-tmps this))))

;*---------------------------------------------------------------------*/
;*    j2s-use ::J2SForIn ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-use this::J2SForIn ctx deval infun)
   (with-access::J2SForIn this (lhs obj body)
      (j2s-use obj ctx deval infun)
      (j2s-use body 'ref deval infun)
      (j2s-use lhs 'assig deval infun)))

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

