;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/uninit.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 24 13:11:25 2019                          */
;*    Last change :  Sun Aug 29 17:59:29 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Mark global variables potentially used before being initialized. */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_uninit

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-uninit-stage
	   j2s-uninit-globprop-stage
	   j2s-uninit-force-stage))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-uninit-stage
   (instantiate::J2SStageProc
      (name "uninit")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit! n args)))))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-uninit-globprop-stage
   (instantiate::J2SStageProc
      (name "uninit.globprop")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit! n args)))
      (optional :optim-globprop)))

;*---------------------------------------------------------------------*/
;*    j2s-uninit-force-stage ...                                       */
;*---------------------------------------------------------------------*/
(define j2s-uninit-force-stage
   (instantiate::J2SStageProc
      (name "uninit.force")
      (comment "Global variable initialization optimization")
      (proc (lambda (n args) (j2s-uninit-force! n args)))))

;*---------------------------------------------------------------------*/
;*    funinfo ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct funinfo globals protos invalidated)

;*---------------------------------------------------------------------*/
;*    j2s-uninit! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-uninit! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (cond
	    ((not (config-get args :optim-uninit #f))
	     (j2s-uninit-force! this args))
	    (direct-eval
	     (for-each (lambda (decl)
			  (unless (isa? decl J2SDeclFun)
			     (decl-usage-add! decl 'uninit)))
		decls))
	    (else
	     (uninit-nodes* decls '())
	     (uninit-nodes* nodes '())))))
;* 	     ;; collect all the globals used by all global functions   */
;* 	     (for-each function-collect-globals decls)                 */
;* 	     ;; scan in sequence all the declarations and all the nodes */
;* 	     (for-each scan-decl decls)                                */
;* 	     (for-each scan-node nodes)))))                            */
   this)

;*---------------------------------------------------------------------*/
;*    j2s-uninit-force! ::J2SProgram ...                               */
;*---------------------------------------------------------------------*/
(define (j2s-uninit-force! this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (for-each uninit-force! decls)
	 (for-each uninit-force! nodes)))
   this)

;*---------------------------------------------------------------------*/
;*    intersection ...                                                 */
;*---------------------------------------------------------------------*/
(define (intersection el er)
   (for-each (lambda (d) (with-access::J2SDecl d (%info) (set! %info #t))) el)
   (for-each (lambda (d) (with-access::J2SDecl d (%info) (set! %info #f))) er)
   (append! (filter (lambda (d) (with-access::J2SDecl d (%info) %info)) el) er))

;*---------------------------------------------------------------------*/
;*    uninit-nodes* ...                                                */
;*---------------------------------------------------------------------*/
(define (uninit-nodes* nodes env)
   (let loop ((nodes nodes)
	      (env env))
      (if (null? nodes)
	  env
	  (loop (cdr nodes) (uninit* (car nodes) env)))))

;*---------------------------------------------------------------------*/
;*    uninit-par* ...                                                  */
;*---------------------------------------------------------------------*/
(define (uninit-par* nodes env0)
   (let loop ((nodes nodes)
	      (env env0))
      (if (null? nodes)
	  env
	  (let ((nenv (uninit* (car nodes) env0)))
	     (loop (cdr nodes) (intersection nenv env))))))

;*---------------------------------------------------------------------*/
;*    uninit* ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (uninit* this env)
   (if (pair? this)
       (uninit-par* this env)
       env))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SNode env)
   (let* ((clazz (object-class this))
	  (ctor (class-constructor clazz))
	  (inst ((class-allocator clazz)))
	  (fields (class-all-fields clazz)))
      (let loop ((i (-fx (vector-length fields) 1))
		 (env env))
	 (if (>=fx i 0)
	     (let* ((f (vector-ref-ur fields i))
		    (fi (class-field-info f))
		    (v ((class-field-accessor f) this))
		    (env (if (and (pair? fi) (member "ast" fi))
			     (uninit* v env)
			     env)))
		(loop (-fx i 1) env))
	     env))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SRef ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SRef env)
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (scope)
	 (when (memq scope '(global %scope))
	    (unless (or (decl-usage-has? decl '(uninit)) (memq decl env))
	       (decl-usage-add! decl 'uninit)))))
   env)

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SInit ...                                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SInit env)
   (with-access::J2SInit this (lhs rhs)
      (let ((env (uninit* rhs env)))
	 (if (isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (scope)
		   (if (memq scope '(global %scope))
		       (if (decl-usage-has? decl '(uninit))
			   env
			   (cons decl env))
		       env)))
	     (uninit* lhs env)))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SSeq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SSeq env)
   (with-access::J2SSeq this (nodes)
      (uninit-nodes* nodes env)))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SIf ...                                              */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SIf env)
   (with-access::J2SIf this (test then else)
      (let* ((testenv (uninit* test env))
	     (thenenv (uninit* then testenv))
	     (elseenv (uninit* else testenv)))
	 (intersection thenenv elseenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SLetBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SLetBlock env)
   (with-access::J2SLetBlock this (decls nodes)
      (let loop ((decls decls)
		 (env env))
	 (if (null? decls)
	     (uninit-nodes* nodes env)
	     (loop (cdr decls) (uninit* (car decls) env))))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SSwitch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SSwitch env)
   (with-access::J2SSwitch this (key cases)
      (let ((kenv (uninit* key env)))
	 (uninit-par* cases kenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SCase ...                                            */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SCase env)
   (with-access::J2SCase this (expr body)
      (let ((eenv (uninit* expr env)))
	 (uninit* body eenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SFor ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SFor env)
   (tprint "j2sfor")
   (with-access::J2SFor this (init test incr body)
      (let* ((ienv (uninit* init env))
	     (tenv (uninit* test ienv))
	     (benv (uninit* body ienv)))
	 (uninit* incr benv)
	 (filter (lambda (d) (not (decl-usage-has? d '(uninit)))) ienv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SForIn ...                                           */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SForIn env)
   (with-access::J2SForIn this (lhs obj body)
      (let* ((lenv (uninit* lhs env))
	     (oenv (uninit* obj lenv)))
	 (uninit* body oenv)
	 (filter (lambda (d) (not (decl-usage-has? d '(uninit)))) oenv))))

;*---------------------------------------------------------------------*/
;*    uninit* ::J2SWhile ...                                           */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SWhile env)
   (with-access::J2SWhile this (test body)
      (let* ((tenv (uninit* test env)))
	 (uninit* body tenv)
	 (filter (lambda (d) (not (decl-usage-has? d '(uninit)))) tenv))))
      
;*---------------------------------------------------------------------*/
;*    uninit* ::J2SDo ...                                              */
;*---------------------------------------------------------------------*/
(define-method (uninit* this::J2SDo env)
   (with-access::J2SDo this (test body need-bind-exit-break need-bind-exit-continue)
      (let ((benv (uninit* body env)))
	 (uninit* test 
	    (filter (lambda (d) (not (decl-usage-has? d '(uninit))))
	       (if (or need-bind-exit-break need-bind-exit-continue)
		   env
		   benv))))))

;*---------------------------------------------------------------------*/
;*    decl-global? ...                                                 */
;*---------------------------------------------------------------------*/
(define (decl-global? decl::J2SDecl)
   (unless (isa? decl J2SDeclExtern)
      (with-access::J2SDecl decl (scope)
	 (memq scope '(global %scope)))))

;* {*---------------------------------------------------------------------*} */
;* {*    function-collect-globals ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define (function-collect-globals decl::J2SDecl)                    */
;*    (when (isa? decl J2SDeclFun)                                     */
;*       (with-access::J2SDeclFun decl (%info val)                     */
;* 	 (set! %info                                                   */
;* 	    (funinfo (collect-globals* val) '() #f)))))                */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    collect-globals* ::J2SNode ...                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (collect-globals* this::J2SNode)                */
;*    (call-default-walker))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    collect-globals* ::J2SRef ...                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (collect-globals* this::J2SRef)                 */
;*    (with-access::J2SRef this (decl)                                 */
;*       (if (decl-global? decl)                                       */
;* 	  (list decl)                                                  */
;* 	  '())))                                                       */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    scan-decl ...                                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define (scan-decl decl::J2SDecl)                                   */
;*    (when (and (isa? decl J2SDeclInit) (not (isa? decl J2SDeclFun))) */
;*       (with-access::J2SDeclInit decl (val %info binder)             */
;* 	 (when (memq binder '(let-opt let-forin))                      */
;* 	    (unless (decl-usage-has? decl '(uninit))                   */
;* 	       (with-access::J2SDecl decl (%info)                      */
;* 		  (set! %info 'init))))                                */
;* 	 (unless (and (isa? decl J2SDeclClass) (isa? val J2SClass)     */
;* 		      (with-access::J2SClass val (need-dead-zone-check) */
;* 			 need-dead-zone-check))                        */
;* 	    (invalidate! val)))))                                      */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    scan-node ...                                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define-generic (scan-node this::J2SNode)                           */
;*    (let* ((clazz (object-class this))                               */
;* 	  (ctor (class-constructor clazz))                             */
;* 	  (inst ((class-allocator clazz)))                             */
;* 	  (fields (class-all-fields clazz)))                           */
;*       (let loop ((i (-fx (vector-length fields) 1)))                */
;* 	 (when (>=fx i 0)                                              */
;* 	    (let* ((f (vector-ref-ur fields i))                        */
;* 		   (fi (class-field-info f))                           */
;* 		   (v ((class-field-accessor f) this)))                */
;* 	       (cond                                                   */
;* 		  ((and (pair? fi) (member "notraverse" fi)) v)        */
;* 		  ((pair? v) (map scan-node v))                        */
;* 		  ((isa? v J2SExpr) (invalidate! v))                   */
;* 		  ((isa? v J2SNode) (scan-node v)))                    */
;* 	       (loop (-fx i 1)))))                                     */
;*       this))                                                        */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    scan-node ::J2SStmtExpr ...                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (scan-node this::J2SStmtExpr)                        */
;*                                                                     */
;*    (define (is-prototype? field)                                    */
;*       (when (isa? field J2SString)                                  */
;* 	 (with-access::J2SString field (val)                           */
;* 	    (and (string? val) (string=? val "prototype")))))          */
;*                                                                     */
;*    (define (global-prototype-assign assig::J2SAssig)                */
;*       (with-access::J2SAssig assig (lhs rhs)                        */
;* 	 (if (isa? rhs J2SFun)                                         */
;* 	     (if (isa? lhs J2SAccess)                                  */
;* 		 (with-access::J2SAccess lhs (obj field)               */
;* 		    (if (isa? obj J2SAccess)                           */
;* 			(with-access::J2SAccess obj (obj field)        */
;* 			   (if (isa? obj J2SRef)                       */
;* 			       (with-access::J2SRef obj (decl)         */
;* 				  (if (and (decl-global? decl)         */
;* 					   (is-prototype? field))      */
;* 				      (values decl rhs)                */
;* 				      (values #f #f)))                 */
;* 			       (values #f #f)))                        */
;* 			(values #f #f)))                               */
;* 		 (values #f #f))                                       */
;* 	     (values #f #f))))                                         */
;*                                                                     */
;*    (with-access::J2SStmtExpr this (expr loc)                        */
;*       (cond                                                         */
;* 	 ((isa? expr J2SInit)                                          */
;* 	  (with-access::J2SInit expr (lhs rhs)                         */
;* 	     (invalidate! rhs)                                         */
;* 	     (if (isa? lhs J2SRef)                                     */
;* 		 (with-access::J2SRef lhs (decl)                       */
;* 		    (if (decl-global? decl)                            */
;* 			(unless (decl-usage-has? decl '(uninit))       */
;* 			   (with-access::J2SDecl decl (%info)          */
;* 			      (set! %info 'init)))                     */
;* 			(invalidate! lhs)))                            */
;* 		 (invalidate! lhs))))                                  */
;* 	 ((isa? expr J2SAssig)                                         */
;* 	  (multiple-value-bind (decl fun)                              */
;* 	     (global-prototype-assign expr)                            */
;* 	     (if (and decl fun)                                        */
;* 		 (with-access::J2SDecl decl (%info id)                 */
;* 		    (if (or (not (funinfo? %info)) (funinfo-invalidated %info)) */
;* 			(invalidate! fun)                              */
;* 			(funinfo-protos-set! %info                     */
;* 			   (cons fun (funinfo-protos %info)))))        */
;* 		 (invalidate! expr))))                                 */
;* 	 (else                                                         */
;* 	  (invalidate! expr)))))                                       */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate! ...                                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define-generic (invalidate! this)                                  */
;*    (when (pair? this)                                               */
;*       (for-each invalidate! this)))                                 */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate! ::J2SNode ...                                        *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (invalidate! this::J2SNode)                          */
;*    (with-access::J2SNode this (%info)                               */
;*       (unless (eq? %info 'invalidated)                              */
;* 	 (set! %info 'invalidated)                                     */
;* 	 (let* ((clazz (object-class this))                            */
;* 		(ctor (class-constructor clazz))                       */
;* 		(inst ((class-allocator clazz)))                       */
;* 		(fields (class-all-fields clazz)))                     */
;* 	    (let loop ((i (-fx (vector-length fields) 1)))             */
;* 	       (when (>=fx i 0)                                        */
;* 		  (let* ((f (vector-ref-ur fields i))                  */
;* 			 (v ((class-field-accessor f) this))           */
;* 			 (fi (class-field-info f)))                    */
;* 		     (cond                                             */
;* 			((and (pair? fi) (member "notraverse" fi)) v)  */
;* 			((pair? v) (for-each invalidate! v))           */
;* 			((isa? v J2SExpr) (invalidate! v))             */
;* 			((isa? v J2SNode) (invalidate! v)))            */
;* 		     (loop (-fx i 1)))))))                             */
;*       this))                                                        */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate! ::J2SRef ...                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (invalidate! this::J2SRef)                           */
;*    (with-access::J2SRef this (decl loc)                             */
;*       (when (decl-global? decl)                                     */
;* 	 (with-access::J2SDecl decl (%info id scope)                   */
;* 	    (cond                                                      */
;* 	       ((isa? decl J2SDeclFun)                                 */
;* 		(unless (funinfo-invalidated %info)                    */
;* 		   (funinfo-invalidated-set! %info #t)                 */
;* 		   (with-access::J2SDeclFun decl (val)                 */
;* 		      (invalidate! val))                               */
;* 		   (for-each invalidate! (funinfo-protos %info))))     */
;* 	       ((not (eq? %info 'init))                                */
;* 		(decl-usage-add! decl 'uninit)))))))                   */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-overridden-decl ::J2SNode ...                         *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Scan the whole program and invalidate all global variables       *} */
;* {*    that are initialized several times.                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-overridden-decl this::J2SNode)      */
;*    (call-default-walker))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-overridden-decl ::J2SInit ...                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-overridden-decl this::J2SInit)      */
;*    (with-access::J2SInit this (lhs rhs)                             */
;*       (invalidate-overridden-decl rhs)                              */
;*       (if (isa? lhs J2SRef)                                         */
;* 	  (with-access::J2SRef lhs (decl)                              */
;* 	     (with-access::J2SDecl decl (%info)                        */
;* 		(case %info                                            */
;* 		   ((unknown)                                          */
;* 		    (set! %info 'init1))                               */
;* 		   ((init1)                                            */
;* 		    (set! %info 'overridden)))))                       */
;* 	  (invalidate-overridden-decl lhs))))                          */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-overridden-decl ::J2SDeclInit ...                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-overridden-decl this::J2SDeclInit)  */
;*    (with-access::J2SDeclInit this (%info val)                       */
;*       (invalidate-overridden-decl val)                              */
;*       (case %info                                                   */
;* 	 ((unknown)                                                    */
;* 	  (set! %info 'init0))                                         */
;* 	 ((init0)                                                      */
;* 	  (set! %info 'overridden)))))                                 */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SNode ...                              *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Scan the whole program and invalidate all global variables       *} */
;* {*    that can possibily be accessed before initialized.               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SNode initp::bool) */
;*    (call-default-walker))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SRef ...                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SRef initp)      */
;*    (with-access::J2SRef this (decl loc)                             */
;*       (with-access::J2SDecl decl (%info id key)                     */
;* 	 (when (memq %info '(unknown init0 overridden))                */
;* 	    (if (isa? decl J2SDeclFun)                                 */
;* 		(begin                                                 */
;* 		   (set! %info 'init)                                  */
;* 		   (with-access::J2SDeclFun decl (val)                 */
;* 		      (invalidate-early-decl val #f)))                 */
;* 		(begin                                                 */
;* 		   (set! %info 'uninit)                                */
;* 		   (decl-usage-add! decl 'uninit)))))))                */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SFun ...                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SFun initp)      */
;*    (with-access::J2SFun this (body)                                 */
;*       (invalidate-early-decl body #f)))                             */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SInit ...                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SInit initp)     */
;*    (with-access::J2SInit this (lhs rhs %%dump)                      */
;*       (invalidate-early-decl rhs initp)                             */
;*       (if (isa? lhs J2SRef)                                         */
;* 	  (with-access::J2SRef lhs (decl)                              */
;* 	     (with-access::J2SDecl decl (%info id)                     */
;* 		(when (and initp (eq? %info 'init0))                   */
;* 		   ;; for sure this is initialized                     */
;* 		   (set! %info 'init))))                               */
;* 	  (invalidate-early-decl lhs initp))))                         */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SDeclInit ...                          *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SDeclInit initp) */
;*    (with-access::J2SDeclInit this (%info val)                       */
;*       (invalidate-early-decl val initp)                             */
;*       (when (eq? %info 'init0)                                      */
;* 	 ;; for sure this is initialized                               */
;* 	 (set! %info 'init))))                                         */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SDeclFun ...                           *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SDeclFun initp)  */
;*    (with-access::J2SDeclInit this (%info val)                       */
;*       (when (eq? %info 'initi0)                                     */
;* 	 ;; for sure this is initialized                               */
;* 	 (set! %info 'init))))                                         */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SLabel ...                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SLabel initp)    */
;*    (with-access::J2SLabel this (body)                               */
;*       (invalidate-early-decl body #f)))                             */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2STry ...                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2STry initp)      */
;*    (with-access::J2STry this (body catch finally)                   */
;*       (invalidate-early-decl body #f)                               */
;*       (invalidate-early-decl catch #f)                              */
;*       (invalidate-early-decl finally #f)))                          */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SLoop ...                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SLoop initp)     */
;*    (with-access::J2SLoop this (body)                                */
;*       (invalidate-early-decl body #f)))                             */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SFor ...                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SFor initp)      */
;*    (with-access::J2SFor this (init test incr)                       */
;*       (invalidate-early-decl init initp)                            */
;*       (invalidate-early-decl test initp)                            */
;*       (invalidate-early-decl incr initp)                            */
;*       (call-next-method)))                                          */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    invalidate-early-decl ::J2SForIn ...                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (invalidate-early-decl this::J2SForIn initp)    */
;*    (with-access::J2SForIn this (lhs obj)                            */
;*       (invalidate-early-decl lhs initp)                             */
;*       (invalidate-early-decl obj initp)                             */
;*       (call-next-method)))                                          */

;*---------------------------------------------------------------------*/
;*    uninit-force! ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (uninit-force! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    uninit-force! ::J2SDeclInit ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (uninit-force! this::J2SDeclInit)
   (unless (isa? this J2SDeclFun)
      (decl-usage-add! this 'uninit))
   (call-default-walker))
