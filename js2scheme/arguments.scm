;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/arguments.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  5 09:14:00 2019                          */
;*    Last change :  Sun Jul 23 08:39:45 2023 (serrano)                */
;*    Copyright   :  2019-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Arguments optimization                                           */
;*    -------------------------------------------------------------    */
;*    This stage annotates ARGUMENTS and OPTIONAL arguments usages     */
;*    so that the Scheme code generation can better allocate and       */
;*    use this special variable.                                       */
;*    -------------------------------------------------------------    */
;*    This analysis is "smart" enough to keep track of "arguments"     */
;*    that are merely aliases to local variable (see arguments-alias). */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_arguments
   
   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_node-size
	   __js2scheme_alpha)

   (export j2s-arguments-stage))

;*---------------------------------------------------------------------*/
;*    j2s-arguments-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-arguments-stage
   (instantiate::J2SStageProc
      (name "arguments")
      (comment "Arguments optimization (annotation)")
      (optional :optim-arguments)
      (proc j2s-arguments)))

;*---------------------------------------------------------------------*/
;*    j2s-arguments ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-arguments this conf)
   (when (isa? this J2SProgram)
      (unless (> (config-get conf :debug 0) 0)
	 (argsuse this)
	 ))
   this)

;*---------------------------------------------------------------------*/
;*    argsuse ...                                                      */
;*    -------------------------------------------------------------    */
;*    Compute ARGUMENTS specific usage properties.                     */
;*---------------------------------------------------------------------*/
(define-walk-method (argsuse this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    argsuse ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (argsuse this::J2SFun)
   (with-access::J2SFun this (argumentsp params mode body)
      (when argumentsp
	 ;; approximate the argument range
	 (argsrange this #f '())
	 (with-access::J2SDecl argumentsp (%info)
	    (set! %info '()))
	 ;; will be restored if used in a "true" ref
	 (decl-usage-rem! argumentsp 'ref)
	 (decl-usage-rem! argumentsp 'get))
      (when (pair? params)
	 (let ((lastp (car (last-pair params))))
	    (when (isa? lastp J2SDeclRest)
	       (with-access::J2SDeclRest lastp (mode %info)
		  (set! %info '())
		  (decl-usage-rem! lastp 'method)
		  (decl-usage-rem! lastp 'ref)
		  (decl-usage-rem! lastp 'get)))))
      (call-default-walker)
      (when (and argumentsp
		 ;; either strict mode or all parameters are read-only
		 (or (not (eq? mode 'normal))
		     (every (lambda (p)
			       (not (decl-usage-has? p '(assig))))
			params)))
	 (with-access::J2SDeclArguments argumentsp (alloc-policy usage useinloop)
	    (cond
	       ((usage-strict? usage '(length))
		(set! alloc-policy 'lonly))
	       ((usage-strict? usage '(slice getx length spread))
		(set! alloc-policy 'stack))
	       ((usage-strict? usage '(slice getx length spread apply geti))
		(set! alloc-policy 'lazy))))
	 (patch-type body))
      (when (pair? params)
	 (let ((lastp (car (last-pair params))))
	    (when (isa? lastp J2SDeclRest)
	       (with-access::J2SDeclRest lastp (alloc-policy usage ctype vtype)
		  (cond
		     ((usage-strict? usage '(slice getx length spread apply geti rest))
		      (set! ctype 'vector)
		      (set! vtype 'vector)
		      (set! alloc-policy 'stack)))))))))

;*---------------------------------------------------------------------*/
;*    argsuse ::J2SSvc ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (argsuse this::J2SSvc)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    argsuse ::J2SRef ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (argsuse this::J2SRef)
   (with-access::J2SRef this (decl)
      (when (isa? decl J2SDeclRest)
	 (decl-usage-add! decl 'ref))))

;*---------------------------------------------------------------------*/
;*    argsuse ::J2SAccess ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (argsuse this::J2SAccess)
   
   (define (field-length? field)
      (and (isa? field J2SString)
	   (with-access::J2SString field (val)
	      (string=? val "length"))))
   
   (define (field-index? field)
      (let ((ty (j2s-type field)))
	 (or (type-fixnum? ty) (type-int53? ty))))
   
   (with-access::J2SAccess this (obj field %info range)
      (argsuse field)
      (if (isa? obj J2SRef)
	  (with-access::J2SRef obj (decl)
	     (if (isa? decl J2SDeclRest)
		 (decl-usage-add! decl
		    (cond
		       ((field-length? field)
			'length)
		       ((field-index? field)
			(if (eq? %info 'in-range)
			    (begin
			       (set! range 'in-range)
			       'getx)
			    'geti))
		       (else
			'get)))
		 (call-default-walker)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    argsuse ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (argsuse this::J2SCall)
   
   (define (apply? fun args)
      (when (and (isa? fun J2SAccess) (=fx (length args) 2))
	 (with-access::J2SAccess fun (field)
	    (when (isa? field J2SString)
	       (with-access::J2SString field (val)
		  (string=? val "apply"))))))

   (define (spread? args)
      (when (and (pair? args) (null? (cdr args)))
	 (isa? (car args) J2SSpread)))

   (define (array-prototype? obj)
      (when (isa? obj J2SAccess)
	 (with-access::J2SAccess obj (obj field)
	    (when (isa? obj J2SRef)
	       (with-access::J2SRef obj (decl)
		  (when (isa? decl J2SDeclExtern)
		     (with-access::J2SDeclExtern decl (id)
			(when (and (eq? id 'Array) (decl-ronly? decl))
			   (when (isa? field J2SString)
			      (with-access::J2SString field (val)
				 (string=? val "prototype")))))))))))

   (define (builtin-array-prototype-slice? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (when (and (isa? decl J2SDeclInit) (decl-ronly? decl))
	       (with-access::J2SDeclInit decl (val)
		  (array-prototype-slice? val))))))
	 
   (define (array-prototype-slice? obj)
      (cond
	 ((isa? obj J2SAccess)
	  (with-access::J2SAccess obj (obj field)
	     (when (isa? field J2SString)
		(with-access::J2SString field (val)
		   (when (string=? val "slice")
		      (array-prototype? obj))))))
	 ((isa? obj J2SRef)
	  (builtin-array-prototype-slice? obj))
	 (else
	  #f)))
      
   (define (arguments-slice? fun args)
      (when (and (isa? fun J2SAccess) (>=fx (length args) 1))
         (with-access::J2SAccess fun (obj field)
            (when (isa? field J2SString)
               (with-access::J2SString field (val)
                  (when (string=? val "call")
                     (array-prototype-slice? obj)))))))

   (define (rest-slice? fun args)
      (when (and (isa? fun J2SAccess) (>=fx (length args) 1))
	 (with-access::J2SAccess fun (obj field)
	    (when (and (isa? obj J2SRef) (isa? field J2SString))
	       (with-access::J2SString field (val)
		  (when (string=? val "slice")
		     (when (isa? obj J2SRef)
			(with-access::J2SRef obj (decl)
			   (and (isa? decl J2SDeclRest)
				(not (isa? decl J2SDeclArguments)))))))))))
   
   (with-access::J2SCall this (fun args)
      (cond
	 ((apply? fun args)
	  (let ((arg1 (cadr args)))
	     (if (isa? arg1 J2SRef)
		 (with-access::J2SRef arg1 (decl)
		    (if (isa? decl J2SDeclRest)
			(begin
			   (argsuse fun)
			   (argsuse (car args))
			   (decl-usage-add! decl 'apply))
			(call-default-walker)))
		 (call-default-walker))))
	 ((spread? args)
	  (with-access::J2SSpread (car args) (expr)
	     (if (isa? expr J2SRef)
		 (with-access::J2SRef expr (decl)
		    (if (isa? decl J2SDeclRest)
			(begin
			   (argsuse fun)
			   (decl-usage-add! decl 'spread))
			(call-default-walker)))
		 (call-default-walker))))
	 ((arguments-slice? fun args)
	  (let ((arg0 (car args)))
	     (if (isa? arg0 J2SRef)
		 (with-access::J2SRef arg0 (decl)
		    (if (isa? decl J2SDeclRest)
			(begin
			   (argsuse fun)
			   (for-each argsuse (cdr args))
			   (decl-usage-add! decl 'slice))
			(call-default-walker)))
		 (call-default-walker))))
	 ((rest-slice? fun args)
	  (with-access::J2SAccess fun (obj)
	     (with-access::J2SRef obj (decl)
		(for-each argsuse args)
		(decl-usage-add! decl 'slice))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    arguments-alias ...                                              */
;*---------------------------------------------------------------------*/
(define-struct arguments-alias arguments)

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::obj ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (annotate-arguments this::obj parent lhs::bool arguments)
   (when (pair? this)
      (for-each (lambda (n) (annotate-arguments n parent lhs arguments)) this)))

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SNode parent lhs arguments)
   (vector-for-each (lambda (f)
		       (let ((info (class-field-info f)))
			  (when (and (pair? info) (member "ast" info))
			     (annotate-arguments
				((class-field-accessor f) this) this
				(or lhs
				    (and (eq? (class-field-name f) 'lhs)
					 (isa? f J2SAssig)))
				arguments))))
      (class-all-fields (object-class this))))

;*---------------------------------------------------------------------*/
;*    annotate-arguments ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SFun parent lhs arguments)
   (with-access::J2SFun this (body argumentsp mode params loc)
      (when (and argumentsp
		 (or (memq mode '(strict hopscript)) (null? params)))
	 (with-access::J2SDeclArguments argumentsp (alloc-policy)
	    (set! alloc-policy 'lazy)))
      (when (pair? params)
	 (let ((decl (car (last-pair params))))
	    (when (isa? decl J2SDeclRest)
	       (with-access::J2SDeclRest decl (alloc-policy)
		  (set! alloc-policy 'lazy)))))
      (annotate-arguments body parent #f arguments)))

;*---------------------------------------------------------------------*/
;*    annotate-arguments ...                                           */
;*---------------------------------------------------------------------*/
(define-method (annotate-arguments this::J2SRef parent lhs arguments)
   
   (define (arguments-invalidate! decl)
      (with-access::J2SDeclRest decl (alloc-policy)
	 (set! alloc-policy 'eager)))
   
   (define (get-length? node::J2SAccess)
      (with-access::J2SAccess node (field)
	 (and (not lhs)
	      (isa? field J2SString)
	      (with-access::J2SString field (val)
		 (string=? val "length")))))
   
   (define (apply? node::J2SCall)
      (with-access::J2SCall node (fun)
	 (when (isa? fun J2SAccess)
	    (with-access::J2SAccess fun (field)
	       (when (isa? field J2SString)
		  (with-access::J2SString field (val)
		     (string=? val "apply")))))))

   (define (rtwin? node::J2SCall)
      (with-access::J2SCall node (fun)
	 (when (isa? fun J2SAccess)
	    (with-access::J2SAccess fun (obj field)
	       (when (isa? obj J2SSuper)
		  (with-access::J2SSuper obj (context super)
		     (eq? context super)))))))
   
   (with-access::J2SRef this (decl)
      (let loop ((decl decl))
	 (cond
	    ((isa? decl J2SDeclRest)
	     (cond
		((isa? parent J2SAccess)
		 (with-access::J2SAccess parent (field)
		    (unless (or (memq (j2s-type field)
				   '(integer uint32 int32 fixnum int53))
				(get-length? parent))
		       (arguments-invalidate! decl))))
		((isa? parent J2SCall)
		 (unless (or (apply? parent) (rtwin? parent))
		    (arguments-invalidate! decl)))
		((isa? parent J2SDeclInit)
		 (if (decl-usage-strict? parent '(init get))
		     (with-access::J2SDecl parent (%info)
			(unless (arguments-alias arguments)
			   (set! %info (arguments-alias arguments))))
		     (arguments-invalidate! decl)))
		(else
		 (arguments-invalidate! decl))))
	    ((isa? decl J2SDecl)
	     (with-access::J2SDecl decl (%info)
		(when (arguments-alias? %info)
		   (loop (arguments-alias-arguments %info)))))))))
   
;*---------------------------------------------------------------------*/
;*    argsrange ...                                                    */
;*    -------------------------------------------------------------    */
;*    For each access "arguments[X]", approximate X to check whether   */
;*    it is in the range [0..arguments.length] or not.                 */
;*---------------------------------------------------------------------*/
(define-walk-method (argsrange this::J2SNode range env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    argsrange ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (argsrange this::J2SAccess range env)
   
   (define (field-index? field)
      (let ((ty (j2s-type field)))
	 (or (type-fixnum? ty) (type-int53? ty))))
   
   (call-default-walker)
   (with-access::J2SAccess this (obj field %info)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (when (and (isa? decl J2SDeclRest) (field-index? field))
	       (cond
		  ((isa? field J2SNumber)
		   (with-access::J2SNumber field (val)
		      (when (and range (<= val range))
			 (set! %info 'in-range))))
		  ((isa? field J2SRef)
		   (with-access::J2SRef field (decl)
		      (when (memq decl env)
			 (set! %info 'in-range))))))))))

;*---------------------------------------------------------------------*/
;*    argsrange-length? ...                                            */
;*---------------------------------------------------------------------*/
(define (argsrange-length? this::J2SExpr)
   
   (define (field-length? field)
      (and (isa? field J2SString)
	   (with-access::J2SString field (val)
	      (string=? val "length"))))
   
   (define (args-ref? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (isa? decl J2SDeclRest))))
   
   (when (isa? this J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (and (field-length? field) (args-ref? obj)))))

;*---------------------------------------------------------------------*/
;*    argsrange-test ...                                               */
;*---------------------------------------------------------------------*/
(define (argsrange-test this::J2SExpr env)

   (define (inv op)
      (case op
	 ((<) '>=)
	 ((<=) '>)
	 ((>) '<=)
	 ((>=) '<)
	 (else op)))

   (define (argsrange-test-number op num)
      (with-access::J2SNumber num (val)
	 (case op
	    ((>= == ===) (values val #f '() '()))
	    ((>) (values (+ val 1) #f '() '()))
	    ((<) (values #f val '() '()))
	    ((<=) (values #f (+ 1 val) '() '()))
	    (else (values #f #f '() '())))))

   (define (argsrange-test-ref op ref)
      (with-access::J2SRef ref (decl)
	 (if (memq decl env)
	     (case op
		((>=) (values #f #f '() (list decl)))
		((<) (values #f #f (list decl) '()))
		(else (values #f #f '() '())))
	     (values #f #f '() '()))))

   (if (isa? this J2SBinary)
       (with-access::J2SBinary this (op lhs rhs)
	  (cond
	     ((and (argsrange-length? lhs) (isa? rhs J2SNumber))
	      (argsrange-test-number op rhs))
	     ((and (argsrange-length? rhs) (isa? lhs J2SNumber))
	      (argsrange-test-number (inv op) lhs))
	     ((and (argsrange-length? lhs) (isa? rhs J2SRef))
	      (argsrange-test-ref (inv op) rhs))
	     ((and (argsrange-length? rhs) (isa? lhs J2SRef))
	      (argsrange-test-ref op lhs))
	     (else
	      (values #f #f '() '()))))
       (values #f #f '() '())))

;*---------------------------------------------------------------------*/
;*    argsrange ::J2SIf ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (argsrange this::J2SIf range env)
   (with-access::J2SIf this (test then else)
      (argsrange test range env)
      (multiple-value-bind (nrange+ nrange- env+ env-)
	 (argsrange-test test env)
	 (argsrange then (or nrange+ range) (append env+ env))
	 (argsrange else (or nrange- range) (append env- env)))))

;*---------------------------------------------------------------------*/
;*    argsrange ::J2SSwitch ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (argsrange this::J2SSwitch range env)
   (with-access::J2SSwitch this (key cases)
      (if (argsrange-length? key)
	  (for-each (lambda (c::J2SCase)
		       (with-access::J2SCase c (expr body)
			  (if (isa? expr J2SNumber)
			      (with-access::J2SNumber expr (val)
				 (argsrange body val env))
			      (argsrange body range env))))
	     cases)
	  (call-default-walker))))
      
;*---------------------------------------------------------------------*/
;*    argsrange ::J2SLetBlock ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (argsrange this::J2SLetBlock range env)
   (with-access::J2SLetBlock this (decls nodes)
      (if (or (null? decls) (not (isa? (car nodes) J2SFor)))
	  (call-default-walker)
	  (begin
	     (for-each (lambda (n) (argsrange n range env)) (cdr nodes))
	     (or (argsrange-letblock-for this range env)
		 (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    argsrange-letblock-for ...                                       */
;*    -------------------------------------------------------------    */
;*    This is where the true range analysis happens. It analyses       */
;*                                                                     */
;*       (letblock (...(v0 init0)...)                                  */
;*          (for ( ; test; incr) body))                                */
;*                                                                     */
;*    It checks if v0 is in the range [0..arguments.length].           */
;*---------------------------------------------------------------------*/
(define (argsrange-letblock-for this::J2SLetBlock range env)
   
   (define (inc this::J2SExpr)
      ;; check that this is an inc/dec expression, returns the decl
      ;; of assigned variable if it is.
      (when (isa? this J2SAssig)
	 ;; e1 = e2
	 (with-access::J2SAssig this (lhs rhs)
	    (cond
	       ((and (isa? lhs J2SRef) (isa? rhs J2SBinary))
		;; v = op(x, y)
		(with-access::J2SRef lhs (decl)
		   (with-access::J2SBinary rhs (op lhs rhs)
		      (when (memq op '(+ ++ - --))
			 ;; v = binop(x, y)
			 (cond
			    ((isa? lhs J2SRef)
			     (with-access::J2SRef lhs ((ldecl decl))
				(when (eq? ldecl decl)
				   ;; v = binop(v, y)
				   (when (isa? rhs J2SNumber)
				      ;; v = binop(v, num)
				      (with-access::J2SNumber rhs (val)
					 (when (= val 1)
					    ;; v = binop(v, 1)
					    (cons decl op)))))))
			    ((and (memq op '(+ ++)) (isa? rhs J2SRef))
			     (with-access::J2SRef rhs ((ldecl decl))
				(when (eq? ldecl decl)
				   ;; v = +(x, v)
				   (when (isa? lhs J2SNumber)
				      ;; v = +(num, b)
				      (with-access::J2SNumber lhs (val)
					 (when (= val 1)
					    ;; v = +(1, v)
					    (cons decl '+))))))))))))
	       ((and (isa? this J2SAssigOp) (isa? lhs J2SRef))
		(with-access::J2SAssigOp this (op)
		   (when (memq op '(+ -))
		      ;; v op= val
		      (with-access::J2SRef lhs (decl)
			 (when (isa? rhs J2SNumber)
			    ;; v op= num
			    (with-access::J2SNumber rhs (val)
			       (when (= val 1)
				  ;; v op= 1
				  (cons decl op))))))))))))

   (define (incs::pair-nil this::J2SExpr)
      (if (isa? this J2SSeq)
	  (with-access::J2SSeq this (nodes)
	     (let loop ((nodes nodes)
			(incs '()))
		(cond
		   ((null? nodes)
		    incs)
		   ((inc (car nodes))
		    =>
		    (lambda (inc)
		       (loop (cdr nodes) (cons inc incs))))
		   (else
		    (loop (cdr nodes) incs)))))
	  (let ((inc (inc this)))
	     (if inc
		 (list inc)
		 '()))))

   (define (arguments-length?::bool this::J2SExpr)
      (or (argsrange-length? this)
	  (when (isa? this J2SRef)
	     (with-access::J2SRef this (decl)
		(when (decl-ronly? decl)
		   (when (isa? decl J2SDeclInit)
		      (with-access::J2SDeclInit decl (val)
			 (argsrange-length? val))))))))
	     
   (define (find-test- decl test::J2SExpr)
      (when (isa? test J2SBinary)
	 (with-access::J2SBinary test (op lhs rhs)
	    (cond
	       ((eq? op '>=)
		;; (>= lhs rhs)
		(when (and (isa? lhs J2SRef) (isa? rhs J2SNumber))
		   ;; (>= var num)
		   (with-access::J2SRef lhs ((vdecl decl))
		      (with-access::J2SNumber rhs (val)
			 ;; (>= decl 0)
			 (and (eq? vdecl decl) (= val 0))))))
	       ((eq? op 'and)
		(or (find-test- decl lhs) (find-test- decl rhs)))))))

   (define (find-test+ decl test::J2SExpr)
      (when (isa? test J2SBinary)
	 (with-access::J2SBinary test (op lhs rhs)
	    (cond
	       ((eq? op '<)
		;; (< lhs rhs)
		(when (and (isa? lhs J2SRef) (arguments-length? rhs))
		   ;; (< var arguments.length)
		   (with-access::J2SRef lhs ((vdecl decl))
		      (with-access::J2SNumber rhs (val)
			 ;; (< decl arguments.length)
			 (eq? vdecl decl)))))
	       ((eq? op 'and)
		(or (find-test+ decl lhs) (find-test+ decl rhs)))))))
   
   (define (in-arguments-range+? decl::J2SDecl test::J2SExpr)
      (find-test+ decl test))
   
   (define (in-arguments-range-? decl::J2SDecl test::J2SExpr)
      ;; decl--, find a test decl >= 0 
      (when (find-test- decl test)
	 (when (isa? decl J2SDeclInit)
	    (with-access::J2SDeclInit decl (val)
	       (arguments-length? val)))))
      
   (define (in-arguments-range? decl::J2SDecl op::symbol test::J2SExpr)
      (if (memq op '(+ ++))
	  (in-arguments-range+? decl test)
	  (in-arguments-range-? decl test)))

   (define (find-arguments-induction-variable test::J2SExpr incr decls::pair)
      (find (lambda (inc)
	       (let ((d (car inc)))
		  (when (memq d decls)
		     (in-arguments-range? d (cdr inc) test))))
	 (incs incr)))
   
   (with-access::J2SLetBlock this (decls nodes)
      (with-access::J2SFor (car nodes) (init test incr body)
	 (let ((incs (incs incr)))
	    (let ((indv (find-arguments-induction-variable test incr decls)))
	       (when indv
		  (let ((decl (car indv)))
		     (when (ronly-in? body decl)
			(argsrange body range (cons decl env))))))))))
   
;*---------------------------------------------------------------------*/
;*    ronly-in? ...                                                    */
;*---------------------------------------------------------------------*/
(define (ronly-in? body::J2SStmt decl::J2SDecl)
   (let ((cell (make-cell #t)))
      (ronly? body decl cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    ronly? ...                                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly? this::J2SNode decl::J2SDecl cell::cell)
   (when (cell-ref cell)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    ronly? ::J2SAssig ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (ronly? this::J2SAssig decl cell)
   (with-access::J2SAssig this (lhs)
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs ((vdecl decl))
	     (when (eq? vdecl decl)
		(cell-set! cell #f)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    patch-type ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function propagates the type int53 for ARGUMENTS.length     */
;*    expressions, when ARGUMENTS is known not to escape.              */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-type this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    patch-type ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (patch-type this::J2SRef)
   (with-access::J2SRef this (decl type)
      (unless (eq? type 'int53)
	 (with-access::J2SDecl decl (vtype)
	    (when (eq? vtype 'int53)
	       (set! type 'int53))))))

;*---------------------------------------------------------------------*/
;*    patch-type ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-type this::J2SDeclInit)
   (call-default-walker)
   (with-access::J2SDeclInit this (vtype ctype itype mtype usage val usage)
      (let ((ty (j2s-type val)))
	 (when (and (decl-ronly? this)
		    (eq? ty 'int53)
		    (not (eq? vtype 'int53)) )
	    (set! mtype 'int53)
	    (set! vtype 'int53)
	    (set! ctype 'int53)
	    (set! itype 'int53)))))

;*---------------------------------------------------------------------*/
;*    patch-type ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-type this::J2SAccess)
   (if (argsrange-length? this)
       (with-access::J2SAccess this (obj type)
	  (with-access::J2SRef obj (decl)
	     (with-access::J2SDecl decl (usage)
		(when (usage-strict? usage '(getx length spread rest))
		   (set! type 'int53)))))
       (call-default-walker)))
