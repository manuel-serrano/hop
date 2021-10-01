;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/procedure.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 27 07:35:02 2019                          */
;*    Last change :  Fri Oct  1 16:46:05 2021 (serrano)                */
;*    Copyright   :  2019-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Procedure optimization.                                          */
;*                                                                     */
;*    This optimization replaces full-fledged JS functions with        */
;*    lighter Scheme procedures. A JS function is replaced with a      */
;*    SCM procedure when the compiler can keep track of all its        */
;*    usages and when it can show that it is only used in application  */
;*    nodes.                                                           */
;*                                                                     */
;*    This optimization annotates the AST nodes:                       */
;*      J2SFun.mode: opt procedures are marked with "scheme" mode      */
;*      J2SCall.protocol: opt calls are marked with "scheme" proto     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_procedure
   
   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-procedure-stage))

;*---------------------------------------------------------------------*/
;*    j2s-procedure-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-procedure-stage
   (instantiate::J2SStageProc
      (name "procedure")
      (comment "Procedure optimization")
      (proc j2s-procedure!)
      (optional (lambda (conf)
		   (and (config-get conf :optim-procedure #f)
			(not (config-get conf :profile-call #f)))))))

;*---------------------------------------------------------------------*/
;*    j2s-procedure! ...                                               */
;*    -------------------------------------------------------------    */
;*    compute the escape property by fix point iteration               */
;*    a function escapes if one of the following is meet:              */
;*       - it is passed to an escape function                          */
;*       - it is stored in an object                                   */
;*       - it is assiged to an unknown variable                        */
;*       - one of its attributes is read or written                    */
;*---------------------------------------------------------------------*/
(define (j2s-procedure! this conf)
   (when (isa? this J2SProgram)
      ;; prepare optimization dump
      (when (or *debug-procedure*
		*debug-procedure-ast-pre*
		*debug-procedure-ast-post*)
	 (j2s-dump-register-struct-info! 'node-procedure-info
	    (lambda (i) (node-procedure-info->list i)))
	 (j2s-dump-register-struct-info! 'fun-procedure-info
	    (lambda (i) (fun-procedure-info->list i))))
      (with-access::J2SProgram this (direct-eval)
	 (unless direct-eval
	    ;; the fix point iteration
	    (let ((fix (make-cell #f)))
	       (let loop ((i 1)
			  (s #\space))
		  (when (>=fx (config-get conf :verbose 0) 3)
		     (display s (current-error-port))
		     (display i (current-error-port)))
		  (cell-set! fix #t)
		  (eval-procedure this fix)
		  (unless (cell-ref fix)
		     (loop (+fx i 1) #\.))))
	    ;; dump the decorate tree
	    (when *debug-procedure-ast-pre*
	       (pp/width (j2s->sexp this) (current-output-port)))
	    ;; disable all non-optimizable functions
	    (disable-non-optimizable this)
	    ;; dump the decorate tree
	    (when *debug-procedure-ast-post*
	       (pp/width (j2s->sexp this) (current-output-port)))
	    ;; annotate the ast
	    (annotate-procedure this conf))))
   this)

;*---------------------------------------------------------------------*/
;*    debug control                                                    */
;*---------------------------------------------------------------------*/
(define *debug-env*
   (or (getenv "HOPTRACE") ""))
(define *debug-procedure*
   (string-contains *debug-env* "j2s:procedure"))
(define *debug-procedure-unfix*
   (string-contains *debug-env* "j2s:procedure-unfix"))
(define *debug-procedure-escape*
   (string-contains *debug-env* "j2s:procedure-escape"))
(define *debug-procedure-ast-pre*
   (string-contains *debug-env* "j2s:procedure-ast-pre"))
(define *debug-procedure-ast-post*
   (string-contains *debug-env* "j2s:procedure-ast-post"))

;*---------------------------------------------------------------------*/
;*    node-procedure-info ...                                          */
;*---------------------------------------------------------------------*/
(define-struct node-procedure-info vals optimizablep)
(define-struct fun-procedure-info retvals escapep optimizablep)

;*---------------------------------------------------------------------*/
;*    vals->list ...                                                   */
;*---------------------------------------------------------------------*/
(define (vals->list vals)
   (if (eq? vals 'top)
       'top
       (map (lambda (f)
	       (with-access::J2SFun f (%info loc params)
		  (if (fun-procedure-info? %info)
		      `(@ ,(caddr loc)
			  :opt ,(fun-procedure-info-optimizablep %info)
			  :escape ,(fun-procedure-info-escapep %info))
		      `(@ ,(caddr loc)))))
	  vals)))

;*---------------------------------------------------------------------*/
;*    node-procedure-info->list ...                                    */
;*---------------------------------------------------------------------*/
(define (node-procedure-info->list info)
   `(:opt ,(node-procedure-info-optimizablep info)
       :vals ,(vals->list (node-procedure-info-vals info))))

;*---------------------------------------------------------------------*/
;*    fun-procedure-info->list ...                                     */
;*---------------------------------------------------------------------*/
(define (fun-procedure-info->list info)
   `(:escape ,(fun-procedure-info-escapep info)
       :opt ,(fun-procedure-info-optimizablep info)
       :retvals ,(vals->list (fun-procedure-info-retvals info))))
	   
;*---------------------------------------------------------------------*/
;*    node-vals->list ...                                              */
;*---------------------------------------------------------------------*/
(define (node-vals->list this)
   (with-access::J2SNode this (%info)
      (if (not (node-procedure-info? %info))
	  (typeof %info)
	  (node-procedure-info->list %info))))

;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix::cell reason loc)
   (when *debug-procedure-unfix* (tprint "unfix " reason " " loc))
   (cell-set! fix #f))

;*---------------------------------------------------------------------*/
;*    node-init-info! ...                                              */
;*---------------------------------------------------------------------*/
(define (node-init-info! this::J2SNode vals fix::cell)
   (when (isa? this J2SFun)
      (error "node-init-info!" "Illegal type" (j2s->sexp this)))
   (with-access::J2SNode this (%info loc)
      (unfix! fix "node-init-info!" loc)
      (set! %info (node-procedure-info vals #t))
      vals))

;*---------------------------------------------------------------------*/
;*    fun-init-info! ...                                               */
;*---------------------------------------------------------------------*/
(define (fun-init-info! this::J2SFun retvals esc::bool fix::cell)
   (with-access::J2SFun this (%info params loc thisp)
      (unfix! fix "fun-init-info!" loc)
      (set! %info (fun-procedure-info retvals esc #t))
      (for-each (lambda (p) (node-init-info! p '() fix)) params)
      (when thisp (node-init-info! thisp 'top fix))
      (list this)))

;*---------------------------------------------------------------------*/
;*    escape! ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (escape! this fix)
   (cond
      ((eq? this 'top) #unspecified)
      ((pair? this) (for-each (lambda (v) (escape! v fix)) this))
      ((null? this) #unspecified)
      (else (error "escape!" "Illegal type" this))))

;*---------------------------------------------------------------------*/
;*    escape! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-method (escape! this::J2SNode fix)
   (with-access::J2SNode this (%info loc)
      (when *debug-procedure-escape* (tprint "escape " (typeof this) " " loc))
      (cond
	 ((not (node-procedure-info? %info))
	  (node-init-info! this 'top fix))
	 ((eq? (node-procedure-info-vals %info) 'top)
	  'top)
	 (else
	  (unfix! fix (string-append "escape! ::" (typeof this)) loc)
	  (for-each (lambda (v) (escape! v fix))
	     (node-procedure-info-vals %info))
	  (node-procedure-info-vals-set! %info 'top)
	  'top))))

;*---------------------------------------------------------------------*/
;*    escape! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (escape! this::J2SFun fix)
   (with-access::J2SFun this (params %info loc)
      (when *debug-procedure-escape* (tprint "escape " (typeof this) " " loc))
      (cond
	 ((not (fun-procedure-info? %info))
	  (fun-init-info! this 'top #t fix)
	  (for-each (lambda (p) (escape! p fix)) params))
	 ((fun-procedure-info-escapep %info)
	  #unspecified)
	 (else
	  (unfix! fix "escape! ::J2SFun" loc)
	  (fun-procedure-info-escapep-set! %info #t)
	  (fun-procedure-info-optimizablep-set! %info #f)
	  (for-each (lambda (p) (escape! p fix)) params)
	  (when (pair? (fun-procedure-info-retvals %info))
	     (for-each (lambda (v) (escape! v fix))
		(fun-procedure-info-retvals %info)))
	  (fun-procedure-info-retvals-set! %info 'top))))
   (list this))

;*---------------------------------------------------------------------*/
;*    node-add-vals! ...                                               */
;*---------------------------------------------------------------------*/
(define (node-add-vals! this::J2SNode vals fix::cell)
   (with-access::J2SNode this (%info loc)
      (cond
	 ((not (node-procedure-info? %info))
	  (node-init-info! this vals fix))
	 ((eq? vals 'top)
	  (escape! this fix))
	 ((eq? (node-procedure-info-vals %info) 'top)
	  (for-each (lambda (v) (escape! v fix)) vals)
	  'top)
	 (else
	  (for-each (lambda (v)
		       (unless (memq v (node-procedure-info-vals %info))
			  (node-procedure-info-vals-set! %info
			     (cons v (node-procedure-info-vals %info)))))
	     vals)
	  vals))))

;*---------------------------------------------------------------------*/
;*    expr-add-vals! ...                                               */
;*---------------------------------------------------------------------*/
(define (expr-add-vals! this::J2SExpr vals fix::cell)
   (node-add-vals! this vals fix))

;*---------------------------------------------------------------------*/
;*    decl-add-vals! ...                                               */
;*---------------------------------------------------------------------*/
(define (decl-add-vals! this::J2SDecl vals fix::cell)
   (node-add-vals! this vals fix))

;*---------------------------------------------------------------------*/
;*    fun-add-retvals! ...                                             */
;*---------------------------------------------------------------------*/
(define (fun-add-retvals! this::J2SFun retvals fix::cell)
   (with-access::J2SFun this (%info loc)
      (cond
	 ((not (fun-procedure-info? %info))
	  (fun-init-info! this retvals #f fix))
	 ((or (eq? (fun-procedure-info-retvals %info) 'top)
	      (fun-procedure-info-escapep %info))
	  (when (pair? retvals)
	     (for-each (lambda (v) (escape! v fix)) retvals)))
	 ((eq? retvals 'top)
	  (unfix! fix "fun-add-retvals" loc)
	  (for-each (lambda (v) (escape! v fix))
	     (fun-procedure-info-retvals %info))
	  (fun-procedure-info-retvals-set! %info 'top))
	 (else
	  (for-each (lambda (v)
		       (unless (memq v (fun-procedure-info-retvals %info))
			  (fun-procedure-info-retvals-set! %info
			     (cons v (fun-procedure-info-retvals %info)))))
	     retvals)))))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SNode ...                                     */
;*    -------------------------------------------------------------    */
;*    The EVAL-PROCEDURE fuction returns:                              */
;*      - top                                                          */
;*      - a list of procedures                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SNode fix)
   (call-default-walker)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SExpr ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SExpr fix)
   (let* ((clazz (object-class this))
	  (fields (class-all-fields clazz)))
      (let loop ((i (-fx (vector-length fields) 1)))
	 (when (>=fx i 0)
	    (let* ((f (vector-ref-ur fields i))
		   (fi (class-field-info f))
		   (v ((class-field-accessor f) this)))
	       (cond
		  ((and (pair? fi) (member "notraverse" fi))
		   #unspecified)
		  ((pair? v)
		   (for-each (lambda (e)
				(when (isa? e J2SNode)
				   (escape! (eval-procedure e fix) fix)) )
		      v))
		  ((isa? v J2SNode)
		   (escape! (eval-procedure v fix) fix)))
	       (loop (-fx i 1)))))
      (node-add-vals! this 'top fix)))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SLiteralValue ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SLiteralValue fix)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SNull ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SNull fix)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SUndefined ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SUndefined fix)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SDProducer ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SDProducer fix)
   (with-access::J2SDProducer this (decl expr)
      (escape! decl fix)
      (escape! (eval-procedure expr fix) fix))
   'top)

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SDataPropertyInit ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SDataPropertyInit fix)
   (with-access::J2SDataPropertyInit this (val)
      (escape! (eval-procedure val fix) fix)
      'top))
      
;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SAccessorPropertyInit ...                     */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SAccessorPropertyInit fix)
   (with-access::J2SAccessorPropertyInit this (get set)
      (escape! (eval-procedure get fix) fix)
      (escape! (eval-procedure set fix) fix)
      'top))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SRef ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SRef fix)
   (with-access::J2SRef this (decl (refinfo %info))
      (with-access::J2SDecl decl (%info)
	 (if (node-procedure-info? %info)
	     (begin
		(set! refinfo %info)
		(node-procedure-info-vals %info))
	     (let ((v (node-init-info! decl '() fix)))
		(set! refinfo %info)
		v)))))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SDeclFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SDeclFun fix)
   (with-access::J2SDeclFun this (val %info id key id)
      (call-default-walker)
      (decl-add-vals! this (eval-procedure val fix) fix)))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SDeclInit ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SDeclInit fix)
   (with-access::J2SDeclInit this (val %info id loc key)
      (call-default-walker)
      (decl-add-vals! this (eval-procedure val fix) fix)))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SDeclExtern ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SDeclExtern fix)
   (with-access::J2SDeclExtern this (val %info id loc)
      (escape! (call-next-method) fix)))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SAssig fix)
   (with-access::J2SAssig this (lhs rhs)
      (let ((rhsv (eval-procedure rhs fix)))
	 (if (isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(with-access::J2SDecl decl (scope)
		   (if (memq scope '(global %scope))
		       (begin
			  (escape! rhsv fix)
			  (escape! decl fix)
			  'top)
		       (decl-add-vals! decl rhsv fix))))
	     (let ((lhsv (eval-procedure lhs fix)))
		(escape! lhsv fix)
		(escape! rhsv fix)
		'top)))))
		 
;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SFun fix)
   (with-access::J2SFun this (body %info loc params)
      (unless (fun-procedure-info? %info)
	 (fun-init-info! this '() #f fix))
      (eval-procedure body fix)
      (list this)))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SMethod ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SMethod fix)
   (with-access::J2SMethod this (function method)
      (escape! (eval-procedure function fix) fix)
      (escape! (eval-procedure method fix) fix)
      'top))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SReturn ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SReturn fix)
   (with-access::J2SReturn this (expr from loc)
      (let ((vals (eval-procedure expr fix)))
	 (cond
	    ((isa? from J2SFun)
	     (fun-add-retvals! from vals fix))
	    ((isa? from J2SBindExit)
	     (expr-add-vals! from vals fix))
	    (else
	     (escape! vals fix))))))

;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SBindExit ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SBindExit fix)
   (with-access::J2SBindExit this (%info stmt)
      (unless (node-procedure-info? %info)
	 (node-init-info! this '() fix))
      (eval-procedure stmt fix)
      (node-procedure-info-vals %info)))
		       
;*---------------------------------------------------------------------*/
;*    eval-procedure ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-procedure this::J2SCall fix)
   
   (define (bind-args params argsv fix)
      (let loop ((params params)
		 (argsv argsv))
	 (cond
	    ((null? argsv)
	     (for-each (lambda (p) (escape! p fix)) params))
	    ((pair? params)
	     (decl-add-vals! (car params) (car argsv) fix)
	     (loop (cdr params) (cdr argsv))))))
   
   (define (call fun::J2SFun argsv::pair-nil fix::cell)
      (with-access::J2SFun fun (params %info)
	 (bind-args params argsv fix)
	 (if (fun-procedure-info? %info)
	     (fun-procedure-info-retvals %info)
	     (fun-init-info! fun '() #f fix))))
   
   (with-access::J2SCall this (fun thisargs args loc %info loc)
      ;; initialize the call if needed
      (unless (node-procedure-info? %info)
	 (node-init-info! this '() fix))
      ;; escape untracked this argument
      (for-each (lambda (t) (escape! t fix)) thisargs)
      ;; evaluate all components
      (let ((funvals (eval-procedure fun fix))
	    (thisvals (map (lambda (t) (eval-procedure t fix)) thisargs))
	    (argsvals (map (lambda (a) (eval-procedure a fix)) args)))
	 (if (eq? funvals 'top)
	     (begin
		;;(for-each (lambda (a) (escape! a fix)) args)
		(for-each (lambda (a) (escape! a fix)) argsvals)
		(escape! this fix))
	     (let ((vals* (map (lambda (fv) (call fv argsvals fix)) funvals)))
		(if (eq? (node-procedure-info-vals %info) 'top)
		    (for-each (lambda (vals) (escape! vals fix)) vals*)
		    (for-each (lambda (vals) (node-add-vals! this vals fix)) vals*))
		(node-procedure-info-vals %info))))))

;*---------------------------------------------------------------------*/
;*    disable-non-optimizable ...                                      */
;*---------------------------------------------------------------------*/
(define (disable-non-optimizable this)
   (let ((fix (make-cell #t)))
      (let loop ()
	 (optimize-procedure this fix)
	 (unless (cell-ref fix)
	    (cell-set! fix #t)
	    (loop)))))

;*---------------------------------------------------------------------*/
;*    optimize-procedure ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (optimize-procedure this::J2SNode fix)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    optimize-procedure ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (optimize-procedure this::J2SFun fix)
   (call-default-walker)
   (with-access::J2SFun this (vararg generator thisp argumentsp %info loc)
      (when (fun-procedure-info-optimizablep %info)
	 (when (or generator vararg argumentsp
		   (and (not thisp) (not (isa? this J2SArrow))))
	    (fun-procedure-info-optimizablep-set! %info #f)
	    (cell-set! fix #f)))))

;*---------------------------------------------------------------------*/
;*    optimize-procedure ::J2SExpr ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (optimize-procedure this::J2SExpr fix)
   (call-default-walker)
   (with-access::J2SExpr this (%info loc)
      (when (node-procedure-info? %info)
	 (when (node-procedure-info-optimizablep %info)
	    (let ((vals (node-procedure-info-vals %info)))
	       (when (pair? vals)
		  (when (find (lambda (v)
				 (with-access::J2SFun v (%info)
				    (not (fun-procedure-info-optimizablep %info))))
			   vals)
		     (cell-set! fix #f)
		     (for-each (lambda (v)
				  (with-access::J2SFun v (%info)
				     (fun-procedure-info-optimizablep-set! %info #f)))
			vals)
		     (node-procedure-info-optimizablep-set! %info #f))))))))

;*---------------------------------------------------------------------*/
;*    optimize-procedure ::J2SCall ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (optimize-procedure this::J2SCall fix)
   (call-next-method)
   (with-access::J2SCall this (fun loc protocol)
      (with-access::J2SExpr fun (%info)
	 (when (node-procedure-info? %info)
	    (when (node-procedure-info-optimizablep %info)
	       (let ((vals (node-procedure-info-vals %info)))
		  (when (pair? vals)
		     (when (find (lambda (v)
				    (with-access::J2SFun v (%info)
				       (not (fun-procedure-info-optimizablep %info))))
			      vals)
			(cell-set! fix #f)
			(for-each (lambda (v)
				     (with-access::J2SFun v (%info)
					(fun-procedure-info-optimizablep-set! %info #f)))
			   vals)
			(node-procedure-info-optimizablep-set! %info #f)))))))))

;*---------------------------------------------------------------------*/
;*    optimize-procedure ::J2SDecl ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (optimize-procedure this::J2SDecl fix)
   (call-default-walker)
   (with-access::J2SDecl this (%info id scope)
      (when (node-procedure-info? %info)
	 (when (node-procedure-info-optimizablep %info)
	    (let ((vals (node-procedure-info-vals %info)))
	       (when (pair? vals)
		  (when (find (lambda (v)
				 (with-access::J2SFun v (%info)
				    (not (fun-procedure-info-optimizablep %info))))
			   vals)
		     (cell-set! fix #f)
		     (node-procedure-info-optimizablep-set! %info #f))))))))

;*---------------------------------------------------------------------*/
;*    optimize-procedure ::J2SDeclInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (optimize-procedure this::J2SDeclInit fix)
   (call-default-walker)
   (when (read-only-function? this)
      (with-access::J2SDeclInit this (%info val id loc)
	 (when (node-procedure-info-optimizablep %info)
	    (let loop ((val val))
	       (cond
		  ((isa? val J2SFun)
		   (with-access::J2SFun val ((vinfo %info))
		      (cond
			 ((not (fun-procedure-info-optimizablep vinfo))
			  (cell-set! fix #f)
			  (node-procedure-info-optimizablep-set! %info #f))
			 ((not (decl-usage-has? this '(ref assig)))
			  (cell-set! fix #f)
			  (node-procedure-info-optimizablep-set! %info #f)
			  (fun-procedure-info-optimizablep-set! vinfo #f)))))
		  ((isa? val J2SMethod)
		   (with-access::J2SMethod val (function)
		      (loop function)))
		  (else
		   (with-access::J2SExpr val ((vinfo %info))
		      (unless (node-procedure-info-optimizablep vinfo)
			 (cell-set! fix #f)
			 (node-procedure-info-optimizablep-set! %info #f))))))))))
      
;*---------------------------------------------------------------------*/
;*    annotate-procedure ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-procedure this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    annotate-procedure ::J2SExpr ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-procedure this::J2SExpr conf)
   (call-default-walker)
   (with-access::J2SExpr this (%info type)
      (when (node-procedure-info? %info)
	 (when (and (node-procedure-info-optimizablep %info)
		    (pair? (node-procedure-info-vals %info)))
	    (set! type 'procedure)))))

;*---------------------------------------------------------------------*/
;*    annotate-procedure ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-procedure this::J2SFun conf)
   (with-access::J2SFun this (%info loc mode params name rtype type id)
      (when (and (fun-procedure-info-optimizablep %info)
		 (not (fun-procedure-info-escapep %info)))
	 (when (>=fx (config-get conf :verbose 0) 3)
	    (fprintf (current-error-port) " ~a@~a" name (caddr loc)))
	 (set! type 'procedure))
      (when (and (pair? (fun-procedure-info-retvals %info))
		 (with-access::J2SFun (car (fun-procedure-info-retvals %info)) (%info)
		    (fun-procedure-info-optimizablep %info)))
	 (set! rtype 'procedure))
      (for-each (lambda (p)
		   (with-access::J2SDecl p (vtype %info id)
		      (when (and (node-procedure-info-optimizablep %info)
				 (pair? (node-procedure-info-vals %info)))
			 (set! vtype 'procedure))))
	 params)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    annotate-procedure ::J2SCall ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-procedure this::J2SCall conf)
   
   (define (correct-arities? funs arity)
      (every (lambda (f)
		(with-access::J2SFun f (params vararg)
		   (and (=fx (length params) arity) (not vararg))))
	 funs))
   
   (with-access::J2SCall this (fun loc protocol args)
      (with-access::J2SExpr fun (%info)
	 (cond
	    ((node-procedure-info? %info)
	     (when (pair? (node-procedure-info-vals %info))
		(cond
		   ((node-procedure-info-optimizablep %info)
		    (set! protocol
		       (if (correct-arities? (node-procedure-info-vals %info)
			      (length args))
			   'procedure-this
			   'procedure-this-arity)))
		   ((not (eq? protocol 'spread))
		    (set! protocol
		       (if (correct-arities? (node-procedure-info-vals %info)
			      (length args))
			   'function
			   'function-arity))))))
	    ((fun-procedure-info? %info)
	     (when (fun-procedure-info-optimizablep %info)
		(set! protocol
		   (if (correct-arities? (list fun) (length args))
		       'procedure-this
		       'procedure-this-arity))))))
      (when (>=fx (config-get conf :verbose 0) 3)
	 (case protocol
	    ((procedure-this)
	     (fprintf (current-error-port) " ~a+++" (caddr loc)))
	    ((procedure-this-arity)
	     (fprintf (current-error-port) " ~a++" (caddr loc)))
	    ((function)
	     (fprintf (current-error-port) " ~a+" (caddr loc)))
	    ((function-arity)
	     (fprintf (current-error-port) " ~a" (caddr loc)))))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    annotate-procedure ::J2SDeclInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-procedure this::J2SDeclInit conf)
   (call-default-walker)
   (with-access::J2SDeclInit this (vtype id loc key val %info)
      (let loop ((val val))
	 (with-access::J2SExpr val (%info)
	    (cond
	       ((isa? val J2SMethod)
		(with-access::J2SMethod val (function)
		   (loop function)))
	       ((node-procedure-info? %info)
		(when (and (node-procedure-info-optimizablep %info)
			   (pair? (node-procedure-info-vals %info)))
		   (set! vtype 'procedure)))
	       ((fun-procedure-info? %info)
		(when (fun-procedure-info-optimizablep %info)
		   (set! vtype 'procedure))))))))

;*---------------------------------------------------------------------*/
;*    annotate-procedure ::J2SDecl ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-procedure this::J2SDecl conf)
   (call-default-walker)
   (with-access::J2SDecl this (vtype id loc key %info)
      (when (node-procedure-info? %info)
	 (when (and (node-procedure-info-optimizablep %info)
		    (pair? (node-procedure-info-vals %info)))
	    (set! vtype 'procedure)))))

;*---------------------------------------------------------------------*/
;*    read-only-function? ...                                          */
;*---------------------------------------------------------------------*/
(define (read-only-function? decl)
   (cond
      ((isa? decl J2SDeclSvc)
       #t)
      ((isa? decl J2SDeclFun)
       (decl-ronly? decl))
      ((j2s-let-opt? decl)
       (with-access::J2SDeclInit decl (val)
	  (and (isa? val J2SFun) (decl-ronly? decl))))
      (else
       #f)))

