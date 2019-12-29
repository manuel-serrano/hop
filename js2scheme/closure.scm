;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/closure.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 27 07:35:02 2019                          */
;*    Last change :  Sun Dec 29 19:19:51 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Closure optimization.                                            */
;*                                                                     */
;*    This optimization replaces full-fledged JS functions with        */
;*    lighter Scheme closures. A JS function is replaced with a        */
;*    SCM closure when the compiler can keep track of all its          */
;*    usages and when it can show that it is only used in application  */
;*    nodes.                                                           */
;*                                                                     */
;*    This optimization annotates the AST nodes:                       */
;*      J2SFun.mode: opt closures are marked with "scheme" mode        */
;*      J2SCall.protocol: opt calls are marked with "scheme" proto     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_closure
   
   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-closure-stage))

;*---------------------------------------------------------------------*/
;*    j2s-closure-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-closure-stage
   (instantiate::J2SStageProc
      (name "closure")
      (comment "Closure optimization")
      (proc j2s-closure!)
      (optional :optim-closure)))

;*---------------------------------------------------------------------*/
;*    j2s-closure! ...                                                 */
;*    -------------------------------------------------------------    */
;*    compute the escape property by fix point iteration               */
;*    a function escapes if one of the following is meet:              */
;*       - it is passed to an escape function                          */
;*       - it is stored in an object                                   */
;*       - it is assiged to an unknown variable                        */
;*       - one of its attributes is read or written                    */
;*---------------------------------------------------------------------*/
(define (j2s-closure! this conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (direct-eval)
	 (unless direct-eval
	    ;; the fix point iteration
	    (let ((fix (make-cell #f)))
	       (let loop ()
		  (cell-set! fix #t)
		  (eval-closure this fix)
		  (unless (cell-ref fix)
		     (loop))))
	    ;; annotate the ast
	    (annotate-closure this conf))))
   this)

;*---------------------------------------------------------------------*/
;*    debug control                                                    */
;*---------------------------------------------------------------------*/
(define *debug-env* (or (getenv "HOPTRACE") ""))
(define *debug-closure* (string-contains *debug-env* "j2s:closure"))

;*---------------------------------------------------------------------*/
;*    node-closure-info ...                                            */
;*---------------------------------------------------------------------*/
(define-struct node-closure-info vals)
(define-struct fun-closure-info retvals escapep)

;*---------------------------------------------------------------------*/
;*    vals->list ...                                                   */
;*---------------------------------------------------------------------*/
(define (vals->list vals)
   (if (eq? vals 'top)
       'top
       (map (lambda (f) (with-access::J2SFun f (loc) loc)) vals)))

;*---------------------------------------------------------------------*/
;*    node-closure-info->sexp ...                                      */
;*---------------------------------------------------------------------*/
(define (node-closure-info->sexp info)
   (cond
      ((node-closure-info? info)
       (if (eq? (node-closure-info-vals info) 'top)
	   'top
	   (map (lambda (f) (with-access::J2SFun f (loc) loc))
	      (node-closure-info-vals info))))
      ((fun-closure-info? info)
       (vector (if (fun-closure-info-escapep info) 'escape '-)
	  (if (eq? (fun-closure-info-retvals info) 'top)
	      'top
	      (map (lambda (f) (with-access::J2SFun f (loc) loc))
		 (fun-closure-info-retvals info)))))
      (else
       (typeof info))))

;*---------------------------------------------------------------------*/
;*    unfix! ...                                                       */
;*---------------------------------------------------------------------*/
(define (unfix! fix::cell reason loc)
   (when *debug-closure*
      (tprint "unfix " reason " " loc))
   (cell-set! fix #f))

;*---------------------------------------------------------------------*/
;*    eval-init-info! ...                                              */
;*---------------------------------------------------------------------*/
(define (eval-init-info! this::J2SNode vals fix::cell)
   (with-access::J2SNode this (%info loc)
      (set! %info (node-closure-info vals))
      (unfix! fix "eval-init-info!" loc)
      vals))

;*---------------------------------------------------------------------*/
;*    fun-init-info! ...                                               */
;*---------------------------------------------------------------------*/
(define (fun-init-info! this::J2SFun vals escapep fix::cell)
   (with-access::J2SFun this (%info params loc)
      (set! %info (fun-closure-info vals escapep))
      (for-each (lambda (p) (eval-init-info! p '() fix)) params)
      (unfix! fix "fun-init-info!" loc)
      vals))

;*---------------------------------------------------------------------*/
;*    escape! ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (escape! this fix)
   (when (pair? this)
      (for-each (lambda (t) (escape! t fix)) this))
   'top)

;*---------------------------------------------------------------------*/
;*    escape! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-method (escape! this::J2SNode fix)
   (with-access::J2SNode this (%info loc)
      (cond
	 ((not (node-closure-info? %info))
	  (eval-init-info! this 'top fix))
	 ((eq? (node-closure-info-vals %info) 'top)
	  'top)
	 (else
	  (unfix! fix (string-append "escape! ::" (typeof this)) loc)
	  (for-each (lambda (f) (escape! f fix))
	     (node-closure-info-vals %info))
	  (node-closure-info-vals-set! %info 'top)
	  'top))))

;*---------------------------------------------------------------------*/
;*    escape! ::J2SRef ...                                             */
;*---------------------------------------------------------------------*/
(define-method (escape! this::J2SRef fix)
   (with-access::J2SRef this (decl)
      (escape! decl fix)))

;*---------------------------------------------------------------------*/
;*    escape! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (escape! this::J2SFun fix)
   (with-access::J2SFun this (params %info loc)
      (tprint "escale fun " loc)
      (cond
	 ((not (fun-closure-info? %info))
	  (fun-init-info! this '() #t fix))
	 ((fun-closure-info-escapep %info)
	  'top)
	 (else
	  (unfix! fix "escape! ::J2SFun" loc)
	  (fun-closure-info-escapep-set! %info #t)
	  (for-each (lambda (p) (escape! p fix)) params)
	  'top))))
      
;*---------------------------------------------------------------------*/
;*    node-add-vals! ...                                               */
;*---------------------------------------------------------------------*/
(define (node-add-vals! this::J2SNode vals fix::cell)
   (with-access::J2SNode this (%info loc)
      (cond
	 ((not (node-closure-info? %info))
	  (eval-init-info! this vals fix))
	 ((eq? vals 'top)
	  (escape! this fix)
	  (node-closure-info-vals-set! %info 'top)
	  'top)
	 ((eq? (node-closure-info-vals %info) 'top)
	  (escape! vals fix)
	  'top)
	 (else
	  (for-each (lambda (v)
		       (unless (memq v (node-closure-info-vals %info))
			  (node-closure-info-vals-set! %info
			     (cons v (node-closure-info-vals %info)))
			  (unfix! fix "node-add-vals!" loc)))
	     vals)
	  vals))))



;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SNode ...                                       */
;*    -------------------------------------------------------------    */
;*    The EVAL-CLOSURE fuction returns:                                */
;*      - top                                                          */
;*      - a list of closures                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SNode fix)
   (call-default-walker)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SExpr ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SExpr fix)
   (let* ((clazz (object-class this))
	  (ctor (class-constructor clazz))
	  (inst ((class-allocator clazz)))
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
		   (for-each (lambda (e) (escape! (eval-closure e fix) fix)) v))
		  ((isa? v J2SNode)
		   (escape! (eval-closure v fix) fix))
		  (else
		   (loop (-fx i 1)))))))
      'top))

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SLiteralValue ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SLiteralValue fix)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SNull ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SNull fix)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SUndefined ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SUndefined fix)
   'top)

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SRef fix)
   (with-access::J2SRef this (decl %info)
      (with-access::J2SDecl decl (%info)
	 (if (node-closure-info? %info)
	     (node-closure-info-vals %info)
	     (eval-init-info! decl '() fix)))))

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SDeclFun ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SDeclFun fix)
   (with-access::J2SDeclFun this (val %info)
      (call-default-walker)
      (if (node-closure-info? %info)
	  (node-closure-info-vals %info)
	  (eval-init-info! this (list val) fix))))

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SAssig ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SAssig fix)
   (with-access::J2SAssig this (lhs rhs)
      (let ((rhsv (eval-closure rhs fix)))
	 (if (isa? lhs J2SRef)
	     (with-access::J2SRef lhs (decl)
		(node-add-vals! decl rhsv fix))
	     (let ((lhsv (eval-closure lhs fix)))
		(escape! lhsv fix)
		(escape! rhsv fix)
		'top)))))
		 
;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SFun fix)
   (with-access::J2SFun this (body %info loc params)
      (unless (fun-closure-info? %info)
	 (fun-init-info! this '() #f fix))
      (eval-closure body fix)
      (list this)))

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SMethod ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SMethod fix)
   (with-access::J2SMethod this (function method)
      (escape! (eval-closure function fix) fix)
      (escape! (eval-closure method fix) fix)
      'top))

;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SReturn ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SReturn fix)

   (define (fun-add-retvals! this::J2SFun vals fix::cell locr)
      (with-access::J2SFun this (%info loc)
	 (cond
	    ((eq? vals 'top)
	     (unless (eq? (fun-closure-info-retvals %info) 'top)
		(escape! (fun-closure-info-retvals %info) fix)
		(fun-closure-info-retvals-set! %info 'top)
		(unfix! fix "fun-add-retvals!" loc)))
	    ((eq? (fun-closure-info-retvals %info) 'top)
	     (escape! vals fix))
	    (else
	     (for-each (lambda (v)
			  (unless (memq v (fun-closure-info-retvals %info))
			     (fun-closure-info-retvals-set! %info
				(cons v (fun-closure-info-retvals %info)))
			     (unfix! fix "fun-add-retvals!" loc)))
		vals)))))
   
   (with-access::J2SReturn this (expr from loc)
      (let ((vals (eval-closure expr fix)))
	 (if (isa? from J2SFun)
	     (fun-add-retvals! from vals fix loc)
	     (escape! vals fix)))))
		       
;*---------------------------------------------------------------------*/
;*    eval-closure ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (eval-closure this::J2SCall fix)
   
   (define (for-each-args params argsv fix)
      (let loop ((params params)
		 (argsv argsv))
	 (cond
	    ((null? argsv)
	     (for-each (lambda (p) (escape! p fix)) params))
	    ((pair? params)
	     (node-add-vals! (car params) (car argsv) fix)
	     (loop (cdr params) (cdr argsv))))))
   
   (define (call fun::J2SFun argsv::pair-nil fix::cell)
      (with-access::J2SFun fun (params %info)
	 (for-each-args params argsv fix)
	 (if (fun-closure-info? %info)
	     (fun-closure-info-retvals %info)
	     (fun-init-info! fun '() #f fix))))
   
   (with-access::J2SCall this (fun thisarg args loc %info)
      (unless (node-closure-info? %info)
	 (eval-init-info! this '() fix))
      (for-each (lambda (t) (escape! t fix)) thisarg)
      (let ((funvals (eval-closure fun fix))
	    (thisvals (map (lambda (t) (eval-closure t fix)) thisarg))
	    (argsvals (map (lambda (a) (eval-closure a fix)) args)))
	 (if (eq? funvals 'top)
	     (begin
		(for-each (lambda (a) (escape! a fix)) args)
		(escape! this fix))
	     (let ((vals* (map (lambda (fv) (call fv argsvals fix)) funvals)))
		(if (eq? (node-closure-info-vals %info) 'top)
		    (for-each (lambda (vals) (escape! vals fix)) vals*)
		    (for-each (lambda (vals) (node-add-vals! this vals fix)) vals*))
		(node-closure-info-vals %info))))))

;*---------------------------------------------------------------------*/
;*    annotate-closure ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-closure this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    annotate-closure ::J2SFun ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-closure this::J2SFun conf)
   (with-access::J2SFun this (%info loc mode generator params name)
      (tprint "ANNOATE CLOSURE name=" name " loc=" loc " info=" (typeof %info))
      (when (and (not (fun-closure-info-escapep %info)) (not generator))
	 (when (>=fx (config-get conf :verbose 0) 3)
	    (fprintf (current-error-port) " ~a@~a" name (caddr loc)))
	 (set! mode 'procedure))
      (tprint "ANNOATE CLOSURE.2 name=" name " loc=" loc " info=" (typeof %info))
      (for-each (lambda (p)
		   (with-access::J2SDecl p (vtype %info id)
		      (tprint "p=" id " info=" (typeof %info))
		      (unless (eq? (node-closure-info-vals %info) 'top)
			 (set! vtype 'procedure))))
	 params)
      (tprint "ANNOATE CLOSURE.3 name=" name " loc=" loc " info=" (typeof %info))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    annotate-closure ::J2SCall ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (annotate-closure this::J2SCall conf)
   (with-access::J2SCall this (fun loc protocol)
      (let ((vals (eval-closure fun (make-cell #f))))
	 (unless (eq? vals 'top)
	    (set! protocol 'procedure)))
      (call-default-walker)))
