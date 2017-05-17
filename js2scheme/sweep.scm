;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/sweep.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Tue May 16 17:00:30 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Dead code removable stage.                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_sweep

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-sweep-stage))

;*---------------------------------------------------------------------*/
;*    j2s-sweep-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-sweep-stage
   (instantiate::J2SStageProc
      (name "sweep")
      (comment "Remove unreachable definitions")
      (proc (lambda (n args) (j2s-sweep! n args)))
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-sweep! ::J2SProgram ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-sweep! this::J2SProgram args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (let ((deval (make-cell #f)))
	    (for-each (lambda (n) (mark n deval)) nodes)
	    (for-each (lambda (d)
			 (cond
			    ((isa? d J2SDeclSvc)
			     (mark d deval))
			    ((isa? d J2SDeclInit)
			     (with-access::J2SDeclInit d (val %info)
				(mark val deval)
				(unless (dead-expr? val)
				   (set! %info 'sweep))))))
	       decls)
	    (unless (cell-ref deval)
	       (set! direct-eval #f)
	       (let ((rems '()))
		  (set! decls
		     (filter (lambda (d)
				(with-access::J2SDecl d (%info id)
				   (or (eq? %info 'sweep)
				       (begin
					  (set! rems (cons id rems))
					  #f))))
			(map! sweep! decls)))
		  (for-each sweep! nodes)
		  (when (>= (config-get args :verbose 0) 2)
		     (when (pair? rems)
			(fprintf (current-error-port) " (~(, ))" rems))))))))
   this)

;*---------------------------------------------------------------------*/
;*    mark ::J2SNode ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SNode deval)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark ::J2SDecl ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SDecl deval)
   (mark-decl! this deval))

;*---------------------------------------------------------------------*/
;*    mark ::J2SDeclFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SDeclFun deval)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    mark ::J2SDeclSvc ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SDeclSvc deval)
   (mark-decl! this deval))

;*---------------------------------------------------------------------*/
;*    mark ::J2SRef ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SRef deval)
   (with-access::J2SRef this (decl)
      (mark-decl! decl deval))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    mark ::J2SInit ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SInit deval)
   (if (dead-init? this)
       this
       (call-default-walker)))
		  
;*---------------------------------------------------------------------*/
;*    mark ::J2SCall ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SCall deval)
   (call-default-walker)
   (with-access::J2SCall this (fun args)
      (when (isa? fun J2SUnresolvedRef)
	 (with-access::J2SUnresolvedRef fun (id)
	    (when (eq? id 'eval)
	       (cell-set! deval #t)))))
   this)

;*---------------------------------------------------------------------*/
;*    dead-expr? ...                                                   */
;*---------------------------------------------------------------------*/
(define (dead-expr? this::J2SExpr)
   (or (isa? this J2SLiteralCnst)
       (isa? this J2SFun)
       (isa? this J2SRef)))

;*---------------------------------------------------------------------*/
;*    dead-init? ...                                                   */
;*---------------------------------------------------------------------*/
(define (dead-init? this::J2SInit)
   (with-access::J2SInit this (lhs rhs)
      (when (isa? lhs J2SRef)
	 (with-access::J2SRef lhs (decl)
	    (with-access::J2SDecl decl (usecnt)
	       (and (=fx usecnt 1)
		    (dead-expr? rhs)))))))

;*---------------------------------------------------------------------*/
;*    mark-decl! ...                                                   */
;*---------------------------------------------------------------------*/
(define (mark-decl! this::J2SDecl deval)
   (with-access::J2SDecl this (%info)
      (unless (eq? %info 'sweep)
	 (set! %info 'sweep)
	 (when (isa? this J2SDeclInit)
	    (with-access::J2SDeclInit this (val)
	       (mark val deval))))))

;*---------------------------------------------------------------------*/
;*    sweep! ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    sweep! ::J2SLeBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls)
      (set! decls (filter (lambda (d)
			     (with-access::J2SDecl d (%info)
				(eq? %info 'sweep)))
		     decls))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    sweep! ::J2SInit ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SInit)
   (if (dead-init? this)
       (with-access::J2SNode this (loc)
	  (instantiate::J2SUndefined (loc loc)))
       (call-default-walker)))
