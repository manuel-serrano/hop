;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x-new-types/js2scheme/sweep.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Mon Aug 13 10:57:20 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dead code removal                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_sweep

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_use)

   (export j2s-sweep-stage))

;*---------------------------------------------------------------------*/
;*    j2s-sweep-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-sweep-stage
   (instantiate::J2SStageProc
      (name "sweep")
      (comment "Remove unreachable definitions")
      (proc (lambda (n args) (j2s-sweep! n args)))
      (optional 2)))

;*---------------------------------------------------------------------*/
;*    j2s-sweep! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-sweep! this::J2SNode args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (let loop ((stamp (cons 1 2))
		    (removed '()))
	    (let ((deval (make-cell #f)))
	       (mark this deval stamp)
	       (unless (cell-ref deval)
		  (set! direct-eval #f)
		  (let ((rems (make-cell '())))
		     (sweep! this rems stamp)
		     (cond
			((pair? (cell-ref rems))
			 (loop (cons 1 2)
			    (append (cell-ref rems) removed)))
			((>= (config-get args :verbose 0) 3)
			 (fprintf (current-error-port) " (~(, ))"
			    removed)))))))
	 (reinit-use-count! this)))
   this)

;*---------------------------------------------------------------------*/
;*    use-decl! ...                                                    */
;*---------------------------------------------------------------------*/
(define (use-decl! this::J2SDecl stamp)
   (with-access::J2SDecl this (%info)
      (unless (eq? %info stamp)
	 (set! %info stamp)
	 #t)))
   
;*---------------------------------------------------------------------*/
;*    mark-decl! ...                                                   */
;*---------------------------------------------------------------------*/
(define (mark-decl! this::J2SDecl deval::cell stamp)
   (with-access::J2SDecl this (%info)
      (unless (eq? %info stamp)
	 (set! %info stamp)
	 (when (isa? this J2SDeclInit)
	    (with-access::J2SDeclInit this (val)
	       (mark val deval stamp))))))

;*---------------------------------------------------------------------*/
;*    mark ::J2SNode ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SNode deval::cell stamp)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    mark ::J2SProgram ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SProgram deval stamp)
   (with-access::J2SProgram this (nodes decls)
      ;; nodes
      (for-each (lambda (n) (mark n deval stamp)) nodes)
      ;; decls
      (for-each (lambda (n)
		   (when (isa? n J2SDeclInit)
		      (mark n deval stamp)))
	 decls)))
   
;*---------------------------------------------------------------------*/
;*    mark ::J2SDeclInit ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SDeclInit deval stamp)
   (with-access::J2SDeclInit this (val id)
      (mark val deval stamp)))

;*---------------------------------------------------------------------*/
;*    mark ::J2SDeclFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SDeclFun deval stamp)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    mark ::J2SDeclSvc ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SDeclSvc deval stamp)
   (with-access::J2SDeclSvc this (val)
      (use-decl! this stamp)
      (mark val deval stamp)))

;*---------------------------------------------------------------------*/
;*    mark ::J2SRef ...                                                */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SRef deval stamp)
   (with-access::J2SRef this (decl)
      (when (use-decl! decl stamp)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val id)
	       (mark val deval stamp))))))

;*---------------------------------------------------------------------*/
;*    mark ::J2SInit ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SInit deva stampl)
   (if (dead-init? this)
       this
       (call-default-walker)))
		  
;*---------------------------------------------------------------------*/
;*    mark ::J2SCall ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (mark this::J2SCall deval stamp)
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
	    (with-access::J2SDecl decl (usecnt usage)
	       (and (=fx usecnt 0)
		    (not (usage? '(eval) usage))
		    (dead-expr? rhs)))))))

;*---------------------------------------------------------------------*/
;*    sweep! ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SNode rems::cell stamp)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    sweep! ::J2SProgram ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SProgram rems stamp)
   (with-access::J2SProgram this (decls nodes)
      (set! decls (filter (lambda (d)
			     (with-access::J2SDecl d (%info id)
				(if (eq? %info stamp)
				    ;; used, keep it
				    (begin
				       (sweep! d rems stamp)
				       #t)
				    (begin
				       ;; unused, remove it
				       (cell-set! rems
					  (cons id (cell-ref rems)))
				       #f))))
		     decls))
      (for-each (lambda (n) (sweep! n rems stamp)) nodes)
      this))
      
;*---------------------------------------------------------------------*/
;*    sweep! ::J2SDeclInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SDeclInit rems stamp)
   (with-access::J2SDeclInit this (%info loc id val)
      (if (eq? %info stamp)
	  ;; used, keep it
	  (call-default-walker)
	  (begin
	     ;; unused, remove it
	     (cell-set! rems (cons id (cell-ref rems)))
	     (if (isa? this J2SDeclFun)
		 (J2SNop)
		 (J2SStmtExpr val))))))

;*---------------------------------------------------------------------*/
;*    sweep! ::J2SLeBlock ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SLetBlock rems stamp)
   (with-access::J2SLetBlock this (decls)
      (set! decls (filter (lambda (d)
			     (with-access::J2SDecl d (%info id)
				(eq? %info stamp)))
		     decls))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    sweep! ::J2SInit ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (sweep! this::J2SInit rems stamp)
   (if (dead-init? this)
       (with-access::J2SNode this (loc)
	  (instantiate::J2SUndefined (loc loc)))
       (call-default-walker)))
