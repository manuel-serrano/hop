;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/property.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Tue Feb 28 11:49:02 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init the this variable of all function in non-strict mode        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_property
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)
   
   (export j2s-property-stage))

;*---------------------------------------------------------------------*/
;*    j2s-property-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-property-stage
   (instantiate::J2SStageProc
      (name "property")
      (comment "Add a cache to each object property lookup")
      (proc j2s-property)
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-property ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-property this::obj args)

   (define j2s-verbose (config-get args :verbose 0))
   (define j2s-ccall (config-get args :optim-ccall #f))

   (when (>= j2s-verbose 4)
      (fprintf (current-error-port) (format " [ccall=~a]" j2s-ccall)))
   
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls loc pcache-size)
	 (let* ((count (make-counter 0))
		(env (cons '() '()))
		(caches (append
			   (append-map (lambda (s)
					  (property* s count env j2s-ccall #f))
			      headers)
			   (append-map (lambda (s)
					  (property* s count env j2s-ccall #f))
			      decls)
			   (append-map (lambda (s)
					  (property* s count env j2s-ccall #f))
			      nodes))))
	    (set! pcache-size (get count))))
      
      this))

;*---------------------------------------------------------------------*/
;*    make-counter ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-counter val)
   (make-cell val))

;*---------------------------------------------------------------------*/
;*    inc! ...                                                         */
;*---------------------------------------------------------------------*/
(define (inc! count)
   (let ((c (cell-ref count)))
      (cell-set! count (+fx c 1))
      c))

;*---------------------------------------------------------------------*/
;*    get ...                                                          */
;*---------------------------------------------------------------------*/
(define (get count)
   (cell-ref count))

;*---------------------------------------------------------------------*/
;*    decl-cache ...                                                   */
;*    -------------------------------------------------------------    */
;*    The ENV is split in two, read env and writ env, discriminated    */
;*    with the ASSIG parameter.                                        */
;*---------------------------------------------------------------------*/
(define (decl-cache decl::J2SDecl field::bstring count::cell env::pair assig::bool)
   (let ((o (assq decl (if assig (cdr env) (car env)))))
      (cond
	 (o
	  =>
	  (lambda (cd)
	     (let ((cf (assoc field (cdr cd))))
		(if (pair? cf)
		    (cdr cf)
		    (let ((cache (inc! count)))
		       (set-cdr! cd (cons (cons field cache) (cdr cd)))
		       cache)))))
	 (else
	  (let ((cache (inc! count)))
	     (if assig 
		 (set-cdr! env
		    (cons (cons decl (list (cons field cache))) (cdr env)))
		 (set-car! env
		    (cons (cons decl (list (cons field cache))) (car env))))
	     cache)))))
	 
;*---------------------------------------------------------------------*/
;*    property* ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (property* this::J2SNode count env ccall assig)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    property* ::J2SAccess ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (property* this::J2SAccess count env ccall assig)
   (with-access::J2SAccess this (cache obj field)
      (if (and (isa? obj J2SRef) (isa? field J2SString))
	  (with-access::J2SRef obj (decl)
	     (with-access::J2SDecl decl (ronly)
		(if ronly
		    (with-access::J2SString field (val)
		       (set! cache (decl-cache decl val count env assig)))
		    (set! cache (inc! count)))))
	  (set! cache (inc! count)))
      (cons cache (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    property* ::J2SAssig ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (property* this::J2SAssig count env ccall assig)
   (with-access::J2SAssig this (lhs rhs)
      (append (property* lhs count env ccall #t)
	 (property* rhs count env ccall #f))))

;*---------------------------------------------------------------------*/
;*    property* ::J2SUnresolvedRef ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (property* this::J2SUnresolvedRef count env ccall assig)
   (with-access::J2SUnresolvedRef this (cache)
      (set! cache (inc! count))
      (cons cache (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    read-only-function? ...                                          */
;*---------------------------------------------------------------------*/
(define (read-only-function? fun::J2SExpr)
   (when (isa? fun J2SRef)
      (with-access::J2SRef fun (decl usage)
	 (cond
	    ((isa? decl J2SDeclSvc)
	     #f)
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDecl decl (ronly)
		(when ronly decl)))
	    ((isa? decl J2SDeclFunCnst)
	     decl)
	    ((j2s-let-opt? decl)
	     (with-access::J2SDeclInit decl (usage id val)
		(when (isa? val J2SFun)
		   (unless (memq 'assig usage) decl))))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    property* ::J2SCall ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (property* this::J2SCall count env ccall assig)
   (with-access::J2SCall this (cache fun)
      (cond
	 ((not ccall)
	  (call-default-walker))
	 ((isa? fun J2SAccess)
	  (set! cache (inc! count))
	  (cons cache (call-default-walker)))
	 ((read-only-function? fun)
	  (call-default-walker))
	 (else
	  (set! cache (inc! count))
	  (cons cache (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    property* ::J2SNew ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (property* this::J2SNew count env ccall assig)
   
   (define (ctor-function? clazz args)
      (when (read-only-function? clazz)
	 (with-access::J2SRef clazz (decl)
	    (with-access::J2SDeclFun decl (val)
	       (with-access::J2SFun val (params vararg generator)
		  (and (not vararg) (not generator)
		       (=fx (length params) (length args))))))))
   
   (with-access::J2SNew this (clazz args cache)
      (if (ctor-function? clazz args)
	  (begin
	     (set! cache (inc! count))
	     (cons cache (call-default-walker)))
	  (call-default-walker))))
	 
	 
