;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/propcache.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Sat Sep 18 09:01:17 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Add caches to object property lookups                            */
;*    -------------------------------------------------------------    */
;*    Newtarget must have been executed first otherwise constructors   */
;*    using new.target will be incorrectly optimized.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_propcache

   (include "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)
   
   (export j2s-propcache-stage))

;*---------------------------------------------------------------------*/
;*    j2s-propcache-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-propcache-stage
   (instantiate::J2SStageProc
      (name "propcache")
      (comment "Add a cache to each object property lookup")
      (proc j2s-propcache)
      (optional :optim-propcache)))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-propcache this::obj args)

   (define j2s-verbose (config-get args :verbose 0))
   (define j2s-ccall (config-get args :optim-ccall #f))
   (define j2s-shared-pcache (config-get args :shared-pcache #f))

   (when (>= j2s-verbose 4)
      (when j2s-ccall
	 (fprintf (current-error-port) " [optim-ccall]"))
      (when j2s-shared-pcache
	 (fprintf (current-error-port) " [shared-pcache]")))
   
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls loc pcache-size)
	 (let* ((count (make-counter pcache-size))
		(env (cons '() '()))
		(caches (append
			   (append-map (lambda (s)
					  (propcache* s count env j2s-ccall
					     #f #f j2s-shared-pcache args))
			      headers)
			   (append-map (lambda (s)
					  (propcache* s count env j2s-ccall
					     #f #f j2s-shared-pcache args))
			      decls)
			   (append-map (lambda (s)
					  (propcache* s count env j2s-ccall
					     #f #f j2s-shared-pcache args))
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
;*    The ENV is split in two, read env and write env, discriminated   */
;*    with the ASSIG parameter.                                        */
;*---------------------------------------------------------------------*/
(define (decl-cache decl::J2SDecl field::bstring count::cell env::pair assig::bool shared-pcache)
   (let ((o (when shared-pcache
	       (assq decl (if assig (cdr env) (car env))))))
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
	     (when shared-pcache
		(if assig 
		    (set-cdr! env
		       (cons (cons decl (list (cons field cache))) (cdr env)))
		    (set-car! env
		       (cons (cons decl (list (cons field cache))) (car env)))))
	     cache)))))
	 
;*---------------------------------------------------------------------*/
;*    propcache* ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SNode count env ccall assig infunloop shared-pcache conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SMeta ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SMeta count env ccall assig infunloop shared-pcache conf)
   (with-access::J2SMeta this (optim)
      (if (=fx optim 0)
	  '()
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SFun count env ccall assig infunloop shared-pcache conf)
   (with-access::J2SFun this (body)
      (propcache* body count env ccall assig #t shared-pcache conf)))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SAccess count env ccall assig infunloop shared-pcache conf)
   
   (define (canbe-object? obj)
      (and (not (type-number? (j2s-type obj)))
	   (not (eq? (j2s-type obj) 'pair))))
   
   (if infunloop
       (with-access::J2SAccess this (cache obj field loc)
	  (unless (canbe-object? obj)
	     (when (>=fx (config-get conf :verbose 0) 2)
		(fprintf (current-error-port)
		   " ~a@~a" (j2s-type obj) (caddr loc))))
	  (if (and (canbe-object? obj) (not (eq? (j2s-type obj) 'jsvector)))
	      (begin
		 (cond
		    ((and (isa? obj J2SRef) (isa? field J2SString))
		     (with-access::J2SRef obj (decl)
			(if (not (decl-usage-has? decl '(assig)))
			    (with-access::J2SString field (val)
			       (set! cache
				  (decl-cache decl val count env assig shared-pcache)))
			    (set! cache (inc! count)))))
		    ((not (memq (j2s-type field) '(integer number real)))
		     (set! cache (inc! count))))
		 (let ((nexts (call-default-walker)))
		    (if cache
			(cons cache nexts)
			nexts)))
	      (call-default-walker)))
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SAssig count env ccall assig infunloop shared-pcache conf)
   (with-access::J2SAssig this (lhs rhs)
      (append (propcache* lhs count env ccall #t infunloop shared-pcache conf)
	 (propcache* rhs count env ccall #f infunloop shared-pcache conf))))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SAssigOp ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SAssigOp count env ccall assig infunloop shared-pcache conf)
   (if infunloop
       (with-access::J2SAssigOp this (cache lhs)
	  (if (isa? lhs J2SAccess)
	      (begin
		 (set! cache (inc! count))
		 (cons cache (call-next-method)))
	      (call-next-method)))
       (call-next-method)))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SUnresolvedRef ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SGlobalRef count env ccall assig infunloop shared-pcache conf)
   (if infunloop
       (with-access::J2SGlobalRef this (cache loc)
	  (set! cache (inc! count))
	  (cons cache (call-default-walker)))
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SPropertyInit ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SPropertyInit count env ccall assig infunloop shared-pcache conf)
   (with-access::J2SPropertyInit this (cache)
      (if (or (isa? this J2SMethodPropertyInit)
	      (isa? this J2SAccessorPropertyInit))
	  (call-next-method)
	  (begin
	     (set! cache (inc! count))
	     (cons cache (call-next-method))))))
	 
;*---------------------------------------------------------------------*/
;*    read-only-function? ...                                          */
;*---------------------------------------------------------------------*/
(define (read-only-function? fun::J2SExpr)
   (when (isa? fun J2SRef)
      (with-access::J2SRef fun (decl)
	 (cond
	    ((isa? decl J2SDeclSvc)
	     #f)
	    ((isa? decl J2SDeclFun)
	     (not (decl-usage-has? decl '(assig))))
	    ((j2s-let-opt? decl)
	     (with-access::J2SDeclInit decl (val)
		(when (isa? val J2SFun)
		   (unless (decl-usage-has? decl '(assig init)) decl))))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    read-only-loop-ref? ...                                          */
;*---------------------------------------------------------------------*/
(define (read-only-loop-ref? fun::J2SExpr)
   (when (isa? fun J2SRef)
      (with-access::J2SRef fun (decl)
	 (unless (decl-usage-has? decl '(assig))
	    (with-access::J2SDecl decl (useinloop)
	       useinloop)))))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SCall count env ccall assig infunloop shared-pcache conf)
   (with-access::J2SCall this (cache fun loc)
      (cond
	 (cache
	  (call-default-walker))
	 ((not infunloop)
	  (call-default-walker))
	 ((not ccall)
	  (call-default-walker))
	 ((isa? fun J2SAccess)
	  (with-access::J2SAccess fun (obj)
	     (if (eq? (j2s-type obj) 'pair)
		 (call-default-walker)
		 (begin
		    (unless cache (set! cache (inc! count)))
		    (cons cache (call-default-walker))))))
	 ((read-only-function? fun)
	  (call-default-walker))
	 ((not (read-only-loop-ref? fun))
	  (call-default-walker))
	 (else
	  (set! cache (inc! count))
	  (cons cache (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SNew ...                                          */
;*    -------------------------------------------------------------    */
;*    !!! Warning: special treatment for new Buffer( ... ).            */
;*    -------------------------------------------------------------    */
;*    Buffer are special objects partially allocated in JavaScript     */
;*    and partially allocated in Scheme. Because of that specificity   */
;*    the buffer allocator _cannot_ be optimized.                      */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SNew count env ccall assig infunloop shared-pcache conf)
   
   (define (ctor-function? clazz args)
      (when (read-only-function? clazz)
	 (with-access::J2SRef clazz (decl)
	    (let ((val (j2sdeclinit-val-fun decl)))
	       (with-access::J2SFun val (params vararg generator new-target)
		  (with-access::J2SDecl decl (id)
		     (and (not (eq? id 'Buffer))
			  (not vararg) (not generator) (eq? new-target 'no)
			  (=fx (length params) (length args)))))))))
   
   (with-access::J2SNew this (clazz args caches loc)
      (cond
	 ((is-builtin-ref? clazz 'Proxy)
	  (let* ((gcache (inc! count))
		 (scache (inc! count))
		 (acache (inc! count)))
	     (set! caches (list gcache scache acache))
	     (append caches (call-default-walker))))
	 ((and infunloop (ctor-function? clazz args))
	  (let ((cache (inc! count)))
	     (set! caches (list cache))
	     (cons cache (call-default-walker))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SFor ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SFor count env ccall assig inloopfunp shared-pcache conf)
   (with-access::J2SFor this (init test incr body)
      (append
	 (propcache* init count env ccall assig inloopfunp shared-pcache conf)
	 (propcache* test count env ccall assig inloopfunp shared-pcache conf)
	 (propcache* incr count env ccall assig inloopfunp shared-pcache conf)
	 (propcache* body count env ccall assig #t shared-pcache conf))))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SWhile ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SWhile count env ccall assig inloopfunp shared-pcache conf)
   (with-access::J2SWhile this (test body)
      (append
	 (propcache* test count env ccall assig inloopfunp shared-pcache conf)
	 (propcache* body count env ccall assig #t shared-pcache conf))))

;*---------------------------------------------------------------------*/
;*    propcache* ::J2SDo ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (propcache* this::J2SDo count env ccall assig inloopfunp shared-pcache conf)
   (with-access::J2SDo this (test body)
      (append
	 (propcache* test count env ccall assig inloopfunp shared-pcache conf)
	 (propcache* body count env ccall assig #t shared-pcache conf))))
