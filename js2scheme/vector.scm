;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/vector.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 22 09:52:17 2017                          */
;*    Last change :  Fri Aug 10 09:53:22 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Mapping JS Arrays to Scheme vectors                              */
;*    -------------------------------------------------------------    */
;*    The transformation applies to arrays that satisfy:               */
;*      1- they are only used in array indexed accesses                */
;*      2- the accesses are correctly bound                            */
;*      3- the allocation size is statically known                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_vector

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils)

   (export j2s-vector-stage))

;*---------------------------------------------------------------------*/
;*    j2s-vector-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-vector-stage
   (instantiate::J2SStageProc
      (name "vector")
      (comment "Array-to-Vector optimiziation")
      (optional :optim-vector)
      (proc j2s-vector!)))

;*---------------------------------------------------------------------*/
;*    range ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct range intervals)

;*---------------------------------------------------------------------*/
;*    j2s-vector! ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-vector! this args)
   (when (isa? this J2SProgram)
      (j2s-vector-program! this args)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-vector-program! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-vector-program! this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each collect-ranges decls)
      (for-each collect-ranges nodes)
      (let ((verb (make-cell '())))
	 (for-each (lambda (n) (vector! n #f #f #f verb)) decls)
	 (for-each (lambda (n) (vector! n #f #f #f verb)) nodes)
	 (when (and (>= (config-get args :verbose 0) 2) (pair? (cell-ref verb)))
	    (fprintf (current-error-port)
	       (format " [~a: ~(,)]"
		  (let ((fst (car (cell-ref verb))))
		     (cadr (if (pair? fst) fst (cell-ref fst))))
		  (map (lambda (c)
			  (if (cell? c)
			      (format "~a*" (caddr (cell-ref c)))
			      (caddr c)))
		     (cell-ref verb))))))
      (for-each patch-vector decls)
      (for-each patch-vector nodes)
      this))

;*---------------------------------------------------------------------*/
;*    collect-ranges ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-ranges this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-ranges ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-ranges this::J2SAccess)
   (with-access::J2SAccess this (obj field)
      (if (isa? obj J2SRef)
	  (with-access::J2SRef obj (decl)
	     (with-access::J2SDecl decl (vartype %info)
		(when (eq? vartype 'array)
		   (unless (and (range? %info) (not (range-intervals %info)))
		      (unless (range? %info) (set! %info (range '())))
		      (with-access::J2SExpr field (range)
			 (unless (interval? range)
			    ;; disable optimization for this array
			    (range-intervals-set! %info #f)))))))
	  (collect-ranges obj))
      (collect-ranges field)))
	 
;*---------------------------------------------------------------------*/
;*    collect-ranges ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-ranges this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs)
      (collect-ranges rhs)
      (collect-ranges lhs)
      (when (isa? lhs J2SAccess)
	 (with-access::J2SAccess lhs (obj field)
	    (when (isa? obj J2SRef)
	       (with-access::J2SRef obj (decl)
		  (with-access::J2SDecl decl (vartype %info)
		     (when (eq? vartype 'array)
			(unless (and (range? %info) (not (range-intervals %info)))
			   (unless (range? %info) (set! %info (range '())))
			   (with-access::J2SExpr field (range)
			      (if (interval? range)
				  (range-intervals-set! %info
				     (cons range (range-intervals %info)))
				  (range-intervals-set! %info #f))))))))))))
	 
;*---------------------------------------------------------------------*/
;*    vector! ::J2SNode ...                                            */
;*    -------------------------------------------------------------    */
;*    The VECTOR! function performs the array-to-vector                */
;*    transformation. For that, it walks the AST searching for         */
;*    array creations. When one array is found and when it             */
;*    satisfies the optimization criteria, it's type is changed for    */
;*    vector. Additionaly, VECTOR! also tries to lift up array         */
;*    declarations. This is the purpose of the  arguments, SCOPE,      */
;*    HOOK, INLOOP and this is the reason why there is an VECTOR!      */
;*    implementation for statement nodes such as J2SIf and J2SLoop.    */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SNode scope hook inloop verb)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SDeclFun scope hook inloop verb)
   (with-access::J2SDeclFun this (val)
      (set! val (vector! val scope hook inloop verb))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SFun scope hook inloop verb)
   (with-access::J2SFun this (body)
      (set! body (vector! body #f body #f verb))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SIf ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SIf scope hook inloop verb)
   (with-access::J2SIf this (test then else)
      (set! test (vector! test scope hook inloop verb))
      (set! then (vector! then scope then #f verb))
      (set! else (vector! else scope else #f verb))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SLetBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SLetBlock scope hook inloop verb)
   (with-access::J2SLetBlock this (decls nodes)
      (set! decls (map! (lambda (d) (vector! d scope hook inloop verb)) decls))
      (set! nodes (map! (lambda (n) (vector! n this hook inloop verb)) nodes))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SFor ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SFor scope hook inloop verb)
   (with-access::J2SFor this (init test incr body)
      (set! init (vector! init scope hook inloop verb))
      (set! test (vector! test scope hook #t verb))
      (set! incr (vector! incr scope hook #t verb))
      (set! body (vector! body scope hook #t verb))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SForIn ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SForIn scope hook inloop verb)
   (with-access::J2SForIn this (lhs obj body)
      (set! lhs (vector! lhs scope hook inloop verb))
      (set! obj (vector! obj scope hook inloop verb))
      (set! body (vector! body scope hook #t verb))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SWhile ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SWhile scope hook inloop verb)
   (with-access::J2SWhile this (test body)
      (set! test (vector! test scope hook #t verb))
      (set! body (vector! body scope hook #t verb))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SDeclInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SDeclInit scope hook inloop verb)
   
   (define (in-range? sz intv)
      (and (>= (interval-min intv) 0) (< (interval-max intv) sz)))

   (with-access::J2SDeclInit this (vartype id %info val usage hint loc)
      (when (and (eq? vartype 'array)
		 (only-usage? '(init get set) usage)
		 (range? %info)
		 (or (pair? (range-intervals %info))
		     (null? (range-intervals %info))))
	 (let ((size (vector-init-size val)))
	    (when (and size
		       (every (lambda (i) (in-range? size i))
			  (range-intervals %info)))
	       ;; optimize this array by turning it into a vector
	       (set! vartype 'vector)
	       (set! hint (list size))
	       (with-access::J2SExpr val (type)
		  (set! type 'vector))
	       (cell-set! verb (cons loc (cell-ref verb)))
	       (when (and inloop hook scope (not (capture? scope this)))
		  (set-car! (cell-ref verb) (make-cell (car (cell-ref verb))))
		  ;; extra optimization: lift the allocation out of the loop
		  (let ((decl (hook-alloc! hook size loc)))
		     (set! val (init-array! val decl size))))))))
   this)

;*---------------------------------------------------------------------*/
;*    vector-init-sisze ::J2SNew ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (vector-init-size this::J2SExpr)
   #f)

;*---------------------------------------------------------------------*/
;*    vector-init-size ::J2SNew ...                                    */
;*---------------------------------------------------------------------*/
(define-method (vector-init-size this::J2SNew)
   
   (define (is-array? clazz)
      (when (isa? clazz J2SGlobalRef)
	 (with-access::J2SGlobalRef clazz (decl)
	    (with-access::J2SDecl decl (id)
	       (eq? id 'Array)))))
      
   (with-access::J2SNew this (clazz args)
      (when (and (is-array? clazz) (pair? args) (null? (cdr args)))
	 (let loop ((expr (car args)))
	    (cond
	       ((isa? expr J2SNumber)
		(with-access::J2SNumber expr (val)
		   val))
	       ((isa? expr J2SCast)
		(with-access::J2SCast expr (expr)
		   (loop expr)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    vector-init-size ::J2SArray ...                                  */
;*---------------------------------------------------------------------*/
(define-method (vector-init-size this::J2SArray)
   (with-access::J2SArray this (exprs)
      (length exprs)))

;*---------------------------------------------------------------------*/
;*    init-array! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (init-array! this::J2SExpr decl size))

;*---------------------------------------------------------------------*/
;*    init-array! ::J2SNew ...                                         */
;*---------------------------------------------------------------------*/
(define-method (init-array! this::J2SNew decl size)
   (with-access::J2SNew this (loc)
      (J2SSequence
	 (J2SCall (J2SHopRef 'vector-fill!)
	    (J2SRef decl)
	    (J2SUndefined)
	    (J2SNumber 0) (J2SNumber size))
	 (J2SRef decl))))

;*---------------------------------------------------------------------*/
;*    init-array! ::J2SArray ...                                       */
;*---------------------------------------------------------------------*/
(define-method (init-array! this::J2SArray decl size)
   (with-access::J2SArray this (exprs loc)
      (J2SSequence*
	 (append 
	    (map (lambda (e i)
		    (let ((ref (J2SRef decl :type 'vector)))
		       (J2SAssig
			  (J2SAccess (J2SRef decl) (J2SNumber i))
			  e)))
	       exprs (iota size))
	    (list (J2SRef decl))))))
   
;*---------------------------------------------------------------------*/
;*    patch-vector ::J2SNode ...                                       */
;*    -------------------------------------------------------------    */
;*    Replace ARRAY type vith VECTOR type.                             */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-vector this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    patch-vector ::J2SRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-vector this::J2SRef)
   (with-access::J2SRef this (decl type)
      (with-access::J2SDecl decl (vartype)
	 (when (eq? vartype 'vector)
	    (set! type 'vector)))))

;*---------------------------------------------------------------------*/
;*    capture? ...                                                     */
;*    -------------------------------------------------------------    */
;*    Is DECL used free in an inner function?                          */
;*---------------------------------------------------------------------*/
(define (capture? node::J2SNode decl::J2SDecl)
   (let ((res (make-cell #f)))
      (node-capture node decl #f res)
      (cell-ref res)))

;*---------------------------------------------------------------------*/
;*    node-capture ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-capture this::J2SNode decl inlambda res)
   (or (cell-ref res) (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    node-capture ::J2SRef ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-capture this::J2SRef decl inlambda res)
   (or (cell-ref res)
       (not inlambda)
       (with-access::J2SRef this ((d decl))
	  (when (eq? d decl)
	     (cell-set! res #t)))))

;*---------------------------------------------------------------------*/
;*    node-capture ::J2SFun ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (node-capture this::J2SFun decl inlambda res)
   (or (cell-ref res)
       (with-access::J2SFun this (body)
	  (node-capture body decl #t res))))

;*---------------------------------------------------------------------*/
;*    make-decl-vector ...                                             */
;*---------------------------------------------------------------------*/
(define (make-decl-vector size loc)
   (let* ((id (gensym '%vector))
	  (val (J2SNew
		  (J2SGlobalRef 'Array)
		  (J2SNumber size)))
	  (decl (J2SLetOptVtype 'vector '(init ref) id val)))
      (with-access::J2SExpr val (type)
	 (set! type 'vector))
      decl))

;*---------------------------------------------------------------------*/
;*    hook-alloc! ...                                                  */
;*    -------------------------------------------------------------    */
;*    Create a new local variable for the lifted allocation. Hooked    */
;*    it in a let block, freshly created if needed.                    */
;*---------------------------------------------------------------------*/
(define-generic (hook-alloc! this::J2SBlock size loc)
   (let ((decl (make-decl-vector size loc)))
      (with-access::J2SBlock this (nodes)
	 (set! nodes (list (J2SLetBlock* (list decl) nodes))))
      decl))

;*---------------------------------------------------------------------*/
;*    hook-alloc! ::J2SLetBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hook-alloc! this::J2SLetBlock size loc)
   (let ((decl (make-decl-vector size loc)))
      (with-access::J2SLetBlock this (decls)
	 (set! decls (cons decl decls)))
      decl))
	       
