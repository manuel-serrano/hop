;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/vector.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 22 09:52:17 2017                          */
;*    Last change :  Wed Nov 22 16:28:45 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Mapping array to Scheme vectors                                  */
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
      (when (and (= (bigloo-debug) 0) (config-get args :vector #f))
	 (j2s-vector-program! this args))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-vector-program! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-vector-program! this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each collect-ranges decls)
      (for-each collect-ranges nodes)
      (for-each (lambda (n) (vector! n '())) decls)
      (for-each (lambda (n) (vector! n '())) nodes)
      (for-each patch-vector decls)
      (for-each patch-vector nodes)
      this))

;*---------------------------------------------------------------------*/
;*    collect-ranges ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-ranges this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-ranges ::J2SDeclFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-ranges this::J2SDeclFun)
   (with-access::J2SDeclFun this (val)
      (collect-ranges val)))

;*---------------------------------------------------------------------*/
;*    collect-ranges ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-ranges this::J2SAccess)
   (with-access::J2SAccess this (obj field)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (with-access::J2SDecl decl (vtype %info)
	       (when (eq? vtype 'array)
		  (unless (and (range? %info) (not (range-intervals %info)))
		     (unless (range? %info) (set! %info (range '())))
		     (with-access::J2SExpr field (range)
			(unless (interval? range)
			   ;; disable optimization for this array
			   (range-intervals-set! %info #f))))))))))
	 
;*---------------------------------------------------------------------*/
;*    collect-ranges ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-ranges this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SAccess)
	 (with-access::J2SAccess lhs (obj field)
	    (when (isa? obj J2SRef)
	       (with-access::J2SRef obj (decl)
		  (with-access::J2SDecl decl (vtype %info)
		     (when (eq? vtype 'array)
			(unless (and (range? %info) (not (range-intervals %info)))
			   (unless (range? %info) (set! %info (range '())))
			   (with-access::J2SExpr field (range)
			      (if (interval? range)
				  (range-intervals-set! %info
				     (cons range (range-intervals %info)))
				  (range-intervals-set! %info #f))))))))))))
	 
;*---------------------------------------------------------------------*/
;*    vector! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SDeclFun env::pair-nil)
   (with-access::J2SDeclFun this (val)
      (set! val (vector! val env))
      this))

;*---------------------------------------------------------------------*/
;*    vector! ::J2SDeclInit ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (vector! this::J2SDeclInit env::pair-nil)
   
   (define (in-range? sz intv)
      (and (>= (interval-min intv) 0) (< (interval-max intv) sz)))
   
   (with-access::J2SDeclInit this (vtype itype id %info val usage hint)
      (when (and (eq? vtype 'array)
		 (only-usage? '(init get set) usage)
		 (range? %info)
		 (pair? (range-intervals %info)))
	 (let ((size (vector-init-size val)))
	    (when (and size
		       (every (lambda (i) (in-range? size i))
			  (range-intervals %info)))
	       (set! vtype 'vector)
	       (set! itype 'vector)
	       (set! hint size)
	       (with-access::J2SExpr val (type)
		  (set! type 'vector))))))
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
	 (when (isa? (car args) J2SNumber)
	    (with-access::J2SNumber (car args) (val)
	       val)))))

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
      (with-access::J2SDecl decl (vtype)
	 (when (eq? vtype 'vector)
	    (set! type 'vector)))))
