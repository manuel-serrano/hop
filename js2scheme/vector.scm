;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/vector.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 22 09:52:17 2017                          */
;*    Last change :  Wed Nov 22 10:59:23 2017 (serrano)                */
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
      (when (>=fx (config-get args :optim 0) 6)
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
			(if (interval? range)
			    (range-intervals-set! %info
			       (cons range (range-intervals %info)))
			    (range-intervals-set! %info #f))))))))))
	 
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
   (with-access::J2SDeclInit this (vtype usage id %info val)
      (when (and (eq? vtype 'array) (only-usage? '(init get set) usage))
	 (let ((size (vector-init-size val)))
	    (when size
	       (tprint "GOT ONE " id " " %info " size=" size)))))
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

   
