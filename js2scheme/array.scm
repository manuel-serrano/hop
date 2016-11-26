;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/array.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Thu Nov 24 10:29:56 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Array loop optimization                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_array

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint)

   (export j2s-array-stage))

;*---------------------------------------------------------------------*/
;*    j2s-array-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-array-stage
   (instantiate::J2SStageProc
      (name "array")
      (comment "Array loop optimization")
      (proc j2s-array!)))

;*---------------------------------------------------------------------*/
;*    j2s-array! ::obj ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-array! this args)
   (when (isa? this J2SProgram)
      (when (>=fx (config-get args :optim 0) 6)
	 (j2s-array-program! this args))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-array-program! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-array-program! this::J2SProgram args)
   (with-access::J2SProgram this (headers decls nodes)
      (for-each (lambda (n) (array! n '())) decls)
      (for-each (lambda (n) (array! n '())) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    array! ::J2SNode ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (array! this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    array! ::J2SFun ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (array! this::J2SFun env::pair-nil)
   (with-access::J2SFun this (body params)
      (set! body (array! body (append params env)))
      this))

;*---------------------------------------------------------------------*/
;*    array! ::J2SLetBlock ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (array! this::J2SLetBlock env::pair-nil)
   (with-access::J2SLetBlock this (nodes decls)
      (let ((nenv (append decls env)))
	 (set! nodes (map! (lambda (n) (array! n nenv)) nodes))
	 this)))
	 
;*---------------------------------------------------------------------*/
;*    array! ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (array! this::J2SFor env::pair-nil)

   (define (decl->adecl::J2SDeclInit decl::J2SDecl)
      (with-access::J2SDecl decl (id loc)
	 (J2SLetOpt '(read write)
	    (symbol-append '|%a:| id)
	    (J2SCall (J2SHopRef 'js-array-vec) (J2SRef decl)))))

   (define (decl->ldecl::J2SDeclInit adecl::J2SDecl decl::J2SDecl)
      (with-access::J2SDecl decl (id loc)
	 (J2SLetOpt '(read write)
	    (symbol-append '|%l:| id)
	    (J2SCall (J2SHopRef 'js-array-vlen) (J2SRef decl)))))

   (with-access::J2SFor this (init test incr body loc)
      (let ((arrs (delete-duplicates! (array-collect* body env))))
	 (if (null? arrs)
	     (call-default-walker)
	     (let* ((adecls (map decl->adecl arrs))
		    (ldecls (map decl->ldecl adecls arrs))
		    (nenv (append (map list arrs adecls ldecls) env))
		    (init (array-ref! init nenv))
		    (incr (array-ref! incr nenv))
		    (test (array-ref! test nenv))
		    (body (array-ref! body nenv)))
;* 		(J2SLetBlock (append adecls ldecls)                    */
;* 		   (duplicate::J2SFor this                             */
;* 		      (init init)                                      */
;* 		      (incr incr)                                      */
;* 		      (test test)                                      */
;* 		      (body body)))                                    */
		(duplicate::J2SFor this
		      (init init)
		      (incr incr)
		      (test test)
		      (body body)))))))

;*---------------------------------------------------------------------*/
;*    array-collect* ::J2SNode ...                                     */
;*    -------------------------------------------------------------    */
;*    array-collect all the array variables that are used in a node.   */
;*---------------------------------------------------------------------*/
(define-walk-method (array-collect* this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    array-collect* ::J2SRef ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (array-collect* this::J2SRef env::pair-nil)
   (with-access::J2SRef this (type decl)
      (if (and (eq? type 'array)
	       (with-access::J2SDecl decl (ronly) #t)
	       (memq decl env))
	  (list decl)
	  '())))

;*---------------------------------------------------------------------*/
;*    array-collect* ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (array-collect* this::J2SFun env::pair-nil)
   '())
   
;*---------------------------------------------------------------------*/
;*    array-ref! ::J2SNode ...                                         */
;*    -------------------------------------------------------------    */
;*    Transform all the access and assignment of arrays in env.        */
;*---------------------------------------------------------------------*/
(define-walk-method (array-ref! this::J2SNode env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    array-ref! ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (array-ref! this::J2SRef env::pair-nil)
   (with-access::J2SRef this (decl type loc)
      (cond
	 ((not (eq? type 'array))
	  (call-default-walker))
	 ((assq decl env)
	  =>
	  (lambda (arr)
	     (duplicate::J2SAref this
		(array (cadr arr))
		(alen (caddr arr)))))
	 (else
	  (call-default-walker)))))

;* {*---------------------------------------------------------------------*} */
;* {*    array-ref! ::J2SAccess ...                                       *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (array-ref! this::J2SAccess env::pair-nil)      */
;*                                                                     */
;*    (define (access->aref this::J2SAccess arr::J2SDecl len::J2SDecl) */
;*       (with-access::J2SAccess this (loc type hint cache obj field)  */
;* 	 (instantiate::J2SAref                                         */
;* 	    (loc loc)                                                  */
;* 	    (type type)                                                */
;* 	    (hint hint)                                                */
;* 	    (cache cache)                                              */
;* 	    (obj obj)                                                  */
;* 	    (field field)                                              */
;* 	    (array arr)                                                */
;* 	    (alen len))))                                              */
;*                                                                     */
;*    (with-access::J2SAccess this (obj field)                         */
;*       (if (isa? obj J2SRef)                                         */
;* 	  (with-access::J2SRef obj (decl)                              */
;* 	     (with-access::J2SExpr field (type)                        */
;* 		(if (or (memq  type '(index integer)) (j2s-field-length? field)) */
;* 		    (let ((arr (assq decl env)))                       */
;* 		       (if arr                                         */
;* 			   (apply access->aref this (cdr arr))         */
;* 			   (call-default-walker)))                     */
;* 		    (call-default-walker))))                           */
;* 	  (call-default-walker))))                                     */
		
	     
				      
