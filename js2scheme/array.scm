;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/array.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 18:13:46 2016                          */
;*    Last change :  Wed Dec 20 17:31:40 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
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

   (export *j2s-array-cache*)
   
   (export j2s-array-stage))

(define *j2s-array-cache* #t)

;*---------------------------------------------------------------------*/
;*    j2s-array-stage ...                                              */
;*---------------------------------------------------------------------*/
(define j2s-array-stage
   (instantiate::J2SStageProc
      (name "array")
      (comment "Array loop optimization")
      (proc j2s-array!)))

;*---------------------------------------------------------------------*/
;*    j2s-array! ...                                                   */
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
   (with-access::J2SFun this (body params optimize)
      (when optimize
	 (set! body (array! body (append params env))))
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
;*    decl->adecl ...                                                  */
;*---------------------------------------------------------------------*/
(define (decl->adecl::J2SDeclInit decl::J2SDecl)
   (with-access::J2SDecl decl (id loc)
      (J2SLetOptVtype 'vector '(read write)
	 (symbol-append '%A id)
	 (J2SCall (J2SHopRef 'js-array-vec) (J2SRef decl)))))

;*---------------------------------------------------------------------*/
;*    decl->ldecl ...                                                  */
;*---------------------------------------------------------------------*/
(define (decl->ldecl::J2SDeclInit adecl::J2SDecl decl::J2SDecl)
   (with-access::J2SDecl decl (id loc)
      (J2SLetOptVtype 'uint32 '(read write)
	 (symbol-append '%L id)
	 (J2SCall (J2SHopRef 'js-array-ilen) (J2SRef decl)))))

;*---------------------------------------------------------------------*/
;*    mark-decl ...                                                    */
;*---------------------------------------------------------------------*/
(define (mark-decl::J2SDeclInit loc)
   (J2SLetOptVtype 'integer '(read write)
      (gensym '%Marray-mark)
      (J2SCall (J2SHopRef 'js-array-mark))))

;*---------------------------------------------------------------------*/
;*    array! ::J2SFor ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (array! this::J2SFor env::pair-nil)
   (with-access::J2SFor this (init test incr body loc)
      (let ((arrs (delete-duplicates! (array-collect* body env))))
	 (if (null? arrs)
	     (call-default-walker)
	     (let* ((adecls (map decl->adecl arrs))
		    (ldecls (map decl->ldecl adecls arrs))
		    (aenv (map list arrs adecls ldecls))
		    (closed? (array-closed? body aenv))
		    (mdecl (unless closed? (mark-decl loc)))
		    (init (array-ref! init aenv mdecl))
		    (incr (array-ref! incr aenv mdecl))
		    (test (array-ref! test aenv mdecl))
		    (body (array-ref! body aenv mdecl)))
		(if *j2s-array-cache*
		    (let* ((decls (append adecls ldecls))
			   (decls (if closed? decls (cons mdecl decls))))
		       (J2SLetBlock decls
			  (duplicate::J2SFor this
			     (init init)
			     (incr incr)
			     (test test)
			     (body (array! body env)))))
		    (duplicate::J2SFor this
		       (init init)
		       (incr incr)
		       (test test)
		       (body (array! body env)))))))))

;*---------------------------------------------------------------------*/
;*    array! ::J2SWhile ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (array! this::J2SWhile env::pair-nil)
   (with-access::J2SWhile this (test body loc)
      (let ((arrs (delete-duplicates! (array-collect* body env))))
	 (if (null? arrs)
	     (call-default-walker)
	     (let* ((adecls (map decl->adecl arrs))
		    (ldecls (map decl->ldecl adecls arrs))
		    (aenv (map list arrs adecls ldecls))
		    (closed? (array-closed? body aenv))
		    (mdecl (unless closed? (mark-decl loc)))
		    (test (array-ref! test aenv mdecl))
		    (body (array-ref! body aenv mdecl)))
		(if *j2s-array-cache*
		    (let* ((decls (append adecls ldecls))
			   (decls (if closed? decls (cons mdecl decls))))
		       (J2SLetBlock decls
			  (duplicate::J2SWhile this
			     (test test)
			     (body body))))
		    (duplicate::J2SWhile this
		       (test test)
		       (body body))))))))

;*---------------------------------------------------------------------*/
;*    array! ::J2SDo ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (array! this::J2SDo env::pair-nil)
   (with-access::J2SWhile this (test body loc)
      (let ((arrs (delete-duplicates! (array-collect* body env))))
	 (if (null? arrs)
	     (call-default-walker)
	     (let* ((adecls (map decl->adecl arrs))
		    (ldecls (map decl->ldecl adecls arrs))
		    (aenv (map list arrs adecls ldecls))
		    (closed? (array-closed? body aenv))
		    (mdecl (unless closed? (mark-decl loc)))
		    (test (array-ref! test aenv mdecl))
		    (body (array-ref! body aenv mdecl)))
		(if *j2s-array-cache*
		    (let* ((decls (append adecls ldecls))
			   (decls (if closed? decls (cons mdecl decls))))
		       (J2SLetBlock decls
			  (duplicate::J2SDo this
			     (test test)
			     (body body))))
		    (duplicate::J2SDo this
		       (test test)
		       (body body))))))))

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
   '())

;*---------------------------------------------------------------------*/
;*    array-collect* ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (array-collect* this::J2SFun env::pair-nil)
   '())

;*---------------------------------------------------------------------*/
;*    array-collect* ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (array-collect* this::J2SAccess env::pair-nil)
   (with-access::J2SAccess this (obj field)
      (let ((fdaref (array-collect* field env)))
	 (if (not (isa? obj J2SRef))
	     (append (array-collect* obj env) fdaref)
	     (with-access::J2SRef obj (type decl)
		(if (and (eq? type 'array)
			 (memq decl env)
			 (type-number? (j2s-type field)))
		    (cons decl fdaref)
		    fdaref))))))
      
;*---------------------------------------------------------------------*/
;*    array-ref! ::J2SNode ...                                         */
;*    -------------------------------------------------------------    */
;*    Transform all the access and assignment of arrays in env.        */
;*---------------------------------------------------------------------*/
(define-walk-method (array-ref! this::J2SNode env::pair-nil mdecl)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    array-ref! ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (array-ref! this::J2SRef env::pair-nil mdecl)
   (with-access::J2SRef this (decl type loc)
      (cond
	 ((not (eq? type 'array))
	  (call-default-walker))
	 ((assq decl env)
	  =>
	  (lambda (arr)
	     (duplicate::J2SAref this
		(deps env)
		(amark mdecl)
		(array (cadr arr))
		(alen (caddr arr)))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    array-closed? ...                                                */
;*---------------------------------------------------------------------*/
(define (array-closed? body env::pair-nil)
   (bind-exit (return)
      (array-closed body env return)
      #t))

;*---------------------------------------------------------------------*/
;*    array-closed ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (array-closed this::J2SNode env return)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    array-closed ::J2SAccess ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (array-closed this::J2SAccess env return)
   (with-access::J2SAccess this (obj field)
      (if (isa? obj J2SRef)
	  (with-access::J2SRef obj (decl type loc)
	     (cond
		((not (eq? type 'array))
		 (return #f))
		((assq decl env)
		 =>
		 (lambda (arr)
		    (array-closed field env return)))
		(else
		 (return #f))))
	  (return #f))))

;*---------------------------------------------------------------------*/
;*    array-closed ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (array-closed this::J2SCall env return)
   ;; could be improved by a following the function body
   (return #f))
   
