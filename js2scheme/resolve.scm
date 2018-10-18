;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/js2scheme/resolve.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  8 09:03:28 2013                          */
;*    Last change :  Sat Dec 26 19:36:43 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bind all the unresolved variables.                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_resolve

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils)

   (export j2s-resolve-stage
	   (generic j2s-resolve ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-resolve-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-resolve-stage
   (instantiate::J2SStageProc
      (name "resolve")
      (comment "Pre-bind in the global object unresolved global variables")
      (proc j2s-resolve)
      (optional #f)))

;*---------------------------------------------------------------------*/
;*    j2s-resolve ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (j2s-resolve this)
   this)

;*---------------------------------------------------------------------*/
;*    j2s-resolve ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define-method (j2s-resolve this::J2SProgram)
   (let ((env (make-hashtable)))
      (with-access::J2SProgram this (nodes headers decls)
	 (for-each (lambda (o) (resolve! o env)) headers)
	 (for-each (lambda (o) (resolve! o env)) decls)
	 (for-each (lambda (o) (resolve! o env)) nodes)
	 ;; add the the newly declared global variables
	 (set! decls (append (hashtable->list env) decls)))
      this))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    resolve! ::J2SUnresolvedRef ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (resolve! this::J2SUnresolvedRef env)
   (with-access::J2SUnresolvedRef this (id loc)
      (let ((decl (hashtable-get env id)))
	 (if (or (isa? decl J2SDeclSvc)
		 (isa? decl J2SDeclFun)
		 (isa? decl J2SDeclExtern))
	     (instantiate::J2SRef
		(loc loc)
		(decl decl))
	     (let ((decl (instantiate::J2SDecl
			    (scope '%scope)
			    (id id)
			    (loc loc))))
		(hashtable-put! env id decl)
		(instantiate::J2SRef
		   (loc loc)
		   (decl decl)))))))



   
