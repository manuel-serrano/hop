;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/recstatic.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  6 14:30:50 2018                          */
;*    Last change :  Wed Sep 22 09:15:47 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bind record static methods at top-leval and replace static       */
;*    invocations with direct calls.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_recstatic

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha
	   __js2scheme_classutils)

   (export j2s-recstatic-stage))

;*---------------------------------------------------------------------*/
;*    j2s-recstatic-stage ...                                          */
;*---------------------------------------------------------------------*/
(define j2s-recstatic-stage
   (instantiate::J2SStageProc
      (name "recstatic")
      (comment "Lift record static method definitions")
      (proc j2s-recstatic)
      (optional :optim-recstatic)))

;*---------------------------------------------------------------------*/
;*    j2s-recstatic ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-recstatic this args)
   (when (isa? this J2SProgram)
      (recstatic! this this args)
      (callstatic! this args))
   this)

;*---------------------------------------------------------------------*/
;*    recstatic! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (recstatic! this::J2SNode prgm::J2SProgram args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    recstatic! ::J2SRecord ...                                       */
;*---------------------------------------------------------------------*/
(define-method (recstatic! this::J2SRecord prgm::J2SProgram args)
   (let ((sds (map (lambda (el)
		      (with-access::J2SClassElement el (prop %info)
			 (with-access::J2SMethodPropertyInit prop (val loc)
			    (let* ((d (instantiate::J2SDeclFun
					 (loc loc)
					 (writable #f)
					 (scope 'record)
					 (binder 'let-opt)
					 (usage (usage '(ref)))
					 (id (class-element-id this el))
					 (val val)))
				   (np (duplicate::J2SDataPropertyInit prop
					  (val (J2SRef d)))))
			       (set! %info d)
			       (set! prop np)
			       d))))
		 (j2s-class-static-methods this))))
      (with-access::J2SProgram prgm (decls)
	 (set! decls (append decls sds))
	 this)))
   
;*---------------------------------------------------------------------*/
;*    callstatic! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (callstatic! this::J2SNode args)
   (call-default-walker))
   
;*---------------------------------------------------------------------*/
;*    callstatic! ::J2SCall ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (callstatic! this::J2SCall args)
   (with-access::J2SCall this (fun args thisarg)
      (when (isa? fun J2SAccess)
	 (let ((el (record-static-method fun)))
	    (when (isa? el J2SClassElement)
	       (with-access::J2SClassElement el (%info)
		  (when (isa? %info J2SDeclFun)
		     (with-access::J2SAccess fun (loc)
			(decl-usage-add! %info 'call)
			(set! fun (J2SRef %info))))))))
      (call-default-walker)))
   
;*---------------------------------------------------------------------*/
;*    record-static-method ...                                         */
;*    -------------------------------------------------------------    */
;*    If this is an access to a static record method, returns the      */
;*    associated class element.                                        */
;*---------------------------------------------------------------------*/
(define (record-static-method this::J2SExpr)
   (when (isa? this J2SAccess)
      (with-access::J2SAccess this (obj field)
	 (when (and (isa? field J2SString) (isa? obj J2SRef))
	    (with-access::J2SRef obj (decl)
	       (when (isa? decl J2SDeclClass)
		  (with-access::J2SDeclClass decl ((clazz val) %info)
		     (when (isa? clazz J2SRecord)
			(with-access::J2SString field (val)
			   (let ((el (j2s-class-find-element clazz val :super #f)))
			      (when (isa? el J2SClassElement)
				 (with-access::J2SClassElement el (prop static %info)
				    (when (and static
					       (isa? prop J2SDataPropertyInit)
					       (isa? %info J2SDeclFun))
				       el)))))))))))))
