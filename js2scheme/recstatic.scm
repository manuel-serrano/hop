;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/recstatic.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  6 14:30:50 2018                          */
;*    Last change :  Thu Sep 30 07:25:27 2021 (serrano)                */
;*    Copyright   :  2018-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bind record static methods at top-level and replace static       */
;*    invocations with direct calls or occurrences with references.    */
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
      (refstatic! this args))
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
   
   (define (static-method el)
      (with-access::J2SClassElement el (prop %info)
	 (with-access::J2SMethodPropertyInit prop (val loc)
	    (let* ((d (instantiate::J2SDeclFun
			 (loc loc)
			 (writable #f)
			 (scope 'record)
			 (binder 'let-opt)
			 (id (class-element-id this el))
			 (val val)))
		   (np (duplicate::J2SDataPropertyInit prop
			  (val (J2SRef d)))))
	       (set! %info d)
	       (set! prop np)
	       d))))

   (define (static-data el)
      (with-access::J2SClassElement el (prop %info)
	 (with-access::J2SDataPropertyInit prop (val loc)
	    (let* ((d (instantiate::J2SDeclInit
			 (loc loc)
			 (writable #t)
			 (scope 'record)
			 (binder 'let-opt)
			 (id (class-element-id this el))
			 (val val)))
		   (np (duplicate::J2SDataPropertyInit prop
			  (val (J2SRef d)))))
	       (set! %info d)
	       (set! prop np)
	       d))))
   
   (with-access::J2SRecord this (elements)
      (let ((sds (filter-map (lambda (el)
				(with-access::J2SClassElement el (prop static)
				   (when static
				      (cond
					 ((isa? prop J2SMethodPropertyInit)
					  (static-method el))
					 ((isa? prop J2SDataPropertyInit)
					  (static-data el))))))
		    elements)))
	 (with-access::J2SProgram prgm (decls)
	    (set! decls (append decls sds))
	    this))))
   
;*---------------------------------------------------------------------*/
;*    refstatic! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (refstatic! this::J2SNode args)
   (call-default-walker))
   
;*---------------------------------------------------------------------*/
;*    refstatic! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (refstatic! this::J2SCall args)
   (with-access::J2SCall this (fun args thisarg)
      (when (isa? fun J2SAccess)
	 (let ((el (record-static-method fun)))
	    (when (isa? el J2SClassElement)
	       (with-access::J2SClassElement el (%info)
		  (when (isa? %info J2SDeclFun)
		     (with-access::J2SAccess fun (loc)
			(set! fun (J2SRef %info))))))))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    refstatic! ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (refstatic! this::J2SAccess args)
   (let ((el (record-static-property this)))
      (if (isa? el J2SClassElement)
	 (with-access::J2SClassElement el (%info)
	    (if (isa? %info J2SDeclInit)
		(with-access::J2SAccess this (loc)
		   (J2SRef %info))
		(call-default-walker)))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    record-static-element ...                                        */
;*    -------------------------------------------------------------    */
;*    If this is an access to a static record element, returns the     */
;*    associated class element.                                        */
;*---------------------------------------------------------------------*/
(define (record-static-element this::J2SAccess j2sdecl)
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
					    (isa? %info j2sdecl))
				    el))))))))))))

;*---------------------------------------------------------------------*/
;*    record-static-method ...                                         */
;*---------------------------------------------------------------------*/
(define (record-static-method this::J2SAccess)
   (record-static-element this J2SDeclFun))

;*---------------------------------------------------------------------*/
;*    record-static-property ...                                       */
;*---------------------------------------------------------------------*/
(define (record-static-property this::J2SAccess)
   (record-static-element this J2SDeclInit))
