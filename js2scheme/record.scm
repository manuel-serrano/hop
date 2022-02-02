;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/record.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  6 14:30:50 2018                          */
;*    Last change :  Wed Feb  2 11:54:43 2022 (serrano)                */
;*    Copyright   :  2018-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Record (aka, sealed class) specific optimizations:               */
;*      - Create type specialized method versions                      */
;*      - Bind record static methods at top-level                      */
;*      - replace static invocations with direct calls                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_record

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha
	   __js2scheme_classutils)

   (export j2s-record-stage))

;*---------------------------------------------------------------------*/
;*    j2s-record-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-record-stage
   (instantiate::J2SStageProc
      (name "record")
      (comment "Records (aka sealed classes) optimizations")
      (proc j2s-record)
      (optional :optim-record)))

;*---------------------------------------------------------------------*/
;*    j2s-record ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-record this args)
   (when (isa? this J2SProgram)
      (record! this this args)
      (refstatic! this args))
   this)

;*---------------------------------------------------------------------*/
;*    record! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (record! this::J2SNode prgm::J2SProgram args)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    record! ::J2SRecord ...                                          */
;*---------------------------------------------------------------------*/
(define-method (record! this::J2SRecord prgm::J2SProgram args)

   (define (record-method el)
      (with-access::J2SClassElement el (prop %info)
	 (with-access::J2SMethodPropertyInit prop (val loc name)
	    (let* ((nval (j2s-alpha val '() '()))
		   (nprop (duplicate::J2SMethodPropertyInit prop
			     (name (duplicate::J2SString name
				      (val (with-access::J2SString name (val)
					      (string-append val "%%R")))))
			     (val nval)))
		   (nel (duplicate::J2SClassElement el
			   (prop nprop))))
	       ;; patch the function name and "this" type
	       (with-access::J2SFun nval (thisp name)
		  (set! name (symbol-append name '%%R))
		  (with-access::J2SDecl thisp (ctype)
		     (set! ctype this)))
	       nel))))
   
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
      (let ((ndecls '())
	    (nels '()))
	 (for-each (lambda (el)
		      (with-access::J2SClassElement el (prop static)
			 (cond
			    ((isa? prop J2SMethodPropertyInit)
			     (cond
				(static
				 (set! ndecls (cons (static-method el) ndecls)))
				((not (j2s-class-property-constructor? prop))
				 (set! nels (cons (record-method el) nels)))))
			    ((and static (isa? prop J2SDataPropertyInit))
			     (set! ndecls (cons (static-data el) ndecls))))))
	    elements)
	 (set! elements (append elements nels))
	 (with-access::J2SProgram prgm (decls)
	    (set! decls (append decls ndecls))
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
   (with-access::J2SCall this (fun args thisargs)
      (when (isa? fun J2SAccess)
	 (let ((el (record-static-method fun)))
	    (when (isa? el J2SClassElement)
	       (with-access::J2SClassElement el (%info)
		  (when (isa? %info J2SDeclFun)
		     (with-access::J2SAccess fun (loc obj)
			(set! fun (J2SRef %info))
			(set! thisargs (list obj))))))))
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
			(let ((el (j2s-class-find-super-element clazz val)))
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
