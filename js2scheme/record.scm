;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/record.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  6 14:30:50 2018                          */
;*    Last change :  Sat Feb 12 16:10:41 2022 (serrano)                */
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

   (define (patch-method-element! el intwinp)
      (with-access::J2SClassElement el (prop %info rtwin clazz)
	 (with-access::J2SMethodPropertyInit prop (val)
	    (with-access::J2SFun val (body)
	       (patch-super-call! body (j2s-class-super-val clazz) intwinp)))))
      
   (define (record-method el)
      (with-access::J2SClassElement el (prop %info rtwin clazz)
	 (patch-method-element! el #f)
	 (let ((nel (record-rtwin-element el)))
	    (patch-method-element! nel #t)
	    (record-property-dispatch! el nel args)
	    nel)))
   
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
				((and (not (j2s-class-property-constructor? prop))
				      (with-access::J2SDataPropertyInit prop (name)
					 (isa? name J2SString)))
				 (set! nels (cons (record-method el) nels)))))
			    ((and static (isa? prop J2SDataPropertyInit))
			     (set! ndecls (cons (static-data el) ndecls))))))
	    elements)
	 (set! elements (append elements nels))
	 (with-access::J2SProgram prgm (decls)
	    (set! decls (append decls ndecls))
	    this))))

;*---------------------------------------------------------------------*/
;*    property-rtwin-name ...                                          */
;*---------------------------------------------------------------------*/
(define (property-rtwin-name::J2SString prop::J2SMethodPropertyInit)
   (with-access::J2SMethodPropertyInit prop (name)
      (with-access::J2SString name (val)
	 (duplicate::J2SString name
	    (val (string-append val "%%R"))))))

;*---------------------------------------------------------------------*/
;*    record-rtwin-element ...                                         */
;*---------------------------------------------------------------------*/
(define (record-rtwin-element el::J2SClassElement)
   (with-access::J2SClassElement el (prop %info rtwin clazz)
      (with-access::J2SMethodPropertyInit prop (val loc name)
	 (let* ((nval (j2s-alpha val '() '()))
		(nname (property-rtwin-name prop))
		(nprop (duplicate::J2SMethodPropertyInit prop
			  (name nname)
			  (val nval)))
		(nel (duplicate::J2SClassElement el
			(prop nprop))))
	    ;; self reference
	    (with-access::J2SClassElement nel (rtwin)
	       (set! rtwin nel))
	    ;; patch the function name and "this" type
	    (with-access::J2SFun nval (thisp name)
	       (with-access::J2SString nname (val)
		  (set! name (string->symbol val)))
	       (with-access::J2SDecl thisp (ctype)
		  (set! ctype clazz)))
	    nel))))

;*---------------------------------------------------------------------*/
;*    record-proprty-dispatch! ...                                     */
;*---------------------------------------------------------------------*/
(define (record-property-dispatch! el::J2SClassElement nel args)
   
   (define (J2SIsaClass decl clazz)
      (with-access::J2SClass clazz ((rec decl))
	 (with-access::J2SDecl decl (loc)
	    (instantiate::J2SCall
	       (loc loc)
	       (type 'bool)
	       (fun (instantiate::J2SHopRef
		       (loc loc)
		       (type 'function)
		       (rtype 'bool)
		       (id (class-predicate-id clazz))))
	       (thisargs '())
	       (args (list (instantiate::J2SRef
			      (loc loc)
			      (decl decl))))))))
   
   (define (J2SIsaProxy decl)
      (with-access::J2SDecl decl (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (type 'bool)
	    (fun (instantiate::J2SHopRef
		    (loc loc)
		    (type 'function)
		    (rtype 'bool)
		    (id 'js-proxy?)))
	    (thisargs '())
	    (args (list (instantiate::J2SRef
			   (loc loc)
			   (decl decl)))))))
   
   (with-access::J2SClassElement el (prop clazz rtwin)
      (with-access::J2SMethodPropertyInit prop (val name)
	 (with-access::J2SFun val (thisp params body loc)
	    (set! rtwin nel)
	    (let ((endloc (node-endloc body))
		  (self (duplicate::J2SDeclInit thisp
			   (key (ast-decl-key))
			   (id '!this)
			   (_scmid '!this)
			   (ctype 'proxy)
			   (val (J2SRef thisp))
			   (binder 'let-opt)
			   (hint '()))))
	       (set! body
		  (J2SBlock
		     (J2SIf (J2SIsaClass thisp clazz)
			(J2SReturn #t
			   (J2SMethodCall*
			      (J2SAccess (J2SSuper/super thisp clazz clazz)
				 (property-rtwin-name prop))
			      (list (J2SRef thisp :type clazz))
			      (map (lambda (p) (J2SRef p)) params)))
			(J2SIf (J2SIsaProxy thisp)
			   (J2SLetRecBlock #f (list self)
			      (if (config-get args :optim-proxy)
				  (patch-this-access!
				     (j2s-alpha body (list thisp) (list self)))
				  (J2SMeta 'inline 0 4
				     (patch-this-access!
					(j2s-alpha body (list thisp) (list self))))))
			   (J2SReturn #t
			      (J2SPragma/bindings 'any
				 '(^this) (list (J2SThis thisp))
				 `(js-raise-type-error %this
				     ,(format "Not ~a instance:~~a"
					 (with-access::J2SRecord clazz (name)
					    name))
				     ^this))))))))))))

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

;*---------------------------------------------------------------------*/
;*    patch-super-call! ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-super-call! this::J2SNode superclazz inrtwinp::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    patch-super-call! ::J2SCall ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-super-call! this::J2SCall superclazz inrtwinp)
   (with-access::J2SCall this (fun)
      (call-default-walker)
      (when (isa? fun J2SAccess)
	 (with-access::J2SAccess fun (obj)
	    (when (isa? obj J2SSuper)
	       (with-access::J2SSuper obj (super rtwinp)
		  (set! rtwinp inrtwinp)
		  (set! super superclazz)))))
      this))

;*---------------------------------------------------------------------*/
;*    patch-this-access! ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-this-access! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    patch-this-access! ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (patch-this-access! this::J2SAccess)
   (with-access::J2SAccess this (obj cspecs)
      (when (isa? obj J2SThis)
	 (with-access::J2SThis obj (decl)
	    (with-access::J2SDecl decl (ctype)
	       (when (eq? ctype 'proxy)
		  (set! cspecs #f))))))
   (call-default-walker))


