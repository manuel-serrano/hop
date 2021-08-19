;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-record.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 15 07:09:51 2021                          */
;*    Last change :  Thu Aug 19 18:09:44 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Record generation                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-record

   (include "ast.sch"
	    "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_classutils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_checksum
	   __js2scheme_scheme
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant
	   __js2scheme_scheme-class)

   (export (j2s-collect-records*::pair-nil ::J2SProgram)
	   (record-index ::J2SNode ::J2SRecord ::bstring)
	   (record-scmid::symbol ::J2SRecord)
	   (record-prototype-scmid::symbol ::J2SRecord)
	   (record-constructor-scmid::symbol ::J2SRecord)
	   (j2s-record-declaration ::J2SRecord)
	   (j2s-record-predicate ::J2SRecord)
	   (j2s-record-new ::J2SRecord ::pair-nil mode return ctx)
	   (j2s-record-prototype-constructor::pair this::J2SRecord)
	   (j2s-scheme-record-super ::J2SCall mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SRecord ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SRecord mode return ctx)
   
   (define (bind-class-property clazz obj prop)
      (cond
	 ((isa? prop J2SMethodPropertyInit)
	  #f)
	 ((isa? prop J2SDataPropertyInit)
	  (with-access::J2SDataPropertyInit prop (name val)
	     (unless #f ;;(j2s-class-property-constructor? prop)
		(with-access::J2SString name ((id val))
		   `(js-object-inline-set! ,obj
		       ,(record-index clazz clazz id)
		       ,(j2s-scheme val mode return ctx))))))
	 (else
	  #f)))
   
   (define (bind-static clazz obj m)
      (with-access::J2SClassElement m (prop static)
	 (when static
	    (j2s-scheme-bind-class-method prop obj mode return ctx))))
   
   (define (bind-prototype clazz obj m)
      (with-access::J2SClassElement m (prop static)
	 (when (and (not static)
		    (or (isa? prop J2SMethodPropertyInit)
			(isa? prop J2SAccessorPropertyInit)))
	    (j2s-scheme-bind-class-method prop obj mode return ctx))))
   
   (define (bind-property clazz obj m)
      (with-access::J2SClassElement m (prop static)
	 (when (not static)
	    (bind-class-property clazz obj prop))))
   
   (define (class-prototype this::J2SClass super)
      (cond
	 ((eq? super #f)
	  `(with-access::JsGlobalObject %this (js-object)
	      (js-new0 %this js-object)))
	 ((null? super)
	  `(with-access::JsGlobalObject %this (js-object)
	      (let ((o (js-new0 %this js-object)))
		 (js-object-proto-set! o (js-null))
		 o)))
	 (else
	  `(js-new-sans-construct %this ,super))))

   (define (make-class this name super els constructor arity length ctorsz src loc)
      (let* ((cname (or name (gensym 'class)))
	     (clazz (symbol-append cname '%CLASS))
	     (ctor (symbol-append cname '%CTOR))
	     (proto (symbol-append cname '%PROTOTYPE)))
	 `(letrec* ((,ctor ,constructor)
		    (,proto ,(class-prototype this super))
		    (,clazz (js-make-function %this
			       (lambda (this . args)
				  (with-access::JsGlobalObject %this (js-new-target)
				     (if (eq? js-new-target (js-undefined))
					 (js-raise-type-error/loc %this ',loc
					    ,(format "Record constructor '~a' cannot be invoked without 'new'"
						name)
					    (js-undefined))
					 (set! js-new-target (js-undefined)))
				     (apply ,ctor this args)))
			       ,arity
			       (js-function-info :name ,(symbol->string cname)
				  :len ,length)
			       :strict ',mode
			       :alloc (lambda (%this ctor)
					 ,(let ((rec (gensym 'this)))
					     `(let ((,rec ,(j2s-alloc-record this mode return ctx)))
						 (with-access::JsGlobalObject %this (js-new-target)
						    (set! js-new-target ,rec))
						 ,rec)))
			       :prototype  ,proto
			       :__proto__ ,(if (null? super)
					       '(with-access::JsGlobalObject %this (js-function-prototype)
						 js-function-prototype)
					       super)
			       :constrsize ,ctorsz))
		    ,@(if name `((,(j2s-class-id this ctx) (js-make-let))) '()))
	     (set! ,(record-prototype-scmid this) ,proto)
	     (set! ,(record-constructor-scmid this) ,ctor)
	     ,@(filter-map (lambda (m) (bind-static this clazz m)) els)
	     ,@(filter-map (lambda (m) (bind-prototype this proto m)) els)
	     ,@(if name `((set! ,(j2s-class-id this ctx) ,clazz)) '())
	     ,clazz)))

   (define (let-super super proc)
      (cond
	 ((isa? super J2SUndefined)
	  (proc #f))
	 ((isa? super J2SNull)
	  (proc '()))
	 (else
	  (let ((superid (gensym 'super)))
	     `(let* ((,superid ,(j2s-scheme super mode return ctx))
		     (%super (js-get ,superid
				,(& "prototype" (context-program ctx))
				%this))
		     (%superctor ,superid))
		 ,(proc superid))))))

   (define (ctor->lambda val::J2SFun name mode return ctx proto ctor-only super)
      
      (define (unthis this loc)
	 (instantiate::J2SStmtExpr
	    (loc loc)
	    (expr (instantiate::J2SPragma
		     (loc loc)
		     (expr `(set! ,this (js-make-let)))))))
      
      (define (returnthis this loc)
	 (J2SStmtExpr (J2SRef this)))

      (jsfun->lambda val mode return ctx proto ctor-only))
   
   (with-access::J2SClass this (super elements name src loc decl)
      (let ((ctor (j2s-class-get-constructor this)))
	 (let-super super
	    (lambda (super)
	       (cond
		  (ctor
		   (with-access::J2SClassElement ctor (prop)
		      (with-access::J2SDataPropertyInit prop (val)
			 (with-access::J2SFun val (constrsize params thisp)
			    (make-class this name super elements
			       (ctor->lambda val name mode return ctx #f #t super)
			       (j2s-function-arity val ctx)
			       (length params) constrsize
			       src loc)))))
		  (super
		   (make-class this name super elements
		      `(lambda (this . args)
			  (let ((%nothis this))
			     (js-apply %this %superctor this args)
			     ,@(filter-map (lambda (p)
					      (bind-property this 'this p))
				  elements)
			     (set! this %nothis)
			     (js-undefined)))
		      `(with-access::JsFunction %superctor (arity) arity)
		      0 0 src loc))
		  (else
		   (make-class this name super elements
		      `(lambda (this)
			  (with-access::JsGlobalObject %this (js-new-target)
			     (if (eq? js-new-target (js-undefined))
				 (js-raise-type-error/loc %this ',loc
				    (format
				       "Class constructor '~a' cannot be invoked without 'new'"
				       ',name)
				    (js-undefined))
				 (begin
				    (set! js-new-target (js-undefined))
				    ,@(filter-map (lambda (p)
						     (bind-property this 'this p))
					 elements)
				    this))))
		      1 0 0 src loc))))))))

;*---------------------------------------------------------------------*/
;*    record-index ...                                                 */
;*---------------------------------------------------------------------*/
(define (record-index this::J2SNode ty::J2SRecord field::bstring)
   (multiple-value-bind (index el)
      (j2s-class-instance-get-property ty field)
      (when el
	 index)))

;*---------------------------------------------------------------------*/
;*    record-scmid ...                                                 */
;*---------------------------------------------------------------------*/
(define (record-scmid clazz)
   (with-access::J2SRecord clazz (name)
      (string->symbol (string-append "&" (symbol->string! name)))))

;*---------------------------------------------------------------------*/
;*    record-prototype-scmid ...                                       */
;*---------------------------------------------------------------------*/
(define (record-prototype-scmid clazz)
   (with-access::J2SRecord clazz (name)
      (string->symbol
	 (string-append "&" (symbol->string! name) "%PROTOTYPE"))))

;*---------------------------------------------------------------------*/
;*    record-constructor-scmid ...                                     */
;*---------------------------------------------------------------------*/
(define (record-constructor-scmid clazz)
   (with-access::J2SRecord clazz (name)
      (string->symbol
	 (string-append "&" (symbol->string! name) "%CTOR"))))

;*---------------------------------------------------------------------*/
;*    j2s-record-declaration ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-record-declaration this::J2SRecord)
   (with-access::J2SRecord this (name)
      (let ((super (j2s-class-super-val this)))
	 `(class ,(string->symbol
		     (if super
			 (format "~a::~a"
			    (record-scmid this) (record-scmid super))
			 (format "~a::JsRecord"
			    (record-scmid this))))))))

;*---------------------------------------------------------------------*/
;*    j2s-record-predicate ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-record-predicate this::J2SRecord)
   (with-access::J2SRecord this (name)
      `(define-inline (,(symbol-append 'js- name '?) o)
	  (isa? o ,(record-scmid this)))))

;*---------------------------------------------------------------------*/
;*    j2s-collect-records* ::J2SProgram ...                            */
;*---------------------------------------------------------------------*/
(define (j2s-collect-records* this::J2SProgram)
   (with-access::J2SProgram this (decls)
      (filter-map (lambda (d)
		     (when (isa? d J2SDeclClass)
			(with-access::J2SDeclClass d (val)
			   (when (isa? val J2SRecord)
			      val))))
	 decls)))

;*---------------------------------------------------------------------*/
;*    j2s-record-new ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-record-new this::J2SRecord args mode return ctx)
   
   (define (constructor this rec)
      (multiple-value-bind (clazz ctor)
	 (j2s-class-find-constructor this)
	 (if ctor
	     `(,(record-constructor-scmid clazz)
	       ,rec
	       ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args))
	     #unspecified)))
   
   (let ((rec (gensym 'this)))
      `(let ((,rec ,(j2s-alloc-record this mode return ctx)))
	  ,(constructor this rec)
	  ,rec)))

;*---------------------------------------------------------------------*/
;*    j2s-alloc-record ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-alloc-record this::J2SRecord mode return ctx)
   (with-access::J2SRecord this (cmap elements)
      (let ((rec (gensym 'this))
	    (props (filter (lambda (p)
			      (and (not (isa? p J2SMethodPropertyInit))
				   (isa? p J2SDataPropertyInit)))
		      (j2s-class-instance-properties this))))
	 `(let ((,rec (js-make-jsrecord ,(length props)
			 ,(j2s-scheme cmap mode return ctx)
			 ,(record-prototype-scmid this)
			 ,(record-scmid this))))
	     ,@(map (lambda (prop idx)
		       (with-access::J2SDataPropertyInit prop (val)
			  `(js-object-inline-set! ,rec ,idx
			      ,(j2s-scheme val mode return ctx))))
		  props (iota (length props)))
	     ,rec))))

;*---------------------------------------------------------------------*/
;*    j2s-record-prototype-constructor ...                             */
;*---------------------------------------------------------------------*/
(define (j2s-record-prototype-constructor this::J2SRecord)
   (let ((super (j2s-class-super-val this)))
      `((define ,(record-prototype-scmid this) (js-undefined))
	(define ,(record-constructor-scmid this) (js-undefined)))))
       
;*---------------------------------------------------------------------*/
;*    j2s-scheme-record-super ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-record-super this::J2SCall mode return ctx)
   (with-access::J2SCall this (loc fun this args protocol cache)
      (with-access::J2SSuper fun (context)
	 (with-access::J2SRecord context (name)
	    (let ((super (j2s-class-super-val context)))
	       (if (isa? super J2SRecord)
		   (multiple-value-bind (rec ctor)
		      (j2s-class-find-constructor super)
		      (if ctor
			  ;; invoke the super class constructor directly
			  `(,(record-constructor-scmid rec)
			    this
			    ,@(map (lambda (a)
				      (j2s-scheme a mode return ctx))
				 args))
			  ;; no constructor in the class hierarchy but still need
			  ;; to evaluate the arguments
			  `(begin
			      ,@(map (lambda (a)
					(j2s-scheme a mode return ctx))
				   args))))
		   (raise
		      (instantiate::&io-parse-error
			 (proc "hopc (scheme)")
			 (msg "Record has not super class")
			 (obj name)
			 (fname (cadr loc))
			 (location (caddr loc))))))))))
		   

   
   
