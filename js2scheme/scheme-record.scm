;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-record.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 15 07:09:51 2021                          */
;*    Last change :  Mon Sep  6 08:20:10 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Record generation                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-record

   (include "ast.sch"
	    "usage.sch"
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

   (export (j2s-scheme-record-new ::J2SNew ::J2SRecord args mode return ctx))
	   
   (export (j2s-collect-records*::pair-nil ::J2SProgram)
	   (record-index ::J2SNode ::J2SRecord ::bstring)
	   (record-scmid::symbol ::J2SRecord)
	   (j2s-record-declaration ::J2SRecord)
	   (j2s-record-predicate ::J2SRecord)
	   
	   (j2s-record-prototype-constructor::pair this::J2SRecord mode return ctx)
	   (j2s-scheme-record-super ::J2SCall mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-call-record-constructor ...                           */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme-call-class-constructor record::J2SRecord ecla enewtarget eobj args loc mode return ctx)
   `(,(class-constructor-id record)
     ,eobj
     ,@(if (class-new-target? record) (list enewtarget) '())
     ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-record-new ...                                        */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-record-new this::J2SNew record args mode return ctx)
   (let ((obj (gensym 'this))
	 (rec (gensym 'record))
	 (res (gensym 'res))
	 (props (j2s-class-instance-properties record)))
      (with-access::J2SRecord record (decl cmap)
	 (with-access::J2SDeclClass decl (id)
	    (with-access::J2SNew this (loc)
	       `(let* ((,obj (js-make-jsrecord ,(length props)
				,(j2s-scheme cmap mode return ctx)
				,(class-prototype-id record)
				,(record-scmid record)))
		       (,res ,(j2s-scheme-call-class-constructor record rec rec obj args loc
				 mode return ctx)))
		   ,(if (j2s-class-constructor-might-return? record)
			`(if (eq? ,res ,obj)
			     ,obj
			     (js-raise-type-error %this
				,(format "Record constructor '~s' must return the new record" id) this))
			obj)))))))

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
   
   (define (make-class this name super els arity length ctorsz src loc)
      (let* ((cname (or name (gensym 'record)))
	     (clazz (class-class-id this))
	     (ctorf (class-constructor-id this))
	     (proto (class-prototype-id this)))
	 `(let ((,clazz (js-make-function %this
			   (lambda (this . args)
			      (with-access::JsGlobalObject %this (js-new-target)
				 (if (eq? js-new-target (js-undefined))
				     (js-raise-type-error/loc %this ',loc
					,(format "Record constructor '~a' cannot be invoked without 'new'"
					    name)
					(js-undefined))
				     (set! js-new-target (js-undefined)))
				 (apply ,ctorf this args)))
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
	     (set! ,proto ,(j2s-record-prototype this mode return ctx))
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
   
   (with-access::J2SClass this (super elements name src loc decl)
      (let ((ctor (j2s-class-get-constructor this))
	    (props (j2s-class-instance-properties this)))
	 (let-super super
	    (lambda (super)
	       (cond
		  (ctor
		   (with-access::J2SClassElement ctor (prop)
		      (with-access::J2SDataPropertyInit prop (val)
			 (with-access::J2SFun val (constrsize params thisp)
			    (make-class this name super elements
			       (j2s-function-arity val ctx)
			       (length params) constrsize
			       src loc)))))
		  (super
		   (make-class this name super elements
		      '(js-function-arity 0 -1 'scheme)
		      0 (length props) src loc))
		  (else
		   (make-class this name super elements
		      '(js-function-arity 0 -1 'scheme)
		      0 (length props) src loc))))))))

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
      (string->symbol (string-append "&JsRec" (symbol->string! name)))))

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
	     `(,(class-constructor-id clazz)
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
	    (props (j2s-class-instance-properties this)))
	 `(js-make-jsrecord ,(length props)
	     ,(j2s-scheme cmap mode return ctx)
	     ,(class-prototype-id this)
	     ,(record-scmid this)))))

;*---------------------------------------------------------------------*/
;*    j2s-record-constructor ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-record-constructor this::J2SRecord mode return ctx)

   (define (unthis this loc)
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr (instantiate::J2SPragma
		  (loc loc)
		  (expr `(set! ,this (js-make-let)))))))
   
   (define (returnthis this loc)
      (J2SStmtExpr (J2SRef this)))
   
   (define (new-target-param loc)
      (instantiate::J2SDecl
	 (loc loc)
	 (id 'new-target)
	 (_scmid 'new-target)
	 (binder 'param)))

   (define (this-for-super-decl thisp)
      (with-access::J2SDecl thisp (loc)
	 (let* ((thisp-safe (duplicate::J2SDecl thisp (binder 'let-opt)))
		(decl (J2SLetOpt '(ref) '!this (J2SThis thisp-safe))))
	    (with-access::J2SDecl thisp (binder)
	       (set! binder 'let))
	    (with-access::J2SDecl decl (_scmid)
	       (set! _scmid '!this))
	    decl)))

   (define (j2s-record-constructor-fun this::J2SRecord fun::J2SFun)
      (with-access::J2SRecord this (super)
	 (with-access::J2SFun fun (idthis body params thisp new-target)
	    (with-access::J2SBlock body (loc endloc nodes)
	       (cond
		  ((and (isa? super J2SRecord)
			(j2s-scheme-need-super-check? fun))
		   (with-access::J2SRecord this (need-super-check)
		      (set! need-super-check #t)
		      (let ((decl (this-for-super-decl thisp)))
			 ;; for dead-zone check
			 (decl-usage-add! thisp 'uninit)
			 (set! body
			    (instantiate::J2SLetBlock
			       (loc loc)
			       (rec #f)
			       (endloc endloc)
			       (decls (list decl))
			       (nodes (list (unthis idthis loc)
					 (J2STry body
					    (J2SNop)
					    (returnthis thisp loc)))))))))
		  ((isa? super J2SRecord)
		   ;; dont add instance properties has they will be
		   ;; introduced when invoking super
		   body)
		  (else
		   ;; no super class initializes the
		   ;; instance properties first
		   (set! body
		      (J2SBlock
			 (J2SStmtExpr
			    (J2SPragma
			       `(begin
				   ,@(j2s-scheme-init-instance-properties
					this mode return ctx))))
			 body))))
	       (when (class-new-target? this)
		  (set! new-target 'argument)
		  (set! params (cons (new-target-param loc) params))))
	    (jsfun->lambda fun mode return ctx #f #t))))
   
   (define (j2s-record-constructor/ctor this ctor::J2SClassElement)
      (with-access::J2SClassElement ctor (prop)
	 (with-access::J2SDataPropertyInit prop (val)
	    (let ((dup (duplicate::J2SFun val)))
	       (j2s-record-constructor-fun this dup)))))

   (define (j2s-record-constructor/w-ctor this)
      (let ((superctor (j2s-class-find-constructor this)))
	 (with-access::J2SRecord this (loc decl)
	    (let ((self (instantiate::J2SDecl
			   (loc loc)
			   (id 'this)
			   (_scmid 'this)
			   (binder 'param)))
		  (endloc loc)
		  (params (if (isa? superctor J2SClassElement)
			      (with-access::J2SClassElement superctor (prop)
				 (with-access::J2SMethodPropertyInit prop (val)
				    (with-access::J2SFun val (params)
				       (map (lambda (d) (duplicate::J2SDecl d))
					  params))))
			      '())))
	       (j2s-record-constructor-fun this
		  (J2SFun 'constructor params
		     (J2SBlock
			(J2SCall* (J2SSuper self this)
			   (map (lambda (d) (J2SRef d)) params)))))))))
   
   (let ((ctor (j2s-class-get-constructor this)))
      (if ctor
	  (j2s-record-constructor/ctor this ctor)
	  (j2s-record-constructor/w-ctor this))))

;*---------------------------------------------------------------------*/
;*    j2s-record-prototype ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-record-prototype this::J2SRecord mode return ctx)
   (let ((super (j2s-class-super-val this)))
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
	  (with-access::J2SClass super (loc decl)
	     `(js-new-sans-construct %this
		 ,(j2s-scheme (J2SRef decl) mode return ctx)))))))

;*---------------------------------------------------------------------*/
;*    j2s-record-prototype-constructor ...                             */
;*---------------------------------------------------------------------*/
(define (j2s-record-prototype-constructor this::J2SRecord mode return ctx)
   `((define ,(class-prototype-id this)
	#unspecified)
     (define ,(class-constructor-id this)
	,(j2s-record-constructor this mode return ctx))))
       
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
			  `(,(class-constructor-id rec)
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
		   

   
   
