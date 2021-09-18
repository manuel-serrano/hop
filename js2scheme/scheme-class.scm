;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-class.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:01:46 2017                          */
;*    Last change :  Sat Sep 18 06:04:27 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    ES2015 Scheme class generation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-class

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
	   __js2scheme_scheme
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant)

   (export (j2s-scheme-class-new this::J2SNew ::J2SClass args mode return ctx)
	   (j2s-scheme-class-propname ::J2SExpr mode return ctx)
	   (j2s-scheme-bind-class-method prop::J2SPropertyInit obj mode return ctx)
	   (j2s-scheme-class-call-super ::J2SCall mode return ctx)
	   (j2s-scheme-need-super-check?::bool ::J2SFun)
	   (j2s-scheme-init-instance-properties ::J2SClass mode return ctx)
	   (generic j2s-scheme-call-class-constructor clazz::J2SClass ecla enewtarget eobj args ::J2SNode mode return ctx)
	   (j2s-scheme-class-put-info! ::J2SClass ::keyword ::obj)
	   (j2s-scheme-class-get-info ::J2SClass ::keyword)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-call-class-constructor ...                            */
;*---------------------------------------------------------------------*/
(define-generic (j2s-scheme-call-class-constructor clazz::J2SClass ecla enewtarget eobj args node mode return ctx)
   (with-access::J2SNode node (loc)
      (let loop ((ctor (j2s-class-find-constructor clazz)))
	 (cond
	    ((isa? ctor J2SClassElement)
	     (with-access::J2SClassElement ctor (prop)
		(with-access::J2SMethodPropertyInit prop (val)
		   (loop val))))
	    ((isa? ctor J2SFun)
	     `(with-access::JsClass ,ecla (constructor)
		 ,(with-access::J2SFun ctor (params)
		     (let ((lparams (length params))
			   (largs (length args))
			   (nt (if (class-new-target? clazz)
				   (list enewtarget)
				   '()))
			   (vals (map (lambda (a)
					 (j2s-scheme a mode return ctx))
				    args)))
			(cond
			   ((=fx lparams largs)
			    `(constructor ,eobj ,@nt ,@vals))
			   ((<fx largs lparams)
			    `(constructor ,eobj ,@nt ,@vals
				,@(make-list (-fx lparams largs)
				     '(js-undefined))))
			   (else
			    (let ((ts (map (lambda (a) (gensym '%t)) args)))
			       `(let* ,(map (lambda (t a) (list t a))
					  ts vals)
				   (constructor ,eobj ,@nt ,@(take ts lparams))))))))))
	    ((not ctor)
	     `(with-access::JsClass ,ecla (constructor)
		 ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)
		 (constructor ,eobj)))
	    (else
	     `(with-access::JsClass ,ecla (constructor)
		 ;; default constructor
		 `(constructor ,eobj
		     ,@(if (class-new-target? clazz) (list enewtarget) '())
		     ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-call-fun-constructor ...                              */
;*    -------------------------------------------------------------    */
;*    Used when the class is statically unknown                        */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-call-fun-constructor ecla eobj args loc mode return ctx)
   `(begin
       (when (js-function-new-target? ,ecla)
	  (js-new-target-push! %this new-target))
       (js-call-jsprocedure %this ,ecla ,eobj
	  ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-call-expr-constructor ...                             */
;*    -------------------------------------------------------------    */
;*    Used when the class is statically unknown                        */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-call-expr-constructor ecla eobj args loc mode return ctx)
   
   (define (let-args args proc)
      (if (every (lambda (a) (or (symbol? a) (number? a) (boolean? a))) args)
	  (proc args)
	  (let ((names (map (lambda (i)
			       (string->symbol (format "%a~a" i)))
			  (iota (length args)))))
	     `(let ,(map list names args)
		 ,(proc names)))))
   
   `(cond
       ((isa? ,ecla JsClass)
	,(let-args (map (lambda (a) (j2s-scheme a mode return ctx)) args)
	    (lambda (args)
	       `(with-access::JsClass ,ecla (constructor)
		  (if (js-function-new-target? ,ecla)
		      (js-call-procedure constructor ,eobj new-target ,@args)
		      (js-call-procedure constructor ,eobj ,@args))))))
       ((isa? ,ecla JsFunction)
	,(let ((res (gensym 'res)))
	   `(begin
	       (js-new-target-push! %this new-target)
	       (let ((,res ,(j2s-scheme-call-fun-constructor ecla eobj args loc mode return ctx)))
		  (js-new-target-pop! %this)
		  ,res))))
       (else
	(js-raise-type-error/loc %this ',loc
	   "Class extends value \"~a\" is not a constructor or null"
	   ,ecla))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-class-new ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-class-new this::J2SNew clazz args mode return ctx)

   (define (plain-class? clazz decl)
      ;; a function is a plain class if it only inherits recursively
      ;; from other classes or from user defined functions
      (when (and (isa? decl J2SDeclClass) (not (decl-usage-has? decl '(assig))))
	 (let ((root (j2s-class-root-val clazz)))
	    (or (isa? root J2SClass)
		(isa? root J2SFun)
		(with-access::J2SClass clazz (super)
		   (or (isa? super J2SUndefined)
		       (isa? super J2SNull)))))))
		   
   (let ((obj (gensym 'this))
	 (cla (gensym 'class))
	 (res (gensym 'res)))
      (with-access::J2SClass clazz (decl)
	 (if (plain-class? clazz decl)
	     (with-access::J2SNew this (loc)
		`(let* ((,cla ,(if decl
				   (j2s-scheme (J2SRef decl) mode return ctx)
				   (j2s-scheme clazz mode return ctx)))
			(,obj (js-object-alloc-fast %this ,cla))
			(,res ,(j2s-scheme-call-class-constructor clazz cla cla obj args this
				  mode return ctx)))
		    ,(if (j2s-class-constructor-might-return? clazz)
			 `(if (js-object? ,res) ,res ,obj)
			 obj)))
	     (with-access::J2SNew this (loc clazz)
		(j2s-new loc (j2s-scheme-box clazz mode return ctx)
		   (map (lambda (a)
			   (j2s-scheme-box a mode return ctx))
		      args)))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-class-propname ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-class-propname name::J2SExpr mode return ctx)
   (cond
      ((isa? name J2SString)
       (with-access::J2SString name (val)
	  (let ((str (string-for-read val)))
	     (& val (context-program ctx)))))
      ((isa? name J2SNumber)
       (with-access::J2SNumber name (val)
	  (if (fixnum? val)
	      `(js-integer-name->jsstring ,val)
	      `(js-toname ,(j2s-scheme val mode return ctx) %this))))
      ((isa? name J2SPragma)
       `(js-toname ,(j2s-scheme name mode return ctx) %this))
      ((isa? name J2SLiteralCnst)
       `(js-toname ,(j2s-scheme name mode return ctx) %this))
      ((isa? name J2SLiteralValue)
       (with-access::J2SLiteralValue name (val)
	  `(js-toname ,(j2s-scheme val mode return ctx) %this)))
      (else
       `(js-toname ,(j2s-scheme name mode return ctx) %this))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-bind-class-method ...                                 */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-bind-class-method prop obj mode return ctx)
   (cond
      ((j2s-class-property-constructor? prop)
       #f)
      ((isa? prop J2SMethodPropertyInit)
       (with-access::J2SMethodPropertyInit prop (name val inlinecachevar)
	  (let ((name (j2s-scheme-class-propname name mode return ctx))
		(val (j2s-scheme val mode return ctx)))
	     (if inlinecachevar
		 ;; assign the global variable used on inline call method sites
		 `(begin
		     (set! ,inlinecachevar ,val)
		     (js-bind! %this ,obj ,name
			 :value ,inlinecachevar
			 :writable #t :enumerable #f :configurable #t))
		 `(js-bind! %this ,obj ,name
		     :value ,val
		     :writable #t :enumerable #f :configurable #t)))))
      ((isa? prop J2SDataPropertyInit)
       (with-access::J2SDataPropertyInit prop (name val)
	  `(js-bind! %this ,obj
	      ,(j2s-scheme-class-propname name mode return ctx)
	      :value ,(j2s-scheme val mode return ctx)
	      :writable #t :enumerable #f :configurable #t)))
      ((isa? prop J2SAccessorPropertyInit)
       (with-access::J2SAccessorPropertyInit prop (name get set)
	  `(js-bind! %this ,obj
	      ,(j2s-scheme-class-propname name mode return ctx)
	      :get ,(when get
		       (j2s-scheme get mode return ctx))
	      :set ,(when set
		       (j2s-scheme set mode return ctx))
	      :writable #t :enumerable #f :configurable #t)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SClass ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SClass mode return ctx)
   
   (define (bind-static clazz obj m)
      (with-access::J2SClassElement m (prop static)
	 (when static
	    (j2s-scheme-bind-class-method prop obj mode return ctx))))
   
   (define (bind-prototype clazz obj m)
      (with-access::J2SClassElement m (prop static)
	 (when (and (not static)
		    (or (and (isa? prop J2SMethodPropertyInit)
			     (not (j2s-class-property-constructor? prop)))
			(isa? prop J2SAccessorPropertyInit)))
	    (j2s-scheme-bind-class-method prop obj mode return ctx))))
   
   (define (class-prototype this::J2SClass %super)
      (with-access::J2SClass this (super)
	 (cond
	    ((isa? super J2SUndefined)
	     `(with-access::JsGlobalObject %this (js-object)
		 (js-new0 %this js-object)))
	    ((isa? super J2SNull)
	     `(with-access::JsGlobalObject %this (js-object)
		 (let ((o (js-new0 %this js-object)))
		    (js-object-proto-set! o (js-null))
		    o)))
	    (else
	     `(js-new-sans-construct %this ,%super)))))
   
   (define (class->lambda clazz::J2SClass arity loc)
      (let ((args (map! (lambda (i)
			   (string->symbol (format "%a~s" i)))
		     (iota arity))))
	 `(lambda (this ,@args)
	     (let ((new-target (js-new-target-pop! %this)))
		(if (eq? new-target (js-undefined))
		    (js-raise-type-error/loc %this ',loc
		       ,(with-access::J2SClass clazz (name)
			   (format
			      "Class constructor '~a' cannot be invoked without 'new'"
			      name))
		       (js-undefined))
		    (,(class-constructor-id clazz)
		     this
		     ,@(if (class-new-target? clazz) '(new-target) '())
		     ,@args))))))
   
   (define (class-unknown-super->lambda clazz::J2SClass loc)
      `(lambda (this . args)
	  (let ((new-target (js-new-target-pop! %this)))
	     (if (eq? new-target (js-undefined))
		 (js-raise-type-error/loc %this ',loc
		    ,(with-access::J2SClass clazz (name)
			(format
			   "Class constructor '~a' cannot be invoked without 'new'"
			   name))
		    (js-undefined))
		 (,(class-constructor-id clazz)
		  this
		  ,@(if (class-new-target? clazz) '(new-target) '())
		  args)))))
   
   (define (make-class this super function constructor aritye length src loc)
      (with-access::J2SClass this (elements cmap name constrsize)
	 (let* ((clazz (class-class-id this))
		(ctorf (class-constructor-id this))
		(proto (class-prototype-id this))
		(alloc (cond
			  ((not super)
			   'js-object-alloc/new-target)
			  ((isa? (j2s-class-super-val this) J2SClass)
			   `(with-access::JsFunction ,super (alloc) alloc))
			  (else
			   `(lambda (%this ctor)
			       (with-access::JsFunction ,super (alloc)
				  (let ((o (alloc %this ctor)))
				     (js-new-target-push! %this ctor)
				     o))))))
		(constrmap (if cmap
			       (j2s-scheme cmap mode return ctx)
			       '(with-access::JsGlobalObject %this (js-initial-cmap)
				 js-initial-cmap)))
		(constrsz (cond
			     (cmap
			      constrsize)
			     (super
			      `(with-access::JsFunction ,super (constrsize)
				  constrsize))
			     (else
			      1))))
	    (j2s-scheme-class-put-info! this :scm-cache-constructor constructor)
	    `(letrec* ((,ctorf ,constructor)
		       (,proto ,(class-prototype this super))
		       (,clazz (js-make-function %this ,function
				  ,aritye
				  (js-function-info
				     :name ,(class-info-name this)
				     :len ,length
				     :new-target ,(class-new-target? this))
				  :strict ',mode
				  :alloc ,alloc
				  :constructor ,ctorf
				  :clazz JsObject
				  :prototype  ,proto
				  :__proto__ ,(if (null? super)
						  '(with-access::JsGlobalObject %this (js-function-prototype)
						    js-function-prototype)
						  super)
				  :constrsize ,constrsz
				  :constrmap ,constrmap))
		       ,@(if name `((,(j2s-class-id this ctx) ,clazz)) '()))
		,@(filter-map (lambda (m) (bind-static this clazz m)) elements)
		,@(filter-map (lambda (m) (bind-prototype this proto m)) elements)
		,clazz))))
   
   (define (let-super this::J2SClass proc)
      (with-access::J2SClass this (super)
	 (cond
	    ((isa? super J2SUndefined)
	     (proc #f))
	    ((isa? super J2SNull)
	     (proc #f))
	    ((not (j2s-class-methods-use-super? this))
	     `(let ((%super ,(j2s-scheme super mode return ctx)))
		 ,(proc '%super)))
	    ((j2s-class-super-val this)
	     `(let* ((%super ,(j2s-scheme super mode return ctx))
		     (%super-prototype (js-function-prototype-get %super %super
					  ,(& "prototype" (context-program ctx))
					  %this)))
		 ,(proc '%super)))
	    (else
	     `(let* ((%super ,(j2s-scheme super mode return ctx))
		     (%super-prototype (js-get %super
					  ,(& "prototype" (context-program ctx)) %this)))
		 ,(proc '%super))))))

   (define (ctor-arity ctor)
      (cond
	 ((isa? ctor J2SClassElement)
	  (with-access::J2SClassElement ctor (prop (super clazz))
	     (with-access::J2SMethodPropertyInit prop (val)
		(ctor-arity val))))
	 ((isa? ctor J2SFun)
	  (with-access::J2SFun ctor (params)
	     (length params)))
	 (else
	  0)))

   (define (init->lambda this::J2SClass init arity)
      (let ((names (map (lambda (i)
			   (string->symbol (format "%a~a" i)))
		      (iota arity))))
	 `(lambda (this ,@names)
	     ,@(j2s-scheme-init-instance-properties
		  this mode return ctx)
	     this)))
      
   (with-access::J2SClass this (super elements src loc decl)
      (let ((ctor (j2s-class-get-constructor this)))
	 (let-super this
	    (lambda (%super)
	       (cond
		  (ctor
		   (with-access::J2SClassElement ctor (prop)
		      (with-access::J2SDataPropertyInit prop (val)
			 (with-access::J2SFun val (params thisp)
			    (make-class this %super
			       (class->lambda this (length params) loc)
			       (ctor->lambda val this mode return ctx %super)
			       (j2s-function-arity val ctx)
			       (length params)
			       src loc)))))
		  ((and (not %super) (pair? (j2s-class-instance-properties this :super #f)))
		   (let* ((init (j2s-class-find-constructor this))
			  (arity (ctor-arity init)))
		      (make-class this %super
			 (class->lambda this arity loc)
			 (init->lambda this init arity)
			 `(js-function-arity ,arity 0) arity
			 src loc)))
		  (%super
		   (let ((init (j2s-class-find-initializer this)))
		      (cond
			 ((isa? init J2SClass)
			  (let ((superctor (j2s-class-find-constructor init)))
			     (if superctor
				 (let ((arity (ctor-arity superctor)))
				    (make-class this %super
				       (class->lambda this arity loc)
				       (super-ctor->lambda this superctor mode return ctx)
				       `(with-access::JsFunction ,%super (arity) arity)
				       arity src loc))
				 (let ((superval (j2s-class-super-val this)))
				    (if (isa? superval J2SClass)
					(let ((superinit (j2s-class-find-initializer superval)))
					   (make-class this %super
					      (class->lambda this 0 loc)
					      (super-ctor->lambda this superinit mode return ctx)
					      `(with-access::JsFunction ,%super (arity) arity)
					      0 src loc))
					(make-class this %super
					   (class-unknown-super->lambda this loc)
					   (super-ctor->lambda this #f mode return ctx)
					   `(with-access::JsFunction ,%super (arity) arity)
					   0 src loc))))))
			 ((isa? init J2SClassElement)
			  (let ((arity (ctor-arity init)))
			     (make-class this %super
				(class->lambda this arity loc)
				(super-ctor->lambda this init mode return ctx)
				`(with-access::JsFunction ,%super (arity) arity)
				`(js-function-arity ,arity 0) src loc)))
			 ((isa? init J2SFun)
			  (let ((arity (ctor-arity init)))
			     (make-class this %super
				(class->lambda this arity loc)
				(super-ctor->lambda this init mode return ctx)
				`(with-access::JsFunction ,%super (arity) arity)
				`(js-function-arity ,arity 0) src loc)))
			 (init
			  (make-class this %super
			     (class-unknown-super->lambda this loc)
			     (super-ctor->lambda this init mode return ctx)
			     `(with-access::JsFunction ,%super (arity)
				 arity)
			     `(with-access::JsFunction ,%super (info)
				 (vector-ref info 1))
			     src loc))
			 ((isa? (j2s-class-root-val this) J2SClass)
			  (make-class this %super
			     (class->lambda this 0 loc)
			     '(lambda (this) this)
			     '(js-function-arity 0 0 'scheme) 0 src loc))
			 (else
			  (make-class this %super
			     (class-unknown-super->lambda this loc)
			     (super-ctor->lambda this #unspecified mode return ctx)
			     '(js-function-arity 0 -1 'scheme) 0 src loc)))))
		  (else
		   (make-class this %super
		      (class->lambda this 0 loc)
		      `(lambda (this)
			  ,@(j2s-scheme-init-instance-properties
			       this mode return ctx)
			  this)
		      `(js-function-arity 0 0) 0 src loc))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-init-instance-properties ...                          */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-init-instance-properties this::J2SClass mode return ctx)
   
   (define (bind-class-property el)
      (with-access::J2SClassElement el (prop usage)
	 (cond
	    ((isa? prop J2SMethodPropertyInit)
	     #f)
	    ((isa? prop J2SAccessorPropertyInit)
	     #f)
	    ((isa? prop J2SDataPropertyInit)
	     (with-access::J2SDataPropertyInit prop (name val loc cache)
		(unless (isa? val J2SUndefined)
		   (j2s-put! loc 'this name this
		      (j2s-scheme-class-propname name mode return ctx) 
		      'any
		      (j2s-scheme val mode return ctx)
		      (j2s-type val)
		      (strict-mode? mode) ctx cache))))
	    ((usage-has? usage '(uninit))
	     (with-access::J2SPropertyInit prop (name loc cache)
		(j2s-put! loc 'this name this
		   (j2s-scheme-class-propname name mode return ctx) 
		   'any
		   '(js-undefined)
		   'any
		   (strict-mode? mode) ctx cache)))
	    (else
	     #f))))
   
   (with-access::J2SClass this (elements)
      (filter-map bind-class-property elements)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-class-call-super ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-class-call-super this::J2SCall mode return ctx)
   
   (define (class-ctor-new-target? this::J2SClass)
      (let ((ctor (j2s-class-get-constructor this)))
	 (when (isa? ctor J2SClassElement)
	    (with-access::J2SClassElement ctor (prop)
	       (with-access::J2SDataPropertyInit prop (val)
		  (with-access::J2SFun val (new-target)
		     (memq new-target '(global argument))))))))
   
   (define (scheme-class-super this context new-target)
      (with-access::J2SCall this (fun args)
	 (let* ((tmp (gensym 'tmp))
		(len (length args))
		(call (if (>=fx len 11)
			  `(js-calln
			      ,j2s-unresolved-call-workspace
			      %super this
			      (list ,@(j2s-scheme args mode return ctx)))
			  `(,(string->symbol (format "js-call~a" len))
			    ,j2s-unresolved-call-workspace
			    %super this
			    ,@(j2s-scheme args mode return ctx)))))
	    `(begin
		,new-target
		(let ((,tmp ,call))
		   ,@(j2s-scheme-init-instance-properties context
			mode return ctx)
		   ,tmp)))))
   
   (define (scheme-class-super-declclass this clazz::J2SClass decl)
      (with-access::J2SCall this (args loc)
	 (with-access::J2SClass clazz (need-super-check name)
	    `(begin
		,(j2s-scheme-call-class-constructor (j2s-class-super-val clazz)
		    (j2s-scheme (J2SRef decl) mode return ctx)
		    'new-target
		    (if need-super-check '!this 'this)
		    args this mode return ctx)
		,@(if need-super-check '((set! this !this)) '())
		,@(j2s-scheme-init-instance-properties clazz
		     mode return ctx)))))
   
   (define (scheme-class-super-fun this context val::J2SFun)
      (with-access::J2SFun val (new-target)
	 (scheme-class-super this context
	    (if (memq new-target '(global argument))
		'(js-new-target-push! %this new-target)
		#unspecified))))
   
   (define (scheme-class-super-declfun this context decl::J2SDeclFun)
      (with-access::J2SDeclFun decl (val)
	 (scheme-class-super this context val)))
   
   (define (scheme-class-super-expr this clazz::J2SClass expr::J2SExpr)
      (with-access::J2SCall this (args loc)
	 (with-access::J2SClass clazz (need-super-check)
	    (let ((res (gensym 'res)))
	       `(begin
		   ;; (js-new-target-push! %this new-target)
		   (let ((,res ,(j2s-scheme-call-expr-constructor '%super
				   (if need-super-check '!this 'this)
				   args loc mode return ctx)))
		      ;; (js-new-target-pop! %this)
		      ,@(if need-super-check '((set! this !this)) '())
		      ,@(j2s-scheme-init-instance-properties clazz
			   mode return ctx)
		      ,res))))))
   
   (with-access::J2SCall this (fun args)
      (with-access::J2SSuper fun (context)
	 (with-access::J2SClass context (super)
	    (cond
	       ((isa? super J2SRef)
		(with-access::J2SRef super (decl)
		   (cond
		      ((isa? decl J2SDeclClass)
		       (with-access::J2SDeclClass decl (val)
			  ;; val is a J2SClass only when optimizations
			  ;; are enabled
			  (cond
			     ((not (isa? val J2SClass))
			      (scheme-class-super-expr this context super))
			     ((j2s-scheme-class-get-info val :scm-cache-constructor)
			      ;; try to inline super constructors
			      =>
			      (lambda (ctor)
				 (match-case ctor
				    ((lambda ?params . ?body)
				     (if (eq? (+fx 1 (length args)) (length params))
					 `(begin
					     (,ctor this ,@(j2s-scheme args mode return ctx))
					     ,@(j2s-scheme-init-instance-properties context mode return ctx))
					 (scheme-class-super-declclass this context decl)))
				    ((labels ((?id ?params . ?body)) ?id)
				     (if (eq? (+fx 1 (length args)) (length params))
					 `(labels ((,id ,params ,@body))
					     (,id this ,@(j2s-scheme args mode return ctx))
					     ,@(j2s-scheme-init-instance-properties context mode return ctx))
					 (scheme-class-super-declclass this context decl)))
				    (else
				     (scheme-class-super-declclass this context decl)))))
			     (else
			      (scheme-class-super-declclass this context decl)))))
		      ((isa? decl J2SDeclFun)
		       (scheme-class-super-declfun this context decl))
		      (else
		       (scheme-class-super-expr this context super)))))
	       ((isa? super J2SFun)
		(scheme-class-super-fun this context super))
	       (else
		(scheme-class-super-expr this context super)))))))
   
;*---------------------------------------------------------------------*/
;*    j2s-scheme-need-super-check? ...                                 */
;*    -------------------------------------------------------------    */
;*    A constructor needs a super check, if it cannot be proved        */
;*    statically that                                                  */
;*      1) it always calls the super constructor                       */
;*      2) the call the super preceeds all "this" accesses             */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-need-super-check? val::J2SFun)
   (with-access::J2SFun val (body)
      (not (eq? (super-call body) #t))))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (super-call this::J2SNode)
   #f)

;*---------------------------------------------------------------------*/
;*    super-call ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SExpr)
   #f)

;*---------------------------------------------------------------------*/
;*    super-call ::J2SUnary ...                                        */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SUnary)
   (with-access::J2SUnary this (expr)
      (super-call expr)))
   
;*---------------------------------------------------------------------*/
;*    super-call ::J2SBinary ...                                       */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SBinary)
   (with-access::J2SBinary this (lhs rhs)
      (let ((l (super-call lhs)))
	 (cond
	    ((eq? l #t) #t)
	    ((eq? l #unspecified) (super-call rhs))
	    (else #f)))))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SAssig ...                                        */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs)
      (let ((r (super-call rhs)))
	 (cond
	    ((not r) #f)
	    ((eq? r #unspecified) (super-call lhs))
	    (else #t)))))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SLiteral ...                                      */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SLiteral)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    super-call ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SRef)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    super-call ::J2SThis ...                                         */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SThis)
   #f)

;*---------------------------------------------------------------------*/
;*    super-call ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SAccess)
   (with-access::J2SAccess this (obj field)
      (let ((f (super-call field)))
	 (cond
	    ((eq? f #t) #t)
	    ((eq? f #unspecified) (super-call obj))
	    (else #f)))))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SCall)
   (with-access::J2SCall this (fun args)
      (when (every super-call args)
	 (or (every (lambda (a) (eq? (super-call a) #t)) args)
	     (isa? fun J2SSuper)
	     (super-call fun)))))

;*---------------------------------------------------------------------*/
;*    super-call-cond ...                                              */
;*---------------------------------------------------------------------*/
(define (super-call-cond test then else)
   (let ((t (super-call test)))
      (cond
	 ((eq? t #t)
	  t)
	 ((eq? t #unspecified)
	  (let ((t (super-call then))
		(e (super-call else)))
	     (cond
		((or (not t) (not e)) #f)
		((and (eq? t #t) (eq? e #t)) #t)
		(else #unspecified))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SIf ...                                           */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SIf)
   (with-access::J2SIf this (test then else)
      (super-call-cond test then else)))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SStmt ...                                         */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SStmt)
   #f)

;*---------------------------------------------------------------------*/
;*    super-call ::J2SStmtExpr ...                                     */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SStmtExpr)
   (with-access::J2SStmtExpr this (expr)
      (super-call expr)))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SCond ...                                         */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SCond)
   (with-access::J2SCond this (test then else)
      (super-call-cond test then else)))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SSeq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SSeq)
   (with-access::J2SSeq this (nodes)
      (let loop ((nodes nodes))
	 (if (null? nodes)
	     #unspecified
	     (let ((s (super-call (car nodes))))
		(cond
		   ((not s) #f)
		   ((eq? s #unspecified) (loop (cdr nodes)))
		   (else #t)))))))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls)
      (let loop ((decls decls))
	 (if (null? decls)
	     (call-next-method)
	     (let ((s (super-call (car decls))))
		(cond
		   ((not s) #f)
		   ((eq? s #unspecified) (loop (cdr decls)))
		   (else #t)))))))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SDecl)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    super-call ::J2SDeclInit ...                                     */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SDeclInit)
   (with-access::J2SDeclInit this (val)
      (super-call val)))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SReturn)
   (with-access::J2SReturn this (expr)
      (super-call expr)))

;*---------------------------------------------------------------------*/
;*    super-call ::J2SPragma ...                                       */
;*---------------------------------------------------------------------*/
(define-method (super-call this::J2SPragma)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    function-new-target? ...                                         */
;*---------------------------------------------------------------------*/
(define (function-new-target?::bool this::J2SFun)
   (with-access::J2SFun this (new-target)
      (when (memq new-target '(global argument)) #t)))
      
;*---------------------------------------------------------------------*/
;*    ctor->lambda ...                                                 */
;*---------------------------------------------------------------------*/
(define (ctor->lambda ctor::J2SFun clazz::J2SClass mode return ctx super)
   
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

   (let ((dup (duplicate::J2SFun ctor)))
      (with-access::J2SFun dup (body idthis loc thisp params loc new-target)
	 (with-access::J2SBlock body (loc endloc nodes)
	    (cond
	       ((and (symbol? super) (j2s-scheme-need-super-check? dup))
		(with-access::J2SClass clazz (need-super-check)
		   (set! need-super-check #t))
		(when (> (bigloo-warning) 1)
		   (warning/loc loc "Forced super check in constructor"))
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
				      ;; see test/hopjs/noserv/es6-class.js
				      (J2SNop)
				      (returnthis thisp loc))))))))
	       ((symbol? super)
		body)
	       (else
		;; no super class initializes the instance properties first
		(set! body
		   (J2SBlock
		      (J2SStmtExpr
			 (J2SPragma
			    `(begin
				,@(j2s-scheme-init-instance-properties
				     clazz mode return ctx))))
		      body)))))
	 
	 (when (class-new-target? clazz)
	    (set! new-target 'argument)
	    (set! params (cons (new-target-param loc) params))))
   
      (jsfun->lambda dup mode return ctx #f #t)))

;*---------------------------------------------------------------------*/
;*    super-ctor->lambda ...                                           */
;*---------------------------------------------------------------------*/
(define (super-ctor->lambda clazz::J2SClass superctor mode return ctx)
   (cond
      ((not superctor)
       `(lambda (this new-target args)
	   (cond
	      ((isa? %super JsClass)
	       (with-access::JsClass %super (constructor)
		  (if (js-function-new-target? %super)
		      (js-calln-procedure constructor this (cons new-target args))
		      (js-calln-procedure constructor this args))))
	      ((isa? %super JsFunction)
	       (if (js-function-new-target? %super)
		   (js-calln-jsprocedure %this %super this (cons new-target args))
		   (begin
		      (js-new-target-push! %this new-target)
		      (let ((v (js-calln-jsprocedure %this %super this args)))
			 (js-new-target-pop! %this)
			 v))))
	      (else
	       ,(with-access::J2SClass clazz (name loc)
		   `(js-raise-type-error/loc %this ',loc
		       ,(format "Class `~a' has a wrong super class" name)
		       %super))))
	   ,@(j2s-scheme-init-instance-properties
		clazz mode return ctx)
	   this))
      ((isa? superctor J2SClassElement)
       (with-access::J2SClassElement superctor (prop (super clazz))
	  (if (null? (j2s-class-instance-properties clazz :super #f))
	      `(with-access::JsClass %super (constructor) constructor)
	      (with-access::J2SMethodPropertyInit prop (val)
		 (with-access::J2SFun val (params loc)
		    (with-access::J2SClass clazz (need-super-check)
		       (let ((args (map! (lambda (i)
					    (string->symbol (format "%a~s" i)))
				      (iota (length params)))))
			  `(lambda (this ,@(if (class-new-target? super)
					       '(new-target)
					       '())
				      ,@args)
			      ,(j2s-scheme-call-class-constructor super
				  '%super 'new-target
				  (if need-super-check '!this 'this)
				  (map (lambda (a) (J2SHopRef a)) args)
				  clazz mode return ctx)
			      ,@(if need-super-check '((set! this !this)) '())
			      ,@(j2s-scheme-init-instance-properties
				   clazz mode return ctx)
			      this))))))))
      ((isa? superctor J2SFun)
       (with-access::J2SFun superctor (params loc)
	  (let ((args (map! (lambda (i)
			       (string->symbol (format "%a~s" i)))
			 (iota (length params)))))
	     `(lambda (this ,@(if (function-new-target? superctor)
				  '(new-target)
				  '())
			 ,@args)
		 ,@(if (function-new-target? superctor)
		       '((js-new-target-push! %this new-target))
		       '())
		 (js-call-jsprocedure %this %super this ,@args)
		 ,@(if (function-new-target? superctor)
		       '((js-new-target-pop! %this))
		       '())
		 ,@(j2s-scheme-init-instance-properties
		      clazz mode return ctx)
		 this))))
      ((isa? superctor J2SClass)
       ;; not ctor but one of the super classes has fields
       `(lambda (this)
	   (with-access::JsClass %super (constructor)
	      (constructor this)
	      ,@(j2s-scheme-init-instance-properties
		   clazz mode return ctx)
	      this)))
      (else
       (with-access::J2SFun superctor (params loc)
	  `(lambda (this new-target args)
	      (js-new-target-push! %this new-target)
	      (js-calln-jsprocedure %this %super this args)
	      (js-new-target-pop! %this)
	      ,@(j2s-scheme-init-instance-properties
		   clazz mode return ctx)
	      this)))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-class-put-info! ...                                   */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-class-put-info! this::J2SClass key val)
   (with-access::J2SClass this (%info name)
      (if (pair? %info)
	  (set! %info (cons (cons key val) %info))
	  (set! %info (list (cons key val))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-class-get-info ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-class-get-info this::J2SClass key)
   (with-access::J2SClass this (%info name)
      (when (pair? %info)
	 (let ((o (assq key %info)))
	    (when (pair? o)
	       (cdr o))))))
