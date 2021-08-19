;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-class.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:01:46 2017                          */
;*    Last change :  Thu Aug 19 08:00:05 2021 (serrano)                */
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
	   (j2s-scheme-class-call-super ::J2SCall mode return ctx)))

;*---------------------------------------------------------------------*/
;*    class-constructor-id ...                                         */
;*---------------------------------------------------------------------*/
(define (class-constructor-id::symbol clazz::J2SClass)
   (with-access::J2SClass clazz (name loc)
      (if name
	  (symbol-append '@ name '%CTOR)
	  (string->symbol (format "@~a:~a%CTOR" (cadr loc) (caddr loc))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-call-class-constructor ...                            */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-call-class-constructor clazz::J2SClass ecla eobj args loc mode return ctx)
   (let ((ctor (j2s-class-find-constructor clazz))
	 (args (if (class-new-target? clazz)
		   (cons (J2SHopRef ecla) args)
		   args)))
      `(with-access::JsClass ,ecla (constructor)
	  ,(if ctor
	       (with-access::J2SClassElement ctor (prop)
		  (with-access::J2SMethodPropertyInit prop (val)
		     (let* ((declf (instantiate::J2SDeclFun
				      (loc loc)
				      (id (class-constructor-id clazz))
				      (writable #f)
				      (usage (usage '(call)))
				      (val val)))
			    (call (J2SMethodCall* (J2SRef declf)
				     (list (J2SHopRef eobj))
				     args)))
			;; the call compilation add an extra "@" to
			;; the constructor identifier
			`(let ((,(symbol-append '@ (class-constructor-id clazz))
				constructor))
			    ,(j2s-scheme call mode return ctx)))))
	       ;; default constructor
	       `(let ((,(class-constructor-id clazz) constructor))
		   (constructor ,eobj
		      ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))))))

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
			       (string->symbol (format "%a~a" i)) i)
			  (iota (length args)))))
	     `(let ,(map list names args)
		 (proc names)))))
   
   `(cond
       ((isa? ,ecla JsClass)
	,(let-args (map (lambda (a) (j2s-scheme a mode return ctx)) args)
	    (lambda (args)
	       `(with-access::JsClass ,ecla (constructor)
		  (if (js-function-new-target? ,ecla)
		      (constructor ,eobj new-target ,@args)
		      (constructor ,eobj ,@args))))))
       ((isa? ,ecla JsFunction)
	,(j2s-scheme-call-fun-constructor ecla eobj args loc mode return ctx))
       (else
	(js-raise-type-error/loc %this ',loc
	   "Class extends value \"~a\" is not a constructor or null"
	   ,ecla))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-class-new ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-class-new this::J2SNew clazz args mode return ctx)

   (define (plain-class? clazz)
      ;; a function is a plain class if it only inherits recursively
      ;; from other classes or from user defined functions
      (with-access::J2SClass clazz (super)
	 (cond
	    ((or (isa? super J2SUndefined) (isa? super J2SNull))
	     #t)
	    ((j2s-class-super clazz)
	     =>
	     (lambda (super)
		(cond
		   ((isa? super J2SClass) (plain-class? super))
		   ((isa? super J2SFun) #t)
		   (else #f))))
	    (else
	     #f))))
		   
   (let ((obj (gensym 'this))
	 (cla (gensym 'class)))
      (with-access::J2SClass clazz (decl)
	 (if (plain-class? clazz)
	     (with-access::J2SNew this (loc)
		`(let* ((,cla ,(if decl
				   (j2s-scheme (J2SRef decl) mode return ctx)
				   (j2s-scheme clazz mode return ctx)))
			(,obj (js-object-alloc-fast %this ,cla)))
		    ,(j2s-scheme-call-class-constructor clazz cla obj args loc
			mode return ctx)
		    ,obj))
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
      ((isa? prop J2SMethodPropertyInit)
       (with-access::J2SMethodPropertyInit prop (name val)
	  (unless (j2s-class-property-constructor? prop)
	     `(js-bind! %this ,obj
		 ,(j2s-scheme-class-propname name mode return ctx)
		 :value ,(j2s-scheme val mode return ctx)
		 :writable #t :enumerable #f :configurable #t))))
      ((isa? prop J2SDataPropertyInit)
       (with-access::J2SDataPropertyInit prop (name val)
	  (unless (j2s-class-property-constructor? prop)
	     `(js-bind! %this ,obj
		 ,(j2s-scheme-class-propname name mode return ctx)
		 :value ,(j2s-scheme val mode return ctx)
		 :writable #t :enumerable #f :configurable #t))))
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
		    (or (isa? prop J2SMethodPropertyInit)
			(isa? prop J2SAccessorPropertyInit)))
	    (j2s-scheme-bind-class-method prop obj mode return ctx))))
   
   (define (bind-property clazz obj m)
      (with-access::J2SClassElement m (prop static)
	 (when (not static)
	    (bind-class-property clazz obj prop mode return ctx))))
   
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

   (define (class->lambda clazz::J2SClass arity loc)
      (let ((args (map! (lambda (i)
			   (string->symbol (format "a~s" i)))
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

   (define (class-unknown-super-ctor->lambda clazz::J2SClass loc)
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
   
   (define (make-class this super function constructor aritye length ctorsz src loc)
      (with-access::J2SClass this (name elements cmap)
	 (let* ((clazz (symbol-append name '%CLASS))
		(ctorf (class-constructor-id this))
		(proto (symbol-append name '%PROTOTYPE))
		(alloc (if (or (eq? super #f) (null? super))
			   'js-object-alloc/new-target
			   `(with-access::JsFunction ,super (alloc) alloc)))
		(constrmap (if cmap
			       (j2s-scheme cmap mode return ctx)
			       '(with-access::JsGlobalObject %this (js-initial-cmap)
				  js-initial-cmap))))
	    `(letrec* ((,ctorf ,constructor)
		       (,proto ,(class-prototype this super))
		       (,clazz (js-make-function %this ,function
				  ,aritye
				  (js-function-info
				     :name ,(symbol->string name)
				     :len ,length
				     :new-target ,(class-new-target? this))
				  :strict ',mode
				  :alloc ,alloc
				  :constructor ,ctorf
				  :prototype  ,proto
				  :__proto__ ,(if (null? super)
						  '(with-access::JsGlobalObject %this (js-function-prototype)
						    js-function-prototype)
						  super)
				  :constrsize ,ctorsz
				  :constrmap ,constrmap))
		       ,@(if name `((,(j2s-class-id this ctx) (js-make-let))) '()))
		,@(filter-map (lambda (m) (bind-static this clazz m)) elements)
		,@(filter-map (lambda (m) (bind-prototype this proto m)) elements)
		,@(if name `((set! ,(j2s-class-id this ctx) ,clazz)) '())
		,clazz))))

   (define (let-super super proc)
      (cond
	 ((isa? super J2SUndefined)
	  (proc #f))
	 ((isa? super J2SNull)
	  (proc #f))
	 (else
	  `(let ((%super ,(j2s-scheme super mode return ctx)))
	      ,(proc '%super)))))

   (with-access::J2SClass this (super elements src loc decl constrsize)
      (let ((ctor (j2s-class-get-constructor this)))
	 (let-super super
	    (lambda (%super)
	       (cond
		  ((and ctor (not %super))
		   (with-access::J2SClassElement ctor (prop)
		      (with-access::J2SDataPropertyInit prop (val)
			 (with-access::J2SFun val (params thisp)
			    (make-class this %super
			       (class->lambda this (length params) loc)
			       (ctor->lambda val this mode return ctx %super)
			       (j2s-function-arity val ctx)
			       (length params) constrsize
			       src loc)))))
		  (ctor
		   (with-access::J2SClassElement ctor (prop)
		      (with-access::J2SDataPropertyInit prop (val)
			 (with-access::J2SFun val (params thisp)
			    (make-class this %super
			       (class->lambda this (length params) loc)
			       (ctor->lambda val this mode return ctx %super)
			       (j2s-function-arity val ctx)
			       (length params) constrsize
			       src loc)))))
		  (%super
		   (let ((superctor (j2s-class-find-constructor super)))
		      (if (not superctor)
			  (make-class this %super
			     (class-unknown-super-ctor->lambda this loc)
			     (super-ctor->lambda this superctor mode return ctx)
			     `(with-access::JsFunction ,%super (arity) arity)
			     0 constrsize
			     src loc)
			  (let ((arity (cond
					  ((isa? superctor J2SClassElement)
					   (with-access::J2SClassElement superctor (prop (super clazz))
					      (with-access::J2SMethodPropertyInit prop (val)
						 (with-access::J2SFun val (params)
						    (length params)))))
					  ((isa? superctor J2SFun)
					   (with-access::J2SFun superctor (params)
					      (length params)))
					  (else
					   (error "scheme-class" "wrong superctor" (typeof superctor))))))
			     (make-class this %super
				(class->lambda this arity loc)
				(super-ctor->lambda this superctor mode return ctx)
				`(with-access::JsFunction ,%super (arity) arity)
				arity constrsize
				src loc)))))
		  (else
		   (make-class this %super
		      (class->lambda this 0 loc)
		      `(lambda (this)
			  ,@(j2s-scheme-init-instance-properties
			       this mode return ctx)
			  this)
		      1 0 0 src loc))))))))

;*---------------------------------------------------------------------*/
;*    bind-class-property ...                                          */
;*---------------------------------------------------------------------*/
(define (bind-class-property clazz obj prop mode return ctx)
   (cond
      ((isa? prop J2SMethodPropertyInit)
       #f)
      ((isa? prop J2SDataPropertyInit)
       (with-access::J2SDataPropertyInit prop (name val loc cache)
	  (j2s-put! loc obj name clazz
	     (j2s-scheme-class-propname name mode return ctx) 
	     'any
	     (j2s-scheme val mode return ctx)
	     (j2s-type val)
	     (strict-mode? mode) ctx cache)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    bind-record-property ...                                         */
;*---------------------------------------------------------------------*/
(define (bind-record-property clazz obj prop mode return ctx)
   (cond
      ((isa? prop J2SMethodPropertyInit)
       #f)
      ((isa? prop J2SDataPropertyInit)
       (with-access::J2SDataPropertyInit prop (name val)
	  (let ((expr (j2s-scheme val mode return ctx)))
	     (with-access::J2SClass clazz (super)
		(if (and (isa? super J2SUndefined)
			 (isa? name J2SString))
		    (with-access::J2SString name (val)
		       ;; instance properties of classes with
		       ;; no super class are always inlined
		       (multiple-value-bind (idx el)
			  (j2s-class-instance-get-property clazz val)
			  `(js-object-inline-set! this ,idx ,expr)))
		    `(js-bind! %this ,obj
			,(j2s-scheme-class-propname name mode return ctx)
			:value ,expr
			:writable #t :enumerable #f :configurable #t))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-init-instance-properties ...                          */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-init-instance-properties this::J2SClass mode return ctx)
   (filter-map (lambda (prop)
		  (bind-class-property this 'this
		     prop mode return ctx))
      (j2s-class-instance-properties this :super #f)))

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
			      %superctor this
			      (list ,@(j2s-scheme args mode return ctx)))
			  `(,(string->symbol (format "js-call~a" len))
			    ,j2s-unresolved-call-workspace
			    %superctor this
			    ,@(j2s-scheme args mode return ctx)))))
	    `(begin
		,new-target
		(let ((,tmp ,call))
		   ,@(j2s-scheme-init-instance-properties context
			mode return ctx)
		   ,tmp)))))

   (define (scheme-class-super-declclass this clazz::J2SClass decl)
      (with-access::J2SCall this (args loc)
	 `(begin
	     ,(j2s-scheme-call-class-constructor (j2s-class-super clazz)
		(j2s-scheme (J2SRef decl) mode return ctx)
		'this args loc mode return ctx)
	     ,@(j2s-scheme-init-instance-properties clazz
		  mode return ctx))))
   
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
	 `(begin
	     ,(j2s-scheme-call-expr-constructor '%super
		 'this args loc mode return ctx)
	     ,@(j2s-scheme-init-instance-properties clazz
		  mode return ctx))))
   
   (with-access::J2SCall this (fun)
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
			  (if (isa? val J2SClass)
			      (scheme-class-super-declclass this context decl)
			      (scheme-class-super-expr this context super))))
		      ((isa? decl J2SDeclFun)
		       (scheme-class-super-declfun this context decl))
		      (else
		       (scheme-class-super-expr this context super)))))
	       ((isa? super J2SFun)
		(scheme-class-super-fun this context super))
	       (else
		(scheme-class-super-expr this context super)))))))
   
;*---------------------------------------------------------------------*/
;*    need-super-check? ...                                            */
;*    -------------------------------------------------------------    */
;*    A constructor needs a super check, if it cannot be proved        */
;*    statically that                                                  */
;*      1) it always calls the super constructor                       */
;*      2) the call the super preceeds all "this" accesses             */
;*---------------------------------------------------------------------*/
(define (need-super-check? val::J2SFun)
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
;*    class-new-target? ...                                            */
;*    -------------------------------------------------------------    */
;*    This predicates is #f iff the class constructor DOES not need    */
;*    to bind new-target. It is conservative, in doubt it returns #t.  */
;*---------------------------------------------------------------------*/
(define (class-new-target?::bool this::J2SClass)
   
   (define (expr-new-target? val::J2SExpr)
      (cond
	 ((isa? val J2SClass)
	  (class-new-target? val))
	 ((isa? val J2SFun)
	  (function-new-target? val))
	 ((isa? val J2SParen)
	  (with-access::J2SParen val (expr)
	     (expr-new-target? expr)))
	 (else
	  #t)))
   
   (define (super-new-target? super)
      (cond
	 ((or (isa? super J2SUndefined) (isa? super J2SNull))
	  #f)
	 ((isa? super J2SRef)
	  (with-access::J2SRef super (decl)
	     (if (isa? decl J2SDeclInit)
		 (with-access::J2SDeclInit decl (val)
		    (expr-new-target? val))
		 #t)))
	 (else
	  (expr-new-target? super))))

   (with-access::J2SClass this (super)
      (let ((ctor (j2s-class-get-constructor this)))
	 (if (isa? ctor J2SClassElement)
	     (with-access::J2SClassElement ctor (prop)
		(with-access::J2SMethodPropertyInit prop (val)
		   (or (expr-new-target? val)
		       (super-new-target? super))))
	     (super-new-target? super)))))

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
   
   (with-access::J2SClass clazz (name)
      (with-access::J2SFun ctor (body idthis loc thisp params loc new-target)
	 (with-access::J2SBlock body (loc endloc nodes)
	    (cond
	       ((and (symbol? super) (need-super-check? ctor))
		(when (> (bigloo-warning) 1)
		   (warning/loc loc "Forced super check in constructor"))
		(let* ((thisp-safe (duplicate::J2SDecl thisp (binder 'let-opt)))
		       (decl (J2SLetOpt '(ref) '%nothis (J2SThis thisp-safe))))
		   (with-access::J2SDecl thisp (binder)
		      (set! binder 'let))
		   (with-access::J2SDecl decl (_scmid)
		      (set! _scmid '%nothis))
		   ;; for dead-zone check
		   (decl-usage-add! thisp 'uninit)
		   (set! body
		      (instantiate::J2SLetBlock
			 (loc loc)
			 (endloc endloc)
			 (decls (list decl))
			 (nodes (list (unthis idthis loc)
				   (J2STry
				      (J2SBlock body)
				      (J2SNop)
				      (returnthis thisp loc))))))))
	       ((symbol? super)
		body)
	       (else
		;; no super class initialize the instance properties first
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
	    (set! params (cons (new-target-param loc) params)))))
   
   (jsfun->lambda ctor mode return ctx #f #t))

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
		    (let ((args (map! (lambda (i)
					 (string->symbol (format "a~s" i)))
				   (iota (length params)))))
		       `(lambda (this ,@(if (class-new-target? super)
					    '(new-target)
					    '())
				   ,@args)
			   ,(j2s-scheme-call-class-constructor super
			       '%super 'this
			       (map (lambda (a) (J2SHopRef a)) args)
			       loc mode return ctx)
			   ,@(j2s-scheme-init-instance-properties
				clazz mode return ctx)
			   this)))))))
      ((isa? superctor J2SFun)
       (with-access::J2SFun superctor (params loc)
	  (let ((args (map! (lambda (i)
			       (string->symbol (format "a~s" i)))
			 (iota (length params)))))
	     `(lambda (this ,@(if (function-new-target? superctor)
				  '(new-target)
				  '())
			 ,@args)
		 ,(j2s-scheme-call-fun-constructor 
		     '%super 'this
		     (map (lambda (a) (J2SHopRef a)) args)
		     loc mode return ctx)
		 ,@(j2s-scheme-init-instance-properties
		      clazz mode return ctx)
		 this))))
       (else
	(error "super-ctor->lambda" "wrong super class" superctor))))

;* 	  (with-access::J2SClassElement ctor (prop)                    */
;* 	     (with-access::J2SMethodPropertyInit prop (val)            */
;* 		(with-access::J2SFun val (params)                      */
;* 		   (let ((args (map! (lambda (i)                       */
;* 					(string->symbol (format "a~s" i))) */
;* 				  (iota arity))))                      */
;* 		      `(lambda (this ,@args)                           */
;* 			                                               */
;* 	                                                               */
;* 		 ,(j2s-scheme supcall mode return ctx)                 */
;* 		 ,@(j2s-scheme-init-instance-properties                */
;* 		      this mode return ctx)                            */
;* 		 this))                                                */
;*       (if ctor                                                      */
;* 	  ...                                                          */
;*    (let ((super (j2s-class-super clazz)))                           */
;*       (cond                                                         */
;* 	 ((isa? super                                                  */
;*    (if (class-new-target? clazz)                                    */
;*        `(lambda (this)                                              */
;* 	   (let ((new-target (js-new-target-pop! %this)))              */
;* 	      ,(j2s-scheme supcall mode return ctx)                    */
;* 	      ,@(j2s-scheme-init-instance-properties                   */
;* 		   this mode return ctx)                               */
;* 	      this))                                                   */
;*        `(lambda (this)                                              */
;* 	   ,(j2s-scheme supcall mode return ctx)                       */
;* 	   ,@(j2s-scheme-init-instance-properties                      */
;* 		this mode return ctx)                                  */
;* 	   this)))                                                     */
