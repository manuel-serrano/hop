;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-class.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:01:46 2017                          */
;*    Last change :  Sat Sep 16 06:45:10 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    ES2015 Scheme class generation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-class

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-utils)

   (export (j2s-scheme-super ::J2SCall mode return conf hint totype)))

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SDeclClass ...                                    */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SDeclClass mode return conf hint totype)
   "declclass not implemented yet")

;*---------------------------------------------------------------------*/
;*    j2s-scheme ::J2SClass ...                                        */
;*---------------------------------------------------------------------*/
(define-method (j2s-scheme this::J2SClass mode return conf hint totype)
   
   (define (constructor? prop::J2SDataPropertyInit)
      (with-access::J2SDataPropertyInit prop (name)
	 (when (isa? name J2SLiteralValue)
	    (with-access::J2SLiteralValue name (val)
	       (equal? val "constructor")))))
   
   (define (find-constructor elements)
      (find (lambda (m)
	       (with-access::J2SClassElement m (prop static)
		  (unless static
		     (when (isa? prop J2SDataPropertyInit)
			(constructor? prop)))))
	 elements))

   (define (j2s-propname name)
      (cond
	 ((isa? name J2SString)
	  (with-access::J2SString name (val)
	     (let ((str (string-for-read val)))
		(if (string=? str val)
		    `(quote ,(string->symbol val))
		    `(string->symbol ,val)))))
	 ((isa? name J2SNumber)
	  (with-access::J2SNumber name (val)
	     (if (fixnum? val)
		 `(quote ,(string->symbol (number->string val)))
		 `(js-toname ,(j2s-scheme val mode return conf hint totype) %this))))
	 ((isa? name J2SPragma)
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))
	 ((isa? name J2SLiteralCnst)
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))
	 ((isa? name J2SLiteralValue)
	  (with-access::J2SLiteralValue name (val)
	     `(js-toname ,(j2s-scheme val mode return conf hint totype) %this)))
	 (else
	  `(js-toname ,(j2s-scheme name mode return conf hint totype) %this))))

   (define (bind-class-method obj prop)
      (cond
	 ((isa? prop J2SDataPropertyInit)
	  (with-access::J2SDataPropertyInit prop (name val)
	     (unless (constructor? prop)
		`(js-bind! %this ,obj ,(j2s-propname name)
		    :value ,(j2s-scheme val mode return conf hint totype)
		    :writable #t :enumerable #f :configurable #t))))
	 ((isa? prop J2SAccessorPropertyInit)
	  (with-access::J2SAccessorPropertyInit prop (name get set)
	     `(js-bind! %this ,obj ,(j2s-propname name)
		 :get ,(when get
			  (j2s-scheme get mode return conf hint totype))
		 :set ,(when set
			  (j2s-scheme set mode return conf hint totype))
		 :writable #t :enumerable #f :configurable #t)))
	 (else
	  #f)))
   
   (define (bind-static clazz m)
      (with-access::J2SClassElement m (prop static)
	 (when static
	    (bind-class-method clazz prop))))
   
   (define (bind-method proto m)
      (with-access::J2SClassElement m (prop static)
	 (when (not static)
	    (bind-class-method proto prop))))
   
   (define (let-super super proc)
      (cond
	 ((isa? super J2SUndefined)
	  (proc #f))
	 ((isa? super J2SNull)
	  (proc '()))
	 (else
	  (let ((superid (gensym 'super)))
	     `(let* ((,superid ,(j2s-scheme super mode return conf hint totype))
		     (%super (js-get ,superid 'prototype %this))
		     (%superctor (js-get %super 'constructor %this)))
		 ,(proc superid))))))
   
   (define (make-class name super els constructor length ctorsz src loc)
      (let ((ctor (gensym 'ctor))
	    (clazz (gensym 'clazz))
	    (proto (gensym 'prototype)))
	 `(letrec* ((,ctor ,constructor)
		    (,proto ,(cond
				((eq? super #f)
				 `(with-access::JsGlobalObject %this (js-object)
				     (js-new0 %this js-object)))
				((null? super)
				 `(with-access::JsGlobalObject %this (js-object)
				     (let ((o (js-new0 %this js-object)))
					(with-access::JsObject o (__proto__)
					   (set! __proto__ '())
					   o))))
				(else
				 `(js-new0 %this ,super))))
		    (,clazz (js-make-function %this
			       ,ctor
			       ,length
			       ,(symbol->string! (or name 'toto))
			       :src ,(when src (class-src loc this conf))
			       :strict ',mode
			       :alloc (lambda (o) (js-instance-alloc o %this))
			       :construct ,ctor
			       :prototype  ,proto
			       :__proto__ ,(if (null? super)
					       '(with-access::JsGlobalObject %this (js-function-prototype)
						 js-function-prototype)
					       super)
			       :constrsize ,ctorsz))
		    ,@(if name `((,(j2s-fast-id name) (js-make-let))) '()))
	     ,@(filter-map (lambda (m) (bind-static clazz m)) els)
	     ,@(filter-map (lambda (m) (bind-method proto m)) els)
	     ,@(if name `((set! ,(j2s-fast-id name) ,clazz)) '())
	     ,clazz)))
   
   (with-access::J2SClass this (super elements name src loc decl)
      (let ((ctor (find-constructor elements)))
	 (when decl
	    (with-access::J2SDecl decl (_scmid)
	       (set! _scmid (j2s-fast-id name))))
	 (let-super super
	    (lambda (super)
	       (if ctor
		   (with-access::J2SClassElement ctor (prop)
		      (with-access::J2SDataPropertyInit prop (val)
			 (with-access::J2SFun val (constrsize params thisp)
			    (when super
			       (with-access::J2SDecl thisp (binder)
				  (set! binder 'let)))
			    (make-class name super elements
			       (ctor->lambda val mode return conf #f #t super)
			       (length params) constrsize
			       src loc))))
		   (make-class name super elements
		      '(lambda (this)
			(let ((%nothis (js-check-class-instance this ',loc %this)))
			   %nothis))
		      0 0 src loc)))))))

;*---------------------------------------------------------------------*/
;*    ctor->lambda ...                                                 */
;*---------------------------------------------------------------------*/
(define (ctor->lambda val::J2SFun mode return conf proto ctor-only super)

   (define (check-instance this loc)
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr (instantiate::J2SPragma
		  (loc loc)
		  (expr `(js-check-class-instance ,this ',loc %this))))))

   (define (unthis this loc)
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr (instantiate::J2SPragma
		  (loc loc)
		  (expr `(set! ,this (js-make-let)))))))

   (define (returnthis this loc)
      (J2SRef this))
   
   (with-access::J2SFun val (body idthis loc thisp)
      (with-access::J2SBlock body (loc endloc)
	 (let* ((thisp-safe (duplicate::J2SDecl thisp (binder 'let-opt)))
		(decl (J2SLetOpt '(ref) '%nothis (J2SThis thisp-safe))))
	    (with-access::J2SDecl decl (_scmid)
	       (set! _scmid '%nothis))
	    (set! body
	       (instantiate::J2SLetBlock
		  (loc loc)
		  (endloc endloc)
		  (decls (list decl))
		  (nodes (if (symbol? super)
			     (list (unthis idthis loc)
				body
				(returnthis thisp loc))
			     (list body (returnthis thisp loc))))))))
      (jsfun->lambda val mode return conf proto ctor-only)))

;*---------------------------------------------------------------------*/
;*    class-src ...                                                    */
;*---------------------------------------------------------------------*/
(define (class-src loc val::J2SClass conf)
   (with-access::J2SClass val (src loc endloc)
      (when src
	 (match-case loc
	    ((at ?path ?start)
	     (let ((m (config-get conf :mmap-src)))
		`'(,loc . ,(when (mmap? m)
			      (match-case endloc
				 ((at ?- ?end)
				  (mmap-substring m
				     (fixnum->elong start)
				     (+elong 1 (fixnum->elong end)))))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-scheme-super ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-scheme-super this::J2SCall mode return conf hint totype)
   (with-access::J2SCall this (loc fun this args protocol cache)
      (let* ((len (length args))
	     (call (if (>=fx len 11)
		       'js-calln
		       (string->symbol (format "js-call~a" len))))
	     (ctor (gensym 'ctor)))
	 `(with-access::JsObject %nothis (__proto__)
	     (set! this
		(,call ,j2s-unresolved-call-workspace
		   %superctor
		   %nothis
		   ,@(j2s-scheme args mode return conf hint totype)))
	     this))))
   
