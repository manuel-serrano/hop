;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-class.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 21 07:01:46 2017                          */
;*    Last change :  Wed Aug 23 06:24:56 2017 (serrano)                */
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
	   __js2scheme_scheme-fun))

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
   
   (define (find-constructor methods)
      (find (lambda (m)
	       (with-access::J2SClassElement m (prop static)
		  (unless static
		     (when (isa? prop J2SDataPropertyInit)
			(constructor? prop)))))
	 methods))
   
   (define (bind-class-method obj prop)
      (cond
	 ((isa? prop J2SDataPropertyInit)
	  (with-access::J2SDataPropertyInit prop (name val)
	     `(js-bind! %this ,obj
		 (js-toname
		    ,(j2s-scheme name mode return conf hint totype)
		    %this)
		 :value ,(j2s-scheme val mode return conf hint totype)
		 :writable #t :enumerable #f :configurable #t)))
	 ((isa? prop J2SAccessorPropertyInit)
	  (with-access::J2SAccessorPropertyInit prop (name get set)
	     `(js-bind! %this ,obj
		 (js-toname
		    ,(j2s-scheme name mode return
			conf hint totype)
		    %this)
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
   
   (define (make-class name scmext methods constructor length ctorsz src loc)
      (let ((ctor (gensym 'ctor))
	    (clazz (gensym 'clazz))
	    (proto (gensym 'prototype)))
	 `(letrec* ((,ctor ,constructor)
		    (,proto (with-access::JsGlobalObject %this (js-object)
			       (js-new0 %this js-object)))
		    (,clazz (js-make-function %this
			       ,ctor
			       ,length
			       ,(symbol->string! (or name 'toto))
			       :src ,(when src (class-src loc this conf))
			       :strict ',mode
			       :alloc (lambda (o) (js-instance-alloc o %this))
			       :construct ,ctor
			       :__proto__ ,scmext
			       :prototype  ,proto
			       :constrsize ,ctorsz)))
	     ,@(filter-map (lambda (m) (bind-static clazz m))
		  methods)
	     ,@(filter-map (lambda (m) (bind-method proto m))
		  methods)
	     ,clazz)))
   
   (with-access::J2SClass this (extends methods name src loc)
      (let ((ctor (find-constructor methods))
	    (scmext (when extends
			   (j2s-scheme extends mode return conf hint totype))))
	 (if ctor
	     (with-access::J2SClassElement ctor (prop)
		(with-access::J2SDataPropertyInit prop (val)
		   (with-access::J2SFun val (constrsize params)
		      (make-class name scmext methods
			 (jsmethod->lambda val mode return conf #f #t)
			 (length params) constrsize
			 src loc))))
	     (make-class name scmext methods
		'(lambda (this) this) 0 0 src loc)))))

;*---------------------------------------------------------------------*/
;*    jsmethod->lambda ...                                             */
;*---------------------------------------------------------------------*/
(define (jsmethod->lambda val::J2SFun mode return conf proto ctor-only)

   (define (check-instance this loc)
      (instantiate::J2SStmtExpr
	 (loc loc)
	 (expr (instantiate::J2SPragma
		  (loc loc)
		  (expr `(js-check-class-instance ,this ',loc %this))))))
   
   (with-access::J2SFun val (body idthis loc)
      (set! body
	 (duplicate::J2SBlock body
	    (nodes (list (check-instance idthis loc) body))))
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

