;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-arguments.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Sat Jul 22 06:33:51 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript arguments functions.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-arguments

   (include "ast.sch" "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_array
	   __js2scheme_usage
	   __js2scheme_scheme
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)

   (export (j2s-rest-ref ::J2SAccess mode return ::struct)
	   (j2s-arguments-ref ::J2SAccess mode return ::struct)
	   (j2s-arguments-set! ::J2SAssig mode return ::struct)
	   (j2s-ref-arguments-lazy?::bool ::J2SRef)
	   (j2s-ref-arguments-stack?::bool ::J2SRef)
	   (j2s-ref-arguments-argid::symbol ::J2SRef)
	   (j2s-ref-arguments-mode::symbol ::J2SRef)))

;*---------------------------------------------------------------------*/
;*    j2s-rest-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-rest-ref this::J2SAccess mode return ctx)
   (with-access::J2SAccess this (obj field type)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (when (eq? (object-class decl) J2SDeclRest)
	       (with-access::J2SDeclRest decl (alloc-policy)
		  (cond
		     ((not (eq? alloc-policy 'lazy)) #f)
		     ((maybe-number? field)
		      (cond
			 ((eq? (j2s-type field) 'uint32)
			  `(js-rest-vector-index-ref 
			      ,(j2s-scheme obj mode return ctx)
			      ,(j2s-scheme field mode return ctx)))
			 ((eq? (j2s-type field) 'int32)
			  `(js-rest-vector-ref 
			      ,(j2s-scheme obj mode return ctx)
			      (int32->fixnum ,(j2s-scheme field mode return ctx))))
			 (else
			  `(js-rest-ref
			      ,(j2s-scheme obj mode return ctx)
			      ,(j2s-scheme field mode return ctx)))))
		     ((j2s-field-length? field)
		      `(vector-length ,(j2s-scheme obj mode return ctx)))
		     (else
		      #f))))))))

;*---------------------------------------------------------------------*/
;*    j2s-arguments-ref ...                                            */
;*---------------------------------------------------------------------*/
(define (j2s-arguments-ref this::J2SAccess mode return ctx)
   
   (define (argument-lonly? this::J2SExpr)
      (when (isa? this J2SRef)
	 (with-access::J2SRef this (decl)
	    (when (isa? decl J2SDeclArguments)
	       (with-access::J2SDeclArguments decl (fun)
		  (fun-lonly-vararg? fun))))))
   
   (define (arguments-stack? this::J2SExpr)
      (when (isa? this J2SRef)
	 (with-access::J2SRef this (decl)
	    (when (isa? decl J2SDeclArguments)
	       (with-access::J2SDeclArguments decl (fun)
		  (fun-stack-vararg? fun))))))
   
   (define (arguments-lazy? this::J2SExpr)
      (when (isa? this J2SRef)
	 (with-access::J2SRef this (decl)
	    (when (isa? decl J2SDeclArguments)
	       (with-access::J2SDeclArguments decl (fun)
		  (fun-lazy-vararg? fun))))))
   
   (define (js-uint32->fixnum n)
      (if (uint32? n)
	  (uint32->fixnum n)
	  `(uint32->fixnum ,n)))
   
   (define (js-int32->fixnum n)
      (if (int32? n)
	  (int32->fixnum n)
	  `(int32->fixnum ,n)))
   
   (define (ref-usage-strict? this::J2SRef usages)
      (with-access::J2SRef this (decl)
	 (decl-usage-strict? decl usages)))
   
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((maybe-number? field)
	  (cond
	     ((eq? (j2s-type field) 'uint32)
	      (cond
		 ((arguments-stack? obj)
		  `(vector-ref
		      ,(j2s-scheme obj mode return ctx)
		      ,(js-uint32->fixnum (j2s-scheme field mode return ctx))))
		 ((arguments-lazy? obj)
		  `(js-arguments-vector-index-ref
		      ,(j2s-arguments-stack-id)
		      ,(js-uint32->fixnum (j2s-scheme field mode return ctx))
		      ,(j2s-arguments-length-id)
		      ,(j2s-arguments-object-id)
		      %this
		      ',mode))
		 (else
		  `(js-arguments-index-ref ,(j2s-arguments-object-id)
		      ,(j2s-scheme field mode return ctx)
		      %this))))
	     ((eq? (j2s-type field) 'int32)
	      (cond
		 ((arguments-stack? obj)
		  `(vector-ref
		      ,(j2s-scheme obj mode return ctx)
		      ,(js-int32->fixnum (j2s-scheme field mode return ctx))))
		 ((arguments-lazy? obj)
		  `(,(if (inrange-positive? field)
			 'js-arguments-vector-index-ref
			 'js-arguments-vector-ref)
		      ,(j2s-arguments-stack-id)
		      ,(js-int32->fixnum (j2s-scheme field mode return ctx))
		      ,(j2s-arguments-length-id)
		      ,(j2s-arguments-object-id)
		      %this
		      ',mode))
		 (else
		  (let ((argid (j2s-ref-arguments-argid obj)))
		     `(js-arguments-index-ref ,argid
			 ,(j2s-arguments-object-id)
			 ,(js-int32->fixnum (j2s-scheme field mode return ctx))
			 %this)))))
	     ((arguments-stack? obj)
	      `(vector-ref
		  ,(j2s-scheme obj mode return ctx)
		  ,(j2s-scheme field mode return ctx)))
	     ((arguments-lazy? obj)
	      `(,(if (inrange-positive? field)
		     'js-arguments-vector-index-ref
		     'js-arguments-vector-ref)
		  ,(j2s-arguments-stack-id)
		  ,(j2s-scheme field mode return ctx)
		  ,(j2s-arguments-length-id)
		  ,(j2s-arguments-object-id)
		  %this
		  ',mode))
	     (else
	      `(js-arguments-ref ,(j2s-scheme obj mode return ctx)
		  ,(j2s-scheme field mode return ctx)
		  %this))))
	 ((j2s-field-length? field)
	  (cond
	     ((argument-lonly? obj)
	      ;; see scheme-fun.scm
	      (j2s-arguments-length-id))
	     ((arguments-stack? obj)
	      ;; see scheme-fun.scm
	      (j2s-arguments-length-id))
	     ((arguments-lazy? obj)
	      ;; see scheme-fun.scm
	      `(if ,(j2s-arguments-object-id)
		   (js-arguments-length ,(j2s-arguments-object-id) %this)
		   ,(j2s-arguments-length-id)))
	     (else
	      `(js-arguments-length
		  ,(j2s-scheme obj mode return ctx) %this))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-arguments-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-arguments-set! this::J2SAssig mode return ctx)
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field cache cspecs loc)
	 (if (context-get ctx :optim-arguments)
	     (if (eq? (j2s-type field) 'uint32)
		 (if (and (isa? obj J2SRef)
			  (j2s-ref-arguments-lazy-optimized? obj))
		     (let ((argid (j2s-ref-arguments-argid obj)))
			`(js-arguments-vector-index-set! ,argid
			    ,(j2s-scheme obj mode return ctx)
			    ,(j2s-scheme field mode return ctx)
			    ,(j2s-scheme rhs mode return ctx)
			    %this))
		     `(js-arguments-index-set! ,(j2s-scheme obj mode return ctx)
			 ,(j2s-scheme field mode return ctx)
			 ,(j2s-scheme rhs mode return ctx)
			 %this))
		 (j2s-put! loc (j2s-scheme obj mode return ctx) field
		    (typeof-this obj ctx)
		    (j2s-scheme field mode return ctx)
		    (j2s-type field)
		    (j2s-scheme rhs mode return ctx)
		    (j2s-type rhs)
		    (strict-mode? mode)
		    ctx
		    cache
		    :optim #f
		    :cspecs cspecs
		    :cachefun #f))
	     (j2s-put! loc (j2s-scheme obj mode return ctx) field
		(typeof-this obj ctx)
		(j2s-scheme field mode return ctx)
		(j2s-type field)
		(j2s-scheme rhs mode return ctx)
		(j2s-type rhs)
		(strict-mode? mode)
		ctx
		cache
		:optim #f
		:cspecs cspecs
		:cachefun #f)))))

;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-lazy? ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-lazy? obj)
   (with-access::J2SRef obj (decl)
      (cond
	 ((isa? decl J2SDeclArguments)
	  (with-access::J2SDeclArguments decl (alloc-policy)
	     (eq? alloc-policy 'lazy)))
	 ((isa? decl J2SDeclInit)
	  (with-access::J2SDeclInit decl (val)
	     (when (isa? val J2SRef)
		(j2s-ref-arguments-lazy? val))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-stack? ...                                     */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-stack? obj)
   (with-access::J2SRef obj (decl)
      (cond
	 ((isa? decl J2SDeclArguments)
	  (with-access::J2SDeclArguments decl (alloc-policy)
	     (memq alloc-policy '(stack lazy))))
	 ((isa? decl J2SDeclInit)
	  (with-access::J2SDeclInit decl (val)
	     (when (isa? val J2SRef)
		(j2s-ref-arguments-lazy? val))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-lazy-optimized? ...                            */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-lazy-optimized? obj)
   (when (j2s-ref-arguments-lazy? obj)
      (with-access::J2SRef obj (decl)
	 (with-access::J2SRef obj (decl)
	    (not (decl-usage-has? decl '(ref set get)))))))
      
;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-argid ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-argid obj::J2SRef)
   (with-access::J2SRef obj (decl)
      (if (isa? decl J2SDeclArguments)
	  (with-access::J2SDeclArguments decl (argid)
	     argid)
	  (with-access::J2SDeclInit decl (val)
	     (j2s-ref-arguments-argid val)))))

;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-mode ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-mode obj::J2SRef)
   (with-access::J2SRef obj (decl)
      (with-access::J2SDeclArguments decl (mode)
	 (if (isa? decl J2SDeclArguments)
	     mode
	     (with-access::J2SDeclInit decl (val)
		(j2s-ref-arguments-mode val))))))
