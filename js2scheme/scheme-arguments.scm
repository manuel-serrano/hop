;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-arguments.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Thu Apr 16 18:30:46 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
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
	   __js2scheme_scheme
	   __js2scheme_scheme-cast
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)

   (export (j2s-rest-ref ::J2SAccess mode return ::struct)
	   (j2s-arguments-ref ::J2SAccess mode return ::struct)
	   (j2s-arguments-set! ::J2SAssig mode return ::struct)
	   (j2s-ref-arguments-lazy?::bool ::J2SRef)
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
			 ((eq? (j2s-vtype field) 'uint32)
			  `(js-rest-vector-index-ref 
			      ,(j2s-scheme obj mode return ctx)
			      ,(j2s-scheme field mode return ctx)))
			 ((eq? (j2s-vtype field) 'int32)
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
   (with-access::J2SAccess this (obj field type)
      (cond
	 ((maybe-number? field)
	  (cond
	     ((eq? (j2s-vtype field) 'uint32)
	      (if (and (isa? obj J2SRef) (j2s-ref-arguments-lazy? obj))
		  (let ((argid (j2s-ref-arguments-argid obj)))
		     `(js-arguments-vector-index-ref ,argid
		       ,(j2s-scheme obj mode return ctx)
		       ,(j2s-scheme field mode return ctx)
		       %this))
		  `(js-arguments-index-ref ,(j2s-scheme obj mode return ctx)
		      ,(j2s-scheme field mode return ctx)
		      %this)))
	     ((eq? (j2s-vtype field) 'int32)
	      (let ((argid (j2s-ref-arguments-argid obj)))
		 `(js-arguments-vector-ref ,argid
		   ,(j2s-scheme obj mode return ctx)
		   (int32->fixnum ,(j2s-scheme field mode return ctx))
		   %this)))
	     ((and (isa? obj J2SRef) (j2s-ref-arguments-lazy? obj))
	      (let ((argid (j2s-ref-arguments-argid obj)))
		 `(js-arguments-vector-ref ,argid
		   ,(j2s-scheme obj mode return ctx)
		   ,(j2s-scheme field mode return ctx)
		   %this)))
	     (else
	      `(js-arguments-ref ,(j2s-scheme obj mode return ctx)
		  ,(j2s-scheme field mode return ctx)
		  %this))))
	 ((j2s-field-length? field)
	  (if (and (isa? obj J2SRef) (j2s-ref-arguments-lazy? obj))
	      `(if (object? ,(j2s-scheme obj mode return ctx))
		   (js-arguments-length
		      ,(j2s-scheme obj mode return ctx) %this)
		   (vector-length ,(j2s-ref-arguments-argid obj)))
	      `(js-arguments-length
		  ,(j2s-scheme obj mode return ctx) %this)))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-arguments-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (j2s-arguments-set! this::J2SAssig mode return ctx)
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field cache cspecs loc)
	 (if (context-get ctx :optim-arguments)
	     (if (eq? (j2s-vtype field) 'uint32)
		 (if (and (isa? obj J2SRef) (j2s-ref-arguments-lazy? obj))
		     (let ((argid (j2s-ref-arguments-argid obj)))
			`(js-arguments-vector-index-set! ,argid
			    ,(j2s-scheme obj mode return ctx)
			    ,(j2s-scheme field mode return ctx)
			    ,(j2s-scheme rhs mode return ctx)
			    %this))
		     `(js-arguments-index-ref ,(j2s-scheme obj mode return ctx)
			 ,(j2s-scheme field mode return ctx)
			 ,(j2s-scheme rhs mode return ctx)
			 %this))
		 (j2s-put! loc (j2s-scheme obj mode return ctx) field
		    (typeof-this obj ctx)
		    (j2s-scheme field mode return ctx)
		    (j2s-vtype field)
		    (j2s-scheme rhs mode return ctx)
		    (j2s-vtype rhs)
		    (strict-mode? mode)
		    ctx
		    cache
		    #f
		    :cspecs cspecs
		    :cachefun #f))
	     (j2s-put! loc (j2s-scheme obj mode return ctx) field
		(typeof-this obj ctx)
		(j2s-scheme field mode return ctx)
		(j2s-vtype field)
		(j2s-scheme rhs mode return ctx)
		(j2s-vtype rhs)
		(strict-mode? mode)
		ctx
		cache
		#f
		:cspecs cspecs
		:cachefun #f)))))

;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-lazy? ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-lazy? obj)
   (with-access::J2SRef obj (decl)
      (when (isa? decl J2SDeclArguments)
	  (with-access::J2SDeclArguments decl (alloc-policy)
	     (eq? alloc-policy 'lazy)))))
		 
;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-argid ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-argid obj::J2SRef)
   (with-access::J2SRef obj (decl)
      (with-access::J2SDeclArguments decl (argid)
	 argid)))

;*---------------------------------------------------------------------*/
;*    j2s-ref-arguments-mode ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-ref-arguments-mode obj::J2SRef)
   (with-access::J2SRef obj (decl)
      (with-access::J2SDeclArguments decl (mode)
	 mode)))
