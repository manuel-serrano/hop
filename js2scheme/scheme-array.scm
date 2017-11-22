;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/scheme-array.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Wed Nov 22 16:21:54 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Array functions.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-array

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_scheme
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-fun)

   (export (j2s-new-array ::J2SNew mode return conf hint totype)
	   (j2s-vector-ref ::J2SAccess mode return conf hint totype)
	   (j2s-vector-set! ::J2SAssig mode return conf hint totype)))

;*---------------------------------------------------------------------*/
;*    j2s-new-array ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-new-array this::J2SNew mode return conf hint totype)

   (define (smaller-than? o k)
      (when (isa? o J2SNumber)
	 (with-access::J2SNumber o (val)
	    (and (fixnum? val) (>=fx val 0) (<fx val k)))))

   (with-access::J2SNew this (loc cache clazz args type)
      (cond
	 ((null? args)
	  '(js-empty-vector->jsarray %this))
	 ((eq? type 'vector)
	  `(make-vector
	      ,(j2s-scheme (car args) mode return conf hint totype)
	      (js-undefined)))
	 ((and (is-integer? (car args)) (null? (cdr args)))
	  (if (smaller-than? (car args) 16)
	      `(js-array-construct-alloc-small %this 
		  ,(j2s-scheme (car args) mode return conf hint totype))
	      `(js-array-construct/length %this (js-array-alloc %this)
		  ,(j2s-scheme (car args) mode return conf hint totype))))
	 ((null? (cdr args))
	  `(js-array-construct %this (js-array-alloc %this)
	      (list ,@(j2s-scheme args mode return conf hint totype))))
	 (else
	  `(js-vector->jsarray
	      (vector ,@(j2s-scheme args mode return conf hint totype))
	      %this)))))

;*---------------------------------------------------------------------*/
;*    j2s-vector-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-vector-ref this::J2SAccess mode return conf hint totype)
   (with-access::J2SAccess this (obj field)
      (with-access::J2SRef obj (decl)
	 (with-access::J2SDecl decl (hint)
	    (with-access::J2SExpr field (range)
	       (cond
		  ((and (>= (interval-min range) hint)
			(< (interval-max range) hint))
		   `(vector-ref ,(j2s-scheme obj mode return conf hint totype)
		       ,(j2s-scheme field mode return conf hint totype)))
		  ((eq? (j2s-type field) 'index)
		   (let ((i (gensym 'idx)))
		      `(let ((,i ,(j2s-scheme field mode return conf hint totype)))
			  (if (<fx ,i ,hint)
			      (vector-ref
				 ,(j2s-scheme obj mode return conf hint totype) ,i)
			      (js-undefined)))))
		  (else
		   (let ((i (gensym 'idx)))
		      `(let ((,i ,(j2s-scheme field mode return conf hint totype)))
			  (if (and (<fx ,i ,hint) (>=fx ,i 0))
			      (vector-ref
				 ,(j2s-scheme obj mode return conf hint totype) ,i)
			      (js-undefined)))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-vector-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-vector-set! this::J2SAssig mode return conf hint totype)
   (with-access::J2SAssig this (lhs rhs)
      (with-access::J2SAccess lhs (obj field)
	 `(vector-set! ,(j2s-scheme obj mode return conf hint totype)
	     ,(j2s-scheme field mode return conf hint totype)
	     ,(j2s-scheme rhs mode return conf hint totype)))))
	     
