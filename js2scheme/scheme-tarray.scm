;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-tarray.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Fri Sep 25 07:53:30 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript TypedArray functions.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-tarray

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
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-spread
	   __js2scheme_scheme-cast)

   (export (j2s-new-tarray ::J2SNew mode return conf)
	   (j2s-tarray-set! ::J2SAssig mode return conf)))

;*---------------------------------------------------------------------*/
;*    j2s-new-tarray ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-new-tarray this::J2SNew mode return ctx)
   
   (define (j2s-array-new type)
      (symbol-append 'js-new- type))
   
   (define (uncast a)
      (if (isa? a J2SCast)
	  (with-access::J2SCast a (expr type)
	     (if (eq? type 'any)
		 expr
		 a))
	  a))

   (define (id-to-type id)
      (case id
	 ((Int8Array) 'int8array)
	 ((Uint8Array) 'uint8array)
	 (else (error "new-array-type" "Unknown class" id))))
   
   (define (new-array-type clazz)
      (cond
	 ((isa? clazz J2SRef)
	  (with-access::J2SRef clazz (decl)
	     (with-access::J2SDecl decl (id)
		(id-to-type id))))
	 ((isa? clazz J2SGlobalRef)
	  (with-access::J2SGlobalRef clazz (id)
	     (id-to-type id)))
	 (else
	  (error "new-array-type" "Unknown class" (j2s->list clazz)))))
   
   (with-access::J2SNew this (loc clazz args type)
      (let ((a (uncast (car args)))
	    (t (new-array-type clazz)))
	 `(js-new1 %this ,(j2s-scheme clazz mode return ctx)
	     ,(j2s-scheme a mode return ctx)))))
;* 	 (case (j2s-type a)                                            */
;* 	    ((int32)                                                   */
;* 	     `(,(j2s-array-new t)                                      */
;* 	       (int32->uint32 ,(j2s-scheme a mode return ctx)) %this)) */
;* 	    ((uint32)                                                  */
;* 	     `(,(j2s-array-new t)                                      */
;* 	       ,(j2s-scheme a mode return ctx) %this))                 */
;* 	    ((memq t '(integer bint int53))                            */
;* 	     `(,(j2s-array-new t)                                      */
;* 	       (fixnum->uint32 ,(j2s-scheme a mode return ctx)) %this)) */
;* 	    (else                                                      */
;* 	     `(,(j2s-array-new t)                                      */
;* 	       (js-touint32 ,(j2s-scheme a mode return ctx) %this) %this)))))) */

;*---------------------------------------------------------------------*/
;*    j2s-tarray-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-tarray-set! this::J2SAssig mode return ctx)
   
   (define (j2s-typedarray-set! obj itype)
      (with-access::J2SExpr obj (type)
	 (symbol-append 'js- type '- itype '-set!)))

   (define (j2s-scheme-typedarray-cast obj rhs mode return ctx)
      (with-access::J2SExpr obj (type)
	 (let ((se (j2s-scheme rhs mode return ctx)))
	    (case type
	       ((int8array)
		(j2s-cast se rhs (j2s-type rhs) 'int8 ctx))
	       ((uint8array)
		(j2s-cast se rhs (j2s-type rhs) 'uint8 ctx))
	       (else
		se)))))
      
   (with-access::J2SAssig this (lhs rhs)
      ;; an optimized array set in a loop (see array.scm)
      (with-access::J2SAccess lhs (obj field)
	 (case (j2s-vtype field)
;* 	    ((uint32)                                                  */
;* 	     `(,(j2s-typedarray-set! obj 'index)                       */
;* 	       ,(j2s-scheme obj mode return ctx)                       */
;* 	       ,(j2s-scheme field mode return ctx)                     */
;* 	       ,(j2s-scheme-typedarray-cast obj rhs mode return ctx))) */
;* 	    ((int32)                                                   */
;* 	     `(,(j2s-typedarray-set! obj 'fixnum)                      */
;* 	       ,(j2s-scheme obj mode return ctx)                       */
;* 	       (int32->fixnum ,(j2s-scheme field mode return ctx))     */
;* 	       ,(j2s-scheme-typedarray-cast obj rhs mode return ctx))) */
;* 	    ((fixnum int53)                                            */
;* 	     `(,(j2s-typedarray-set! obj 'fixnum)                      */
;* 	       ,(j2s-scheme obj mode return ctx)                       */
;* 	       ,(j2s-scheme field mode return ctx)                     */
;* 	       ,(j2s-scheme-typedarray-cast obj rhs mode return ctx))) */
	    (else
	     (case (j2s-type field)
;* 		((fixnum int53)                                        */
;* 		 `(,(j2s-typedarray-set! obj 'fixnum)                  */
;* 		   ,(j2s-scheme obj mode return ctx)                   */
;* 		   ,(j2s-scheme field mode return ctx)                 */
;* 		   ,(j2s-scheme-typedarray-cast obj rhs mode return ctx))) */
		(else
		 `(js-put! ,(j2s-scheme obj mode return ctx)
		     ,(j2s-scheme field mode return ctx)
		     ,(j2s-scheme rhs mode return ctx)
		     ,(strict-mode? mode)
		     %this))))))))
