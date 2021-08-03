;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-record.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 15 07:09:51 2021                          */
;*    Last change :  Mon Aug  2 19:25:03 2021 (serrano)                */
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
	   __js2scheme_js
	   __js2scheme_stmtassign
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_checksum
	   __js2scheme_scheme
	   __js2scheme_scheme-fun
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant)

   (export (j2s-collect-records*::pair-nil ::J2SProgram)
	   (record-scmid::symbol ::J2SRecord)
	   (record-prototype-scmid::symbol ::J2SRecord)
	   (record-constructor-scmid::symbol ::J2SRecord)
	   (j2s-record-declaration ::J2SRecord)
	   (j2s-record-predicate ::J2SRecord)
	   (j2s-record-new ::J2SRecord ::pair-nil mode return ctx)
	   (j2s-record-prototype-constructor::pair this::J2SRecord)))

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
      (let ((super (j2s-class-super this)))
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
   
   (define (record-size clazz)
      (let loop ((clazz clazz)
		 (s 0))
	 (with-access::J2SRecord clazz (elements)
	    (let ((super (j2s-class-super clazz)))
	       (if super
		   (loop super (+fx (length elements) s))
		   (with-access::J2SRecord clazz (elements)
		      (+fx s (length elements))))))))
   
   (define (constructor this rec)
      (multiple-value-bind (clazz ctor)
	 (j2s-class-find-constructor this)
	 (if ctor
	     (let ((c (record-constructor-scmid clazz)))
		`(,c ,rec
		    ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))
	     #unspecified)))
   
   (with-access::J2SRecord this (cmap elements)
      (let ((rec (gensym 'this))
	    (props (j2s-class-instance-properties this)))
	 `(let ((,rec (js-make-jsrecord ,(record-size this)
			 ,(j2s-scheme cmap mode return ctx)
			 ,(record-prototype-scmid this)
			 ,(record-scmid this))))
	     ,@(map (lambda (prop idx)
		       (with-access::J2SDataPropertyInit prop (val)
			  `(js-object-inline-set! ,rec ,idx
			      ,(j2s-scheme val mode return ctx))))
		  props (iota (length props)))
	     ,(constructor this rec)
	     ,rec))))

;*---------------------------------------------------------------------*/
;*    j2s-record-prototype-constructor ...                             */
;*---------------------------------------------------------------------*/
(define (j2s-record-prototype-constructor this::J2SRecord)
   (let ((super (j2s-class-super this)))
      `((define ,(record-prototype-scmid this) (js-undefined))
	(define ,(record-constructor-scmid this) (js-undefined)))))
       
