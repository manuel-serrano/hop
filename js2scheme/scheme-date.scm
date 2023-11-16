;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-date.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  5 05:47:06 2017                          */
;*    Last change :  Thu May 11 08:18:58 2023 (serrano)                */
;*    Copyright   :  2017-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Scheme code generation of JavaScript Date functions.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-date
   
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
   
   (export (j2s-new-date ::J2SNew mode return::procedure ctx)
	   (j2s-date-builtin-method fun::J2SAccess args expr mode return ctx)
	   (j2s-date-setminutes obj::J2SExpr args mode return ctx)
	   (j2s-date-maybe-setminutes obj::J2SExpr args mode return ctx)
	   (j2s-date-setutcminutes obj::J2SExpr args mode return ctx)
	   (j2s-date-maybe-setutcminutes obj::J2SExpr args mode return ctx)))

;*---------------------------------------------------------------------*/
;*    j2s-new-date ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-new-date this::J2SNew mode return::procedure ctx)
   
   (define (j2s-scheme-box o mode return ctx)
      (let ((t (j2s-type o)))
	 (box (j2s-scheme o mode return ctx) t ctx)))
   
   (with-access::J2SNew this (args)
      (case (length args)
	 ((0 1 2 3 4 5 6 7)
	  (let ((ctor (symbol-append 'js-new-date
			 (string->symbol
			    (integer->string (length args))))))
	     `(,ctor %this
		 ,@(map (lambda (a)
			   (j2s-scheme-box a mode return ctx))
		      args))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    j2s-date-utc ...                                                 */
;*---------------------------------------------------------------------*/
(define (j2s-date-utc args::pair-nil mode return::procedure ctx)
   
   (define (j2s-scheme-box o mode return ctx)
      (let ((t (j2s-type o)))
	 (box (j2s-scheme o mode return ctx) t ctx)))
   
   (case (length args)
      ((0 1 2 3 4 5 6 7)
       (let ((ctor (symbol-append 'js-date-utc
		      (string->symbol
			 (integer->string (length args))))))
	  `(,ctor %this
	      ,@(map (lambda (a)
			(j2s-scheme-box a mode return ctx))
		   args))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    j2s-date-builtin-method ...                                      */
;*---------------------------------------------------------------------*/
(define (j2s-date-builtin-method fun::J2SAccess args expr mode return conf)
   (with-access::J2SAccess fun (loc obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (cond
	       ((string=? val "now")
		(case (length args)
		   ((0) '(js-date-now))
		   (else #f)))
	       ((string=? val "UTC")
		(j2s-date-utc args mode return conf))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    j2s-date-setminutes ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-date-setminutes obj::J2SExpr args mode return ctx)
   (match-case args
      ((?min ?sec ?ms)
       `(js-date-setminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))
      ((?min ?sec)
       `(js-date-setminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)
	   (js-undefined)))
      ((?min)
       `(js-date-setminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)
	   (js-undefined) (js-undefined)))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-date-maybe-setminutes ...                                    */
;*---------------------------------------------------------------------*/
(define (j2s-date-maybe-setminutes obj::J2SExpr args mode return ctx)
   (match-case args
      ((?min ?sec ?ms ?%this ?cache)
       `(js-date-maybe-setminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,(j2s-scheme min mode return ctx)
	   ,(j2s-scheme sec mode return ctx)
	   ,(j2s-scheme ms mode return ctx)
	   ,%this ,cache))
      ((?min ?sec ?%this ?cache)
       `(js-date-maybe-setminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,(j2s-scheme min mode return ctx)
	   ,(j2s-scheme sec mode return ctx)
	   (js-undefined)
	   ,%this ,cache))
      ((?min ?%this ?cache)
       `(js-date-maybe-setminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,(j2s-scheme min mode return ctx)
	   (js-undefined)
	   (js-undefined)
	   ,%this ,cache))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-date-setutcminutes ...                                       */
;*---------------------------------------------------------------------*/
(define (j2s-date-setutcminutes obj::J2SExpr args mode return ctx)
   (match-case args
      ((?min ?sec ?ms)
       `(js-date-setutcminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)))
      ((?min ?sec)
       `(js-date-setutcminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)
	   (js-undefined)))
      ((?min)
       `(js-date-setutcminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,@(map (lambda (a) (j2s-scheme a mode return ctx)) args)
	   (js-undefined) (js-undefined)))))

;*---------------------------------------------------------------------*/
;*    j2s-date-maybe-setutcminutes ...                                 */
;*---------------------------------------------------------------------*/
(define (j2s-date-maybe-setutcminutes obj::J2SExpr args mode return ctx)
   (match-case args
      ((?min ?sec ?ms ?%this ?cache)
       `(js-date-maybe-setutcminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,(j2s-scheme min mode return ctx)
	   ,(j2s-scheme sec mode return ctx)
	   ,(j2s-scheme ms mode return ctx)
	   ,%this ,cache))
      ((?min ?sec ?%this ?cache)
       `(js-date-maybe-setutcminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,(j2s-scheme min mode return ctx)
	   ,(j2s-scheme sec mode return ctx)
	   (js-undefined)
	   ,%this ,cache))
      ((?min ?%this ?cache)
       `(js-date-maybe-setutcminutes
	   ,(j2s-scheme obj mode return ctx)
	   ,(j2s-scheme min mode return ctx)
	   (js-undefined)
	   (js-undefined)
	   ,%this ,cache))))
	 

	 
