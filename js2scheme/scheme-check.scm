;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-check.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 07:13:28 2017                          */
;*    Last change :  Sat Jul 10 09:56:49 2021 (serrano)                */
;*    Copyright   :  2017-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Checking values from JS.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-check

   (include "context.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_scheme-test
	   __js2scheme_scheme-utils
	   __js2scheme_scheme-constant)
   
   (export (j2s-check expr::obj ::obj ::symbol ::symbol ::struct)))

;*---------------------------------------------------------------------*/
;*    j2s-check ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-check sexp expr from to ctx)
   
   (define (type-compatible? from to)
      (or (eq? from to)
	  (eq? to 'any)
	  (and (eq? to 'number) (memq from '(integer real)))
	  (and (eq? to 'object) (memq from '(string array jsvector)))))

   (define (type-name type)
      (if (eq? type 'jsvector)
	  'vector
	  type))

   (define (type-checker type)
      (case type
	 ((number) 'number?)
	 ((integer) 'fixnum?)
	 ((real) 'real?)
	 ((array) 'js-array?)
	 ((jsvector) 'js-vector?)
	 ((string) 'js-string?)
	 ((object) 'js-object?)
	 ((function) 'js-function?)
	 (else (error "hopc:type-checker" "unknown type" type))))

   (define (need-temp? sexp)
      (not (or (symbol? sexp) (number? sexp) (boolean? sexp))))

   (with-access::J2SExpr expr (loc)
      (if (type-compatible? from to)
	  sexp
	  (let loop ((sexp sexp))
	     (if (need-temp? sexp)
		 (let ((tmp (gensym 'tmp)))
		    `(let ((,tmp ,sexp))
			,(loop tmp)))
		 `(if (,(type-checker to) ,sexp)
		      ,sexp
		      ,(if (>fx (context-get ctx :debug 0) 0)
			   `(js-raise-type-error/loc %this ',loc
			       ,(format "~a expected: ~~a"
				   (type-name to))
			       ,sexp)
			   `(js-raise-type-error %this
			       ,(format "~a expected: ~~a"
				   (type-name to))
			       ,sexp))))))))
(define (mcontext-get ctx::struct key::keyword #!optional default)
   ;; when the new (12apr2020) version is stabilized, these checks
   ;; could be removed
   (case key
      ((:program) (error "context-get" "should use context-program" key))
      ((:array) (error "context-get" "should use context-array" key))
      ((:string) (error "context-get" "should use context-string" key))
      ((:regexp) (error "context-get" "should use context-regexp" key))
      ((:math) (error "context-get" "should use context-math" key))
      ((:object) (error "context-get" "should use context-object" key))
      (else (config-get (context-conf ctx) key default))))

