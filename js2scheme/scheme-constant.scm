;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-constant.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 12 08:03:03 2020                          */
;*    Last change :  Sun Apr 12 16:49:50 2020 (serrano)                */
;*    Copyright   :  2020 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Scheme compilation of JS constants.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_scheme-constant

   (include "ast.sch"
	    "context.sch")
   
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

   (export (& obj ::J2SProgram)
	   (&string ::bstring)))

;*---------------------------------------------------------------------*/
;*    & ...                                                            */
;*---------------------------------------------------------------------*/
(define (& obj prgm)
   (with-access::J2SProgram prgm (strings)
      (let* ((s (cond
		   ((symbol? obj) (symbol->string! obj))
		   ((number? obj) (number->string obj))
		   ((isa? obj J2SString) (with-access::J2SString obj (val) val))
		   (else obj)))
	     (o (assoc s strings)))
	 (if (pair? o)
	     `(& ,s ,(cdr o))
	     (let ((n (length strings)))
		(set! strings (cons (cons s n) strings))
		`(& ,s ,n))))))
      
;*---------------------------------------------------------------------*/
;*    &string ...                                                      */
;*    -------------------------------------------------------------    */
;*    See name_expd.sch                                                */
;*---------------------------------------------------------------------*/
(define (&string val::bstring)

   (define (integer-string? str)
      (let ((len (string-length str)))
	 (case len
	    ((0)
	     #f)
	    ((1)
	     (char-numeric? (string-ref str 0)))
	    (else
	     (case (string-ref str 0)
		((#\0)
		 #f)
		((#\-)
		 (when (and (>fx len 1) (not (char=? (string-ref str 1) #\0)))
		    (let loop ((i 1))
		       (cond
			  ((=fx i len) #t)
			  ((char-numeric? (string-ref str i)) (loop (+fx i 1)))
			  (else #f)))))
		(else
		 (let loop ((i 0))
		    (cond
		       ((=fx i len) #t)
		       ((char-numeric? (string-ref str i)) (loop (+fx i 1)))
		       (else #f)))))))))

   (cond
      ((integer-string? val)
       (let ((n (string->number val)))
	  (cond
	     ((not (fixnum? n))
	      (vector 0 val))
	     ((string=? val "-0")
	      (vector 0 val))
	     ((char=? (string-ref val 0) #\+)
	      (vector 0 val))
	     ((=fx (string-length val) 1)
	      (vector 2 n))
	     ((and (char=? (string-ref val 0) #\0) (not (=fx n 0)))
	      (vector 0 val))
	     (else
	      (vector 2 n)))))
      ((eq? (string-minimal-charset val) 'ascii)
       (vector 0 val))
      (else
       (vector 1 val))))
