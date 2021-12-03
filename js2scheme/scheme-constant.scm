;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/scheme-constant.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 12 08:03:03 2020                          */
;*    Last change :  Fri Sep 10 19:51:58 2021 (serrano)                */
;*    Copyright   :  2020-21 Manuel Serrano                            */
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
	   (&string ::pair)
	   (utf8-codeunit-length::long ::bstring)))

;*---------------------------------------------------------------------*/
;*    & ...                                                            */
;*---------------------------------------------------------------------*/
(define (& obj prgm)
   (with-access::J2SProgram prgm (strings)
      (let* ((s (cond
		   ((symbol? obj)
		    (cons (symbol->string! obj) #f))
		   ((number? obj)
		    (cons (number->string obj) #f))
		   ((isa? obj J2SString)
		    (with-access::J2SString obj (val private)
		       (cons val private)))
		   (else
		    (cons obj #f))))
	     (o (assoc s strings)))
	 (if (pair? o)
	     `(& ,(car s) ,(cadr o))
	     (let ((n (length strings)))
		(set! strings (cons (list s n obj) strings))
		`(& ,(car s) ,n))))))
      
;*---------------------------------------------------------------------*/
;*    &string ...                                                      */
;*    -------------------------------------------------------------    */
;*    See name_expd.sch                                                */
;*---------------------------------------------------------------------*/
(define (&string e)

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

   (let ((val (caar e)))
      (cond
	 ((and (isa? (caddr e) J2SString)
	       (with-access::J2SString (caddr e) (private) private))
	  (vector 4 val))
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
	  (vector 3 val (utf8-codeunit-length val))))))

;*---------------------------------------------------------------------*/
;*    utf8-codeunit-length ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns the number of code points required to encode that        */
;*    UTF8 string (might be bigger than the UTF8 length).              */
;*---------------------------------------------------------------------*/
(define (utf8-codeunit-length::long str::bstring)
   (let ((len (string-length str)))
      (let loop ((r 0)
		 (l 0))
	 (if (>=fx r len)
	     l
	     (let* ((c (string-ref str r))
		    (s (utf8-char-size c)))
		(if (and (=fx s 4)
			 (or (=fx (char->integer c) #xf0)
			     (=fx (char->integer c) #xf4)))
		    (loop (+fx r s) (+fx l 2))
		    (loop (+fx r s) (+fx l 1))))))))
