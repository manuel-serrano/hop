;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names_expd.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  1 08:50:34 2019                          */
;*    Last change :  Wed Jun  5 19:57:02 2019 (serrano)                */
;*    Copyright   :  2019 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript name expanders                                         */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and names.sch                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    &name-expander ...                                               */
;*---------------------------------------------------------------------*/
(define (&name-expander x e)

   (define &strings
      '#("" "__dirname" "__filename" "__proto__"
	 "Array" "Buffer" "Error" "GLOBAL" "HEAD"
	 "Infinity" "-Infinity" "NaN" "Object"
	 "SCRIPT" "String" "Worker"
	 "apply" "call" "callee" "caller"
	 "clearImmediate" "clearInterval" "clearTimeout"
	 "console" "constructor"
	 "default" "exports" "filename" "get" "global"
	 "hop" "length" "module" "process" "prototype"
	 "readable" "require" "set" "setImmediate" "setInterval" "setTimeout"
	 "toString" "value" "write" "writable"))
   
   (define (vector-index val vector)
      (let loop ((i (-fx (vector-length vector) 1)))
	 (when (>=fx i 0)
	    (if (string=? val (vector-ref vector i))
		i
		(loop (-fx i 1))))))

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

   (match-case x
      ((?- strings)
       `',&strings)
      ((?- (and ?val (? string?)))
       (cond
	  ((vector-index val &strings)
	   =>
	   (lambda (i)
	      `(vector-ref js-string-names ,i)))
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
      (else
       (error "&name" "bad syntax" x))))
