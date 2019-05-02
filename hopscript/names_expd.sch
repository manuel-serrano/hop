;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/names_expd.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  1 08:50:34 2019                          */
;*    Last change :  Wed May  1 14:11:56 2019 (serrano)                */
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

   (match-case x
      ((?- strings)
       `',&strings)
      ((?- (and ?val (? string?)))
       (cond
	  ((vector-index val &strings)
	   =>
	   (lambda (i)
	      `(vector-ref js-string-names ,i)))
	  ((string->number val)
	   =>
	   (lambda (n)
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
