;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/runtime/read_js.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:45:35 2005                          */
;*    Last change :  Fri Jan 13 17:59:35 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP javascript parser                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_read-js
   
   (import __hop_read
	   __hop_param
	   __hop_charset)
   
   (export (hop-read-javascript-string s::bstring)
	   (hop-read-javascript #!optional
				(iport::input-port (current-input-port))
				(charset (hop-locale)))))

;*---------------------------------------------------------------------*/
;*    hop-read-javascript-string ...                                   */
;*---------------------------------------------------------------------*/
(define (hop-read-javascript-string s)
   (call-with-input-string s 
      (lambda (p)
	 (hop-read-javascript p (hop-charset)))))

;*---------------------------------------------------------------------*/
;*    hop-read-javascript ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is invoked when the initial "{" character has      */
;*    already been parsed.                                             */
;*---------------------------------------------------------------------*/
(define (hop-read-javascript #!optional
			     (iport::input-port (current-input-port))
			     (charset (hop-locale)))
 
   (let* ((sp (input-port-position iport))
	  (g (regular-grammar (bra-open acc cset)
		("{"
		 (set! bra-open (+fx bra-open 1))
		 (set! acc (cons "{" acc))
		 (ignore))
		("}"
		 (set! bra-open (-fx bra-open 1))
		 (if (>=fx bra-open 0)
		     (begin
			(set! acc (cons "}" acc))
			(ignore))
		     (begin
			(rgc-buffer-unget-char (the-port) (the-byte))
			(reverse! acc))))
		((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
		 (let ((s (the-substring 1 (-fx (the-length) 1))))
		    (set! acc (cons (string-append "\"" (cset s) "\"") acc))
		    (ignore)))
		((: "\'" (* (or (out #a000 #\\ #\') (: #\\ all))) "\'")
		 (let ((s (the-substring 1 (-fx (the-length) 1))))
		    (set! acc (cons (string-append "\'" (cset s) "\'") acc))
		    (ignore)))
		((or (: "// " (* all)) (: " //" (* all)))
		 (ignore))
		((: "/*" (* (or (out #\*) (: #\* (out "/")))) "*/")
		 (ignore))
		((+ (out "${}\"'/"))
		 (set! acc (cons (the-string) acc))
		 (ignore))
		("/"
		 (set! acc (cons "/" acc))
		 (ignore))
		("$$"
		 (set! acc (cons "$" acc))
		 (ignore))
		("$"
		 (let ((exp (hop-read iport charset))
		       (pos (input-port-position iport)))
		    (if (eof-object? exp)
			(read-error/location "Unexpected end-of-file"
					     "Unclosed list"
					     (input-port-name iport)
				 	     pos)
			(begin
			   (set! acc (cons `(call-with-output-string
					     (lambda (op)
						(obj->javascript-attr ,exp op)))
					   acc))
			   (ignore)))))
		(else
		 (let ((char (the-failure)))
		    (if (eof-object? char)
			(if (=fx bra-open 0)
			    (reverse! acc)
			    (read-error/location "Unexpected end-of-file"
						 "Unclosed list"
						 (input-port-name iport)
						 sp))
			(read-error/location "Illegal char"
					     (illegal-char-rep char)
					     (input-port-name iport)
					     (input-port-position iport))))))))
      (let ((cvt (charset-converter! charset (hop-charset))))
	 (let ((exp (read/rp g iport 0 '() cvt)))
	    (cond
	       ((null? exp)
		"")
	       ((null? (cdr exp))
		(car exp))
	       (else
		(if (every? string? exp)
		    (apply string-append exp)
		    `(string-append ,@exp))))))))
		
   
