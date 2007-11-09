;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/read-js.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 19 10:45:35 2005                          */
;*    Last change :  Fri Nov  9 15:34:45 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The HOP javascript parser                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_read-js
   
   (import __hop_read
	   __hop_param)
   
   (export (hop-read-javascript ::input-port)))

;*---------------------------------------------------------------------*/
;*    hop-read-javascript ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is invoked when the initial "{" character has      */
;*    already been parsed.                                             */
;*---------------------------------------------------------------------*/
(define (hop-read-javascript iport)
   (let* ((sp (input-port-position iport))
	  (g (regular-grammar (bra-open)
		("{"
		 (set! bra-open (+fx bra-open 1))
		 (cons "{" (ignore)))
		("}"
		 (set! bra-open (-fx bra-open 1))
		 (if (>fx bra-open 0)
		     (cons "}" (ignore))
		     '()))
		((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
		 (let ((s (the-substring 1 (-fx (the-length) 1))))
		    (cons (string-append "\"" s "\"")
			  (ignore))))
		((: "\'" (* (or (out #a000 #\\ #\') (: #\\ all))) "\'")
		 (let ((s (the-substring 1 (-fx (the-length) 1))))
		    (cons (string-append "\'" s "\'")
			  (ignore))))
		((: "//" (* all))
		 (ignore))
		((: "/*" (* (or (out #\*) (: #\* (out "/")))) "*/")
		 (ignore))
		((+ (out "${}\"'/"))
		 (let ((s (the-string)))
		    (cons s (ignore))))
		("/"
		 (cons "/" (ignore)))
		("$$"
		 (cons "$" (ignore)))
		("$"
		 (let ((exp (hop-read iport))
		       (pos (input-port-position iport)))
		    (if (eof-object? exp)
			(read-error/location "Unexpected end-of-file"
					     "Unclosed list"
					     (input-port-name iport)
				 	     pos)
			(cons `(hop->json ,exp) (ignore)))))
		(else
		 (let ((char (the-failure)))
		    (if (eof-object? char)
			(read-error/location "Unexpected end-of-file"
					     "Unclosed list"
					     (input-port-name iport)
					     sp)
			(read-error/location "Illegal char"
					     (illegal-char-rep char)
					     (input-port-name iport)
					     (input-port-position iport))))))))
      (let ((exp (read/rp g iport 1)))
	 (cond
	    ((null? exp)
	     "")
	    ((null? (cdr exp))
	     (car exp))
	    (else
	     (if (every? string? exp)
		 (apply string-append exp)
		 `(string-append ,@exp)))))))
		
   
