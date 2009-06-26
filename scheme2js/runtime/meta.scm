;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(define (without-META line)
   (define META-pattern (pregexp "(?:/\*\*\* META)(.*)"))
   (cadr (pregexp-match META-pattern line)))


(define (parse-var/function line)
   (define exported-fun-pattern (pregexp "(?:var|function) ((?:sc_)?[^ (;]*)"))

   (let ((match (pregexp-match exported-fun-pattern line)))
      (string->symbol (cadr match))))

(define (parse-arity line)
   (define exported-fun-pattern
      (pregexp "(?:var|function) ((?:sc_)?[^ (;]*)[(]([^)]*)[)]"))

   (define (ltrim str)
      (let loop ((i 0))
	 (cond
	    ((=fx i (string-length str))
	     str)
	    ((char=? #\space (string-ref str i))
	     (loop (+fx i 1)))
	    ((=fx i 0)
	     str)
	    (else
	     (substring str i (string-length str))))))
   (define (count-commas str)
      (let loop ((i 0)
		 (res 0))
	 (cond
	    ((=fx i (string-length str))
	     res)
	    ((char=? #\, (string-ref str i))
	     (loop (+fx i 1) (+fx res 1)))
	    (else
	     (loop (+fx i 1) res)))))
   (let* ((match (pregexp-match exported-fun-pattern line))
	  (args (caddr match)))
      (cond
	 ((string-null? (ltrim args)) 0)
	 (else (+fx 1 (count-commas args))))))

(define (read-metas p)
   (let loop ((rev-result '())
	      (meta #f))
      (let ((line (read-line p)))
	 (if (or (not line)
		 (eof-object? line))
	     (reverse! rev-result)
	     (cond
		((starts-with? line "/*** META")
		 (let luup ((meta-lines (without-META line))
			    (last-line line))
		    (if (ends-with? last-line "*/")
			(let ((meta (with-input-from-string meta-lines read)))
			   (cond
			      ((and (pair? meta)
				    (eq? (car meta) 'define-macro))
			       (loop (cons (list #f 'macro meta) rev-result)
				     #f))
			      ((and (pair? meta)
				    (any (lambda (p)
					    (and (pair? p)
						 (eq? (car p) 'JS)))
					 meta))
			       (loop (cons (list #f 'meta meta)
					   rev-result)
				     #f))
			      (else
			       (loop rev-result meta))))
			(let ((line (read-line p)))
			   (luup (string-append meta-lines "\n" line)
				 line)))))
		((and (starts-with? line "var")
		      meta)
		 (loop (cons (list (parse-var/function line) 'var meta)
			     rev-result)
		       #f))
		((and (starts-with? line "function")
		      meta)
		 (loop (cons (list (parse-var/function line) 'function meta
				   (parse-arity line))
			     rev-result)
		       #f))
		(else
		 (loop rev-result meta)))))))
