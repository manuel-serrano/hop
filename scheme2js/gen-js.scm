(module gen-js
   (export (mangle-JS-sym::bstring sym::symbol)
	   (gen-JS-sym::bstring sym::symbol)))

(define counter 0)

(define (mangle-JS-sym sym)
   (let ((cs (string->list (symbol->string sym))))
      (let loop ((cs cs)
		 (acc '()))
	 (if (null? cs)
	     (list->string (reverse acc))
	     (let ((c (car cs)))
		(cond
		   ((char-alphabetic? c)
		    (loop (cdr cs) (cons c acc)))
		   ((char-numeric? c)
		    (loop (cdr cs) (cons c acc)))
		   (else
		    (loop
		     (cdr cs)
		     (append
		      ;; get ascii-number, and transform the
		      ;; number into string.
		      ;; the number will be reversed, but still
		      ;; unique
		      (string->list
		       (integer->string
			(char->integer c)))
		      ;; prefix with "_" (don't forget: reverse acc)
		      (cons #\_ acc))))))))))
   
;; mangle variables, so they are valid JS-vars.
;; kind of adapted gen-sym
(define (gen-JS-sym sym)
   (set! counter (+ counter 1))
   (mangle-JS-sym (symbol-append 'sc_ ;; start with "sc_"
				 sym
				 '_
				 (string->symbol (integer->string counter)))))
