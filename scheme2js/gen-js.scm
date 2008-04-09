(module gen-js
   (export (mangle-JS-sym::bstring sym::symbol)
	   (gen-JS-sym::bstring sym::symbol)))

(define counter 0)

(define *js-counter-mutex* (make-mutex))

;; mangle variables, so they are valid JS-vars.
(define (mangle-JS-sym sym)
   ;; MS: 21 mar 2006
   (let ((s (symbol->string sym)))
      (if (bigloo-need-mangling? s)
	  (bigloo-mangle s)
	  s)))

;; kind of adapted gen-sym
(define (gen-JS-sym sym)
   (with-lock *js-counter-mutex*
      (lambda ()
	 (set! counter (+ counter 1))
	 (mangle-JS-sym
	  (symbol-append 'sc_ ;; start with "sc_"
			 sym
			 '_
			 (string->symbol (integer->string counter)))))))
