(module __hopscheme_tilde-escape
   (library hop
	    scheme2js)
   (export (compile-hop-client e))
   (import __hopscheme_config))

;; ===========================================================================
;; add hook for '~'-escaped expressions.
;; ===========================================================================

(define (js->hop js-expr)
   (hop-read-javascript (open-input-string (string-append js-expr "}"))
			(hop-charset)))

(define (compile-hop-client e)
   (let ((s-port (open-output-string)))
      (with-handler
	 (lambda (e)
	    (close-output-port s-port)
	    (raise e))
	 (scheme2js-compile-expr
	  e              ;; top-level
	  s-port         ;; out-port
	  '((((import hop-runtime)) . merge-first)) ;; module-headers
	  (hopscheme-config #f)) ;; config
	 (close-output-port s-port))))

(define (new-scheme-expr p expr)
   (let ((js (compile-hop-client expr)))
      (js->hop js)))

(hop-make-escape-set! new-scheme-expr)
