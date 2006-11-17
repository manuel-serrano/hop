(module tilde-escape
   (import  hopscheme_aliases)
   (library hop
	    scheme2js)
   (export (compile-hop-client e))
   (import hopscheme-config))

;; ===========================================================================
;; add hook for '~'-escaped expressions.
;; ===========================================================================

(define *rev-scheme-exprs* (make-hashtable))

(define (js->hop js-expr)
   (list (hop-read-javascript (open-input-string (string-append js-expr "}")))))

(define (new-scheme-expr p expr)
   (let* ((proxy (list 'begin #f))
	  (module
	   (list 'module expr
		 (lambda (p)
		    (cons (open-output-string)
			  (lambda (string-port)
			     (let ((js-expr (close-output-port string-port)))
				(set-cdr! proxy
					  (js->hop js-expr))
				(display ";" p))))))))
      (hashtable-update! *rev-scheme-exprs*
			 p
			 (lambda (old-l)
			    (cons module old-l))
			 (list module))
      proxy))

(hop-make-escape-set! new-scheme-expr)


(define (compile-hop-client e)
   (let ((s-port (open-output-string)))
      (with-handler
	 (lambda (e)
	    (close-output-port s-port)
	    (raise e))
	 (scheme2js (list e) (hopscheme-aliases) (hopscheme-config) s-port)
	 (close-output-port s-port))))

;; ===========================================================================
;; and one, once an expression has been read.
;; ===========================================================================

(define (post-compile p)
;   (print "post-compile")
   (let ((rev-scheme-exprs (hashtable-get *rev-scheme-exprs* p)))
      (when rev-scheme-exprs
	 (hashtable-remove! *rev-scheme-exprs* p)
	 (for-each compile-hop-client (reverse! rev-scheme-exprs)))))

(hop-read-post-hook-set! post-compile)
