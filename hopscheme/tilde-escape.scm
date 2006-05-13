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
	  (part (list 'part expr
		      (lambda (js-expr)
			 (set-cdr! proxy (js->hop js-expr))
			 ";"))))
      (hashtable-update! *rev-scheme-exprs*
			 p
			 (lambda (old-l)
			    (cons part old-l))
			 (list part))
      proxy))

(hop-make-escape-set! new-scheme-expr)


(define (compile-hop-client e)
   (scheme2js (list e) (hopscheme-aliases) (hopscheme-config)))

;; ===========================================================================
;; and one, once an expression has been read.
;; ===========================================================================

(define (post-compile p)
;   (print "post-compile")
   (let ((rev-scheme-exprs (hashtable-get *rev-scheme-exprs* p)))
      (when rev-scheme-exprs
	 (hashtable-remove! *rev-scheme-exprs* p)
	 (with-output-to-string
	    (lambda ()
	       (for-each compile-hop-client (reverse! rev-scheme-exprs)))))))

(hop-read-post-hook-set! post-compile)
