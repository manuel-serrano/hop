(module __hopscheme_tilde-escape
   (library scheme2js)
   (export (compile-scheme-expression e)
	   (JS-expression t)
	   (JS-statement t)
	   (JS-return t))
   (import __hopscheme_config))

(define (compile-scheme-expression e)
   (let ((s-port (open-output-string))
	 (assig-var (gensym 'result)))
      (with-handler
	 (lambda (e)
	    (close-output-port s-port)
	    (raise e))
	 (cons assig-var
	       (scheme2js-compile-expr
		e              ;; top-level
		s-port         ;; out-port
		'((((import hop-runtime)) . merge-first)) ;; module-headers
		(extend-config (hopscheme-config #f)
			       'module-result-var assig-var))) ;; config
	 (*hop-postprocess* (close-output-port s-port)))))

(define (JS-expression t)
   (let* ((assig-var (car t))
	  (assig-var-str (symbol->string t))
	  (e (cdr t)))
      (string-append
       "(function() { var " assig-var-str "; " e "\n"
       "return " assig-var-str "; })"
       ".call(this)")))
(define (JS-statement t)
   (let* ((assig-var (car t))
	  (assig-var-str (symbol->string t))
	  (e (cdr t)))
      (string-append
       "{ var " assig-var-str "; " e "}")))
(define (JS-return t)
   (let* ((assig-var (car t))
	  (assig-var-str (symbol->string t))
	  (e (cdr t)))
      (string-append
       "{ var " assig-var-str "; " e "\n"
       "return " assig-var-str "; }")))
