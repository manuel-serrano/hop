(module __hopscheme_tilde-escape
   (library scheme2js)
   (export (compile-scheme-expression e)
	   (compile-hop-client e)
	   (JS-expression::bstring t::pair)
	   (JS-statement::bstring t::pair)
	   (JS-return::bstring t::pair))
   (import __hopscheme_config))

(define (compile-scheme-expression e)
   (let ((s-port (open-output-string))
	 (assig-var (gensym 'result)))
      (with-handler
	 (lambda (e)
	    (close-output-port s-port)
	    (raise e))
	 (scheme2js-compile-expr
	  e              ;; top-level
	  s-port         ;; out-port
	  '()            ;; module-headers
	  (extend-config (hopscheme-config #f) 'module-result-var assig-var)) ;; config
	 `(cons ',assig-var ,(*hop-postprocess* (close-output-port s-port))))))

(define (JS-expression t)
   (let* ((assig-var (car t))
	  (assig-var-str (symbol->string assig-var))
	  (e (cdr t)))
      (string-append
       "(function() { " e "\n"
       "return " assig-var-str "; })"
       ".call(this)")))
(define (JS-statement t)
   (cdr t))
(define (JS-return t)
   (let* ((assig-var (car t))
	  (assig-var-str (symbol->string assig-var))
	  (e (cdr t)))
      (string-append
       "{ " e "\n"
       "return " assig-var-str "; }")))

(define (compile-hop-client e)
   ;; This function is used from weblets, don't remove it!
   (let ((ce (compile-scheme-expression e)))
      (match-case ce
	 ((cons ((kwote quote) ?var) ?expr)
	  (JS-expression (cons var expr)))
	 (else
	  (error 'compile-hop-client "Compilation failed" e)))))
       
   
