(module __hopscheme_tilde-escape
   (library scheme2js)
   (export (compile-scheme-expression e ::obj ::obj)
	   (compile-hop-client e #!optional (env '()) (menv #f))
	   (JS-expression::bstring t::pair)
	   (JS-statement::bstring t::pair)
	   (JS-return::bstring t::pair)
	   (create-empty-hopscheme-macro-environment))
   (import __hopscheme_config
	   __hop_exports))

(define (create-empty-hopscheme-macro-environment)
   (instantiate::Compilation-Unit
      (name (gensym 'macro))
      (top-level '())
      (exported-macros (make-hashtable))
      (exports '())))

(define (only-macros? e)
   (match-case e
      ((define-macro (?- . ?-) ?- . ?-)
       #t)
      ((begin ?e0 . ?es)
       (and (only-macros? e0)
	    (list? es)
	    (every? only-macros? es)))
      (else #f)))

;; this function is called during parsing. It returns an expression that is
;; supposed to take the place of a tilde expression. We therefore return a
;; quotted 'cons (instead of a pair).
;;
;; When the expression exclusively consists of define-macros, then we add it to
;; the 'menv'. (Otherwise it would not have any effect.)
;; This is a special case (and thus slightly inconsistent with the rest.
(define (compile-scheme-expression e env menv)
   (when (not (Compilation-Unit? menv))
      (error #f "" #f))
   (if (only-macros? e)
       (begin
	  (add-macros! e menv)
	  #unspecified)
       (compile-expression e env menv)))

(define (add-macros! e menv)
   (match-case e
      ((define-macro (?- . ?-) ?- . ?-)
       (module-exported-macro-add! menv e))
      ((begin . ?es)
       ;; we don't test for 'list?' anymore. this has been done before.
       (for-each (lambda (e) (add-macros! e menv)) es))
      (else (error "add-macros"
		   "internal Error. e different than macro or begin"
		   e))))

(define (compile-expression e env menv)
   (let ((s-port (open-output-string))
	 (assig-var (gensym 'result)))
      (with-handler
	 (lambda (e)
	    (close-output-port s-port)
	    (raise e))
	 (scheme2js-compile-expr
	  e              ;; top-level
	  s-port         ;; out-port
	  `(             ;; override-headers
	    (merge-first (import ,(hop-runtime-module)))
	    (merge-last (import ,menv))
	    ,@env)
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

(define (compile-hop-client e #!optional (env '()) (menv #f))
   ;; This function is used from weblets, don't remove it!
   (let ((ce (compile-scheme-expression e env menv)))
      (match-case ce
	 ((cons ((kwote quote) ?var) ?expr)
	  (JS-expression (cons var expr)))
	 (else
	  (error 'compile-hop-client "Compilation failed" e)))))
       
   
