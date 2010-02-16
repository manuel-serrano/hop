(module __hopscheme_tilde-escape
   (library scheme2js)
   (export (compile-scheme-expression e ::obj ::obj)
	   (compile-hop-client e #!optional (env '()) (menv #f))
	   (expr->precompiled expr)
	   (precompiled->expr precompiled)
	   (precompiled->JS-statement::bstring precompiled)
	   (precompiled->JS-expression::bstring precompiled)
	   (precompiled->JS-return::bstring precompiled)
	   (create-empty-hopscheme-macro-environment))
   (import __hopscheme_config
	   __hopscheme_hop_runtime
	   __hopscheme_dollar-escape))

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
      (error 'compile-scheme-expression "Illegal macro environment" menv))
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
   (define (quasiquote-map dollar-map)
      `(,(begin 'quasiquote)
	,(map (lambda (p)
		 `(,(car p) ,(list 'unquote (cadr p))))
	      dollar-map)))
      
   (let ((s-port (open-output-string))
	 (assig-var (gensym 'result)))
      (receive (expr dollar-map)
	 (dollar-extraction! e)
	 (unwind-protect
	    (let* ((exported '())
		   (unresolved '())
		   (exported-declare! (lambda (scm-id js-id)
					 (set! exported
					       (cons (cons scm-id js-id)
						     exported))))
		   (unresolved-declare! (lambda (scm-id js-id)
					   (set! unresolved
						 (cons (cons scm-id js-id)
						       unresolved)))))
	       (scheme2js-compile-expr
		expr           ;; top-level
		s-port         ;; out-port
		`(             ;; override-headers
		  (merge-first (import ,@(hop-runtime-modules)))
		  (merge-last (import ,menv))
		  ,@env)
		(extend-config* (hopscheme-config #f)	;; config
				`((module-result-var . ,assig-var)
				  (unresolved-declare . ,unresolved-declare!)
				  (exported-declare . ,exported-declare!))))
	       (let ((js-code (close-output-port s-port))
		     (command (gensym 'command))
		     (scm-expr (gensym 'scm-expr))
		     (js (gensym 'js))
		     (replaced? (gensym 'replaced?))
		     (dmap (gensym 'dmap)))
		  `(let* ((,scm-expr ,(list 'quote expr))
			  ,@(if (null? dollar-map)
				'()
				`((,replaced? #f)
				  ,@dollar-map
				  (,dmap ,(quasiquote-map dollar-map))))
			  (,js (cons ',assig-var ,(*hop-postprocess* js-code))))
		      (lambda (,command)
			 (case ,command
			    ((JS) ,js)
			    ((scheme)
			     ,(unless (null? dollar-map)
				 `(when (not ,replaced?)
				     (set! ,scm-expr
					   (replace-dollars! ,scm-expr ,dmap))
				     (set! ,replaced? #t)))
			     ,scm-expr)
			    ((exported) ',exported)
			    ((unresolved) ',unresolved))))))
	    (close-output-port s-port)))))

(define (expr->precompiled expr)
   (let ((compiled (compile-hop-client expr)))
      (lambda (command)
	 (case command
	    ((JS) (cons 'foo compiled))
	    ((scheme) expr)))))

(define (precompiled->JS-expression precompiled)
   (let* ((t (precompiled 'JS))
	  (assig-var (car t))
	  (assig-var-str (symbol->string assig-var))
	  (e (cdr t)))
      (string-append
       "(function() { " e "\n"
       "return " assig-var-str "; })"
       ".call(this)")))

(define (precompiled->JS-statement precompiled)
   (let ((t (precompiled 'JS)))
      (if (>fx (bigloo-debug) 0)
	  (string-append "{ " (cdr t) "\n undefined; }" )
	  (cdr t))))

(define (precompiled->JS-return precompiled)
   (let* ((t (precompiled 'JS))
	  (assig-var (car t))
	  (assig-var-str (symbol->string assig-var))
	  (e (cdr t)))
      (string-append
       "{ " e "\n"
       "return " assig-var-str "; }")))

(define (precompiled->expr precompiled)
   (precompiled 'scheme))

(define (compile-hop-client e #!optional (env '()) (menv #f))
   ;; This function is used from weblets, don't remove it!
   (let ((unit (or menv (instantiate::Compilation-Unit
			   (name (gensym 'macro))
			   (top-level '())
			   (exported-macros (create-hashtable :size 1))
			   (exports '()))))
	 (s-port (open-output-string)))
      (with-handler
	 (lambda (e)
	    (error 'compile-hop-client "Compilation failed" e))
	 (scheme2js-compile-expr
	  e              ;; top-level
	  s-port         ;; out-port
	  `(             ;; override-headers
	    (merge-first (import ,@(hop-runtime-modules)))
	    (merge-last (import ,unit))
	    ,@env)
	  (hopscheme-config #f))
	 (close-output-port s-port))))
