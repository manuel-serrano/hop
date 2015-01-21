;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopscheme/hopscheme.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Wed Feb 17 18:39:39 2010                          */
;*    Last change :  Tue Jan 20 08:38:51 2015 (serrano)                */
;*    Copyright   :  2010-15 Florian Loitsch and Manuel Serrano        */
;*    -------------------------------------------------------------    */
;*    Hopscheme                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    THe module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme
   
   (library scheme2js)
   
   (import __hopscheme_config
	   __hopscheme_dollar-escape
	   __hopscheme_precompilation
	   __hopscheme_hop_runtime
	   __dollar_scheme2js_module)
   
   (export (hopscheme-compile-module clauses::pair-nil)
	   (hopscheme-compile-file file::bstring ::bstring ::obj)
	   (hopscheme-create-empty-macro-environment)
	   (hopscheme-compile-expression e ::obj ::obj ::procedure)
	   (hopscheme-compile-value ::obj ::output-port ::procedure ::procedure ::obj)
	   (hopscheme-compile-hop-client e #!optional (env '()) (menv #f))
	   (hopscheme->JS-expression::bstring ::vector) 
	   (hopscheme->JS-statement::bstring ::vector)
	   (hopscheme->JS-return::bstring ::vector)
	   (hopscheme-declared::pair-nil ::vector)
	   (hopscheme-free::pair-nil ::vector)
	   (sexp->hopscheme::vector ::obj ::obj ::obj)
	   (hopscheme->sexp::obj ::vector ::procedure)))

;*---------------------------------------------------------------------*/
;*    *hopscheme-mutex* ...                                            */
;*---------------------------------------------------------------------*/
(define *hopscheme-mutex* (make-mutex "hopscheme"))

;*---------------------------------------------------------------------*/
;*    hopscheme-compile-module ...                                     */
;*    -------------------------------------------------------------    */
;*    Precompiles the given clauses, so they can be used as            */
;*    "module-header" for expressions that are then compiled by        */
;*    HOPSCHEME-COMPILE-EXPRESSION. Clauses should be something like   */
;*    '(import m1), etc.                                               */
;*---------------------------------------------------------------------*/
(define (hopscheme-compile-module clauses)
   (list (precompile-headers clauses)))

;*---------------------------------------------------------------------*/
;*    hopscheme-compile-file ...                                       */
;*---------------------------------------------------------------------*/
(define (hopscheme-compile-file ifile ofile env)
   (scheme2js-compile-file
      ;; input-files
      ifile
      ;; output-file
      ofile
      ;; headers-overrides
      `((merge-first (import ,@(hop-runtime-modules))) ,@env)
      (get-file-cached-config)
      :reader *hop-reader*))

;*---------------------------------------------------------------------*/
;*    hopscheme-create-empty-macro-environment ...                     */
;*---------------------------------------------------------------------*/
(define (hopscheme-create-empty-macro-environment)
   (instantiate::Compilation-Unit
      (name (gensym 'macro))
      (top-level '())
      (exported-macros (make-hashtable))
      (exports '())))

;*---------------------------------------------------------------------*/
;*    hopscheme-compile-expression ...                                 */
;*    -------------------------------------------------------------    */
;*    this function is called during parsing. It returns an expression */
;*    that is supposed to take the place of a tilde expression. We     */
;*    therefore return a quotted 'cons (instead of a pair).            */
;*                                                                     */
;*    When the expression exclusively consists of define-macros, then  */
;*    we add it to the 'menv'. (Otherwise it would not have any        */
;*    effect.) This is a special case (and thus slightly inconsistent  */
;*    with the rest.                                                   */
;*---------------------------------------------------------------------*/
(define (hopscheme-compile-expression e env menv postproc)
   (unless (isa? menv Compilation-Unit)
      (error "hopscheme-compile-expression" "Illegal macro environment" menv))
   (if (only-macros? e)
       (begin
	  (add-macros! e menv)
	  #unspecified)
       (compile-expression e env menv postproc)))

;*---------------------------------------------------------------------*/
;*    hopscheme-compile-value ...                                      */
;*---------------------------------------------------------------------*/
(define (hopscheme-compile-value v p host-compiler host-register loc)
   (scheme2js-compile-value v p host-compiler host-register loc))
   
;*---------------------------------------------------------------------*/
;*    hopscheme-compile-hop-client ...                                 */
;*---------------------------------------------------------------------*/
(define (hopscheme-compile-hop-client e #!optional (env '()) (menv #f))
   ;; This function is used from weblets, don't remove it!
   (let ((unit (or menv (instantiate::Compilation-Unit
			   (name (gensym 'macro))
			   (top-level '())
			   (exported-macros (create-hashtable :size 1))
			   (exports '()))))
	 (s-port (open-output-string)))
      (with-handler
	 (lambda (e)
	    (error "compile-hop-client" "Compilation failed" e))
	 (begin
	    (scheme2js-compile-expr
	       ;; top-level
	       e
	       ;; out-port
	       s-port         
	       ;; override-headers
	       `((merge-first (import ,@(hop-runtime-modules)))
		 (merge-last (import ,unit))
		 ,@env)
	       (hopscheme-config #f))
	    (close-output-port s-port)))))

;*---------------------------------------------------------------------*/
;*    hopscheme ...                                                    */
;*---------------------------------------------------------------------*/
(define (hopscheme-src h) (vector-ref h 0))
(define (hopscheme-var h) (vector-ref h 1))
(define (hopscheme-declared h) (vector-ref h 2))
(define (hopscheme-free h) (vector-ref h 3))
(define (hopscheme-jstr h) (vector-ref h 4))
(define (hopscheme-env h) (vector-ref h 5))

;*---------------------------------------------------------------------*/
;*    hopscheme->JS-expression ...                                     */
;*---------------------------------------------------------------------*/
(define (hopscheme->JS-expression hs)
   (let* ((jstr (hopscheme-jstr hs))
	  (assig-var (hopscheme-var hs))
	  (assig-var-str (if (symbol? assig-var)
			     (symbol->string assig-var)
			     "")))
      (string-append
	 "(function() { " jstr "\n"
	 "return " assig-var-str "; })"
	 ".call(this)")))

;*---------------------------------------------------------------------*/
;*    hopscheme->JS-statement ...                                      */
;*---------------------------------------------------------------------*/
(define (hopscheme->JS-statement hs)
   (let ((jstr (hopscheme-jstr hs)))
      (if (>fx (bigloo-debug) 0)
	  (string-append "{ " jstr "\n undefined; }" )
	  jstr)))

;*---------------------------------------------------------------------*/
;*    hopscheme->JS-return ...                                         */
;*---------------------------------------------------------------------*/
(define (hopscheme->JS-return hs)
   (let* ((jstr (hopscheme-jstr hs))
	  (assig-var (hopscheme-var hs))
	  (assig-var-str (if (symbol? assig-var)
			     (symbol->string assig-var)
			     "")))
      (string-append
	 "{ " jstr "\n"
	 "return " assig-var-str "; }")))

;*---------------------------------------------------------------------*/
;*    only-macros? ...                                                 */
;*---------------------------------------------------------------------*/
(define (only-macros? e)
   (match-case e
      ((define-macro (?- . ?-) ?- . ?-)
       #t)
      ((begin ?e0 . ?es)
       (and (only-macros? e0)
	    (list? es)
	    (every only-macros? es)))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    add-macros! ...                                                  */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    compile-expression ...                                           */
;*---------------------------------------------------------------------*/
(define (compile-expression e env menv postproc)
   
   (define (quasiquote-map-old dollar-map)
      `(,(begin 'quasiquote)
	,(map (lambda (p)
		 `(,(car p) ,(list 'unquote (cadr p))))
	    dollar-map)))
   
   (define (quasiquote-map dollar-map)
      `(,(begin 'quasiquote)
	,(map (lambda (p)
		 `(,(car p) ,(list 'unquote (car p))))
	    dollar-map)))
   
   (let ((s-port (open-output-string))
	 (assig-var (gensym 'result)))
      (multiple-value-bind (expr dollar-map)
	 (dollar-extraction! e)
	 (unwind-protect
	    (let* ((exported '())
		   (unresolved '())
		   (exported-declare! (lambda (scm-id js-id)
					 (set! exported
					    (cons (cons scm-id js-id)
					       exported))))
		   (unresolved-declare! (lambda (scm-id js-id location)
					   (set! unresolved
					      (cons (list scm-id js-id location)
						 unresolved)))))
	       (scheme2js-compile-expr
		  ;; top-level
		  expr
		  ;; out-port
		  s-port
		  ;; override-headers
		  `((merge-first (import ,@(hop-runtime-modules)))
		    (merge-last (import ,menv))
		    ,@env)
		  ;; config
		  (extend-config* (hopscheme-config #f)	
		     `((module-result-var . ,assig-var)
		       (unresolved-declare . ,unresolved-declare!)
		       (exported-declare . ,exported-declare!))))
	       (let ((js-code (close-output-port s-port))
		     (command (gensym 'command))
		     (scm-expr (gensym 'scm-expr)))
		  `(let* (,@dollar-map)
		      (vector ',expr
			 ',assig-var
			 ',exported
			 ',unresolved
			 ,(postproc js-code)
			 ,(quasiquote-map dollar-map)))))
	    (close-output-port s-port)))))

;*---------------------------------------------------------------------*/
;*    sexp->hopscheme ...                                              */
;*---------------------------------------------------------------------*/
(define (sexp->hopscheme e env menv)
   (let ((s-port (open-output-string))
	 (menv (instantiate::Compilation-Unit
		  (name (gensym 'macro))
		  (top-level '())
		  (exported-macros (create-hashtable :size 1))
		  (exports '())))
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
		   (unresolved-declare! (lambda (scm-id js-id location)
					   (set! unresolved
					      (cons (list scm-id js-id location)
						 unresolved)))))
	       (scheme2js-compile-expr
		  ;; top-level
		  expr
		  ;; out-port
		  s-port
		  ;; override-headers
		  `((merge-first (import ,@(hop-runtime-modules)))
		    (merge-last (import ,menv))
		    ,@env)
		  ;; config
		  (extend-config* (hopscheme-config #f)	
		     `((module-result-var . ,assig-var)
		       (unresolved-declare . ,unresolved-declare!)
		       (exported-declare . ,exported-declare!))))
	       (let ((js-code (close-output-port s-port)))
		  (vector expr
		     assig-var
		     exported
		     unresolved
		     js-code
		     '())))
	    (close-output-port s-port)))))

;*---------------------------------------------------------------------*/
;*    hopscheme->sexp ...                                              */
;*---------------------------------------------------------------------*/
(define (hopscheme->sexp hs wrapper)
   (let ((env (map (lambda (l)
		      (list (car l) (wrapper (cadr l))))
		   (hopscheme-env hs))))
      (replace-dollars! (tree-copy (hopscheme-src hs)) env)))

