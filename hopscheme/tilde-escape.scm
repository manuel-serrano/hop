;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/hopscheme/tilde-escape.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Wed Feb 17 18:09:56 2010                          */
;*    Last change :  Thu Feb 18 06:15:37 2010 (serrano)                */
;*    Copyright   :  2010 Florian Loitsch and Manuel Serrano           */
;*    -------------------------------------------------------------    */
;*    Interface between Scheme2JS and Hop.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscheme_tilde-escape
   
   (library scheme2js)
   
   (export (hopscheme-create-empty-macro-environment)
	   (hopscheme-compile-expression e ::obj ::obj)
	   (hopscheme-compile-hop-client e #!optional (env '()) (menv #f))
	   (hopscheme->JS-expression::bstring ::vector) 
	   (hopscheme->JS-statement::bstring ::vector)
	   (hopscheme->JS-return::bstring ::vector)
	   
	   (sexp->precompiled sexp)
	   (precompiled->sexp precompiled)
;* 	   (precompiled->JS-statement::bstring precompiled)            */
;* 	   (precompiled->JS-expression::bstring precompiled)           */
;* 	   (precompiled->JS-return::bstring precompiled)               */
	   )
   
   (import __hopscheme_config
	   __hopscheme_hop_runtime
	   __hopscheme_dollar-escape))

;*---------------------------------------------------------------------*/
;*    %hopscheme ...                                                   */
;*---------------------------------------------------------------------*/
(define (%hopscheme-src h) (vector-ref h 0))
(define (%hopscheme-var h) (vector-ref h 1))
(define (%hopscheme-jstr h) (vector-ref h 2))
(define (%hopscheme-exported h) (vector-ref h 3))
(define (%hopscheme-unresolved h) (vector-ref h 4))

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
(define (hopscheme-compile-expression e env menv)
   (unless (Compilation-Unit? menv)
      (error 'hopscheme-compile-expression "Illegal macro environment" menv))
   (if (only-macros? e)
       (begin
	  (add-macros! e menv)
	  #unspecified)
       (compile-expression e env menv)))

;*---------------------------------------------------------------------*/
;*    hopscheme->JS-expression ...                                     */
;*---------------------------------------------------------------------*/
(define (hopscheme->JS-expression hs)
   (let* ((jstr (%hopscheme-jstr hs))
	  (assig-var (%hopscheme-var hs))
	  (assig-var-str (symbol->string assig-var)))
      (string-append
       "(function() { " jstr "\n"
       "return " assig-var-str "; })"
       ".call(this)")))

;*---------------------------------------------------------------------*/
;*    hopscheme->JS-statement ...                                      */
;*---------------------------------------------------------------------*/
(define (hopscheme->JS-statement hs)
   (let ((jstr (%hopscheme-jstr hs)))
      (if (>fx (bigloo-debug) 0)
	  (string-append "{ " jstr "\n undefined; }" )
	  jstr)))

;*---------------------------------------------------------------------*/
;*    hopscheme->JS-return ...                                         */
;*---------------------------------------------------------------------*/
(define (hopscheme->JS-return hs)
   (let* ((jstr (%hopscheme-jstr hs))
	  (assig-var (%hopscheme-var hs))
	  (assig-var-str (symbol->string assig-var)))
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
	    (every? only-macros? es)))
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
		     (hs (gensym 'hs)))
		  `(let ,dollar-map
		      '#(,(list 'quote expr)
			 ',assig-var
			 ,(*hop-postprocess* js-code)
			 ',exported
			 ',unresolved))))
	    (close-output-port s-port)))))

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

(define (sexp->precompiled sexp)
   'todo)
;*    (let ((compiled (compile-hop-client sexp)))                      */
;*       (lambda (command)                                             */
;* 	 (case command                                                 */
;* 	    ((JS) (cons 'foo compiled))                                */
;* 	    ((scheme) sexp)))))                                        */

(define (precompiled->sexp precompiled)
   'todo)
;*    (precompiled 'scheme))                                           */

