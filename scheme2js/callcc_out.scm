;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/callcc_out.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep  4 12:52:47 2013                          */
;*    Last change :                                                    */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JavaScript code generation with call/cc                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module callcc-out
   
   (import config
	   tools
	   nodes
	   dump-node
	   export-desc
	   walk
	   gen-js
	   mutable-strings
	   verbose
	   allocate-names
	   compile-optimized-call
	   compile-optimized-boolify
	   compile-optimized-set
	   template-display
	   pipe-port
	   js-pp
	   push-declarations
	   tail
	   error
	   scheme2js)

   (export (call/cc-start-code p)
	   (call/cc-end-code p)
	   (call/cc-while-body-out node env p)))

;*---------------------------------------------------------------------*/
;*    call/cc-frame-name ...                                           */
;*---------------------------------------------------------------------*/
(define (call/cc-frame-name scope)
   (with-access::Call/cc-Scope-Info scope (id)
      (string-append "'frame-" (symbol->string id) "'")))

;*---------------------------------------------------------------------*/
;*    call/cc-counter-name ...                                         */
;*---------------------------------------------------------------------*/
(define (call/cc-counter-name nb)
   (string-append "sc_counter_" (number->string nb)))

;*---------------------------------------------------------------------*/
;*    call/cc-start-code ...                                           */
;*---------------------------------------------------------------------*/
(define (call/cc-start-code p)

   (define (decrement-counter-variables)
      ;; by decrementing all counter-variables, we can increment the variable
      ;; unconditionally at the beginning of whiles, thus removing an 'if'.
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (let loop ((i 0))
	    (unless (>=fx i call/cc-nb-while-counters)
	       (template-display p
		  "~a--;\n" (call/cc-counter-name i))
	       (loop (+fx i 1))))))

   (define (do-hoisted)
      (define (emit-commands rev-hoisted)
	 (if (null? rev-hoisted)
	     (template-display p "sc_callCcTmp")
	     (match-case (car rev-hoisted)
		((set! ?v)
		 (template-display p
		    "~a = ~e"
		    (with-access::Named-Var v (js-id) js-id)
		    (emit-commands (cdr rev-hoisted))))
		((return)
		 (template-display p
		    "return (~e)" (emit-commands (cdr rev-hoisted))))
		(else
		 (error "out"
		    "Internal Error: forgot to implement hoisted command"
		    (car rev-hoisted))))))
      
      (with-access::Lambda this (call/cc-hoisted)
	 (unless (null? call/cc-hoisted)
	    (template-display p
	       "switch (sc_callCcIndex) {\n"
	       "  ~e"
	       "}\n"
	       (each (lambda (index/rev-hoisted)
			"case ~a: ~e; break;\n"
			(car index/rev-hoisted)
			(emit-commands (cdr index/rev-hoisted)))
		  call/cc-hoisted)))))
   
   (define (restore-frames)
      (with-access::Lambda this (call/cc-contained-scopes)
	 (for-each
	    (lambda (scope)
	       (with-access::Call/cc-Scope-Info scope (vars)
		  (unless (null? vars) ;; should only happen for lambda-frame
		     (let ((frame-name (call/cc-frame-name scope)))
			(template-display p
			   "if (sc_callCcFrame = sc_callCcState[~a]) {\n"
			   "  ~e"
			   "}\n"
			   frame-name
			   (each (lambda (var)
				    "~a = sc_callCcFrame.~a;\n"
				    (with-access::Named-Var var (js-id) js-id)
				    (with-access::Named-Var var (js-id) js-id))
			      vars))))))
	    call/cc-contained-scopes)))

   (define (restore-counter-variables)
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (let loop ((i 0))
	    (unless (>=fx i call/cc-nb-while-counters)
	       (let ((name (call/cc-counter-name i)))
		  (template-display p
		     ;; if a counter-var is in the state, then we will enter
		     ;; the while during restoration and thus increment it at
		     ;; that time. For simplicity we will decrement _all_
		     ;; loop-variables before "jumping" to the target. Therefore
		     ;; we  assign "1" to those counters that are not yet
		     ;; active. They will then be decrement as well (but not
		     ;; incremented again during the jump).
		     "~a = sc_callCcState.~a || 1;\n" name name))
	       (loop (+fx i 1))))))

   (define (declare/reset-counter-variables)
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (if (zerofx? call/cc-nb-while-counters)
	     (template-display p "/*do nothing*/\n")
	     (let loop ((i 0))
		(unless (>=fx i call/cc-nb-while-counters)
		   (template-display p
		      "var ~a = 0;\n" (call/cc-counter-name i))
		   (loop (+fx i 1)))))))
   
   (let ((storage (scm2js-global "CALLCC_STORAGE")))
      (template-display p
	 "var sc_callCcTmp;\n"
	 "var sc_callCcIndex = 0;\n"
	 "var sc_callCcState = false;\n"
	 "if (~a" storage "['doCall/CcDefault?']) {\n"
	 "  ~e" (declare/reset-counter-variables)
	 "} else if (~a" storage "['doCall/CcRestore?']) {\n"
	 "  var sc_callCcFrame;\n"
	 "  sc_callCcState = ~a" storage ".pop();\n"
	 "  sc_callCcIndex = sc_callCcState.sc_callCcIndex;\n"
	 "  ~e" (restore-counter-variables)
	 "  ~e" (restore-frames)
	 "  sc_callCcTmp = ~a" storage ".callNext();\n"
	 "  ~e" (decrement-counter-variables)
	 "  ~e" (do-hoisted)
	 "} else { // root-fun\n"
	 "  return sc_callCcRoot(this, arguments);\n"
	 "}\n")))

;*---------------------------------------------------------------------*/
;*    call/cc-end-code ...                                             */
;*---------------------------------------------------------------------*/
(define (call/cc-end-code p)

      ;; if the was already an old state, copy the relevant frames from the old
   ;; one into the new one.
   (define (save-frames)
      (define (save-frame scope)
	 ;; if this is a while-frame always save the counter-id
	 (let* ((frame-counter-id (with-access::Call/cc-Scope-Info scope (counter-id-nb) counter-id-nb))
		(frame-counter (and frame-counter-id
				    (call/cc-counter-name frame-counter-id))))
	    (when frame-counter
	       (template-display p
		  "sc_callCcState[~a] = ~a;\n"
		  frame-counter frame-counter)))
	 
	 (let* ((frame-name (call/cc-frame-name scope))
		(call/cc? (with-access::Out-Env env (call/cc?) call/cc?)) ;; and not just suspend
		(while (with-access::Call/cc-Scope-Info scope (surrounding-while) surrounding-while))
		(while-nb (and while (with-access::While while (call/cc-counter-nb) call/cc-counter-nb)))
		(counter (and while-nb (call/cc-counter-name while-nb)))
		(vars (with-access::Call/cc-Scope-Info scope (vars) vars)))
	    (template-display p
	       "if (sc_old_callCcState && "
	       (?? (and call/cc? counter)
		  "sc_old_callCcState.~a " counter " === ~a" counter " && ")
	       "    sc_old_callCcState[~a" frame-name"]) {\n"
	       "  sc_callCcState[~a" frame-name "] = "
	       "                sc_old_callCcState[~a" frame-name"];\n"
	       ;; no need to update the variables. If this was necessary it
	       ;; will happen in the finally clause.
	       "} else {\n"
	       "  sc_callCcState[~a" frame-name "] = { ~e };\n"
	       "}\n"
	       (separated ", "
		  (lambda (var)
		     "~a: ~a"
		     (with-access::Named-Var var (js-id) js-id)
		     (with-access::Named-Var var (js-id) js-id))
		  vars))))
      
      (define (conditional-save-frame scope)
	 (with-access::Lambda this (call/cc-nb-indices)
	    (let* ((indices (with-access::Call/cc-Scope-Info scope (indices) indices))
		   (sorted (sort <fx indices))
		   (continuous? (let loop ((i (car sorted))
					   (l (cdr sorted)))
				   (cond
				      ((null? l) #t)
				      ((=fx (+fx i 1) (car l))
				       (loop (car l) (cdr l)))
				      (else #f))))
		   (len (length sorted))
		   (single? (=fx len 1))
		   (always? (and continuous? (=fx len call/cc-nb-indices))))
	       (cond
		  (always? (save-frame scope))
		  (single?
		   (template-display p
		      "if (sc_callCcIndex === ~a) {\n" (car sorted)
		      "  ~e" (save-frame scope)
		      "}\n"))
		  ((and continuous? (=fx (car sorted) 1))
		   (template-display p
		      "if (sc_callCcIndex <= ~a) {\n"
		      (car (last-pair sorted))
		      "  ~e" (save-frame scope)
		      "}\n"))
		  ((and continuous?
			(=fx (car (last-pair sorted)) call/cc-nb-indices))
		   (template-display p
		      "if (sc_callCcIndex >= ~a) {\n"
		      (car sorted)
		      "  ~e" (save-frame scope)
		      "}\n"))
		  (continuous?
		   (template-display p
		      "if (sc_callCcIndex >= ~a && sc_callCcIndex <= ~a) {\n"
		      (car sorted) (car (last-pair sorted))
		      "  ~e" (save-frame scope)
		      "}\n"))
		  (else
		   (template-display p
		      "switch (sc_callCcIndex) {\n"
		      "  ~e\n" (each (lambda (i) "case ~a: " i) indices)
		      "  ~e"   (save-frame scope)
		      "}\n"))))))
      
      (with-access::Lambda this (call/cc-contained-scopes)
	 (for-each conditional-save-frame call/cc-contained-scopes)))

   (define (finally-updates)
      (define (vars-update scope)
	 (with-access::Call/cc-Scope-Info scope (finally-vars)
	    (let ((frame-name (call/cc-frame-name scope)))
	       (template-display p
		  ;; note: the '=' is intentional (and should not be a '===')
		  "if (sc_callCcFrame = sc_callCcState[~a" frame-name "]) {\n"
		  "  ~e"
		  "}\n"
		  (each (lambda (var)
			   "sc_callCcFrame.~a = ~a;\n"
			   (with-access::Named-Var var (js-id) js-id)
			   (with-access::Named-Var var (js-id) js-id))
		     finally-vars)))))
      
      (with-access::Lambda this (call/cc-finally-scopes)
	 (unless (null? call/cc-finally-scopes)
	    (template-display p
	       "finally {\n"
	       "  if (sc_callCcState) {\n"
	       "    ~e" (for-each vars-update call/cc-finally-scopes)
	       "  }\n"
	       "}"))))
   
   (template-display p
      "catch (sc_e) {\n"
      "  if (sc_e instanceof sc_CallCcException && sc_e.backup "
      "      && sc_callCcIndex !== 0) {\n" ;; if 0 then it was tail-call
      "    var sc_old_callCcState = sc_callCcState;\n"
      "    sc_callCcState = {};\n"
      "    ~e" (save-frames)
      "    sc_callCcState.sc_callCcIndex = sc_callCcIndex;\n"
      "    sc_callCcState.this_ = this;\n"
      "    sc_callCcState.callee = arguments.callee;\n"
      "    sc_e.push(sc_callCcState);\n"
      "  }\n"
      "  throw sc_e;\n"
      "} ~e" (finally-updates)))
   
;*---------------------------------------------------------------------*/
;*    call/cc-while-body-out ...                                       */
;*---------------------------------------------------------------------*/
(define (call/cc-while-body-out this env p)
   
   (define (finally-outs scope)
      (with-access::Call/cc-Scope-Info scope (finally-vars)
	 (let ((frame-name (call/cc-frame-name scope))
	       (counter (call/cc-counter-name
			   (with-access::While this (call/cc-counter-nb) call/cc-counter-nb))))
	    (template-display p
	       "if (sc_callCcState.~a === ~a ? sc_callCcState[~a] : false) {\n"
	       ;; state.counter === counter && state.frame
	       counter counter frame-name
	       "  ~e"
	       "}\n"
	       (each (lambda (var)
			"sc_callCcState[~a].~a = ~a;\n"
			;; state.frame.var-id = var-id;
			frame-name
			(with-access::Named-Var var (js-id) js-id)
			(with-access::Named-Var var (js-id) js-id))
		  finally-vars)))))
   
   (with-access::While this (body call/cc? call/cc-finally-scopes
			       call/cc-counter-nb dstposition)
      (with-access::Out-Env env (host-compiler host-register source-map?)
	 (when source-map?
	    (set! dstposition (output-port-position p))))
      (template-display p
	 (?? call/cc? "~a++;\n" (call/cc-counter-name call/cc-counter-nb))
	 (?@ (not (null? call/cc-finally-scopes))
	    "try {"
	    "  ~@"
	    "} finally {"
	    "  if (sc_callCcState) {"
	    "    ~e"
	    "  }"
	    "}\n"
	    (for-each finally-outs call/cc-finally-scopes)))))
