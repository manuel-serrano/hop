;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-09 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module out
   (cond-expand
      (enable-threads (library pthread)))
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   gen-js
	   mutable-strings
	   verbose
	   allocate-names
	   callcc
	   compile-optimized-call
	   compile-optimized-boolify
	   compile-optimized-set
	   template-display
	   pipe-port
	   js-pp
	   push-declarations)
   (static
    (class Pragmas
       lock
       pragmas::pair ;; will start with "dummy"-pair
       last-pragma::pair) ;; last in list
    (class Out-Env::Display-Env
	      trampoline?::bool
	      max-tail-depth::bint
	      suspend-resume?::bool
	      call/cc?::bool
	      optimize-calls?::bool
	      debug?::bool

	      pp?::bool
	      pragmas       ;; ::Pragmas or #f

	      last-line
	      last-file)
	   (wide-class Out-Lambda::Lambda
	      lvalue))
   (export (out tree::Module p)))

(define (out tree p)
   (verbose "Compiling")
   (gen-var-names tree)
   (push-var-declarations tree)
   (cond-expand
      ((not enable-threads)
       (config-set! 'pp #f)
       (when (>=fx (bigloo-debug) 1)
	  (warning "pretty-printing/compression only works when pthreads are enabled"))))
   (if (config 'pp)
       (pp-gen-code tree p)
       (gen-code tree p #f)))

(define (pp-gen-code tree p)
   (let* ((dummy-pair (list 'dummy))
	  (pragmas (instantiate::Pragmas
		     (lock (make-mutex))
		     (pragmas dummy-pair)
		     (last-pragma dummy-pair))))
      (receive (out-p in-p out-p-closer)
	 (open-pipe-port)
	 (let* ((compress? (config 'compress))
		(indent-width (let ((t (config 'indent)))
				 (if (and (fixnum? t)
					  (positive? t))
				     t
				     0)))
		(t (make-thread (lambda ()
				   (js-pp in-p
					  p
					  (lambda ()
					     (consume-next-pragma! pragmas))
					  compress?
					  indent-width)))))
	    (thread-start-joinable! t)
	    (gen-code tree out-p pragmas)
	    (out-p-closer)
	    (thread-join! t)))))
   
(define (gen-code tree p pragmas)
   (verbose "  generating code")
   (let ((env (instantiate::Out-Env
		 (indent (or (config 'indent) 0))
		 (indent-level 0)
		 (indent-str "")

		 (trampoline? (and (config 'trampoline) #t))
		 (max-tail-depth (config 'max-tail-depth))
		 (suspend-resume? (and (config 'suspend-resume) #t))
		 (call/cc? (and (config 'call/cc) #t))
		 (optimize-calls? (and (config 'optimize-calls) #t))
		 (debug? (and (config 'debug) #t))

		 (pp? (and (config 'pp) #t))
		 (pragmas pragmas)

		 (last-line 0)
		 (last-file ""))))
      (compile tree env p #t)))

(define *tmp-var* "sc_tmp") ;; can't conflict with generated names.

;; when using immutable values.
(define *symbol-prefix* "\\uEBAC")
(define *keyword-prefix* "\\uEBAD")

;; TODO propose other global access as option.
; (define (scm2js-globals-init p)
;    'do-nothing)
; (define (scm2js-global global)
;    (string-append "SC_" global))

(define (scm2js-globals-init p env)
   (template-display p env
      "var sc_globals = SC_SCM2JS_GLOBALS;\n"))
(define (scm2js-global global)
   (string-append "sc_globals." global))

(define (call/cc-frame-name scope)
   (with-access::Call/cc-Scope-Info scope (id)
      (string-append "'frame-" (symbol->string id) "'")))

(define (call/cc-counter-name nb)
   (string-append "sc_counter_" (number->string nb)))

(define-nmethod (Node.compile ignored ignored2)
   (error #f "Internal Error: forgot node-type" this))

;; return true, if we should use nested new sc_Pairs instead of sc_list(...).
(define (small-list/pair? l)
   (define (smaller? l n)
      (cond
	 ((< n 0) #f)
	 ((null? l) #t)
	 ((not (pair? l)) #t)
	 (else
	  (smaller? (cdr l) (- n 1)))))

   (smaller? l 5))

(define (my-string-for-read str)
   (let loop ((i 0)
	      (rev-str '()))
      (cond
	 ((= i (string-length str))
	  (apply string (reverse! rev-str)))
	 (else
	  (case (string-ref str i)
	     ((#\\ #\")
	      (loop (+fx i 1)
		    (cons* (string-ref str i) #\\ rev-str)))
	     ((#\return)  (loop (+fx i 1) (cons* #\r #\\ rev-str)))
	     ((#\newline) (loop (+fx i 1) (cons* #\n #\\ rev-str)))
	     ((#\null)    (loop (+fx i 1) (cons* #\0 #\0 #\0 #\0 #\u rev-str)))
	     (else
	      (loop (+fx i 1)
		    (cons (string-ref str i) rev-str))))))))

(define (compile-const const p env)
   (cond
      ((null? const) (template-display p env "null"))
      ((boolean? const)
       (template-display p env "~a" (if const "true" "false")))
      ((symbol? const)
       (template-display p env
	  "\"~?~a\""
	  (and (not (use-mutable-strings?)) *symbol-prefix*)
	  const))
      ((char? const)
       (template-display p env
	  "(new sc_Char(\"~a\"))" (my-string-for-read (string const))))
      ((number? const)
       ;; CARE: initially I had "($const)" here. and I suppose there was a
       ;; reason I put the parenthesis around. this will probably come back and
       ;; bite me...
       (template-display p env
	  (?@ (< const 0) "(~@)")
	  "$const"))
      ((string? const)
       (if (use-mutable-strings?)
	   (template-display p env
	      "(new sc_String(\"~a\"))" (my-string-for-read const))
	   (template-display p env
	      "\"~a\"" (my-string-for-read const))))
      ((vector? const)
       (template-display p env
	  "[~e]"
	  (let loop ((i 0))
	     (unless (>= i (vector-length const))
		(if (not (= i 0))
		    (template-display p env ", "))
		(compile-const (vector-ref const i) p env)
		(loop (+ i 1))))))
      ((pair? const)
       (if (small-list/pair? const)
	   (template-display p env
	      "(new sc_Pair(~e,~e))"
	      (compile-const (car const) p env)
	      (compile-const (cdr const) p env))
	   (template-display p env
	      "sc_list(~e)"
	      (separated ", " 
			 (lambda (e) "~e" (compile-const e p env))
			 const))))
      ((eq? const #unspecified) (template-display p env "undefined"))
      ((keyword? const)
       (if (use-mutable-strings?)
	   (template-display p env
	      "(new sc_Keyword(\"~a\"))" (keyword->string const))
	   (template-display p env
	      "\"~a~a\"" *keyword-prefix* (keyword->string const))))
      (else (error #f "Internal Error: forgot Const-type" const))))
   
(define-nmethod (Const.compile p stmt?)
   (with-access::Const this (value)
      (template-display p env
	 (?@ stmt? "~@;\n")
	 "~e" (compile-const value p env))))

(define-nmethod (Ref.compile p stmt?)
   (with-access::Ref this (var)
      (with-access::Named-Var var (js-id)
	 (template-display p env
	    (?@ stmt? "~@;\n")
	    "$js-id"))))

(define-nmethod (Module.compile p stmt?)
   ;; exported-vars
   (with-access::Module this (scope-vars)
      (unless (null? scope-vars)
	 (template-display p env
	    "/* Exported Variables */\n"
	    "~e" (for-each (lambda (var)
			      (with-access::Named-Var var (js-id)
				 (template-display p env
				    "var $js-id;\n")))
			   scope-vars)
	    "/* End Exports */\n\n")))

   ;; trampoline-code
   (when (Out-Env-trampoline? env)
      (scm2js-globals-init p env)
      (let ((tail-obj (scm2js-global "TAIL_OBJECT"))
	    (max-tail-depth (Out-Env-max-tail-depth env)))
	 [assert (max-tail-depth) (fixnum? max-tail-depth)]
	 (template-display p env
	    "var sc_tailTmp;\n"
	    "var sc_tailObj = ~a;\n" tail-obj
	    "~a.calls = ~a;\n" tail-obj max-tail-depth
	    "~a.MAX_TAIL_CALLs = ~a;\n" tail-obj max-tail-depth
	    "var sc_funTailCalls = ~a;\n"max-tail-depth)))

   ;; declared variables
   (with-access::Module this (declared-vars body)
      (for-each (lambda (var)
		   (unless (eq? (Var-kind var) 'exported)
		      (with-access::Named-Var var (js-id)
			 (template-display p env
			    "var $js-id;\n"))))
		declared-vars)

      ;; finally the body.
      (walk body p #t)))

(define-nmethod (Lambda.compile p stmt?)
   (widen!::Out-Lambda this
      (lvalue #f))
   (walk this p stmt?))

;; Llvalue/stmt?, if given, indicate, that this lambda should be assigned to
;; the given lvalue. stmt? is true, if we should treat this node, as if it was
;; a statement-node.
;; If there's a lvalue and it's not a stmt?, the return value of the compiled
;; code is unspecified. (ie, it doesn't anymore necessarily return the
;; lambda).
(define-nmethod (Out-Lambda.compile p stmt?)
   (define (vaarg-code vaarg nb-args)
      (with-access::Named-Var vaarg (js-id)
	 (template-display p env
	    "var $js-id = null;\n"
	    "for (var $*tmp-var* = arguments.length - 1;"
	    "     $*tmp-var* >= $(number->string nb-args);"
	    "     --$*tmp-var*) {\n"
	    "  $js-id = sc_cons(arguments[$*tmp-var*], $js-id);\n"
	    "}\n")))

   (define (trampoline-start-code)
      (template-display p env
	 "var sc_tailTmp;\n"
	 "var sc_tailObj;\n"
	 "var sc_funTailCalls = (sc_tailObj = ~a).calls;\n"
	 (scm2js-global "TAIL_OBJECT")))

   (define (declare/reset-counter-variables)
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (if (zerofx? call/cc-nb-while-counters)
	     (template-display p env "/*do nothing*/\n")
	     (let loop ((i 0))
		(unless (>=fx i call/cc-nb-while-counters)
		   (template-display p env
		      "var ~a = 0;\n" (call/cc-counter-name i))
		   (loop (+fx i 1)))))))

   (define (restore-counter-variables)
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (let loop ((i 0))
	    (unless (>=fx i call/cc-nb-while-counters)
	       (let ((name (call/cc-counter-name i)))
		  (template-display p env
		     ;; if a counter-var is in the state, then we will enter
		     ;; the while during restoration and thus increment it at
		     ;; that time. For simplicity we will decrement _all_
		     ;; loop-variables before "jumping" to the target. Therefore
		     ;; we  assign "1" to those counters that are not yet
		     ;; active. They will then be decrement as well (but not
		     ;; incremented again during the jump).
		     "~a = sc_callCcState.~a || 1;\n" name name))
	       (loop (+fx i 1))))))

   (define (restore-frames)
      (with-access::Lambda this (call/cc-contained-scopes)
	 (for-each
	  (lambda (scope)
	     (with-access::Call/cc-Scope-Info scope (vars)
		(unless (null? vars) ;; should only happen for lambda-frame
		   (let ((frame-name (call/cc-frame-name scope)))
		      (template-display p env
			 "if (sc_callCcFrame = sc_callCcState[~a]) {\n"
			 "  ~e"
			 "}\n"
			 frame-name
			 (each (lambda (var)
				  "~a = sc_callCcFrame.~a;\n"
				  (Named-Var-js-id var) (Named-Var-js-id var))
			       vars))))))
	  call/cc-contained-scopes)))

   (define (do-hoisted)
      (define (emit-commands rev-hoisted)
	 (if (null? rev-hoisted)
	     (template-display p env "sc_callCcTmp")
	     (match-case (car rev-hoisted)
		((set! ?v)
		 (template-display p env
		    "~a = ~e"
		    (Named-Var-js-id v)
		    (emit-commands (cdr rev-hoisted))))
		((return)
		 (template-display p env
		    "return ~e" (emit-commands (cdr rev-hoisted))))
		(else
		 (error "out"
			"Internal Error: forgot to implement hoisted command"
			(car rev-hoisted))))))

      (with-access::Lambda this (call/cc-hoisted)
	 (unless (null? call/cc-hoisted)
	    (template-display p env
	       "switch (sc_callCcIndex) {\n"
	       "  ~e"
	       "}\n"
	       (each (lambda (index/rev-hoisted)
			"case ~a: ~e; break;\n"
			(car index/rev-hoisted)
			(emit-commands (cdr index/rev-hoisted)))
		     call/cc-hoisted)))))

   (define (decrement-counter-variables)
      ;; by decrementing all counter-variables, we can increment the variable
      ;; unconditionally at the beginning of whiles, thus removing an 'if'.
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (let loop ((i 0))
	    (unless (>=fx i call/cc-nb-while-counters)
	       (template-display p env
		  "~a--;\n" (call/cc-counter-name i))
	       (loop (+fx i 1))))))
   
   (define (call/cc-start-code)
      (let ((storage (scm2js-global "CALLCC_STORAGE")))
	 (template-display p env
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

   (define (finally-updates)
      (define (vars-update scope)
	 (with-access::Call/cc-Scope-Info scope (finally-vars)
	    (let ((frame-name (call/cc-frame-name scope)))
	       (template-display p env
		  ;; note: the '=' is intentional (and should not be a '===')
		  "if (sc_callCcFrame = sc_callCcState[~a" frame-name "]) {\n"
		  "  ~e"
		  "}\n"
		  (each (lambda (var)
			   "sc_callCcFrame.~a = ~a;\n"
			   (Named-Var-js-id var) (Named-Var-js-id var))
			finally-vars)))))

      (with-access::Lambda this (call/cc-finally-scopes)
	 (unless (null? call/cc-finally-scopes)
	    (template-display p env
	       "finally {\n"
	       "  if (sc_callCcState) {\n"
	       "    ~e" (for-each vars-update call/cc-finally-scopes)
	       "  }\n"
	       "}"))))

   ;; if the was already an old state, copy the relevant frames from the old
   ;; one into the new one.
   (define (save-frames)
      (define (save-frame scope)
	 ;; if this is a while-frame always save the counter-id
	 (let* ((frame-counter-id (Call/cc-Scope-Info-counter-id-nb scope))
		(frame-counter (and frame-counter-id
				    (call/cc-counter-name frame-counter-id))))
	    (when frame-counter
	       (template-display p env
		  "sc_callCcState[~a] = ~a;\n"
		  frame-counter frame-counter)))

	 (let* ((frame-name (call/cc-frame-name scope))
		(call/cc? (Out-Env-call/cc? env)) ;; and not just suspend
		(while (Call/cc-Scope-Info-surrounding-while scope))
		(while-nb (and while (While-call/cc-counter-nb while)))
		(counter (and while-nb (call/cc-counter-name while-nb)))
		(vars (Call/cc-Scope-Info-vars scope)))
	    (template-display p env
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
			     (Named-Var-js-id var) (Named-Var-js-id var))
			  vars))))

      (define (conditional-save-frame scope)
	 (with-access::Lambda this (call/cc-nb-indices)
	    (let* ((indices (Call/cc-Scope-Info-indices scope))
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
		   (template-display p env
		      "if (sc_callCcIndex === ~a) {\n" (car sorted)
		      "  ~e" (save-frame scope)
		      "}\n"))
		  ((and continuous? (=fx (car sorted) 1))
		   (template-display p env
		      "if (sc_callCcIndex <= ~a) {\n"
		      (car (last-pair sorted))
		      "  ~e" (save-frame scope)
		      "}\n"))
		  ((and continuous?
			(=fx (car (last-pair sorted)) call/cc-nb-indices))
		   (template-display p env
		      "if (sc_callCcIndex >= ~a) {\n"
		      (car sorted)
		      "  ~e" (save-frame scope)
		      "}\n"))
		  (continuous?
		   (template-display p env
		      "if (sc_callCcIndex >= ~a && sc_callCcIndex <= ~a) {\n"
		      (car sorted) (car (last-pair sorted))
		      "  ~e" (save-frame scope)
		      "}\n"))
		  (else
		   (template-display p env
		      "switch (sc_callCcIndex) {\n"
		      "  ~e\n" (each (lambda (i) "case ~a: " i) indices)
		      "  ~e"   (save-frame scope)
		      "}\n"))))))

      (with-access::Lambda this (call/cc-contained-scopes)
	 (for-each conditional-save-frame call/cc-contained-scopes)))

   (define (call/cc-end-code)
      (template-display p env
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

   (define (split-formals/vaarg)
      (with-access::Lambda this (vaarg? formals)
	 (if (not vaarg?)
	     (values formals #f)
	     (let ((rev (reverse formals)))
		(values (reverse! (cdr rev))
			(car rev))))))
   
   (with-access::Out-Lambda this (lvalue declared-vars body vaarg? formals
					 call/cc? contains-trampoline-call?
					 location)
      (receive (formals-w/o-vaarg vaarg)
	 (split-formals/vaarg)

	 (let* ((declaration? (and #f ;; TODO: currently disabled as function
				   ;; declarations are only allowed at
				   ;; source-element positions. Not inside
				   ;; try/catch (for instance).
				   
				   ;source-element?
				   lvalue
				   stmt?
			     
				   (with-access::Ref lvalue (var)
				      (with-access::Var var (constant?)
					 constant?)) ;; not muted or anything
			     
				   ;; a declaration puts the assignment at the
				   ;; beginning of the scope. This is not good,
				   ;; if we want to access 'with' variables,
				   ;; that represent closures...
				   #f ;; TODO: correct this
			     ))
		;; a function can't be alone as statement. ie.
		;; function () {}; is not a valid statement.
		;; but
		;; (function () {}); is. So we wrap the function expression if
		;; we are in a statement. This only happens, if we don't assign
		;; to some lvalue.
		;;
		;; Note, that this only happens at top-level, where the last
		;; statement-value is returned.
		;;
		;; if we are an expression, and we need to do some assignments,
		;; we prefer parenthesis too. This happens, when we have a
		;; lvalue, or we are needing trampolines.
		(needs-parenthesis? (or (and stmt?
					     (not lvalue))
					(and (not stmt?)
					     lvalue)
					(Out-Env-debug? env))))
	    (template-display p env
	       (?@ stmt? "~@;\n")
	       (?@ needs-parenthesis? "(~@)")
	       (?@ (Out-Env-debug? env)
		   "~a=~@,~a.name=\"~a\",~a.location=\"~a\",~a.sc_arity=~a,~a"
		   *tmp-var*
		   *tmp-var* (cond
				((Ref? lvalue) (Var-id (Ref-var lvalue)))
				(lvalue        (let ((op (open-output-string)))
						  (walk lvalue op #f)
						  (close-output-port op)))
				(else          ""))
		   *tmp-var* location
		   *tmp-var* (if vaarg?
				 (negfx (length formals))
				 (length formals))
		   *tmp-var*)
	       (?@ lvalue "~e = ~@" (walk lvalue p #f))
	       (?@ #t "function(~e) {\n ~e ~@ }" ;; always.
		   (separated ","
			      (lambda (e) "~e" (walk e p #f))
			      formals-w/o-vaarg)
		   (when vaarg?
		      (with-access::Ref vaarg (var)
			 (vaarg-code var (- (length formals) 1)))))
	       (?? (or call/cc? contains-trampoline-call?)
		   "~e" (scm2js-globals-init p env))
	       (?? contains-trampoline-call? "~e" (trampoline-start-code))
	       (?@ call/cc?
		   "try {\n"
		   "  ~e"   (call/cc-start-code)
		   "  ~@"   ;; body
		   "} ~e\n" (call/cc-end-code))
	       "  ~e" ;; declared-vars
	       "  ~e" ;; body
	       (each (lambda (var)
			"var ~a;\n" (Named-Var-js-id var))
		     declared-vars)
	       (walk body p #t))))))

(define-nmethod (Frame-alloc.compile p stmt?)
   (template-display p env
      (?@ stmt? "~@;\n") ;; should never happen.
      "{~e}" ;; literal-object creation
      (with-access::Frame-alloc this (vars)
	 (unless (null? vars)
	    (let loop ((vars vars))
	       (with-access::Named-Var (car vars) (js-id)
		  (if (null? (cdr vars))
		      (template-display p env "$js-id : undefined")
		      (begin
			 (template-display p env "$js-id : undefined, ")
			 (loop (cdr vars))))))))))

(define-nmethod (Frame-push.compile p stmt?)
   (with-access::Frame-push this (body frame-allocs)
      (with-access::Frame-alloc (car frame-allocs) (storage-var)
	 (with-access::Named-Var storage-var (js-id)
	    (template-display p env
	       "with ($js-id) {\n"
	       "  ~e" (walk body p #t)
	       "}\n")))))

(define-nmethod (If.compile p stmt?)
   (with-access::If this (test then else)
      (cond
	 (stmt?
	  (template-display p env
	     "if (~e) {\n" (compile-boolified p env walk test)
	     "  ~e"        (walk then p #t)
	     "}\n"
	     (?? (not (Const? else)) ;; avoid common case 'undefined
		 "else {\n"
		 "  ~e"    (walk else p #t)
		 "}\n")))
	 ((and (Const? then)
	       (eq? (Const-value then) #t))
	  ;; should nearly never happen...
	  ;; usually (or X Y) is converted into:
	  ;;     (tmp = X, X!==false? tmp: Y)
	  (template-display p env
	     "(~e || ~e)"
	     (compile-boolified p env walk test)
	     (walk else p #f)))
	 ((and (Const? else)
	       (eq? (Const-value else) #f))
	  ;; happens more often.
	  (template-display p env
	     "(~e && ~e)"
	     (compile-boolified p env walk test)
	     (walk then p #f)))
	 (else
	  (template-display p env
	     "(~e? ~e: ~e)"
	     (compile-boolified p env walk test)
	     (walk then p #f)
	     (walk else p #f))))))

(define-nmethod (Case.compile p stmt?)
   (with-access::Case this (key clauses)
      (template-display p env
	 "switch (~e) {\n" (walk key p #f)
	 "  ~e"    (for-each (lambda (clause) (walk clause p #t)) clauses)
	 "}\n")))

(define-nmethod (Clause.compile p stmt?)
   (with-access::Clause this (default-clause? consts expr)
      (if default-clause?
	  (template-display p env
	     "default:\n"
	     "~e" (walk expr p #t)
	     "break;\n")
	  (template-display p env
	     ;; consts
	     "~e" (for-each (lambda (const)
			       (template-display p env
				  "case ~e:\n" (walk const p #f)))
			    consts)
	     ;; body
	     "~e" (walk expr p #t)
	     "break;\n"))))

(define-nmethod (Decl-Set!.compile p stmt?)
   [assert (stmt?) stmt?]
   (with-access::Set! this (lvalue val)
      (template-display p env
	 "var ~e;\n" (compile-unoptimized-set! p env walk this))))

(define-nmethod (Set!.compile p stmt?)
   (with-access::Set! this (lvalue val)
      (if (Lambda? val)
	  (begin
	     (widen!::Out-Lambda val
		(lvalue lvalue))
	     (walk val p stmt?))
	  (template-display p env
	     (?@ stmt? "~@;\n")
	     (?@ (not stmt?) "(~@)") ;; for now...
	     "~e" (compile-set! p env walk this)))))

(define-nmethod (Begin.compile p stmt?)
   (define (call/cc-begin-out)
      (with-access::Begin this (exprs call/cc-ranges)
	 (template-display p env
	    "switch (sc_callCcIndex) {\n"
	    " case 0:\n"
	    "  ~e"
	    "}\n"
	    (for-each (lambda (expr range)
			 (template-display p env
			    (?? (and range (not (null? range)))
				"~e\n" ;; the cases
				(each (lambda (index)
					 "case ~a: " index)
				      range))
			    "~e" (walk expr p #t)))
		      exprs
		      call/cc-ranges))))
   
   (with-access::Begin this (exprs call/cc? call/cc-ranges)
      (cond
	 ((and call/cc? (not (null? call/cc-ranges)))
	  (call/cc-begin-out))
	 (stmt?
	  ;; we do not need { }, as all statements that could contain blocks
	  ;; already have them (or do not need them).
	  (for-each (lambda (n) (walk n p #t))
		    exprs))
	 (else
	  (template-display p env
	     "(~e)"
	     (separated ", "
			(lambda (e) "~e" (walk e p #f))
			exprs))))))

(define-nmethod (Call/cc-Resume.compile p stmt?)
   (template-display p env
      (?@ stmt? "~@;\n")
      "sc_callCcIndex = 0"))

(define-nmethod (Call.compile p stmt?)
   (define (compile-operator)
      ;; if the operator is a var-ref and contains a "." we have to do
      ;; some tricks: frame.f() would give 'frame' to f as this-object.
      ;; by using the sequence-construct we can avoid that:
      ;; (0,frame.f)() sends the global-object as 'this'-object.
      (with-access::Call this (operator)
	 (if (and (Ref? operator)
		  (with-access::Ref operator (var)
		     (with-access::Named-Var var (js-id)
			(string-index js-id #\.))))
	     (with-access::Ref operator (var)
		(with-access::Named-Var var (js-id)
		   (template-display p env
		      "(0, $js-id)")))
	     (walk operator p #f))))

   (define (compile-operands)
      (with-access::Call this (operands)
	 (template-display p env
	    "~e"
	    (separated ", "
		       (lambda (operand) "~e" (walk operand p #f))
		       operands))))
      
   (define (compile-trampoline-call)
      (define (do-trampoline-call tail-obj)
	 (with-access::Call this (operator)
	    (template-display p env
	       "(~a.f = ~e," tail-obj (walk operator p #f)
	       " ~a.f(~e))"  tail-obj (compile-operands))))

      (let ((operator (Call-operator this))
	    (max-tail-depth (Out-Env-max-tail-depth env)))
	 (with-access::Call this (operator)
	    (template-display p env
	       "(this === sc_tailObj?\n"
	       "     (!sc_funTailCalls?\n"  ;; == 0
	       "          (this.args = [~e],"   (compile-operands)
	       "           this.f = ~e,"        (walk operator p #f)
	       "           this)\n"
	       "       :\n"
	       "          (this.calls = sc_funTailCalls - 1,\n"
	       "          ~e))\n"         (do-trampoline-call "this")
	       "  :\n"
	       "     (sc_tailObj.calls = ~a,\n" max-tail-depth
	       "      sc_tailTmp = ~e,\n" (do-trampoline-call "sc_tailObj")
	       "      (sc_tailObj === sc_tailTmp?"
	       "                 sc_tailObj.restart()"
	       "               : sc_tailTmp)))"))))

   (with-access::Call this (operator operands call/cc? call/cc-index
				     trampoline? location)
      (template-display p env
	 (?@ stmt? "~@;\n")
	 (?@ #t "(~@)") ;;  was (not stmt?). now always. even for stmt.
	 (?@ (and call/cc? call/cc-index)
	     "sc_callCcIndex = ~a, ~@" call/cc-index)
	 "~e"
	 (cond
	    ((and (Out-Env-optimize-calls? env)
		  (compile-optimized-call p env walk operator operands))
	     'do-nothing) ;; already printed
	    (trampoline?
	     (compile-trampoline-call))
	    ((and (Out-Env-debug? env)
		  (not (Out-Env-call/cc? env)))
	     (let ((len (length operands)))
		(template-display p env
		   "sc_arity_check(~e, ~a)(~e)"
		   (compile-operator) len (compile-operands))))
	    (else
	     (template-display p env
		"~e(~e)" (compile-operator) (compile-operands)))))))

(define-nmethod (While.compile p stmt?)
   (define (finally-outs scope)
      (with-access::Call/cc-Scope-Info scope (finally-vars)
	 (let ((frame-name (call/cc-frame-name scope))
	       (counter (call/cc-counter-name
			 (While-call/cc-counter-nb this))))
	    (template-display p env
	       "if (sc_callCcState.~a === ~a ? sc_callCcState[~a] : false) {\n"
	       ;; state.counter === counter && state.frame
	       counter counter frame-name
	       "  ~e"
	       "}\n"
	       (each (lambda (var)
			"sc_callCcState[~a].~a = ~a;\n"
			;; state.frame.var-id = var-id;
			frame-name (Named-Var-js-id var) (Named-Var-js-id var))
		     finally-vars)))))

   (define (body-out)
      (with-access::While this (body call/cc? call/cc-finally-scopes
				     call/cc-counter-nb)
	 (template-display p env
	    (?? call/cc? "~a++;\n" (call/cc-counter-name call/cc-counter-nb))
	    (?@ (not (null? call/cc-finally-scopes))
		"try {"
		"  ~@"
		"} finally {"
		"  if (sc_callCcState) {"
		"    ~e"
		"  }"
		"}\n"
		(for-each finally-outs call/cc-finally-scopes))
	    "~e" (walk body p #t))))

   (with-access::While this (init test body label call/cc?)
      (template-display p env
	 ;; init
	 (?? (not (Const? init)) "~e" (walk init p #t))
	 ;; label
	 (?? (not (eq? label (default-label)))
	     "~a:\n" (mangle-JS-sym (Label-id label))))

      (cond
	 ((and (Const? test)
	       (eq? (Const-value test) #t))
	  (template-display p env
	     "do {\n"
	     "  ~e" (body-out)
	     "} while (true);\n"))
	 ((not call/cc?)
	  (template-display p env
	     "while (~e) {\n" (compile-boolified p env walk test)
	     "  ~e"           (walk body p #t)
	     "}\n"))
	 (else
	  (template-display p env
	     "while (sc_callCcIndex || (~e)) {\n" (walk test p #f)
	     "  ~e"                               (body-out)
	     "}\n")))))
			  
(define-nmethod (Continue.compile p stmt?)
   (with-access::Continue this (label)
      (if (eq? label (default-label))
	  (template-display p env
	     "continue;\n")
	  (template-display p env
	     "continue ~a;\n" (mangle-JS-sym (Label-id label))))))

(define-nmethod (Return.compile p stmt?)
   (with-access::Return this (val)
      (template-display p env
	 "return ~e;\n" (walk val p #f))))

(define-nmethod (Labeled.compile p stmt?)
   (with-access::Labeled this (label body)
      (with-access::Label label (id)
	 (template-display p env
	    "~a: {\n" (mangle-JS-sym id)
	    "  ~e"    (walk body p #t)
	    "}\n"))))

(define-nmethod (Break.compile p stmt?)
   (with-access::Break this (label val)
      (with-access::Label label (id)
	 (template-display p env
	    "{\n"
	    "  ~e" (walk val p #t)
	    "  break ~a;\n" (if (eq? label (default-label))
				""
				(mangle-JS-sym id))
	    "}\n"))))

; (define-nmethod (Pragma.compile p stmt?)
;    (with-access::Pragma this (str)
;       (template-display p env
; 	 (?@ stmt? "~@;\n")
; 	 "(~a)" str)))


(define-nmethod (Pragma.compile p stmt?)
   (with-access::Pragma this (str)
      (with-access::Out-Env env (pp? pragmas)
	 (when pp?
	    (add-pragma! pragmas str))
	 (let ((to-be-displayed (if pp?
				    ;; placeholder
				    #a000
				    str)))
	    (template-display p env
	       (?@ stmt? "~@;\n")
	       "(~a)" to-be-displayed)))))

(define (add-pragma! pragmas::Pragmas p)
   (with-access::Pragmas pragmas (lock last-pragma)
      (with-lock lock
	 (lambda ()
	    (let ((tmp (list p)))
	       (set-cdr! last-pragma tmp)
	       (set! last-pragma tmp))))))

(define (consume-next-pragma! pragmas::Pragmas)
   (with-access::Pragmas pragmas (lock pragmas)
      (with-lock lock
	 (lambda ()
	    (when (null? (cdr pragmas))
	       ;; should never happen
	       (error "out"
		      "Internal Error: consume-pragma call without pragma"
		      #f))
	    (let ((res (cadr pragmas)))
	       (set! pragmas (cdr pragmas))
	       res)))))


