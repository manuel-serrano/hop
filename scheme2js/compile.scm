(module compile
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (option (loadq "protobject-eval.sch"))
   (export (compile tree::pobject p))
   (import protobject
	   gen-js
	   mutable-strings
	   config
	   nodes
	   var
	   mark-statements
	   locals
	   verbose
	   allocate-names
	   compile-optimized-call
	   compile-optimized-boolify
	   compile-optimized-set))

;; TODO: not thread-save. but doesn't really matter

;; MS 19 Jan 2008: Changed the hashtable for a vector (hey, after level 50
;; it does matter how we indent)
;; Accessing a thread specific variable is very expensive. Hence, a solution
;; should be found for replacing (config 'indent) with a global variable
;; (anyhow, I don't understand why *this* property is thread dependent,
;; isn't it a global configuration?).
(define *indentation* 0)
(define *indent-max* 50)
(define *indent-vec*
   (let ((vec (make-vector (+fx *indent-max* 1))))
      (let loop ((i *indent-max*))
	 (if (=fx i -1)
	     vec
	     (let ((margin (make-string (*fx i 2) #\space)))
		(vector-set! vec i margin)
		(loop (-fx i 1)))))))
      
;; indentation is not thread-safe, but we want to avoid catastrophies.
(define *indent-lock* (make-mutex))

(define (compile tree::pobject p)
   (verbose "Compiling")
   (gen-var-names tree)
   (set! *indentation* 0)
   (gen-code tree p))


(define (gen-code tree p)
   (verbose "  generating code")
   (overload compile compile
	     (Node Module Node Const Var-ref Lambda Frame-alloc
		   Frame-push If Case Clause Set! Begin Call/cc-Resume
		   Call Call/cc-Call While Call/cc-Counter-Update Continue
		   Return Labelled Break Pragma)
	     (tree.compile p)))

(define *tmp-var* "sc_tmp") ;; can't conflict with generated names.

;; when using immutable values.
(define *symbol-prefix* "\\u1E9C")
(define *keyword-prefix* "\\u1E9D")

;; TODO: *last-line* is not thread-safe!
(define *last-line* 0)
(define *last-file* "")

(define (scm2js-globals-init p)
   (p-display p (indent) "var sc_globals = SC_SCM2JS_GLOBALS;\n"))
(define (scm2js-global global)
   (string-append "sc_globals." global))

;; TODO propose other global access as option.
; (define (scm2js-globals-init p)
;    'do-nothing)
; (define (scm2js-global global)
;    (string-append "SC_" global))

(define (indent)
   ;; CARE: MS 18 Jan 2008 !!!
   ;; It may happens that indentation is negative and
   ;; this produces a crash in Hop.
   (if (config 'indent)
       (with-lock *indent-lock*
	  (lambda ()
	     (if (<fx *indentation* *indent-max*)
		 (vector-ref *indent-vec* *indentation*)
		 (vector-ref *indent-vec* *indent-max*))))
       ""))

(define (indent++)
   (let ((conf-indent (config 'indent)))
      (if conf-indent
	  (let ((in (indent)))
	     (set! *indentation* (+ *indentation* conf-indent))
	     in)
	  "")))

(define (--indent)
   (let ((conf-indent (config 'indent)))
      (if conf-indent
	  (begin
	     (set! *indentation* (- *indentation* conf-indent))
	     (indent))
	  "")))

(define (indent--)
   (let ((conf-indent (config 'indent)))
      (if conf-indent
	  (let ((in (indent)))
	     (set! *indentation* (- *indentation* conf-indent))
	     in)
	  "")))

(define-macro (check-stmt-form node p . Lbody)
   `(if (statement-form? ,node)
	(let* ((loc/line (pfield ,node 'loc/line))
	       (file (and loc/line (cadr (car loc/line))))
	       (line (and loc/line (cdr loc/line)))
	       (file-changed? (and file (not (string=? *last-file* file))))
	       (line-changed? (and line (not (= *last-line* line)))))

	   (when (or file-changed? line-changed?)
	      (p-display ,p (indent++) "{\n"))
	   (when file-changed?
	      (p-display ,p (indent) "__sc_FILE=\"" (string-for-read file) "\";\n")
	      (if file (set! *last-file* file)))
	   (when line-changed?
	      (p-display ,p (indent) "__sc_LINE=" line ";\n")
	      (set! *last-line* line))
	   (p-display ,p (indent))
	   ,@Lbody
	   (p-display ,p ";")
	   (when (or file-changed? line-changed?)
	      (p-display ,p "\n" (--indent) "}"))
	   (p-display ,p "\n"))
	(begin
	   ,@Lbody)))
	

(define (compile-separated-list p els sep . Ldefault)
   (define (iter els sep default)
      (cond
	 ((null? els) (if default
			  (p-display p default)))
	 ;; last element is displayed verbatim
	 ((null? (cdr els))
	  ((car els).compile p))
	 ;; otherwise add "," between elements
	 (else
	  ((car els).compile p)
	  (p-display p sep)
	  (iter (cdr els) sep default))))
   (iter els sep (and (not (null? Ldefault)) (car Ldefault))))

(define-pmethod (Node-compile ignored)
   (error #f "forgot node-type: " (pobject-name this)))

(define (small-list/pair? l)
   (define (smaller? l n)
      (cond
	 ((< n 0) #f)
	 ((null? l) #t)
	 ((not (pair? l)) #t)
	 (else
	  (smaller? (cdr l) (- n 1)))))

   (smaller? l 5))

(define (compile-const const p)
   (cond
      ((null? const) (p-display p "null"))
      ((boolean? const) (if const
			    (p-display p "true")
			    (p-display p "false")))
      ((symbol? const)
       (p-display p "\"")
       (if (not (use-mutable-strings?))
	   (p-display p *symbol-prefix*))
       (p-display p const)
       (p-display p "\""))
      ((char? const)
       (p-display p "(new sc_Char(\"" (string-for-read (string const)) "\"))"))
      ((number? const)
       (p-display p "(" const ")"))
      ((string? const)
       (if (use-mutable-strings?)
	   (p-display p "(new sc_String(\"" (string-for-read const) "\"))")
	   (p-display p "\"" (string-for-read const) "\"")))
      ((vector? const)
       (p-display p "[")
       (let loop ((i 0))
	  (unless (>= i (vector-length const))
	     (if (not (= i 0))
		 (p-display p ", "))
	     (compile-const (vector-ref const i) p)
	     (loop (+ i 1))))
       (p-display p "]"))
      ((pair? const)
       (if (small-list/pair? const)
	   (begin
	      (p-display p "(new sc_Pair(")
	      (compile-const (car const) p)
	      (p-display p ",")
	      (compile-const (cdr const) p)
	      (p-display p "))"))
	   (begin
	      (p-display p "sc_list(")
	      (compile-const (car const) p)
	      (for-each (lambda (e)
			   (p-display p ", ")
			   (compile-const e p))
			(cdr const))
	      (p-display p ")"))))
      ((eq? const #unspecified) (p-display p "undefined"))
      ((keyword? const)
       (if (use-mutable-strings?)
	   (p-display p "(new sc_Keyword('" (keyword->string const) "'))")
	   (p-display p "\"" *keyword-prefix* (keyword->string const) "\"")))
      (else (error #f "forgot Const-type: " const))))
   
(define-pmethod (Const-compile p)
   (check-stmt-form
    this p
    (compile-const this.value p)))

(define-pmethod (Var-ref-compile p)
   (check-stmt-form
    this p
    (p-display p this.var.compiled)))

(define-pmethod (Module-compile p)
   (define (trampoline-start-code p)
      (when (config 'trampoline)
	 (p-display p (indent) "var sc_tailTmp;\n")
	 (p-display p (indent) (scm2js-global "TAIL_OBJECT") ".calls = "
		    (config 'max-tail-depth) ";\n")
	 (p-display p (indent) "var sc_funTailCalls = "
		    (config 'max-tail-depth) ";\n")))

   (define (exported-vars-code p)
      (unless (null? this.exported-vars)
	 (p-display p (indent) "/* Exported Variables */\n")
	 (for-each (lambda (var)
		      (p-display p (indent) "var " var.compiled ";\n"))
		   this.exported-vars)
	 (p-display p (indent) "/* End Exports */\n\n")))

   (exported-vars-code p)

   (if (or (config 'suspend/resume)
	   (config 'trampoline))
       (scm2js-globals-init p))
   (for-each (lambda (var)
		(unless var.exported?
		   (p-display p (indent) "var " var.compiled ";\n")))
	     this.declared-vars)

   (trampoline-start-code p)
   (this.body.compile p))

(define (call/cc-loop-counter index)
   (string-append "sc_callccLoopCounter" (number->string index) "_"))

(define (call/cc-frame scope)
   (string-append "'frame-" (symbol->string scope.id) "'"))

;; Llvalue/stmt?, if given, indicate, that this lambda should be assigned to
;; the given lvalue. stmt? is true, if we should treat this node, as if it was
;; a statement-node.
;; If there's a lvalue and it's not a stmt?, the return value of the compiled
;; code is unspecified. (ie, it doesn't anymore necessarily return the
;; lambda).
(define-pmethod (Lambda-compile p . Llvalue/stmt?/source-element?)
   (define (vaarg-code vaarg nb-args)
      (let ((L vaarg.compiled))
	 (p-display p (indent) "var " L " = null;\n")
	 (p-display p (indent++)
		    "for (var " *tmp-var* " = arguments.length - 1; "
		    *tmp-var* " >= " (number->string nb-args) "; "
		    *tmp-var* "--) {\n")
	 (p-display p (indent) L " = sc_cons(arguments[" *tmp-var* "], " L
		    ");\n")
	 (p-display p (--indent) "}\n")))

   (define (trampoline-start-code p)
      (let ((in (indent)))
	 (p-display
	  p
	  in
	  "var sc_tailTmp;\n"
	  in
	  "var sc_funTailCalls = " (scm2js-global "TAIL_OBJECT") ".calls;\n")))

   (define (var-names formals vars-ht)
      (append (map (lambda (n)
		      n.var.compiled)
		   formals)
	      (hashtable-key-list vars-ht)))
   
   (define (call/cc-start-code p
			       call/cc-scopes
			       nb-loop-counters
			       resume-indices/vars)
      (let ((in (indent)))
	 (p-display
	  p
	  in "var sc_callCcTmp;\n"
	  in "var sc_callCcIndex = 0;\n"
	  in "var sc_callCcState = false;\n")
	 (p-display p (indent++) "try {\n")
	 (p-display
	  p
	  (indent++) "if (" (scm2js-global "CALLCC_STORAGE")
	  "['doCall/CcDefault?']) {\n"))
      (if (> nb-loop-counters 0)
	  (for-each (lambda (index)
		       (p-display
			p (indent)
			"var " (call/cc-loop-counter index) " = 0;\n"))
		    (iota nb-loop-counters))
	  (p-display p (indent) "/* do nothing */\n"))
      (p-display
       p
       (--indent)
       "} else if (" (scm2js-global "CALLCC_STORAGE") "['doCall/CcRestore?'])")
      (p-display p (indent++) "{\n")
      (let ((in (indent)))
	 (p-display
	  p
	  in "var sc_callCcFrame;\n"
	  in "sc_callCcState = " (scm2js-global "CALLCC_STORAGE") ".pop();\n"
	  in "sc_callCcIndex = sc_callCcState.sc_callCcIndex;\n")
	 (for-each (lambda (index)
		      (let ((counter (call/cc-loop-counter index)))
			 (p-display
			  p in counter " = sc_callCcState[" counter "];\n")))
		   (iota nb-loop-counters)))
      (for-each
       (lambda (scope)
	  (let ((frame (call/cc-frame scope)))
	     (p-display
	      p (indent++) "if (sc_callCcFrame = sc_callCcState[" frame "]) {\n")
	     (for-each (lambda (var)
			  (p-display  p (indent) var.compiled " = "
				      "sc_callCcFrame." var.compiled ";\n"))
		       scope.call/cc-vars)
	     (p-display p (--indent) "}\n")))
       call/cc-scopes)
      (p-display
       p
       (indent)
       "sc_callCcTmp = " (scm2js-global "CALLCC_STORAGE") ".callNext();\n")
      (let ((filtered-indices/vars (filter cdr resume-indices/vars)))
	 (when (not (null? filtered-indices/vars))
	    (p-display
	     p
	     (indent) "switch (sc_callCcIndex) {\n")
	    (for-each (lambda (index/var)
			 (p-display
			  p
			  (indent) "case " (car index/var) ": "
			  (cdr index/var).compiled " = sc_callCcTmp; break;\n"))
		      filtered-indices/vars)
	    (p-display p (indent) "}\n")))
       (p-display
	p
	(--indent) "} else { // root-fun\n")
       (indent++)
       (p-display
	p
	(indent) "return sc_callCcRoot(this, arguments);\n")
       (p-display
	p
	(--indent)
	"}\n"))

   (define (call/cc-end-code p
			     call/cc-scopes
			     nb-loop-counters
			     finally-scopes)
      (p-display
       p
       (--indent) "} catch (sc_e) {\n")
      (indent++)
      (p-display
       p
       (indent++)
       "if (sc_e instanceof sc_CallCcException &&"
       " sc_e.backup && sc_callCcIndex !== 0) {\n") ;;if 0 then it was a tail-call.
      (p-display
       p
       (indent) "var sc_old_callCcState = sc_callCcState;\n"
       (indent) "sc_callCcState = new Object();\n")
      (for-each (lambda (index)
		   (let ((counter (call/cc-loop-counter index)))
		      (p-display
		       p (indent) "sc_callCcState[" counter "] = " counter ";\n")))
		(iota nb-loop-counters))
      (for-each
       (lambda (scope)
	  (p-display
	   p
	   (indent) "switch (sc_callCcIndex) {\n"
	   (indent))
	  (for-each (lambda (index) (p-display p "case " index ":"))
		    scope.call/cc-indices)
	  (p-display p "\n")
	  (indent++)
	  (if (and (config 'call/cc)
		   scope.surrounding-while)
	      (let* ((while scope.surrounding-while)
		     (counter (call/cc-loop-counter while.call/cc-loop-counter))
		     (frame (call/cc-frame scope)))
		 (p-display
		  p
		  (indent) "sc_callCcState[" frame "] = (sc_old_callCcState && "
		  "sc_old_callCcState[" counter "] == " counter " && "
		  "sc_callCcState[" frame "]) || {};\n"))
	      (let ((frame (call/cc-frame scope)))
		 (p-display
		  p (indent) "sc_callCcState[" frame "] = "
		  "(sc_old_callCcState && sc_old_callCcState[" frame "])"
		  " || {};\n")))
	  (p-display p (--indent) "}\n")) ;; switch
       call/cc-scopes)
      (for-each
       (lambda (scope)
	  (when scope.call/cc-indices
	     (let ((frame (call/cc-frame scope)))
		(p-display p (indent++) "if (sc_callCcFrame = sc_callCcState[" frame "]) {\n")
		(for-each
		 (lambda (var)
		    (p-display p (indent) "sc_callCcFrame." var.compiled
			       " = " var.compiled ";\n"))
		 scope.call/cc-vars)
		(p-display p (--indent) "}\n")))) ;; if
       call/cc-scopes)
      (p-display
       p
       (indent) "sc_callCcState.sc_callCcIndex = sc_callCcIndex;\n"
       ;; if we were a tail-call target undo this. (Call/cc removes the caller).
       (indent) "sc_callCcState.this_ ="
       "(this === " (scm2js-global "TAIL_OBJECT") ")? null: this;\n"
       (indent) "sc_callCcState.callee = arguments.callee;\n")
      (p-display
       p
       (indent) "sc_e.push(sc_callCcState);\n")
      (p-display
       p
       (--indent) "}\n") ;; if call/cc-exception
      (p-display
       p
       (indent) "throw sc_e;\n")
      (when (not (null? finally-scopes))
	 (p-display
	  p
	  (--indent) "} finally {\n")
	 (indent++)
	 (p-display
	  p
	  (indent++) "if (sc_callCcState) {\n")
	 (for-each
	  (lambda (scope)
	     (let ((frame (call/cc-frame scope)))
		(p-display
		 p
		 (indent++) "if (sc_callCcFrame = sc_callCcState[" frame "]) {\n")
		(for-each
		 (lambda (var)
		    (p-display p (indent) "sc_callCcFrame."
			       var.compiled " = " var.compiled ";\n"))
		 scope.finally-vars)
		(p-display p (--indent) "}\n")))
	  finally-scopes)
	 (p-display
	  p
	  (--indent) "}\n"))
      (p-display
       p
       (--indent) "}\n")) ;; try or finally

   (let* ((local-names-ht (make-hashtable))
	  (formals-w/o-vaarg/vaarg
	   (if this.vaarg?
	       (let* ((locals-copy (map (lambda (x) x)
					this.formals)))
		  (let loop ((copy locals-copy))
		     (cond
			((null? copy)
			 (error "compile"
				"internal compiler error. (vaarg)"
				'()))
			((null? (cdr copy))
			 (cons '() (car copy)))
			((null? (cddr copy))
			 (let ((vaarg (cadr copy)))
			    (set-cdr! copy '())
			    (cons locals-copy vaarg)))
			(else
			 (loop (cdr copy))))))
	       (cons this.formals #f)))
	  (formals-w/o-vaarg (car formals-w/o-vaarg/vaarg))
	  (vaarg (cdr formals-w/o-vaarg/vaarg))

	  (needs-trampoline? (and (config 'trampoline)
				  this.contains-tail-calls?))
	  (lvalue (and (not (null? Llvalue/stmt?/source-element?))
		       (car Llvalue/stmt?/source-element?)))
	  (stmt? (or (statement-form? this)
		     (and (not (null? Llvalue/stmt?/source-element?))
			  (cadr Llvalue/stmt?/source-element?))))
	  (source-element? (and (not (null? Llvalue/stmt?/source-element?))
				(caddr Llvalue/stmt?/source-element?)))
	  (declaration? (and #f
			     source-element?
			     lvalue
			     stmt?
			     
			     ;; if we optimize the var-number we might reuse
			     ;; variables. Don't play with that...
			     (not (config 'optimize-var-number))
			     
			     lvalue.var.constant? ;; not muted or anything
			     
			     ;; a declaration puts the assignment at the
			     ;; beginning of the scope. This is not good, if we
			     ;; want to access 'with' variables, that represent
			     ;; closures...
			     #f ;; TODO: correct this
			     ))
	  ;; a function can't be alone as statement. ie.
	  ;; function () {}; is not a valid statement.
	  ;; but
	  ;; (function () {}); is. So we wrap the function expression if we are
	  ;; in a statement. This only happens, if we don't assign to same
	  ;; lvalue.
	  ;;
	  ;; if we are an expression, and we need to do some assignments, we
	  ;; prefer parenthesis too. This happens, when we have a lvalue, or we
	  ;; are needing trampolines.
	  (needs-parenthesis? (or (and stmt?
				       (not needs-trampoline?)
				       (not lvalue))
				  (and (not stmt?)
				       (or lvalue
					   needs-trampoline?))))
	  
	  (needs-call/cc? (and this.body.call/cc-range
			       (not (null? this.body.call/cc-range))))
	  (needs-scm2js-globals? (or needs-trampoline? needs-call/cc?)))

      (when stmt? (p-display p (indent)))
      (when needs-parenthesis? (p-display p "("))
      
      (cond
	 ; 	 ;; if it's a declaration we write the beginning of it: 'function name('
	 ; 	 (declaration?
	 ; 	  (p-display p "function ")
	 ; 	  (lvalue.compile p)
	 ; 	  (p-display p "("))
	 ;; if it's an assignment: 'lval = function('
	 (lvalue
	  (lvalue.compile p)
	  (p-display p " = "))
	 (else
	  'do-nothing))
      
      (p-display p "function(")
      (compile-separated-list p formals-w/o-vaarg ", ")
      (p-display p ") {\n")
      (indent++)
      (when this.vaarg? (vaarg-code vaarg.var (- (length this.formals) 1)))
      (for-each (lambda (var) (p-display p (indent) "var " var.compiled ";\n"))
		this.declared-vars)
      (when needs-scm2js-globals? (scm2js-globals-init p))
      (when needs-trampoline? (trampoline-start-code p))
      (when needs-call/cc? (call/cc-start-code p
					       this.call/cc-scopes
					       (or this.call/cc-nb-counters 0)
					       (or this.body.call/cc-range '())))
      (when (and source-element?
		 (not needs-trampoline?)
		 (not needs-call/cc?))
	 (set! this.body.source-element? #t))
      
      (this.body.compile p)
      (when needs-call/cc? (call/cc-end-code p
					     this.call/cc-scopes
					     (or this.call/cc-nb-counters 0)
					     this.finally-scopes))
      
      ;; with the following p-display we close the function-body.
      (p-display p (--indent) "}")
      
      ;; close the previously opened parenthesis
      (when needs-parenthesis? (p-display p ")"))
      
      ;; add the stmt-semicolon. In theory we don't need any when it's a
      ;; function declaration, but this simplifies the code, and some editors
      ;; prefer the semicolon.
      (when stmt? (p-display p ";\n"))))

(define-pmethod (Frame-alloc-compile p)
    (p-display p "{") ;; literal-object creation
    (when (config 'with-closures)
       (let loop ((vars this.vars))
	  (cond
	     ((null? vars) 'done) ;; was empty to begin with.
	     ((null? (cdr vars))
	      (p-display p (car vars).compiled ": undefined"))
	     (else
	      (p-display p (car vars).compiled ": undefined, ")
	      (loop (cdr vars))))))
    (p-display p "}"))

(define-pmethod (Frame-push-compile p)
    (if (config 'with-closures)
	(begin
	   (p-display
	    p
	    (indent++) "with(" (car this.storage-vars).compiled ") {\n")
	   (this.body.compile p)
	   (p-display p (--indent) "}\n"))
	(begin
	   (check-stmt-form
	    this p
	    (if (statement-form? this) (p-display p "("))
	    (p-display p "function(")
	    (p-display p (car this.storage-vars).compiled)
	    (for-each (lambda (var)
			 (p-display p ", " var.compiled))
		      (cdr this.storage-vars))
	    (p-display p ") {\n")
	    (indent++)
	    (p-display p (indent) "return ")
	    (this.body.compile p)
	    (p-display p ";\n")
	    (p-display p (--indent) "}")
	    (if (statement-form? this) (p-display p ") "))
	    (p-display p "(")
	    (p-display p (car this.storage-vars).compiled)
	    (for-each (lambda (var)
			 (p-display p ", " var.compiled))
		      (cdr this.storage-vars))
		(p-display p ")\n")))))

(define-pmethod (If-compile p)
   (cond
      ((or this.call/cc-then-range
	   this.call/cc-else-range)
       ;; call/cc
       (cond
	  ((and this.call/cc-then-range
		this.call/cc-else-range)
	   (p-display p (indent++) "if (sc_callCcIndex && sc_callCcIndex <= "
		      (apply max (map car this.call/cc-then-range))
		      "    || (!sc_callCcIndex &&"))
	  (this.call/cc-then-range
	   (p-display p (indent++) "if (sc_callCcIndex || ("))
	  (else
	   (p-display p (indent++) "if (!sc_callCcIndex && (")))
       (compile-boolified p this.test)
       (p-display p ")) ")
       (this.then.compile p)
       (p-display p (--indent) "else\n")
       (indent++)
       (this.else.compile p)
       (indent--))
      ((statement-form? this)
       (p-display p (indent++) "if (")
       (compile-boolified p this.test)
       (p-display p ")\n")
       (this.then.compile p)
       (indent--)
       (unless (inherits-from? this.else (node 'Const)) ;; avoid common case 'undefined'
	  (p-display p (indent) "else\n")
	  (indent++)
	  (this.else.compile p)
	  (indent--)))
      (else
       (p-display p "(")
       (compile-boolified p this.test)
       (p-display p "?")
       (this.then.compile p)
       (p-display p ":")
       (this.else.compile p)
       (p-display p ")"))))

(define-pmethod (Case-compile p)
   (p-display p (indent) "switch (")
   (when this.call/cc-range
      (p-display p "sc_callCcIndex? sc_getCallCcIndexObject(sc_callCcIndex) :"))
   (this.key.compile p)
   (p-display p ") {\n")
   (for-each (lambda (clause)
		(clause.compile p))
	     this.clauses)
   (p-display p (indent) "}\n"))

(define-pmethod (Clause-compile p)
   (if this.default-clause?
       (begin
	  (p-display p (indent++) "default:\n")
	  (this.expr.compile p)
	  (p-display p (indent--) "break;\n"))
       (begin
	  (when this.call/cc-range
	     (for-each (lambda (index/var)
			  (let ((index (car index/var)))
			     ;; sc_storage can not be used as key, and will hence
			      ;; never match.
			     (p-display
			      p
			      (indent)
			      "case (sc_callCcIndex === " index "?"
			      "sc_getCallCcIndexObject(" index "):"
			      ;; storage can't clash with user values
			      (scm2js-global "CALLCC_STORAGE") "):\n")))
			this.call/cc-range))
	  (for-each (lambda (const)
		       (p-display p (indent) "case ")
		       (const.compile p)
		       (p-display p ":\n"))
		    this.consts)
	  (indent++)
	  (this.expr.compile p)
	  (p-display p (indent--) "break;\n"))))

(define-pmethod (Set!-compile p)
   (if (instance-of? this.val (node 'Lambda))
       (this.val.compile p this.lvalue (statement-form? this) this.source-element?)
       (check-stmt-form
	this p
	(compile-set! p this))))

(define-pmethod (Begin-compile p)
   ;; if only the first element has a call/cc range we don't need a switch, as
   ;; we are entering the first element anyways.
   (define (only-first-el-has-call/cc-range?)
      (not (any? (lambda (n)
		    (let ((call/cc-range n.call/cc-range))
		       (and call/cc-range
			    (not (null? call/cc-range)))))
		 (cdr this.exprs))))
   (cond
      ((and this.call/cc-range
	    (not (null? this.call/cc-range))
	    (not (only-first-el-has-call/cc-range?)))
       (p-display p (indent)   "switch (sc_callCcIndex) {\n")
       (p-display p (indent) "case 0:\n")
       (for-each (lambda (n)
		    (let ((call/cc-range n.call/cc-range))
		       (when (and call/cc-range (not (null? call/cc-range)))
			  (for-each (lambda (indx/var)
				       (p-display
					p
					(indent) "case " (car indx/var) ":\n"))
				    call/cc-range)))
		    (indent++)
		    (n.compile p)
		    (--indent))
		 this.exprs)
       (p-display p (indent) "}\n"))
      ((and (statement-form? this)
	    this.source-element?)
       (for-each (lambda (n)
		    (set! n.source-element? #t)
		    (n.compile p))
		 this.exprs))
      ((statement-form? this)
       (p-display p (indent++) "{\n")
       (for-each (lambda (n)
		    (n.compile p))
		 this.exprs)
       (p-display p (--indent) "}\n"))
      (else
       (p-display p "(")
       (compile-separated-list p this.exprs ", ")
       (p-display p ")"))))

(define-pmethod (Call/cc-Resume-compile p)
   (p-display
    p
    (indent) "sc_callCcIndex = 0;\n"))

(define-pmethod (Call/cc-Call-compile p)
   (if (statement-form? this)
       (p-display p
		  (indent++) "{ sc_callCcIndex = " this.call/cc-index ";\n")
       (p-display p "(sc_callCcIndex = " this.call/cc-index ", "))
   (pcall this Call-compile p)
   (if (statement-form? this)
       (p-display p (--indent) "}\n")
       (p-display p ")")))

(define (tail-call-compile n p)
   (define (tail-call n tail-obj)
      (p-display p "(" tail-obj ".f =")
      (n.operator.compile p)
;      (p-display p ".call(" tail-obj)
      (p-display p "," tail-obj ".f(")
;      (if (not (null? n.operands))
;	  (p-display p ", "))
      (compile-separated-list p n.operands ", ")
      (p-display p "))"))
   
   (p-display
    p
    "(\n"
    (indent) "  (this === (sc_tailTmp =" (scm2js-global "TAIL_OBJECT") "))?\n"
    (indent) "  ((!sc_funTailCalls)?\n"
    (indent) "    (sc_tailTmp = [")
   (compile-separated-list p n.operands ", ")
   (p-display
    p
    "],\n"
    (indent) "    sc_tailTmp.callee = ")
   (n.operator.compile p)
   (p-display
    p
    ",\n"
    (indent) "    new sc_Trampoline(sc_tailTmp, " (config 'max-tail-depth) "))\n"
    (indent) "    :\n"
    (indent) "    (this.calls = sc_funTailCalls - 1,\n"
    (indent) "     ")
   (tail-call n "sc_tailTmp")
   (p-display
    p
    ")\n"
    (indent) "):(\n"
    (indent) "    sc_tailTmp.calls = " (config 'max-tail-depth) ",\n"
    (indent) "    sc_tailTmp = ")
   (tail-call n "sc_tailTmp") ;(scm2js-global "TAIL_OBJECT"))
   (p-display
    p
    ",\n"
    (indent) "    ((typeof sc_tailTmp === 'object' &&"
    "sc_tailTmp instanceof sc_Trampoline)?\n"
    (indent) "       sc_tailTmp.restart() :\n"
    (indent) "       sc_tailTmp)\n"
    (indent) "  )\n"
    (indent) ")"))

(define-pmethod (Call-compile p)
   (check-stmt-form
    this p
    (if this.tail-call?
	(tail-call-compile this p)
	(let ((operator this.operator))
	   (unless (and (config 'optimize-calls)
			(compile-optimized-call p operator this.operands))
	      (p-display p "(")
	      ;; if the operator is a var-ref and contains a "." we have to do
	      ;; some tricks: frame.f() would give 'frame' to f as this-object.
	      ;; by using the sequence-construct we can avoid that:
	      ;; (0,frame.f)() sends the global-object as 'this'-object.
	      (if (and (inherits-from? operator (node 'Var-ref))
		       (string-index operator.var.compiled #\.))
		  (p-display p "(0," operator.var.compiled ")")
		  (operator.compile p))
	      (p-display p "(")
	      (compile-separated-list p this.operands ", ")
	      (p-display p ")")
	      (p-display p ")"))))))

(define-pmethod (While-compile p)
   (let ((needs-call/cc? this.call/cc-range)
	 (needs-loop-var-call/cc? this.call/cc-loop-index))

      (define (compile-test)
	 (p-display p (indent++) "while (")
	 (if (and (inherits-from? this.test (node 'Const))
		  (eq? this.test.value #t))
	     (p-display p "true")
	     (begin
		(if needs-call/cc?
		    (p-display p "sc_callCcIndex || ("))
		(compile-boolified p this.test)
		(if needs-call/cc? (p-display p ")"))))
	 (p-display p ") "))
      
      (define (compile-body)
	 (p-display p "{\n")
	 (this.body.compile p)
	 (p-display p (--indent) "}\n"))
      
      (compile-test)
      (if (or (not (config 'call/cc))
	      (not needs-loop-var-call/cc?))
	  (compile-body)
	  (let* ((index this.call/cc-loop-index)
		 (counter (call/cc-loop-counter index))
		 (finally-scopes this.finally-scopes))
	     (p-display
	      p
	      (indent++) "{\n")
	     (unless (null? finally-scopes)
		(p-display
		 p
		"try {\n"))
	     (compile-body)
	     (unless (null? finally-scopes)
		(p-display
		 p
		 (--indent)
		 "} finally {\n")
		(indent++)
		(p-display
		 p
		 (indent++)
		 "if (sc_callCcState) {\n")
		(for-each
		 (lambda (scope)
		    (let ((frame (call/cc-frame scope)))
		       (p-display
			p
			(indent++) "if (" frame "." counter " === " counter " &&"
			" sc_callCcState[" frame "]) {\n")
		       (for-each
			(lambda (var)
			   (p-display p (indent) "sc_callCcFrame[" frame "]."
				      var.compiled-name " = " var.compiled ";\n"))
			scope.finally-vars)
		       (p-display p (--indent) "}\n")))
		 finally-scopes)
		(p-display p (--indent) "}\n") ;; if sc_callCcState
		(p-display p (--indent) "}\n")) ;; finally
	     (p-display p (--indent) "}\n")))))

			  
(define-pmethod (Call/cc-Counter-Update-compile p)
   (check-stmt-form
    this p
    (p-display p (call/cc-loop-counter this.index) "++")))

(define-pmethod (Continue-compile p)
   ;; TODO: care about label?
   (check-stmt-form
    this p
    (p-display p "continue")))

(define-pmethod (Return-compile p)
   (check-stmt-form
    this p
    (p-display p "return ")
    (this.val.compile p)))

(define-pmethod (Labelled-compile p)
   (p-display p (indent++) (mangle-JS-sym this.label.id) ":{\n")
   (this.body.compile p)
   (p-display p (--indent) "}\n"))

(define-pmethod (Break-compile p)
   ;; TODO: should never happen. Verify.
   (when this.val
      (p-display p (indent++) "{\n")
      (this.val.compile p))
   (if this.label
       (p-display p (indent) "break " (mangle-JS-sym this.label.id) ";\n")
       (p-display p (indent) "break;\n"))
   (when this.val
      (p-display p (--indent) "}\n")))

(define-pmethod (Pragma-compile p)
   (p-display p "(" this.str ")"))
