(module compile
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (include "compile-optimized-call.scm")
   (include "compile-optimized-boolify.scm")
   (include "compile-optimized-set.scm")
   (option (loadq "protobject-eval.sch"))
   (export (compile tree::pobject p)
	   (default-max-tail-call-depth))
   (import protobject
	   gen-js
	   config
	   nodes
	   var
	   mark-statements
	   locals
	   liveness
	   constants
	   verbose
	   allocate-names))

(define *max-tail-call-depth* 40)

(define (default-max-tail-call-depth)
   *max-tail-call-depth*)

(define (max-tail-call-depth)
   (or (config 'max-tail-depth)
       *max-tail-call-depth*))

(define (compile tree::pobject p)
   (verbose "Compiling")
   (liveness tree)
   (if (config 'optimize-consts)
       (constants! tree))

   (locals tree
	   #f)   ;; don't collect formals

   (gen-var-names tree)
   (gen-code tree p))


(define (gen-code tree p)
   (verbose "  generating code")
   (overload compile compile
	     (Node Program Module Node Const Var-ref Lambda
		   If Case Clause Set! Begin Call/cc-Resume
		   Call Call/cc-Call Tail-rec While Tail-rec-call Return
		   Closure-use Closure-with-use Closure-ref
		   Closure-alloc Labelled Break Pragma)
	     (tree.compile p)))

(define *tmp-var* "tmp") ;; can't conflict, as all vars are starting with 'sc_
(define *symbol-prefix* "\\u1E9C") ;; "\\u1E9D\\u1E9E\\u1E9F")

;; TODO: *last-line* is not thread-safe!
(define *last-line* 0)
(define *last-file* "")

(define (scm2js-globals-init p)
   (p-display p "var scm2js_globals = SC_SCM2JS_GLOBALS;\n"))
(define (scm2js-global global)
   (string-append "scm2js_globals." global))

;; TODO propose other global access as option.
; (define (scm2js-globals-init p)
;    'do-nothing)
; (define (scm2js-global global)
;    (string-append "SC_" global))


(define-macro (check-stmt-form node p . Lbody)
   `(if (statement-form? ,node)
	(let* ((loc/line (pfield ,node 'loc/line))
	       (file (and loc/line (cadr (car loc/line))))
	       (line (and loc/line (cdr loc/line)))
	       (file-changed? (and file (not (string=? *last-file* file))))
	       (line-changed? (and line (not (= *last-line* line)))))

	   (when (or file-changed? line-changed?)
	      (p-display ,p "{"))
	   (when file-changed?
	      (p-display ,p "__sc_FILE=\"" (string-for-read file) "\";")
	      (if file (set! *last-file* file)))
	   (when line-changed?
	      (p-display ,p "__sc_LINE=" line ";")
	      (set! *last-line* line))
	   ,@Lbody
	   (p-display ,p ";")
	   (when (or file-changed? line-changed?)
	      (p-display ,p "}"))
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
       (if (not (config 'mutable-strings))
	   (display *symbol-prefix* p))
       (p-display p const)
       (p-display p "\""))
      ((char? const)
       (p-display p "(new sc_Char(\"" (string-for-read (string const)) "\"))"))
      ((number? const)
       (p-display p "(" const ")"))
      ((string? const)
       (if (config 'mutable-strings)
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
       (p-display p "(new sc_Keyword('" (keyword->string const) "'))"))
      (else (error #f "forgot Const-type: " const))))
   
(define-pmethod (Const-compile p)
   (check-stmt-form
    this p
    (compile-const this.value p)))

(define-pmethod (Var-ref-compile p)
   (check-stmt-form
    this p
    ;; don't generate useless statement (like "x;")
    ;; this only happens when "this" is a Decl.
    (unless (statement-form? this)
       (p-display p this.var.compiled))))

(define-pmethod (Program-compile p)
   (this.body.compile p))

(define-pmethod (Module-compile p)
   (define (trampoline-start-code p)
      (when (config 'trampoline)
	 (p-display p "var sc_tailTmp;\n")
	 (p-display p (scm2js-global "TAIL_OBJECT") ".calls = " (max-tail-call-depth) ";\n")
	 (p-display p "var sc_funTailCalls = " (max-tail-call-depth)";\n")))
      
   (define (split-globals local-vars)
      ;; HACK; TODO: outer-vars are for now just global vars.
      ;;    should be all vars that are visible to the outside of the module
      ;;    maybe not. depends on 'semantics' of Module...
      (let ((outer-vars (make-eq-hashtable))
	    (module-vars (make-eq-hashtable)))
	 (hashtable-for-each local-vars
			     (lambda (var ignored)
				(hashtable-put! (if var.is-global?
						    outer-vars
						    module-vars)
						var
						#t)))
	 (cons outer-vars module-vars)))

   (let* ((outer/module-vars (if (or (config 'encapsulate-modules)
				   (config 'call/cc))
			       (split-globals this.local-vars)
			       (cons this.local-vars #f)))
	  (outer-vars (car outer/module-vars))
	  (outer-vars? (not (= 0 (hashtable-size outer-vars))))
	  (module-vars (cdr outer/module-vars))

	  (module-filter-fun this.fun)
	  (module-filter-port/close (module-filter-fun p))
	  (module-filter-port (car module-filter-port/close))
	  (module-filter-close (cdr module-filter-port/close))
	  
	  (encapsulated (if (or (config 'call/cc)
				(and (config 'encapsulate-modules)
				     '(HACK HACK HACK we need a body at the
					    moment cause we introduced a
					    'return in the
					    hack-encapsulation-pass))
				(and (and module-vars
					  (> (hashtable-size module-vars) 0))))
			    (let ((fun (new-node Lambda '() #f this.body)))
			       (set! fun.local-vars (or module-vars (make-eq-hashtable)))
			       (if this.indices/indicators
				   (set! fun.indices/indicators
					 this.indices/indicators))
			       fun)
			    #f)))
      (scm2js-globals-init p)
      (trampoline-start-code p)
      (set! this.body.source-element? #t)
      (if outer-vars?
	  (hashtable-for-each outer-vars
			      (lambda (var ignored)
				 (p-display module-filter-port
					    "var " var.compiled ";\n")
				 (set! var.encapsulated? #t))))
      (if encapsulated
	  (begin
	     (p-display module-filter-port "(")
	     (encapsulated.compile module-filter-port)
	     (p-display module-filter-port
			").call(this)\n"))
	  (this.body.compile module-filter-port))

      (module-filter-close module-filter-port)))


;; Llvalue/stmt?, if given, indicate, that this lambda should be assigned to
;; the given lvalue. stmt? is true, if we should treat this node, as if it was
;; a statement-node.
;; If there's a lvalue and it's not a stmt?, the return value of the compiled
;; code is unspecified. (ie, it doesn't anymore necessarily return the
;; lambda).
(define-pmethod (Lambda-compile p . Llvalue/stmt?/source-element?)
   (define (vaarg-code vaarg nb-args)
      (let ((L vaarg.compiled))
	 (p-display p "var " L " = null;\n"
		    "for (var " *tmp-var* " = arguments.length - 1;"
		    *tmp-var* ">=" (number->string nb-args) ";"
		    *tmp-var* "--) {\n")
	 (p-display p L " = sc_cons(arguments[" *tmp-var* "], " L ");\n}\n")))

   (define (trampoline-start-code p)
      (p-display
       p
       "var sc_tailTmp;\n"
       "var sc_funTailCalls = " (scm2js-global "TAIL_OBJECT") ".calls;\n"))

   (define (var-names formals vaarg vars-ht)
      (append (map (lambda (n)
		      n.var.compiled)
		   (if vaarg
		       (cons vaarg formals)
		       formals))
	      (hashtable-key-list vars-ht)))
   
   (define (call/cc-start-code p formals vaarg locals indices/vars indices/indicators)
      (let* ((formal-vars (map (lambda (n) n.var) formals))
	     (formal/vaarg-vars (if vaarg
				    (cons vaarg.var formal-vars)
				    formal-vars))
	     (local/formal-vars (append! (hashtable-key-list locals)
					 formal/vaarg-vars))
	     (local/formal-names (map (lambda (v)
					 v.compiled)
				      local/formal-vars)))
	 (p-display
	  p
	  "var sc_callCcTmp;\n"
	  "var sc_callCcIndex = 0;\n"
	  "var sc_callCcState;\n"
	  "var sc_callCcFrame = false;\n"
	  "if (" (scm2js-global "CALLCC_STORAGE") "['doCall/CcDefault?']) {\n")
	 (if indices/indicators
	     (let ((ht (make-eq-hashtable)))
		(for-each (lambda (index/indicator-indices)
			     (for-each (lambda (indicator-index)
					  (hashtable-put! ht indicator-index #t))
				       (cdr index/indicator-indices)))
			  indices/indicators)
		(hashtable-for-each ht
				    (lambda (indicator-index ignored)
				       (p-display
					p
					"    var " (call/cc-indicator-name
						indicator-index)
					" = false;\n"))))
	     (p-display p "   /* do nothing */\n"))
	 (p-display
	  p
	  "} else if (" (scm2js-global "CALLCC_STORAGE") "['doCall/CcRestore?']) {\n"
	  "  sc_callCcState = " (scm2js-global "CALLCC_STORAGE") ".pop();\n"
	  "  sc_callCcIndex = sc_callCcState.sc_callCcIndex;\n"
	  "  sc_callCcFrame = sc_callCcState.frame;\n")
	 (for-each (lambda (var-name)
		      (p-display
		       p
		       "  " var-name " = sc_callCcFrame." var-name ";\n"))
		   local/formal-names)
	 (p-display
	  p
	  "  try {\n"
	  "    sc_callCcTmp = "
	  (scm2js-global "CALLCC_STORAGE") ".callNext();\n")
	 
	 (when (not (null? indices/vars))
	    (p-display
	     p
	     "    switch (sc_callCcIndex) {\n")
	    (for-each (lambda (index/var)
			 (if (cdr index/var)
			     (p-display
			      p
			      "case " (car index/var) ": "
			      (cdr index/var).compiled " = sc_callCcTmp; break;")))
		      indices/vars)
	    (p-display
	     p
	     "    }\n"))

	 (when indices/indicators
	    (p-display
	     p
	     "    switch (sc_callCcIndex) {\n")
	    (for-each (lambda (index/indicator-indices)
			 (p-display
			  p
			  "      "
			  "case " (car index/indicator-indices) ":\n")
			 (for-each (lambda (indicator-index)
				      (p-display
				       p
				       "        "
				       (call/cc-indicator-name indicator-index)
				       " = true;\n"))
				   (cdr index/indicator-indices))
			 (p-display
			  p
			  "        break;\n"))
		      indices/indicators)
	    (p-display
	     p
	     "    }\n"))
	 (p-display
	  p
	  "  } catch(e) {\n"
	  "    if (e instanceof sc_CallCcException && e.backup) {\n"
	  "       e.push(sc_callCcState);\n"
	  "    }\n"
	  "    throw e;\n"
	  "  }\n"
	  "} else { // root-fun\n"
	  "  return sc_callCcRoot(this, arguments);\n"
	  "}\n"
	  "try {\n")))

   (define (call/cc-end-code p formals vaarg locals)
      (let* ((formal-vars (map (lambda (n) n.var) formals))
	     (formal/vaarg-vars (if vaarg
				    (cons vaarg.var formal-vars)
				    formal-vars))
	     (local/formal-vars (append! (hashtable-key-list locals)
					 formal/vaarg-vars))
	     (muted-locals-str (map (lambda (v) v.compiled)
				    (filter (lambda (v)
					       v.muted?)
					    local/formal-vars)))
	     (constant-locals-str (map (lambda (v) v.compiled)
				       (filter (lambda (v)
						  (not v.muted?))
					       local/formal-vars))))
	 (p-display
	  p
	  "} catch (e) {\n"
	  "  if (e instanceof sc_CallCcException &&"
	  " e.backup && sc_callCcIndex !== 0) {\n" ;; if 0 then it was a tail-call.
	  "    if (!sc_callCcFrame)\n"
	  "      sc_callCcFrame = new Object();\n"
	  "    sc_callCcState = new Object();\n"
	  "    sc_callCcState.sc_callCcIndex = sc_callCcIndex;\n"
	  "    sc_callCcState.frame = sc_callCcFrame;\n"
	  ;; if we were a tail-call target undo this. (Call/cc removes the caller).
	  "    sc_callCcState.this_ ="
	  "(this === " (scm2js-global "TAIL_OBJECT") ")? null: this;\n"
	  "    sc_callCcState.callee = arguments.callee;\n")
	 (for-each (lambda (var-name)
		      (p-display
		       p
		       "     sc_callCcFrame." var-name " = " var-name ";\n"))
		   constant-locals-str)
	 (p-display
	  p
	  "    e.push(sc_callCcState);\n"
	  "  }\n"
	  "  throw e;\n"
	  "}\n") ;; try
	 (when (not (null? muted-locals-str))
	    (p-display
	     p
	     "finally {\n"
	     "  if (sc_callCcFrame) {\n")
	    (for-each (lambda (var-name)
			 (p-display
			  p
			  "     sc_callCcFrame." var-name " = " var-name ";\n"))
		      muted-locals-str)
	    (p-display
	     p
	     "  }"
	     "}\n")))) ;; try

   (let* ((locals this.local-vars)
	  (local-vars-ht (make-hashtable))
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
			     
			     ;; call/cc has bad property of putting
			     ;; declarations into an anonymous function.
			     ;; the variable itself is however still accessible
			     ;; outside this function. -> we can't do
			     ;; declarations then.
			     (not lvalue.var.encapsulated?)
			     
			     lvalue.var.single-value ;; not muted or anything
			     
			     ;; a declaration puts the assignment at the
			     ;; beginning of the scope. This is not good, if we
			     ;; want to access 'with' variables, that represent
			     ;; closures...
			     (not this.references-tail-rec-variables?)))
	  
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
      
      ;; local-vars-ht will contain the var-names. by putting them into a
      ;; hashtable we remove the duplicates. Not very elegant, but it should work ;)
      (hashtable-for-each locals
			  (lambda (var ignored)
			     (hashtable-put! local-vars-ht var.compiled #t)))
      
      (if needs-parenthesis?
	  (p-display p "("))
      
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
      
      (compile-separated-list p this.formals ", ")
      (p-display p ") {\n")
      (if this.vaarg
	  (vaarg-code this.vaarg.var (length this.formals)))
      (hashtable-for-each local-vars-ht
			  (lambda (var ignored)
			     (p-display p "var " var ";\n")))
      (if needs-scm2js-globals?
	  (scm2js-globals-init p))
      (if needs-trampoline?
	  (trampoline-start-code p))
      (if needs-call/cc?
	  (call/cc-start-code p this.formals this.vaarg
			      locals
			      this.body.call/cc-range
			      this.indices/indicators))
      (if (and source-element?
	       (not needs-trampoline?)
	       (not needs-call/cc?))
	  (set! this.body.source-element? #t))
      (this.body.compile p)
      (if needs-call/cc?
	  (call/cc-end-code p this.formals this.vaarg locals))
      
      ;; with the following p-display we close the function-body.
      (p-display p "\n}\n")
      
      ;; close the previously opened parenthesis
      (if needs-parenthesis?
	  (p-display p ")"))
      
      ;; add the stmt-semicolon. In theory we don't need any when it's a
      ;; function declaration, but this simplifies the code, and some editors
      ;; prefer the semicolon.
      (if stmt?
	  (p-display p ";\n"))))

(define (compile-unoptimized-boolify p node)
   (p-display p "(")
   (node.compile p)
   (p-display p "!== false)"))
   
(define (compile-boolified p node)
   (if (config 'optimize-boolify)
       (compile-optimized-boolify p node)
       (compile-unoptimized-boolify p node)))

(define-pmethod (If-compile p)
   (cond
      ((or this.call/cc-then-range
	   this.call/cc-else-range)
       ;; call/cc
       (cond
	  ((and this.call/cc-then-range
		this.call/cc-else-range)
	   (p-display p
		      "if (sc_callCcIndex && sc_callCcIndex <= "
		      (apply max (map car this.call/cc-then-range))
		      "    || (!sc_callCcIndex &&"))
	  (this.call/cc-then-range
	   (p-display p "if (sc_callCcIndex || ("))
	  (else
	   (p-display p "if (!sc_callCcIndex && (")))
       (compile-boolified p this.test)
       (p-display p ")) ")
       (this.then.compile p)
       (p-display p "\nelse\n")
       (this.else.compile p))
      ((statement-form? this)
       (p-display p "if (")
       (compile-boolified p this.test)
       (p-display p ") ")
       (this.then.compile p)
       (unless (inherits-from? this.else (node 'Const)) ;; avoid common case 'undefined'
	  (p-display p "\nelse\n")
	  (this.else.compile p)))
      (else
       (p-display p "(")
       (compile-boolified p this.test)
       (p-display p "?\n")
       (this.then.compile p)
       (p-display p ":\n")
       (this.else.compile p)
       (p-display p ")"))))

(define-pmethod (Case-compile p)
   (p-display p "switch (")
   (when this.call/cc-range
      (p-display p "sc_callCcIndex? sc_getCallCcIndexObject(sc_callCcIndex) :"))
   (this.key.compile p)
   (p-display p ") {\n")
   (for-each (lambda (clause)
		(clause.compile p)
		(p-display p "\n"))
	     this.clauses)
   (p-display p "}\n"))

(define-pmethod (Clause-compile p)
   (if this.default-clause?
       (begin
	  (p-display p "default: ")
	  (this.expr.compile p)
	  (p-display p "\nbreak;"))
       (begin
	  (when this.call/cc-range
	      (for-each (lambda (index/var)
			   (let ((index (car index/var)))
			      ;; sc_storage can not be used as key, and will hence
			      ;; never match.
			      (p-display
			       p
			       "case (sc_callCcIndex === " index "?"
			       "sc_getCallCcIndexObject(" index "):"
			       ;; storage can't clash with user values
			       (scm2js-global "CALLCC_STORAGE") "):\n")))
			this.call/cc-range))
	  (for-each (lambda (const)
		       (p-display p "case ")
		       (const.compile p)
		       (p-display p ":\n"))
		    this.consts)
	  (this.expr.compile p)
	  (p-display p "\nbreak;"))))

(define (compile-unoptimized-set! p n)
   (p-display p "(")
   (n.lvalue.compile p)
   (p-display p " = ")
   (n.val.compile p)
   (p-display p ")"))

(define-pmethod (Set!-compile p)
   (if (instance-of? this.val (node 'Lambda))
       (this.val.compile p this.lvalue (statement-form? this) this.source-element?)
       (check-stmt-form
	this p
	(if (config 'optimize-set!)
	    (compile-optimized-set! p this)
	    (compile-unoptimized-set! p this)))))

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
       (p-display p
		  "switch (sc_callCcIndex) {\n"
		  "case 0:\n")
       (for-each (lambda (n)
		    (let ((call/cc-range n.call/cc-range))
		       (when (and call/cc-range (not (null? call/cc-range)))
			  (for-each (lambda (index/var)
				       (p-display p "case " (car index/var) ":\n"))
				    call/cc-range)))
		    (n.compile p))
		 this.exprs)
       (p-display p "}\n"))
      ((and (statement-form? this)
	    this.source-element?)
       (for-each (lambda (n)
		    (set! n.source-element? #t)
		    (n.compile p))
		 this.exprs))
      ((statement-form? this)
       (p-display p "\n{\n")
       (for-each (lambda (n)
		    (n.compile p))
		 this.exprs)
       (p-display p "}\n"))
      (else
       (p-display p "(")
       (compile-separated-list p this.exprs ", ")
       (p-display p ")"))))

(define-pmethod (Call/cc-Resume-compile p)
   (p-display
    p
    "sc_callCcIndex = 0;\n"))

(define-pmethod (Call/cc-Call-compile p)
   (if (statement-form? this)
       (p-display p "{ sc_callCcIndex = " this.call/cc-index ";\n")
       (p-display p "(sc_callCcIndex = " this.call/cc-index ", "))
   (pcall this Call-compile p)
   (if (statement-form? this)
       (p-display p "}\n")
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
    "  (this === (sc_tailTmp =" (scm2js-global "TAIL_OBJECT") "))?\n"
    "  ((!sc_funTailCalls)?\n"
    "    (sc_tailTmp = [")
   (compile-separated-list p n.operands ", ")
   (p-display
    p
    "],\n"
    "    sc_tailTmp.callee = ")
   (n.operator.compile p)
   (p-display
    p
    ",\n"
    "    new sc_Trampoline(sc_tailTmp, " (max-tail-call-depth) "))\n"
    "    :\n"
    "    (this.calls = sc_funTailCalls - 1,\n"
    "     ")
   (tail-call n "sc_tailTmp")
   (p-display
    p
    ")\n   ):(\n"
    "    sc_tailTmp.calls = " (max-tail-call-depth) ","
    "    sc_tailTmp = ")
   (tail-call n "sc_tailTmp") ;(scm2js-global "TAIL_OBJECT"))
   (p-display
    p
    ",\n"
    "    ((typeof sc_tailTmp === 'object' &&"
    "sc_tailTmp instanceof sc_Trampoline)?\n"
    "       sc_tailTmp.restart() :\n"
    "       sc_tailTmp)\n"
    "  )\n"
    ")\n"))

(define-pmethod (Call-compile p)
   (check-stmt-form
    this p
    (if this.tail-call?
	(tail-call-compile this p)
	(let ((operator this.operator))
	   (unless (and (config 'optimize-calls)
			(compile-optimized-call p operator this.operands))
	      (p-display p "(")
	      (operator.compile p)
	      (p-display p "(")
	      (compile-separated-list p this.operands ", ")
	      (p-display p ")")
	      (p-display p ")"))))))

(define-pmethod (Tail-rec-compile p)
   (let ((label (and this.label (mangle-JS-sym this.label))))
      (if label (p-display p label ": "))
      (p-display p "while (true) {")
      (this.body.compile p)
      (if label
	  (p-display p "break " label ";\n")
	  (p-display p "break;\n"))
      (p-display p "}\n")))

(define-pmethod (While-compile p)
   ;; while nodes shouldn't need their label.
   (p-display p "while (")
   (if this.call/cc-range
       (p-display p "sc_callCcIndex || "))
   (this.test.compile p)
   (p-display p ") {\n")
   (this.body.compile p)
   (p-display p "}\n"))

(define-pmethod (Tail-rec-call-compile p)
   (let ((label (and this.label (mangle-JS-sym this.label))))
      (if label
	  (p-display p "continue " label ";\n")
	  (p-display p "continue;\n"))))

(define-pmethod (Return-compile p)
   (p-display p "return ")
   (this.val.compile p)
   (p-display p ";\n"))

(define-pmethod (Closure-alloc-compile p)
   (p-display p "(new Object())"))

(define-pmethod (Closure-use-compile p)
   (define (p-display-closures p closures)
      (p-display p
		 (car closures).var.compiled)
      (for-each (lambda (closure)
		   (p-display p
			      ", "
			      closure.var.compiled))
		(cdr closures)))
   
   (check-stmt-form
    this p
    (p-display p "function(")
    (p-display-closures p this.closures)
    (p-display p ") { return ")
    (this.body.compile p)
    (p-display p "; }(")
    (p-display-closures p this.closures)
    (p-display p ")")))

(define-pmethod (Closure-with-use-compile p)
   (for-each (lambda (closure)
		(p-display
		 p
		 "with (" closure.var.compiled ") {\n"))
	     this.closures)
   (this.body.compile p)
   (for-each (lambda (closure)
		(p-display
		 p
		 "}"))
	     this.closures)
   (p-display p "\n"))

(define-pmethod (Closure-ref-compile p)
   (check-stmt-form
    this p
    (p-display p this.var.compiled)))
   
(define-pmethod (Labelled-compile p)
   (p-display p (mangle-JS-sym this.label) ":{\n")
   (this.body.compile p)
   (p-display p "\n}"))

(define-pmethod (Break-compile p)
   (p-display p "{\n")
   (this.val.compile p)
   (if this.labelled
       (p-display p "break " (mangle-JS-sym this.labelled.label) ";\n")
       (p-display p "break;\n"))
   (p-display p "}\n"))

(define-pmethod (Pragma-compile p)
   (p-display p "(" this.str ")"))
