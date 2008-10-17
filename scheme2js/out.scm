(module out
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   gen-js
	   mutable-strings
	   verbose
	   allocate-names
	   compile-optimized-call
	   compile-optimized-boolify
	   compile-optimized-set
	   template-display)
   (static (class Out-Env::Display-Env
	      trampoline?::bool
	      max-tail-depth
	      suspend-resume?::bool
	      call/cc?::bool
	      optimize-var-number?::bool
	      optimize-calls?::bool

	      last-line
	      last-file)
	   (wide-class Out-Lambda::Lambda
	      lvalue))
   (export (out tree::Module p)))

(define (out tree p)
   (verbose "Compiling")
   (gen-var-names tree)
   (gen-code tree p))


(define (gen-code tree p)
   (verbose "  generating code")
   (let ((env (instantiate::Out-Env
		 (indent (or (config 'indent) 0))
		 (indent-level 0)
		 (indent-str "")

		 (trampoline? (and (config 'trampoline) #t))
		 (max-tail-depth (config 'max-tail-depth))
		 (suspend-resume? (and (config 'suspend-resume) #t))
		 (call/cc? (and (config 'call/cc) #t))
		 (optimize-var-number? (and (config 'optimize-var-number?) #t))
		 (optimize-calls? (and (config 'optimize-calls) #t))

		 (last-line 0)
		 (last-file ""))))
      (compile tree env p #t)))

(define *tmp-var* "sc_tmp") ;; can't conflict with generated names.

;; when using immutable values.
(define *symbol-prefix* "\\uEBAC")
(define *keyword-prefix* "\\uEBAD")

(define (scm2js-globals-init p env)
   (template-display p env
      "var sc_globals = SC_SCM2JS_GLOBALS;\n"))
(define (scm2js-global global)
   (string-append "sc_globals." global))

;; TODO propose other global access as option.
; (define (scm2js-globals-init p)
;    'do-nothing)
; (define (scm2js-global global)
;    (string-append "SC_" global))

(define-nmethod (Node.compile ignored ignored2)
   (error #f "forgot node-type: " this))

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
	  "(new sc_Char(\"~a\"))" (string-for-read (string const))))
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
	      "(new sc_String(\"~a\"))" (string-for-read const))
	   (template-display p env
	      "\"~a\"" (string-for-read const))))
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
      (else (error #f "forgot Const-type: " const))))
   
(define-nmethod (Const.compile p stmt?)
   (with-access::Const this (value)
      (template-display p env
	 (?@ stmt? "~@;\n")
	 "~e" (compile-const value p env))))

(define-nmethod (Ref.compile p stmt?)
   (with-access::Ref this (var)
      (with-access::Var var (js-id)
	 (template-display p env
	    (?@ stmt? "~@;\n")
	    "$js-id"))))

(define-nmethod (Module.compile p stmt?)
   (define (exported-vars-code)
      (with-access::Module this (scope-vars)
	 (unless (null? scope-vars)
	    (template-display p env
	       "/* Exported Variables */\n"
	       "~e" (for-each (lambda (var)
				 (with-access::Var var (js-id)
				    (template-display p env
				       "var $js-id;\n")))
			      scope-vars)
	       "/* End Exports */\n\n"))))

   (exported-vars-code)

   (with-access::Module this (declared-vars body)
      (for-each (lambda (var)
		   (unless (Exported-Var? var)
		      (with-access::Var var (js-id)
			 (template-display p env
			    "var $js-id;\n"))))
		declared-vars)
      
      (walk body p #t)))

(define (call/cc-loop-counter index)
   (string-append "sc_callccLoopCounter" (number->string index) "_"))


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
      (with-access::Var vaarg (js-id)
	 (template-display p env
	    "var $js-id = null;\n"
	    "for (var $*tmp-var* = arguments.length - 1;"
	    "     $*tmp-var* >= $(number->string nb-args);"
	    "     --$*tmp-var*) {\n"
	    "  $js-id = sc_cons(arguments[$*tmp-var*], $js-id);\n"
	    "}\n")))

   (define (split-formals/vaarg)
      (with-access::Lambda this (vaarg? formals)
	 (if (not vaarg?)
	     (values formals #f)
	     (let ((rev (reverse formals)))
		(values (reverse! (cdr rev))
			(car rev))))))
   
   (with-access::Out-Lambda this
	 (lvalue declared-vars body vaarg? formals)
      (receive (formals-w/o-vaarg vaarg)
	 (split-formals/vaarg)

	 (let* ((declaration? (and #f ;; TODO: currently disabled.
				   ;source-element?
				   lvalue
				   stmt?
			     
				   ;; if we optimize the var-number we might reuse
				   ;; variables. Don't play with that...
				   (not (Out-Env-optimize-var-number? env))

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
					     lvalue))))

	    (template-display p env
	       (?@ stmt? "~@;\n")
	       (?@ needs-parenthesis? "(~@)")
	       (?@ lvalue "~e = ~@" (walk lvalue p #f))
	       "function(~e) {\n"
	       "  ~e" ;; vaarg
	       "  ~e" ;; declared-vars
	       "  ~e" ;; body
	       "}"
	       (separated ","
			  (lambda (e) "~e" (walk e p #f))
			  formals-w/o-vaarg)
	       (when vaarg? (with-access::Ref vaarg (var)
			       (vaarg-code var (- (length formals) 1))))
	       (each (lambda (var)
			"var ~a;\n" (Var-js-id var))
		     declared-vars)
	       (walk body p #t))))))

(define-nmethod (Frame-alloc.compile p stmt?)
   (template-display p env
      (?@ stmt? "~@;\n") ;; should never happen.
      "{~e}" ;; literal-object creation
      (with-access::Frame-alloc this (vars)
	 (unless (null? vars)
	    (let loop ((vars vars))
	       (with-access::Var (car vars) (js-id)
		  (if (null? (cdr vars))
		      (template-display p env "$js-id : undefined")
		      (begin
			 (template-display p env "$js-id : undefined, ")
			 (loop (cdr vars))))))))))

(define-nmethod (Frame-push.compile p stmt?)
   (with-access::Frame-push this (body storage-vars)
      (with-access::Var (car storage-vars) (js-id)
	 (template-display p env
	    "with ($js-id) {\n"
	    "  ~e" (walk body p #t)
	    "}\n"))))

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
   (with-access::Begin this (exprs)
      (if stmt?
	  ;; we do not need { }, as all statements that could contain blocks
	  ;; already have them (or do not need them).
	  (for-each (lambda (n) (walk n p #t))
		    exprs)
	  (template-display p env
	     "(~e)"
	     (separated ", "
			(lambda (e) "~e" (walk e p #f))
			exprs)))))

;(define-nmethod (Call/cc-Resume.compile p)
;   (p-display
;    p (indent env)
;    "sc_callCcIndex = 0;\n"))

; (define-nmethod (Call/cc-Call.compile p)
;    (if (statement-form? this)
;        (p-display p
; 		  (indent++) "{ sc_callCcIndex = " this.call/cc-index ";\n")
;        (p-display p "(sc_callCcIndex = " this.call/cc-index ", "))
;    (pcall this Call-compile p)
;    (if (statement-form? this)
;        (p-display p (--indent) "}\n")
;        (p-display p ")")))

; (define (tail-call-compile n p)
;    (define (tail-call n tail-obj)
;       (p-display p "(" tail-obj ".f =")
;       (n.operator.compile p)
; ;      (p-display p ".call(" tail-obj)
;       (p-display p "," tail-obj ".f(")
; ;      (if (not (null? n.operands))
; ;	  (p-display p ", "))
;       (compile-separated-list p n.operands ", ")
;       (p-display p "))"))
   
;    (p-display
;     p
;     "(\n"
;     (indent) "  (this === (sc_tailTmp =" (scm2js-global "TAIL_OBJECT") "))?\n"
;     (indent) "  ((!sc_funTailCalls)?\n"
;     (indent) "    (sc_tailTmp = [")
;    (compile-separated-list p n.operands ", ")
;    (p-display
;     p
;     "],\n"
;     (indent) "    sc_tailTmp.callee = ")
;    (n.operator.compile p)
;    (p-display
;     p
;     ",\n"
;     (indent) "    new sc_Trampoline(sc_tailTmp, " (config 'max-tail-depth) "))\n"
;     (indent) "    :\n"
;     (indent) "    (this.calls = sc_funTailCalls - 1,\n"
;     (indent) "     ")
;    (tail-call n "sc_tailTmp")
;    (p-display
;     p
;     ")\n"
;     (indent) "):(\n"
;     (indent) "    sc_tailTmp.calls = " (config 'max-tail-depth) ",\n"
;     (indent) "    sc_tailTmp = ")
;    (tail-call n "sc_tailTmp") ;(scm2js-global "TAIL_OBJECT"))
;    (p-display
;     p
;     ",\n"
;     (indent) "    ((typeof sc_tailTmp === 'object' &&"
;     "sc_tailTmp instanceof sc_Trampoline)?\n"
;     (indent) "       sc_tailTmp.restart() :\n"
;     (indent) "       sc_tailTmp)\n"
;     (indent) "  )\n"
;     (indent) ")"))

(define-nmethod (Call.compile p stmt?)
   (define (compile-operator)
      ;; if the operator is a var-ref and contains a "." we have to do
      ;; some tricks: frame.f() would give 'frame' to f as this-object.
      ;; by using the sequence-construct we can avoid that:
      ;; (0,frame.f)() sends the global-object as 'this'-object.
      (with-access::Call this (operator)
	 (if (and (Ref? operator)
		  (with-access::Ref operator (var)
		     (with-access::Var var (js-id)
			(string-index js-id #\.))))
	     (with-access::Ref operator (var)
		(with-access::Var var (js-id)
		   (template-display p env
		      "(0, $js-id)")))
	     (walk operator p #f))))
      
   (template-display p env
      (?@ stmt? "~@;\n")
      (?@ (not stmt?) "(~@)") ;; just for now. better than nothing.
      "~e"
      (with-access::Call this (operator operands)
	 (unless (and (Out-Env-optimize-calls? env)
		      (compile-optimized-call p env walk operator operands))
	    (template-display p env
	       "~e(~e)"
	       (compile-operator)
	       (separated ", "
			  (lambda (operand) "~e" (walk operand p #f))
			  operands))))))

(define-nmethod (While.compile p stmt?)
   (with-access::While this (init test body label)
      (template-display p env
	 ;; init
	 (?? (not (Const? init)) "~e" (walk init p #t))
	 ;; label
	 (?? (not (eq? label (default-label)))
	     "~a:\n" (mangle-JS-sym (Label-id label))))

      (if (and (Const? test)
	       (eq? (Const-value test) #t))
	  (template-display p env
	     "do {\n"
	     "  ~e" (walk body p #t)
	     "} while (true);\n")
	  (template-display p env
	     "while (~e) {\n" (walk test p #f)
	     "  ~e"           (walk body p #t)
	     "}\n"))))
			  
;(define-nmethod (Call/cc-Counter-Update.compile p)
;   (with-access::Call/cc-Counter-Update this (index)
;      (template-display p env
;	 (?@ (statement-form? this) "~@;\n")
;	 "~a++" (call/cc-loop-counter index))))

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

(define-nmethod (Pragma.compile p stmt?)
   (with-access::Pragma this (str)
      (template-display p env
	 (?@ stmt? "~@;\n")
	 "(~a)" str)))
