(module compile
   (include "protobject.sch")
   (include "nodes.sch")
   (include "tools.sch")
   (include "compile-optimized-call.scm")
   (include "compile-optimized-boolify.scm")
   (include "compile-optimized-set.scm")
   (option (loadq "protobject-eval.sch"))
   (export (compile tree::pobject p))
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
	     (Node Program Part Node Const Var-ref Lambda
		   If Case Clause Set! Begin Bind-exit With-handler
		   Call Tail-rec While Tail-rec-call Return
		   Closure-alloc Labelled Break Pragma)
	     (tree.compile p)))

(define *tmp-var* "tmp") ;; can't conflict, as all vars are starting with 'sc_
(define *symbol-prefix* "\\u1E9C") ;; "\\u1E9D\\u1E9E\\u1E9F")

(define-macro (check-stmt-form node p . Lbody)
   `(begin
       ,@Lbody
       (if (statement-form? ,node)
	   (display ";\n" ,p))))

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

(define-pmethod (Node-compile)
   (error #f "forgot node-type: " this))

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
       (if (positive? const)
	   (p-display p const)
	   (p-display p "(" const ")")))
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
    (p-display p this.var.compiled)))

(define *max-tail-call-depth* 20)

(define-pmethod (Program-compile p)
   (when (config 'trampoline)
      (p-display p "sc_TAIL_CALLS = " *max-tail-call-depth* ";\n")
      (p-display p "var sc_f;\n")
      (p-display p "var sc_funTailCalls = sc_TAIL_CALLS + 1;\n"))
   (this.body.compile p))

(define-pmethod (Part-compile p)
   (define (split-globals local-vars)
      ;; HACK; TODO: outer-vars are for now just global vars.
      ;;    should be all vars that are visible to the outside of the part
      (let ((outer-vars (make-eq-hashtable))
	    (part-vars (make-eq-hashtable)))
	 (hashtable-for-each local-vars
			     (lambda (var ignored)
				(hashtable-put! (if var.is-global?
						    outer-vars
						    part-vars)
						var
						#t)))
	 (cons outer-vars part-vars)))

   ;; TODO part-filter-fun / this.fun
   (let* ((outer/part-vars (if (config 'encapsulate-parts)
			       (split-globals this.local-vars)
			       (cons this.local-vars #f)))
	  (outer-vars (car outer/part-vars))
	  (outer-vars? (not (= 0 (hashtable-size outer-vars))))
	  (part-vars (cdr outer/part-vars))

	  (part-filter-fun this.fun)
	  (part-filter-port/close (part-filter-fun p))
	  (part-filter-port (car part-filter-port/close))
	  (part-filter-close (cdr part-filter-port/close))
	  
	  (whole-is-stmt-form? (and outer-vars?
				    (statement-form? this)))

	  (new-body (if (and part-vars
			     (> (hashtable-size part-vars) 0))
			(let* ((fun (new-node Lambda '() #f this.body))
			       (call (new-node Call fun '())))
			   (mark-statement-form! call #t)
			   (set! fun.local-vars part-vars)
			   call)
			this.body)))
      (if outer-vars?
	  (begin
	     (p-display part-filter-port "\n{\n")
	     (hashtable-for-each outer-vars
				 (lambda (var ignored)
				    (p-display part-filter-port
					       "var " var.compiled ";\n")))
	     (new-body.compile part-filter-port)
	     (p-display part-filter-port "\n}\n"))
	  (new-body.compile part-filter-port))

      (part-filter-close part-filter-port whole-is-stmt-form?)

      (if (not whole-is-stmt-form?)
	  (p-display p ";"))))


;; Llvalue/stmt?, if given, indicate, that this lambda should be assigned to
;; the given lvalue. stmt? is true, if we should treat this node, as if it was
;; a statement-node.
;; If there's a lvalue and it's not a stmt?, the return value of the compiled
;; code is unspecified. (ie, it doesn't anymore necessarily return the
;; lambda).
(define-pmethod (Lambda-compile p . Llvalue/stmt?)
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
       "var sc_funTailCalls;\n"
       "var sc_f;\n"
       "if (!(sc_funTailCalls=sc_TAIL_CALLS)) throw new sc_TailCall(this, arguments);\n"
       "sc_TAIL_CALLS=" *max-tail-call-depth* ";\n"
       "try {\n"))

   (define (trampoline-end-code p)
      (p-display
       p
       "\n} catch (e) {\n"
       "    if (sc_funTailCalls === " *max-tail-call-depth* ") {\n"
       "       var tmpExc = e;\n"
       "       while (tmpExc instanceof sc_TailCall) {\n"
       "           try {\n"
       "              sc_TAIL_CALLS=sc_funTailCalls-1;\n"
       "              return tmpExc.arguments.callee.apply(tmpExc.funThis,"
       " tmpExc.arguments);\n"
       "           } catch (e2) {\n"
       "              tmpExc = e2;\n"
       "           }\n"
       "       }\n"
       "       throw tmpExc;\n"
       "    } else\n"
       "       throw e;\n"
       "} finally { sc_TAIL_CALLS = sc_funTailCalls; }"))

   (let* ((locals this.local-vars)
	  (ht (make-hashtable))
	  (needs-trampoline? (and (config 'trampoline)
				  this.contains-tail-calls?))
	  (lvalue (and (not (null? Llvalue/stmt?))
		       (car Llvalue/stmt?)))
	  (stmt? (or (statement-form? this)
		     (and (not (null? Llvalue/stmt?))
			  (cadr Llvalue/stmt?))))
	  (declaration? (and lvalue
			     stmt?
			     
			     ;; if we optimize the var-number we might reuse
			     ;; variables. Don't play with that...
			     (not (config 'optimize-var-number))

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
					   needs-trampoline?)))))
      
      ;; ht will contain the var-names. by putting them into a hashtable we
      ;; remove the duplicates. Not very elegant, but it should work ;)
      (hashtable-for-each locals
			  (lambda (var ignored)
			     (hashtable-put! ht var.compiled #t)))
      
      (if needs-parenthesis?
	  (p-display p "("))
      
      (cond
	 ;; if we don't have a lvalue, we use "sc_f" as temporary variable...
	 ((and needs-trampoline?
	       (not lvalue))
	  (p-display p "sc_f = function ("))
	 ;; if it's a declaration we write the beginning of it: 'function name('
	 (declaration?
	  (p-display p "function ")
	  (lvalue.compile p)
	  (p-display p "("))
	 ;; if it's an assignment: 'lval = function('
	 (lvalue
	  (lvalue.compile p)
	  (p-display p " = function("))
	 (else
	  (p-display p "function(")))
	  
      (compile-separated-list p this.formals ", ")
      (p-display p ") {\n")
      (if needs-trampoline?
	  (trampoline-start-code p))
      (if this.vaarg
	  (vaarg-code this.vaarg.var (length this.formals)))
      (hashtable-for-each ht
			  (lambda (var ignored)
			     (p-display p "var " var ";\n")))
      (this.body.compile p)
      (if needs-trampoline?
	  (trampoline-end-code p))
      ;; with the following p-display we close the function-body.
      (p-display p "\n}\n")
      
      (when needs-trampoline?
	 (if stmt?
	     ;; we don't need a semicolon if it was a declaration, but
	     ;; it doesn't hurt.
	     (p-display p "; ")
	     (p-display p ", "))
	 (if lvalue
	     (lvalue.compile p)
	     (p-display p "sc_f"))
	 (p-display p ".hasTailCalls = true")
	 (if (and (not stmt?)
		  (not lvalue)) ;; we need to return the function...
	     ;; if there's a lvalue, we have an assignment. and assignments
	     ;; return unspecified values...
	     (p-display p ", sc_f"))
	 ;; if there's a closing semicolon needed it will be written later on.
	 )

      ;; close the previously opened parenthesis
      (if needs-parenthesis?
	  (p-display p ")"))

      ;; add the stmt-semicolon. In theory we don't need any, when it's a
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
   (if (statement-form? this)
       (begin
	  (p-display p "if (")
	  (compile-boolified p this.test)
	  (p-display p ") ")
	  (this.then.compile p)
	  (p-display p "\nelse\n")
	  (this.else.compile p))
       (begin
	  (p-display p "(")
	  (compile-boolified p this.test)
	  (p-display p "?\n")
	  (this.then.compile p)
	  (p-display p ":\n")
	  (this.else.compile p)
	  (p-display p ")"))))

(define-pmethod (Case-compile p)
   (p-display p "switch (")
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
       (this.val.compile p this.lvalue (statement-form? this))
       (check-stmt-form
	this p
	(if (config 'optimize-set!)
	    (compile-optimized-set! p this)
	    (compile-unoptimized-set! p this)))))

(define-pmethod (Begin-compile p)
   (if (statement-form? this)
       (begin
	  (p-display p "\n{\n")
	  (for-each (lambda (n)
		       (n.compile p))
		    this.exprs)
	  (p-display p "}\n"))
       (begin
	  (p-display p "(")
	  (compile-separated-list p this.exprs ", ")
	  (p-display p ")"))))

(define-pmethod (Bind-exit-compile p)
   (let ((escape-obj (gen-JS-sym 'escape_obj)))
      (p-display p
		 "{\n"
		 "function ")
      (this.escape.compile p)
      (p-display p
		 "(res) { "
		 escape-obj ".res = res;"
		 escape-obj ".bindExitMarker = true;\n"
		 "throw " escape-obj ";"
		 "}\n"
		 "var " escape-obj " = new Object();\n"
		 "try {\n")
      (this.body.compile p)
      (p-display p "} catch (exc) {\n"
		 "if (exc === " escape-obj ") {\n")
      (this.result-decl.compile p)
      (p-display p " = exc.res;\n")
      (this.invoc-body.compile p)
      (p-display p
		 "\n} else throw exc;\n"
		 "}\n"
		 "}\n")))

(define-pmethod (With-handler-compile p)
   (let ((exception this.exception.var.compiled))
      (p-display p "try {\n")
      (this.body.compile p)
      (p-display p
		 "} catch (" exception ") {\n"
		 "if (!" exception ".bindExitMarker) {\n")
      (this.catch.compile p)
      (p-display p
		 "\n} else throw " exception ";\n"
		 "}\n")))

(define-pmethod (Call-compile p)
   (check-stmt-form
    this p
    (let ((operator this.operator))
       (unless (and (config 'optimize-calls)
		    (compile-optimized-call p operator this.operands))
	  (p-display p "(")
	  (cond
	     ((and (config 'trampoline)
		   this.certain-tail-call?)
	      (p-display p "sc_TAIL_CALLS=sc_funTailCalls-1,")
	      (this.operator.compile p))
	     ((and (config 'trampoline)
		   this.tail-call?
		   (instance-of? this.operator (node 'Var-ref)))
	      (p-display p "(")
	      (this.operator.compile p)
	      (p-display p
			 ".hasTailCalls?sc_TAIL_CALLS=sc_funTailCalls-1:true),")
	      (this.operator.compile p))
	     ((and (config 'trampoline)
		   this.tail-call?)
	      (p-display p "sc_f =")
	      (this.operator.compile p)
	      (p-display p
			 ", (sc_f.hasTailCalls?"
			 "sc_TAIL_CALLS=sc_funTailCalls-1"
			 ":true), sc_f"))
	     (else
	      (this.operator.compile p)))
	  (p-display p "(")
	  (compile-separated-list p this.operands ", ")
	  (p-display p "))")))))

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
   (p-display p
	      "{\n"
	      "var " *tmp-var* " = new Object();\n")
   (for-each (lambda (var)
		(p-display p *tmp-var* "." var.compiled " = undefined;\n"))
	     this.allocated-vars)
   (p-display p "with(" *tmp-var* ")")
   (this.body.compile p)
   (p-display p "\n}"))

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
