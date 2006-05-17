(module gen-code
   (include "tools.sch")
   (import gen-js)
   (export (gen-code-pair::bstring fst snd)
	   (gen-code-nil::bstring)
	   (gen-code-vector::bstring els)
	   (gen-code-bool::bstring b)
	   (gen-code-symbol::bstring sym)
	   (gen-code-char::bstring c)
	   (gen-code-number::bstring n)
	   (gen-code-string::bstring s)
	   (gen-code-var::bstring v)
	   (gen-code-var-decls::bstring vars)
	   (gen-code-function::bstring formals vaarg body collected-vars statement-form?)
	   (gen-code-if::bstring test consequent alternate statement-form?)
	   (gen-code-switch::bstring key clauses)
	   (gen-code-clause::bstring consts expr)
	   (gen-code-default-clause::bstring expr)
	   (gen-code-assign::bstring var expr . Lneeds-unspecified?)
	   (gen-code-begin::bstring els statement-form?)
	   (gen-code-call::bstring operator operands)
	   (gen-code-unspecified::bstring)
;	   (gen-code-tail operator operands)
	   (gen-code-bind-exit::bstring escape body result-decl invoc-body)
	   (gen-code-with-handler::bstring exception catch body)
	   (gen-code-while::bstring test body label)
	   (gen-code-continue::bstring label)
	   (gen-code-break::bstring label)
	   (gen-code-return::bstring val)
	   (gen-code-closure-alloc::bstring vars body)
	   (gen-code-label::bstring id body)
	   (gen-code-boolify::bstring test)
	   (gen-code-pragma::bstring str)
	   (gen-code-keyword::bstring kw)))

(define *tmp-var* "tmp") ;; can't conflict, as all vars are starting with 'sc_

(define (gen-code-pair fst snd)
   (string-append "(new sc_Pair(" fst "," snd "))"))

(define (gen-code-nil)
   "null")

;; vectors are JS-arrays stored in a special object.
(define (gen-code-vector els)
   (string-append "(new sc_Vector(["
		  (apply string-append
			 (map (lambda (el)
				 (string-append el ","))
			      els))
		  "]))"))

;; use Javascripts bools
(define (gen-code-bool b)
   (if b
       "true"
       "false"))

;; use native strings as symbols
(define (gen-code-symbol sym)
   (string-append "\""
		  (symbol->string sym)
		  "\""))

(define (escaped-string.old s)
   (with-output-to-string
      (lambda ()
	 (write s))))

;; MS: 13 may 2006
(define (escaped-string str)
   (string-for-read str))

;; new Scheme-Char (unfortunately JS doesn't have chars)
(define (gen-code-char c)
   (string-append "(new sc_Char(\"" (escaped-string (string c)) "\"))"))

;; numbers
(define (gen-code-number n)
   (number->string n))

;; to make a difference to symbols, strings are always
;; stored in Objects.
(define (gen-code-string s)
   (string-append "(new sc_String(\"" (escaped-string s) "\"))"))

;; JS's variables
(define (gen-code-var v)
   (gen-JS-sym v))

(define (gen-code-var-decls vars)
   (apply string-append
	  (map (lambda (var)
		  (string-append "var "
				 var
				 ";\n "))
	       vars)))

(define (gen-code-function formals vaarg body local-vars statement-form?)
   (define (vaarg-code L nb-args)
      (string-append "var " L " = null;\n"
		     "for (var " *tmp-var* " = arguments.length - 1;"
		     *tmp-var* ">=" (number->string nb-args) ";"
		     *tmp-var* "--) {\n"
		     L " = sc_cons(arguments[" *tmp-var* "], " L ");\n"
		     "}\n"))

   (string-append (if statement-form? "(" "")
		  "function("
		  (separated-list formals ", ")
		  ")\n { "
		  (if vaarg (vaarg-code vaarg (length formals)) "")
		  (apply string-append
			 (map (lambda (var)
				 (string-append "var "
						var
						";\n "))
			      local-vars))
		  body
		  "\n"
		  "}"
		  (if statement-form? ")" "")))

(define (gen-code-if test consequent alternate statement-form?)
   (if statement-form?
       (string-append "if("
		      test
		      ") "
		      consequent
		      "\n else \n"
		      alternate)
       (string-append "("
		      test "?\n" consequent ":\n" alternate
		      ")")))

(define (gen-code-switch key clauses)
   (string-append "switch (" key ") {\n"
		  (apply string-append
			 (map (lambda (clause)
				 (string-append clause "\n"))
			      clauses))
		  "}\n"))

(define (gen-code-clause consts expr)
   (string-append
    (apply string-append
	   (map (lambda (const)
		   (string-append "case " const ":\n"))
		consts))
    expr
    "\nbreak;"))

(define (gen-code-default-clause expr)
   (string-append "default: " expr "\nbreak;"))

(define (gen-code-assign var expr . Lneeds-unspecified?)
   (let ((needs-unspecified? (if (null? Lneeds-unspecified?)
				#t
				(car Lneeds-unspecified?))))
      ;; the standard says, the result is 'unspecified'. In our case the
      ;; rvalue seems to be the easiest...
;      (if needs-unspecified?
;	  (string-append "((" var "=" expr "),\n" (gen-code-unspecified) ")")
;	  (string-append "(" var "=" expr ")"))))
      (string-append "(" var "=" expr ")")))

(define (gen-code-begin els statement-form?)
   (if statement-form?
       (string-append "{" (separated-list els "\n") "}\n")
       (string-append "(" (separated-list els ",\n") ")")))

(define (gen-code-bind-exit escape body result-decl invoc-body)
   (let ((escape-obj (gen-JS-sym 'escape_obj)))
      (string-append "{\n"
		     "function " escape "(res) { "
		     escape-obj ".res = res;"
		     escape-obj ".bind-exit-marker = true;\n"
		     "throw " escape-obj ";"
		     "}\n"
		     "var " escape-obj " = new Object();\n"
		     "try {\n"
		     body
		     "} catch (exc) {\n"
		     "if (exc === " escape-obj ") {\n"
		     result-decl " = exc.res;\n"
		     invoc-body
		     "\n} else throw exc;\n"
		     "}\n"
		     "}\n")))

(define (gen-code-with-handler exception catch body)
   (string-append "try {\n"
		  body
		  "} catch (" exception ") {\n"
		  "if (!"exception".bind-exit-marker) {\n"
		  catch
		  "\n} else throw " exception ";\n"
		  "}\n"))

;; ***  with trampoline  ***
; (define (gen-code-call operator operands)
;    (string-append "sc_trampoline("
; 		  "(" operator ")(" (separated-list operands ", ") ")"
; 		  ")"))
; (define (gen-code-tail operator operands)
;    (if (null? operands)
;        (string-append "(new sc_Trampoline(" operator "))")
;        (string-append "(new sc_Trampoline("
; 		      "function() {"
; 		      "return "
; 		      "(" operator ")(" (separated-list operands ", ") ");"
; 		      "}))")))

;; ***  without trampoline  ***
(define (gen-code-call operator operands)
   (string-append "((" operator ")(" (separated-list operands ", ") "))"))

;(define (gen-code-tail operator operands)
;   (string-append "((" operator ")(" (separated-list operands ", ") "))"))

(define (gen-code-boolify b)
   (string-append "(" b "!== false)"))

(define (gen-code-unspecified)
   "undefined")

(define (gen-code-while test body label)
   (string-append (if label
		      (string-append (mangle-JS-sym label) ": ")
		      "")
		  "while (" test ") {" body "}"))

(define (gen-code-continue label)
   (if label
       (string-append "continue " (mangle-JS-sym label) ";")
       "continue;"))

(define (gen-code-break label)
   (if label
       (string-append "break " (mangle-JS-sym label) ";")
       "break;"))

(define (gen-code-return val)
   (string-append "return " val ";"))

(define (gen-code-closure-alloc vars body)
   (string-append "{\n"
		  "var " *tmp-var* " = new Object();\n"
		  (apply string-append
			 (map (lambda (var)
				 (string-append *tmp-var* "." var
						" = undefined;\n"))
			      vars))
		  "with(" *tmp-var* ")" body
		  "\n}"))

(define (gen-code-label id body)
   (string-append (mangle-JS-sym id) ":{\n"
		  body
		  "\n}"))

(define (gen-code-pragma str)
   (string-append "(" str ")"))

(define (gen-code-keyword kw)
  (string-append "(new sc_Keyword('"
		 (keyword->string kw)
		 "'))"))
