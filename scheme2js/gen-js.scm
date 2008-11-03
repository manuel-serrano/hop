(module gen-js
   (export (mangle-JS-sym::bstring sym::symbol)
	   (gen-JS-sym::bstring sym::symbol)
	   (valid-JS-str?::bool str::bstring)
	   (mangle-qualified-var::bstring sym::symbol qualifier)))

(define counter 0)

(define *js-counter-mutex* (make-mutex))

(define (mangle-qualified-var sym qualifier)
   (if (not qualifier)
       (mangle-JS-sym sym)
       (let ((str (string-append (symbol->string sym)
				 "_$"
				 (symbol->string qualifier))))
	  (if (bigloo-need-mangling? str)
	      (bigloo-mangle str)
	      str))))

;; mangle variables, so they are valid JS-vars.
(define (mangle-JS-sym sym)
   (let ((s (symbol->string sym)))
      (if (valid-JS-str? s)
	  s
	  (bigloo-mangle s))))

;; kind of adapted gen-sym
(define (gen-JS-sym sym)
   (with-lock *js-counter-mutex*
      (lambda ()
	 (set! counter (+ counter 1))
	 (mangle-JS-sym
	  (symbol-append 'sc_ ;; start with "sc_"
			 sym
			 '_
			 (string->symbol (integer->string counter)))))))

(define *reserved-js*
   '("as" "break" "case" "catch" "class" "const" "continue" "default"
     "delete" "do" "else" "extends" "false" "finally" "for"
     "function" "if" "import" "in" "instanceof" "is" "namespace"
     "new" "null" "package" "private" "public" "return" "super"
     "switch" "this" "throw" "true" "try" "typeof" "use" "var"
     "void" "while" "with" "abstract" "debugger" "enum" "export"
     "goto" "implements" "interface" "native" "protected"
     "synchronized" "throws" "transient" "volatile"
     ))

(define (valid-JS-str? str)
   (not (or (bigloo-need-mangling? str)
	    (member str *reserved-js*)
	    ;; avoid clashes with runtime :
	    (string-prefix? "sc_" str)
	    (string-prefix? "SC_" str))))
