;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/scheme2js/out.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Wed Dec  9 10:08:43 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JavaScript code generation.                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module out
   
   (cond-expand
      (enable-threads (library pthread)))
   
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
	   push-declarations
	   tail
	   error
	   scheme2js)

   (cond-expand
      (enable-callcc (import callcc callcc-out)))
   
   (static (class Pragmas
	      lock
	      ;; will start with "dummy"-pair
	      pragmas::pair
	      ;; last in list
	      last-pragma::pair)
	   
	   (class Out-Env
	      trampoline?::bool
	      max-tail-depth::bint
	      suspend-resume?::bool
	      call/cc?::bool
	      optimize-calls?::bool
	      debug?::bool
	      source-file
	      meta?::bool
	      arity-check?::bool
	      host-compiler::procedure
	      host-register::procedure
	      use-strict?::bool
	      pragmas       
	      last-line
	      last-file
	      source-map?::bool)
	   
	   (wide-class Out-Lambda::Lambda
	      (lvalue-is-cnst::bool (default #f))
	      (id read-only)
	      (js-id read-only)))
   
   (export (compile-value ::obj ::output-port ::procedure ::procedure ::obj)
	   (out tree::Module source-file p)))

;*---------------------------------------------------------------------*/
;*    out ...                                                          */
;*---------------------------------------------------------------------*/
(define (out tree source-file p)
   (verbose "Compiling")
   (gen-var-names tree)
   (push-var-declarations tree)
   (gen-code tree p #f source-file))

;*---------------------------------------------------------------------*/
;*    gen-code ...                                                     */
;*---------------------------------------------------------------------*/
(define (gen-code tree p pragmas source-file)
   (verbose "  generating code")
   (let ((env (instantiate::Out-Env
		 (trampoline? (and (config 'trampoline) #t))
		 (max-tail-depth (config 'max-tail-depth))
		 (suspend-resume? (and (config 'suspend-resume) #t))
		 (call/cc? (cond-expand
			      (enable-callcc (and (config 'call/cc) #t))
			      (else #f)))
		 (optimize-calls? (and (config 'optimize-calls) #t))
		 (debug? (and (config 'debug) #t))
		 (source-file source-file)
		 (meta? (and (config 'meta) #t))
		 (arity-check? (config 'arity-check))
		 (use-strict? (and (config 'use-strict/function)
				   (not (config 'use-strict/module))))
		 (host-compiler (config 'host-compiler))
		 (host-register (config 'host-register))
		 (pragmas pragmas)
		 (last-line 0)
		 (last-file "")
		 (source-map? (config 'source-map)))))
      (compile tree env p #t #t)))

(define *tmp-var* "sc_lambda") ;; can't conflict with generated names.

;; when using immutable values.
(define *symbol-prefix* "\\uEBAC")
(define *keyword-prefix* "\\uEBAD")

;; TODO propose other global access as option.
; (define (scm2js-globals-init p)
;    'do-nothing)
; (define (scm2js-global global)
;    (string-append "SC_" global))

(define (scm2js-globals-init p)
   (template-display p
      "var sc_globals = SC_SCM2JS_GLOBALS;\n"))
(define (scm2js-global global)
   (string-append "sc_globals." global))

(define-nmethod (Node.compile p stmt? toplevel?)
   (error #f "Internal Error: forgot node-type" this))

;; return true, if we should use nested new sc_Pairs instead of sc_list(...).
(define (small-list/pair? l )
   
   (define (smaller? l n)
      (cond
	 ((<fx n 0) #f)
	 ((null? l) #t)
	 ((not (pair? l)) #t)
	 (else (smaller? (cdr l) (- n 1)))))

   (smaller? l 5))

;*---------------------------------------------------------------------*/
;*    my-string-for-read ...                                           */
;*    -------------------------------------------------------------    */
;*    MS: new implementation 13apr2010.                                */
;*---------------------------------------------------------------------*/
(define (my-string-for-read str)
   
   (define (count str ol)
      (let loop ((i 0)
		 (n 0))
	 (if (=fx i ol)
	     n
	     (let ((c (string-ref str i)))
		(case c
		   ((#\" #\\ #\Newline #\Return)
		    (loop (+fx i 1) (+fx n 2)))
		   ((#\/ #\null)
		    (loop (+fx i 1) (+fx n 6)))
		   (else
		    (loop (+fx i 1) (+fx n 1))))))))
   
   (define (encode str ol nl)
      (if (=fx nl ol)
	  str
	  (let ((res (make-string nl)))
	     (let loop ((i 0)
			(j 0))
		(if (=fx j nl)
		    res
		    (let ((c (string-ref str i)))
		       (case c
			  ((#\" #\\)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) c)
			   (loop (+fx i 1) (+fx j 2)))
			  ((#\Newline)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\n)
			   (loop (+fx i 1) (+fx j 2)))
			  ((#\Return)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\r)
			   (loop (+fx i 1) (+fx j 2)))
			  ((#\/)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\u)
			   (string-set! res (+fx j 2) #\0)
			   (string-set! res (+fx j 3) #\0)
			   (string-set! res (+fx j 4) #\2)
			   (string-set! res (+fx j 5) #\f)
			   (loop (+fx i 1) (+fx j 6)))
			  ((#\null)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\u)
			   (string-set! res (+fx j 2) #\0)
			   (string-set! res (+fx j 3) #\0)
			   (string-set! res (+fx j 4) #\0)
			   (string-set! res (+fx j 5) #\0)
			   (loop (+fx i 1) (+fx j 6)))
			  (else
			   (string-set! res j c)
			   (loop (+fx i 1) (+fx j 1))))))))))
   
   (let ((ol (string-length str)))
      (encode str ol (count str ol))))

;*---------------------------------------------------------------------*/
;*    display-string-for-read ...                                      */
;*---------------------------------------------------------------------*/
(define (display-string-for-read str op)
   (let ((ol (string-length str)))
      (let loop ((i 0))
	 (when (<fx i ol)
	    (let ((c (string-ref str i)))
	       (case c
		  ((#\" #\\)
		   (write-char #\\ op)
		   (write-char c op)
		   (loop (+fx i 1)))
		  ((#\Newline)
		   (write-char #\\ op)
		   (write-char #\n op)
		   (loop (+fx i 1)))
		  ((#\Return)
		   (write-char #\\ op)
		   (write-char #\r op)
		   (loop (+fx i 1)))
		  ((#\/)
		   (write-char #\\ op)
		   (write-char #\u op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (write-char #\2 op)
		   (write-char #\f op)
		   (loop (+fx i 1)))
		  ((#\null)
		   (write-char #\\ op)
		   (write-char #\u op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (loop (+fx i 1)))
		  (else
		   (write-char c op)
		   (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    compile-value ...                                                */
;*---------------------------------------------------------------------*/
(define (compile-value obj p host-compiler host-register loc)
   (let ((cache (register-value obj host-register)))
      (when (pair? cache)
	 (let ((len (length cache)))
	    (display "sc_circle(" p)
	    (display len p)
	    (display (format ", function( cache ) { ~areturn "
			(if (config 'use-strict/function)
			    "\n\"use strict\";\n" "\n"))
	       p)))
      (multiple-value-bind (res cyclic)
	 (compile-value-circle obj p host-compiler loc cache)
	 (when (pair? cache)
	    (display ";}," p)
	    (display (if cyclic "false)" "true)") p)))))

;*---------------------------------------------------------------------*/
;*    register-value ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is used to build a cache of cyclic references.     */
;*---------------------------------------------------------------------*/
(define (register-value obj host-register)
   
   (define (uncircular? obj)
      (or (number? obj)
	  (symbol? obj) (string? obj) (ucs2-string? obj)
	  (cnst? obj) (class? obj) (procedure? obj) (keyword? obj)
	  (date? obj)))
   
   (let ((cache '()))
      (let register ((obj obj))
	 (unless (uncircular? obj)
	    (let ((match (assq obj cache)))
	       (if match
		   (set-cdr! match #t)
		   (begin
		      (set! cache (cons (cons obj #f) cache))
		      (cond
			 ((pair? obj)
			  (register (car obj))
			  (register (cdr obj)))
			 ((vector? obj)
			  (for i 0 (vector-length obj)
			     (register (vector-ref obj i))))
			 ((struct? obj)
			  (for i 0 (struct-length obj)
			     (register (struct-ref obj i))))
			 ((cell? obj)
			  (register (cell-ref obj)))
			 (else
			  (host-register obj register))))))))
      (filter (lambda (x) (cdr x)) cache)))

;*---------------------------------------------------------------------*/
;*    compile-value-circle ...                                         */
;*---------------------------------------------------------------------*/
(define (compile-value-circle val p host-compiler loc cache)
   
   (define (display-ucs2-char p c)
      ;; without the quotes
      (let ((i (ucs2->integer c)))
	 (cond
	    ((and (<=fx i #x7F)
		  (or (ucs2-alphabetic? c)
		      (ucs2-numeric? c)))
	     (template-display p "~a" (ucs2->char c)))
	    ((<=fx i #xF)
	     (template-display p "\\u000~x" i))
	    ((<=fx i #xFF)
	     (template-display p "\\u00~x" i))
	    ((<=fx i #xFFF)
	     (template-display p "\\u0~x" i))
	    (else
	     (template-display p "\\u~x" i)))))
   
   (define index -1)
   
   (define (next-index)
      (set! index (+fx 1 index))
      index)
   
   (define (compile-cyclic val p)
      (cond
	 ((vector? val)
	  (if (class? val)
	      (template-display p
		 "sc_find_class(~e)"
		 (compile (class-name val) p))
	      (template-display p
		 "[~e]"
		 (let loop ((i 0))
		    (unless (>= i (vector-length val))
		       (if (not (= i 0))
			   (template-display p ", "))
		       (compile (vector-ref val i) p)
		       (loop (+ i 1)))))))
	 ((pair? val)
	  (if (or (small-list/pair? val) (assq val cache))
	      (template-display p
		 "(new sc_Pair(~e,~e))"
		 (compile (car val) p)
		 (compile (cdr val) p))
	      (template-display p
		 "sc_list(~e)"
		 (separated ", " 
		    (lambda (e) "~e" (compile e p))
		    val))))
	 ((u8vector? val)
	  (fprintf p "new Uint8Array([ ~(,) ])" (u8vector->list val)))
	 ((s8vector? val)
	  (fprintf p "new Int8Array([ ~(,) ])" (s8vector->list val)))
	 ((u16vector? val)
	  (fprintf p "new Uint16Array([ ~(,) ])" (u16vector->list val)))
	 ((s16vector? val)
	  (fprintf p "new Int16Array([ ~(,) ])" (s16vector->list val)))
	 ((u32vector? val)
	  (fprintf p "new Uint32Array([ ~(,) ])" (u32vector->list val)))
	 ((s32vector? val)
	  (fprintf p "new Int32Array([ ~(,) ])" (s32vector->list val)))
	 ((u64vector? val)
	  (fprintf p "new Uint64Array([ ~(,) ])" (u64vector->list val)))
	 ((s64vector? val)
	  (fprintf p "new Int64Array([ ~(,) ])" (s64vector->list val)))
	 ((f32vector? val)
	  (fprintf p "new Float32Array([ ~(,) ])" (f32vector->list val)))
	 ((f64vector? val)
	  (fprintf p "new Float64Array([ ~(,) ])" (f64vector->list val)))
	 (else
	  (host-compiler val p compile))))
   
   (define (compile val p)
      (cond
	 ((null? val)
	  (display-string "null" p))
	 ((boolean? val)
	  (display-string (if val "true" "false") p))
	 ((symbol? val)
	  (template-display p
	     "\"~?~a\""
	     (and (not (use-mutable-strings?)) *symbol-prefix*)
	     (my-string-for-read (symbol->string! val))))
	 ((char? val)
	  (template-display p
	     "(new sc_Char(\"~a\"))" (my-string-for-read (string val))))
	 ((ucs2? val)
	  (template-display p
	     "(new sc_Char(\"~e\"))" (display-ucs2-char p val)))
	 ((number? val)
	  ;; CARE: initially I had "($val)" here. and I suppose there was a
	  ;; reason I put the parenthesis around. this will probably
	  ;; come back and bite me...
	  (if (and (flonum? val) (nanfl? val))
	      (display-string "NaN" p)
	      (template-display p
		 (?@ (< val 0) "(~@)")
		 "$val")))
	 ((string? val)
	  (template-display p
	     (?@ (use-mutable-strings?) "(new sc_String(~@))")
	     "\"~a\"" (my-string-for-read val)))
	 ((ucs2-string? val)
	  (template-display p
	     (?@ (use-mutable-strings?) "(new sc_String(~@))")
	     "\"~e\""
	     (let loop ((i 0))
		(unless (>= i (ucs2-string-length val))
		   (display-ucs2-char p (ucs2-string-ref val i))
		   (loop (+fx i 1))))))
	 ((eq? val #unspecified)
	  (display-string "undefined" p))
	 ((keyword? val)
	  (if (use-mutable-strings?)
	      (template-display p
		 "(new sc_Keyword(\"~a\"))"
		 (my-string-for-read (keyword->string! val)))
	      (template-display p
		 "\"~a~a\"" *keyword-prefix*
		 (my-string-for-read (keyword->string! val)))))
	 ((date? val)
	  (fprintf p "new Date( ~a000 )" (date->seconds val)))
	 ((assq val cache)
	  =>
	  (lambda (match)
	     (let ((ref (cdr match)))
		(if (fixnum? ref)
		    (begin
		       (display "sc_circle_ref(cache, " p)
		       (display-fixnum ref p)
		       (display ")" p))
		    (let ((i (next-index)))
		       (set-cdr! match i)
		       (display "sc_circle_def(cache, " p)
		       (display i p)
		       (display ", " p)
		       (compile-cyclic val p)
		       (display ")" p))))))
	 (else
	  (compile-cyclic val p))))
   
   (let ((exp (compile val p)))
      (values exp (>fx index -1))))

;*---------------------------------------------------------------------*/
;*    compile ::Const ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Const.compile p stmt? toplevel?)
   (with-access::Const this (value dstposition location)
      (with-access::Out-Env env (host-compiler host-register source-map?)
	 (when source-map?
	    (set! dstposition (output-port-position p)))
	 (template-display p
	    (?@ stmt? "~@;\n")
	    "~e" (compile-value value p host-compiler host-register this)))))

;*---------------------------------------------------------------------*/
;*    compile ::Ref ...                                                */
;*---------------------------------------------------------------------*/
(define-nmethod (Ref.compile p stmt? toplevel?)
   (with-access::Ref this (var dstposition)
      (with-access::Out-Env env (source-map?)
	 (when source-map?
	    (set! dstposition (output-port-position p))))
      (with-access::Named-Var var (js-id)
	 (template-display p
	    (?@ stmt? "~@;\n")
	    "$js-id"))))

;*---------------------------------------------------------------------*/
;*    compile ::Module ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Module.compile p stmt? toplevel?)
   ;; trampoline-code
   (with-access::Out-Env env (trampoline? max-tail-depth)
      (when trampoline?
	 (scm2js-globals-init p)
	 (let ((tail-obj (scm2js-global "TAIL_OBJECT"))
	       (max-tail-depth max-tail-depth))
	    [assert (max-tail-depth) (fixnum? max-tail-depth)]
	    (template-display p
	       "var sc_tailTmp;\n"
	       "var sc_tailObj = ~a;\n" tail-obj
	       "~a.calls = ~a;\n" tail-obj max-tail-depth
	       "~a.MAX_TAIL_CALLs = ~a;\n" tail-obj max-tail-depth
	       "var sc_funTailCalls = ~a;\n"max-tail-depth))))
   
   ;; note: due to "push-declarations" these are not all declared variables.
   ;;   Just the ones that need to be declared by us.
   (with-access::Module this (declared-vars body)
      (for-each (lambda (var)
		   (with-access::Named-Var var (js-id constant? value)
		      (unless (and constant? (cnst-function? var value))
			 (template-display p "var $js-id;\n"))))
	 declared-vars)
      
      ;; finally the body.
      (walk body p #t #t)))

;*---------------------------------------------------------------------*/
;*    compile ::Lambda ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Lambda.compile p stmt? toplevel?)
   (widen!::Out-Lambda this
      (id #f)
      (js-id #f))
   (walk this p stmt? #f))

;*---------------------------------------------------------------------*/
;*    compile ::Out-Lambda ...                                         */
;*---------------------------------------------------------------------*/
(define-nmethod (Out-Lambda.compile p stmt? toplevel?)
   ;; Llvalue/stmt?, if given, indicate, that this lambda should be assigned to
   ;; the given lvalue. stmt? is true, if we should treat this node, as if it was
   ;; a statement-node.
   ;; If there's a lvalue and it's not a stmt?, the return value of the compiled
   ;; code is unspecified. (ie, it doesn't anymore necessarily return the
   ;; lambda).
   
   (define (vaarg-code vaarg nb-args)
      (with-access::Named-Var vaarg (js-id)
	 (let ((tmp (gensym)))
	    (template-display p
	       "var $js-id = null;\n"
	       "for (var $tmp = arguments.length - 1;"
	       "     $tmp >= $(number->string nb-args);"
	       "     --$tmp) {\n"
	       "  $js-id = sc_cons(arguments[$tmp], $js-id);\n"
	       "}\n"))))
   
   (define (trampoline-start-code)
      (template-display p
	 "var sc_tailTmp;\n"
	 "var sc_tailObj;\n"
	 "var sc_funTailCalls = (sc_tailObj = ~a).calls;\n"
	 (scm2js-global "TAIL_OBJECT")))
   
   (define (split-formals/vaarg)
      (with-access::Lambda this (vaarg? formals)
	 (if (not vaarg?)
	     (values formals #f)
	     (let ((rev (reverse formals)))
		(values (reverse! (cdr rev))
		   (car rev))))))
   
   (define (debug-name id location)
      (cond
	 ((string? id)
	  id)
	 ((pair? location)
	  (format "lambda, ~a:~a" (cadr location) (caddr location)))
	 (else
	  "lambda")))
   
   (define (debug-arity fun)
      ;; the actual arity of the function (used in debug mode)
      (with-access::Out-Lambda this (vaarg? formals)
	 (if vaarg? (negfx (length formals)) (length formals))))
   
   (with-access::Out-Env env (debug? meta? use-strict?)
      (with-access::Out-Lambda this (lvalue-is-cnst id js-id
				       declared-vars body vaarg? formals
				       call/cc? contains-trampoline-call?
				       location dstposition)
	 ;; update the output location for source-map
	 (set! dstposition (output-port-position p))
	 ;; generate the function definition
	 (multiple-value-bind (formals-w/o-vaarg vaarg)
	    (split-formals/vaarg)
	    (if (and toplevel? lvalue-is-cnst)
		;; top-level constrants
		(template-display p
		   (?@ debug? "~@;~a.displayName=\"~a\";~a.sc_arity=~a;"
		      js-id
		      (debug-name id location)
		      js-id (debug-arity this))
		   (?? meta? "\n\n/*** META ((export ~a)) */" id)
		   (?? (and debug? (not meta?)) "\n\n/* ~a */" id)
		   (?@ #t "\nfunction ~a(~e) { ~@ }\n"
		      js-id
		      (separated ","
			 (lambda (e) "~e" (walk e p #f #f))
			 formals-w/o-vaarg))
		   (?? use-strict? "\n\"use strict\";\n")
		   (?@ vaarg? "~e ~@"
		      (with-access::Ref vaarg (var)
			 (vaarg-code var (- (length formals) 1))))
		   (?? (or call/cc? contains-trampoline-call?)
		      "~e" (scm2js-globals-init p))
		   (?? contains-trampoline-call? "~e" (trampoline-start-code))
		   (?@ call/cc?
		      "try {\n"
		      "  ~e"  (cond-expand
				 (enable-callcc (call/cc-start-code p))
				 (else #f))
		      "  ~@"   ;; body
		      "} ~e\n" (cond-expand
				  (enable-callcc (call/cc-end-code p))
				  (else #f)))
		   "  ~e" ;; declared-vars
		   "  ~e" ;; body
		   (each (lambda (var)
			    "var ~a;\n"
			    (with-access::Named-Var var (js-id) js-id))
		      declared-vars)
		   (walk body p #t #f))
		(let ((dbg-id (or js-id *tmp-var*)))
		   ;; lambda expressions
		   (template-display p
		      (?@ stmt? "~@;\n")
		      (?@ debug?
			 "(~a = ~@,~a.displayName=\"~a\",~a.sc_arity=~a,~a)"
			 dbg-id
			 dbg-id
			 (debug-name id location)
			 dbg-id (debug-arity this)
			 dbg-id)
		      (?@ (and (not debug?) js-id)
			 "~a = ~@"
			 js-id)
		      (?@ #t "function (~e) { ~@ }"
			 (separated ","
			    (lambda (e) "~e" (walk e p #f #f))
			    formals-w/o-vaarg))
		      (?? use-strict? "\n\"use strict\";\n")
		      (?@ vaarg? "~e ~@"
			 (with-access::Ref vaarg (var)
			    (vaarg-code var (- (length formals) 1))))
		      (?? (or call/cc? contains-trampoline-call?)
			 "~e" (scm2js-globals-init p))
		      (?? contains-trampoline-call? "~e" (trampoline-start-code))
		      (?@ call/cc?
			 "try {\n"
			 "  ~e"  (cond-expand
				    (enable-callcc (call/cc-start-code p))
				    (else #f))
			 "  ~@"   ;; body
			 "} ~e\n" (cond-expand
				     (enable-callcc (call/cc-end-code p))
				     (else #f)))
		      "  ~e" ;; declared-vars
		      "  ~e" ;; body
		      (each (lambda (var)
			       "/* v2 */ var ~a;\n"
			       (with-access::Named-Var var (js-id) js-id))
			 declared-vars)
		      (walk body p #t #f))))))))

;*---------------------------------------------------------------------*/
;*    compile ::Frame-alloc ...                                        */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-alloc.compile p stmt? toplevel?)
   (cond
      ((config 'frame-push)
       (template-display p
	  (?@ stmt? "~@;\n") ;; should never happen.
	  "{~e}" ;; literal-object creation
	  (with-access::Frame-alloc this (vars)
	     (unless (null? vars)
		(let loop ((vars vars))
		   (with-access::Named-Var (car vars) (js-id)
		      (if (null? (cdr vars))
			  (template-display p "$js-id : undefined")
			  (begin
			     (template-display p "$js-id : undefined, ")
			     (loop (cdr vars))))))))))
      ((config 'javascript-let)
       (template-display p
	  (?@ stmt? "~@;\n") ;; should never happen.
	  "/* WITH */ let"))
      (else
       (display "/* WITH */ " p)
       (with-access::Frame-alloc this (vars)
	  (for-each (lambda (v)
		       (with-access::Named-Var v (js-id)
			  (template-display p "var $js-id; ")))
	     vars)))))

;*---------------------------------------------------------------------*/
;*    compile ::Frame-push ...                                         */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-push.compile p stmt? toplevel?)
   
   (define (find-actual f exprs)
      ;; find the first (set! f val) in exprs
      (let loop ((exprs exprs))
	 (cond
	    ((null? exprs)
	     #f)
	    ((not (isa? (car exprs) Set!))
	     #f)
	    ((with-access::Set! (car exprs) (lvalue)
		(with-access::Ref lvalue (var)
		   (eq? f var)))
	     (car exprs))
	    (else
	     (loop (cdr exprs))))))
   
   (with-access::Frame-push this (body frame-alloc)
      (with-access::Frame-alloc frame-alloc (storage-var vars)
	 (cond
	    ((config 'frame-push)
	     (with-access::Named-Var storage-var (js-id)
		(template-display p
		   "with ($js-id) {\n"
		   "  ~e" (walk body p #t #f)
		   "}\n")))
	    ((config 'javascript-let)
	     (let ((nb-vars (length vars)))
		;; get the formals/actuals sequence to build the actual
		;; values of the call
		(with-access::Begin body (exprs)
		   (let loop ((actuals '())
			      (vars vars)
			      (exprs exprs))
		      (cond
			 ((null? vars)
			  (template-display p
			     (?@ stmt? "~@;\n")
			     (?@ (not stmt?) "~@\n")
			     " ~e {\n"
			     (separated ","
				(lambda (e::Set!)
				   "~e"
				   (walk e p #f #f))
				(reverse! actuals))
			     "~e}"
			     exprs))
			 ((find-actual (car vars) exprs)
			  =>
			  (lambda (a)
			     (loop (cons a actuals)
				(cdr vars)
				(remq! a exprs ))))
			 (else
			  (error #f "Internal Error: cannot find actual"
			     (node->list vars))))))))
	    (else
	     (walk frame-alloc p #t #f)
	     (walk body p #t #f))))))

;*---------------------------------------------------------------------*/
;*    compile ::If ...                                                 */
;*---------------------------------------------------------------------*/
(define-nmethod (If.compile p stmt? toplevel?)
   (with-access::If this (test then else dstposition location)
      (with-access::Out-Env env (source-map? debug?)
	 (when source-map?
	    (set! dstposition (output-port-position p)))
	 (cond
	    (stmt?
	     (template-display p
		(?@ (and debug? toplevel?)
		   "sc_context=hop_callback_html_context( \"~a\", \"~a\", ~a ); hop_current_stack_context=sc_context; try { ~@ } catch( e ) { hop_callback_handler(e, sc_context); }"
		   (match-case location
		      ((at ?fname ?-) fname)
		      (else "toplevel"))
		   (match-case location
		      ((at ?fname ?-) fname)
		      (else "???"))
		   (match-case location
		      ((at ?- ?point) point)
		      (else 1)))
		"if (~e) {\n" (compile-boolified p walk test)
		"  ~e"        (walk then p #t #f)
		"}\n"
		(?? (not (isa? else Const)) ;; avoid common case 'undefined
		   "else {\n"
		   "  ~e"    (walk else p #t #f)
		   "}\n")))
	    ((and (isa? then Const)
		  (eq? (with-access::Const then (value) value) #t))
	     ;; should nearly never happen...
	     ;; usually (or X Y) is converted into:
	     ;;     (tmp = X, X!==false? tmp: Y)
	     (template-display p
		"(~e || ~e)"
		(compile-boolified p walk test)
		(walk else p #f #f)))
	    ((and (isa? else Const)
		  (eq? (with-access::Const else (value) value) #f))
	     ;; happens more often.
	     (template-display p
		"(~e && ~e)"
		(compile-boolified p walk test)
		(walk then p #f #f)))
	    (else
	     (template-display p
		"(~e? ~e: ~e)"
		(compile-boolified p walk test)
		(walk then p #f #f)
		(walk else p #f #f)))))))

;*---------------------------------------------------------------------*/
;*    compile ::Case ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Case.compile p stmt? toplevel?)
   (with-access::Case this (key clauses dstposition)
      (with-access::Out-Env env (source-map?)
	 (when source-map?
	    (set! dstposition (output-port-position p))))
      (template-display p
	 "switch (~e) {\n" (walk key p #f #f)
	 "  ~e"    (for-each (lambda (clause) (walk clause p #t #f)) clauses)
	 "}\n")))

;*---------------------------------------------------------------------*/
;*    compile ::Clause ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Clause.compile p stmt? toplevel?)
   (with-access::Clause this (default-clause? consts expr dstposition)
      (with-access::Out-Env env (source-map?)
	 (when source-map?
	    (set! dstposition (output-port-position p))))
      (if default-clause?
	  (template-display p
	     "default:\n"
	     "~e" (walk expr p #t #f)
	     "break;\n")
	  (template-display p
	     ;; consts
	     "~e" (for-each (lambda (const)
			       (template-display p
				  "case ~e:\n" (walk const p #f #f)))
			    consts)
	     ;; body
	     "~e" (walk expr p #t #f)
	     "break;\n"))))

;*---------------------------------------------------------------------*/
;*    cnst-function? ...                                               */
;*---------------------------------------------------------------------*/
(define (cnst-function? var val)
   (with-access::Var var (constant? kind export-desc)
      (when (isa? val Lambda)
	 (and constant?
	      (or (not (eq? kind 'exported))
		  (with-access::Export-Desc export-desc (exported-as-const?)
		     exported-as-const?))))))

;*---------------------------------------------------------------------*/
;*    compile ::Decl-Set! ...                                          */
;*---------------------------------------------------------------------*/
(define-nmethod (Decl-Set!.compile p stmt? toplevel?)
   [assert (stmt?) stmt?]
   (with-access::Set! this (lvalue val dstposition)
      (with-access::Ref lvalue (var)
	 (with-access::Named-Var var (id js-id)
	    (with-access::Out-Env env (source-map? debug?)
	       (cond
		  ((and (isa? val Frame-alloc) (not (config 'frame-push)))
		   (compile val env p stmt? #f))
		  ((cnst-function? var val)
		   ;; strict impose top level functions only
		   (if toplevel?
		       (let ((nval (widen!::Out-Lambda val
				      (lvalue-is-cnst #t)
				      (js-id js-id)
				      (id id))))
			  (compile nval env p #f #t))
		       (let ((nval (widen!::Out-Lambda val
				      (lvalue-is-cnst #t)
				      (js-id js-id)
				      (id id))))
			  (if debug?
			      (fprintf p "var ~a;" js-id)
			      (fprintf p "var "))
			  (compile nval env p #t #f))))
		  (else
		   (when source-map?
		      (set! dstposition (output-port-position p)))
		   (template-display p
		      "var ~e;\n" (compile-unoptimized-set! p walk this)))))))))

;*---------------------------------------------------------------------*/
;*    compile ::Set! ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.compile p stmt? toplevel?)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (with-access::Named-Var var (id js-id)
	    (cond
	       ((isa? val Lambda)
		(widen!::Out-Lambda val
		   (lvalue-is-cnst (cnst-function? var val))
		   (id id)
		   (js-id js-id))
		(walk val p stmt? toplevel?))
	       ((and (isa? val Frame-alloc) (not (config 'frame-push)))
		(walk val p stmt? #f))
	       (else
		(template-display p
		   (?@ stmt? "~@;\n")
		   (?@ (not stmt?) "(~@)") ;; for now...
		   "~e" (compile-set! p walk this))))))))

;*---------------------------------------------------------------------*/
;*    compile ::Begin ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Begin.compile p stmt? toplevel?)
   
   (define (call/cc-begin-out)
      (with-access::Begin this (exprs call/cc-ranges)
	 (template-display p
	    "switch (sc_callCcIndex) {\n"
	    " case 0:\n"
	    "  ~e"
	    "}\n"
	    (for-each (lambda (expr range)
			 (template-display p
			    (?? (and range (not (null? range)))
			       "~e\n" ;; the cases
			       (each (lambda (index)
					"case ~a: " index)
				  range))
			    "~e" (walk expr p #t #f)))
	       exprs
	       call/cc-ranges))))
   
   (with-access::Begin this (exprs call/cc? call/cc-ranges dstposition)
      (cond
	 ((and call/cc? (not (null? call/cc-ranges)))
	  (call/cc-begin-out))
	 (stmt?
	  ;; we do not need { }, as all statements that could contain blocks
	  ;; already have them (or do not need them).
	  (for-each (lambda (n) (walk n p #t toplevel?)) exprs))
	 (else
	  (with-access::Out-Env env (source-map?)
	     (when source-map?
		(set! dstposition (output-port-position p))))
	  (template-display p
	     "(~e)"
	     (separated ", "
		(lambda (e) "~e" (walk e p #f #f))
		exprs))))))

;*---------------------------------------------------------------------*/
;*    compile ::Call/cc-Resume ...                                     */
;*---------------------------------------------------------------------*/
(define-nmethod (Call/cc-Resume.compile p stmt? toplevel?)
   (template-display p
      (?@ stmt? "~@;\n")
      "sc_callCcIndex = 0"))

;*---------------------------------------------------------------------*/
;*    compile ::Call ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Call.compile p stmt? toplevel?)

   (define (static-arity)
      (with-access::Call this (operator)
	 (when (isa? operator Ref)
	    (with-access::Ref operator (var)
	       (with-access::Var var (kind id constant? value export-desc)
		  (when constant?
		     (with-access::Export-Desc export-desc (exported-as-const? arity)
			(cond
			   ((and exported-as-const? (integer? arity))
			    arity)
			   ((and (eq? kind 'local) (isa? value Lambda))
			    (with-access::Lambda value (arity)
			       arity))))))))))

   (define (compile-operator)
      ;; if the operator is a var-ref and contains a "." we have to do
      ;; some tricks: frame.f() would give 'frame' to f as this-object.
      ;; by using the sequence-construct we can avoid that:
      ;; (0,frame.f)() sends the global-object as 'this'-object.
      (with-access::Call this (operator)
	 (if (and (isa? operator Ref)
		  (with-access::Ref operator (var)
		     (with-access::Named-Var var (js-id)
			(string-index js-id #\.))))
	     (with-access::Ref operator (var)
		(with-access::Named-Var var (js-id)
		   (template-display p
		      "(0, $js-id)")))
	     (walk operator p #f #f))))
   
   (define (compile-operands)
      (with-access::Call this (operands)
	 (template-display p
	    "~e"
	    (separated ", "
	       (lambda (operand) "~e" (walk operand p #f #f))
	       operands))))
   
   (define (lambda-call? call::Call)
      (with-access::Call call (operator operands)
	 (when (isa? operator Lambda)
	    (with-access::Lambda operator (scope-vars)
	       (when (=fx (length operands) (length scope-vars)))))))
   
   (define (compile-trampoline-call)
      
      (define (do-trampoline-call tail-obj)
	 (with-access::Call this (operator)
	    (template-display p
	       "(~a.f = ~e," tail-obj (walk operator p #f #f)
	       " ~a.f(~e))"  tail-obj (compile-operands))))
      
      (let ((operator (with-access::Call this (operator) operator))
	    (max-tail-depth (with-access::Out-Env env (max-tail-depth) max-tail-depth)))
	 (with-access::Call this (operator)
	    (template-display p
	       "(this === sc_tailObj?\n"
	       "     (!sc_funTailCalls?\n"  ;; == 0
	       "          (this.args = [~e],"   (compile-operands)
	       "           this.f = ~e,"        (walk operator p #f #f)
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
			      trampoline? location location dstposition)
      (with-access::Out-Env env (debug? call/cc? arity-check? optimize-calls? source-map?)
	 (when source-map?
	    (newline p)
	    ;; 1 for the open parenthesis
	    (set! dstposition (+fx (output-port-position p) 1)))
	 (cond
	    ((not (lambda-call? this))
	     (template-display p
		(?@ (and debug? toplevel?)
		   "sc_context=hop_callback_html_context( \"~a\", \"~a\", ~a ); hop_current_stack_context=sc_context; try { ~@ } catch( e ) { hop_callback_handler(e, sc_context); }"
		   (match-case location
		      ((at ?fname ?-) fname)
		      (else "toplevel"))
		   (match-case location
		      ((at ?fname ?-) fname)
		      (else "???"))
		   (match-case location
		      ((at ?- ?point) point)
		      (else 1)))
		(?@ stmt? "~@;\n")
		;;  was (not stmt?). now always. even for stmt.
		(?@ #t "(~@)")
		(?@ (and call/cc? call/cc-index)
		   "sc_callCcIndex = ~a, ~@" call/cc-index)
		"~e"
		(cond
		   ((and optimize-calls?
			 (compile-optimized-call p walk operator operands
			    debug?))
		    ;; already printed
		    'do-nothing) 
		   (trampoline?
		    (compile-trampoline-call))
		   ((and arity-check? (not call/cc?))
		    (let ((len (length operands))
			  (sa (static-arity)))
		       (cond
			  ((not (integer? sa))
			   (template-display p
			      "sc_arity_check(~e, ~a)(~e)"
			      (compile-operator) len
			      (compile-operands)))
			  ((or (=fx sa len) (>=fx len (negfx (-fx 1 sa))))
			   (template-display p
			      "~e(~e)" (compile-operator) (compile-operands)))
			  (else
			   (scheme2js-error "out"
			      (format "Wrong number of arguments: ~a expected, ~ provided" sa len)
			      this
			      location)))))
		   (else
		    (template-display p
		       "~e(~e)" (compile-operator) (compile-operands))))))
	    ((config 'javascript-let)
	     ;; the call is actually a disguised let
	     (with-access::Call this (operator operand)
		(with-access::Lambda operator (body scope-vars)
		   (template-display p
		      "let (~e) {\n"
		      (each (lambda (var val)
			       "~a = ~e"
			       (with-access::Named-Var var (js-id)
				  js-id)
			       (walk val p #f #f))
			 scope-vars
			 operands)
		      "~e}\n"
		      (walk body p stmt? #f)))))
	    (else
	     (template-display p
		(?@ stmt? "~@;\n")
		;;  was (not stmt?). now always. even for stmt.
		(?@ #t "(~@)")
		(?@ (and call/cc? call/cc-index)
		   "sc_callCcIndex = ~a, ~@" call/cc-index)
		"~e(~e)" (compile-operator) (compile-operands)))))))

;*---------------------------------------------------------------------*/
;*    compile ::While ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (While.compile p stmt? toplevel?)
   
   (define (body-out)
      (with-access::While this (body dstposition)
	 (with-access::Out-Env env (host-compiler host-register source-map?)
	    (when source-map?
	       (set! dstposition (output-port-position p))))
	 (cond-expand
	    (enable-callcc
	     (call/cc-while-body-out this env p)))
	 (template-display p
	    "~e" (walk body p #t #f))))
   
   (with-access::While this (init test body label call/cc?)
      (template-display p
	 ;; init
	 (?? (not (isa? init Const)) "~e" (walk init p #t #f))
	 ;; label
	 (?? (not (eq? label (default-label)))
	    "~a:\n" (mangle-JS-sym (with-access::Label label (id) id))))
      
      (cond
	 ((and (isa? test Const)
	       (eq? (with-access::Const test (value) value) #t))
	  (template-display p
	     "do {\n"
	     "  ~e" (body-out)
	     "} while (true);\n"))
	 ((not call/cc?)
	  (template-display p
	     "while (~e) {\n" (compile-boolified p walk test)
	     "  ~e"           (walk body p #t #f)
	     "}\n"))
	 (else
	  (template-display p
	     "while (sc_callCcIndex || (~e)) {\n" (walk test p #f #f)
	     "  ~e"                               (body-out)
	     "}\n")))))

;*---------------------------------------------------------------------*/
;*    compile ::Continue ...                                           */
;*---------------------------------------------------------------------*/
(define-nmethod (Continue.compile p stmt? toplevel?)
   (with-access::Continue this (label)
      (if (eq? label (default-label))
	  (template-display p
	     "continue;\n")
	  (template-display p
	     "continue ~a;\n" (mangle-JS-sym (with-access::Label label (id) id))))))

;*---------------------------------------------------------------------*/
;*    compile ::Return ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Return.compile p stmt? toplevel?)
   (with-access::Return this (val dstposition)
      (with-access::Out-Env env (host-compiler host-register source-map?)
	 (when source-map?
	    (set! dstposition (+fx 8 (output-port-position p)))))
      (template-display p
	 "return (~e);\n" (walk val p #f #f))))

;*---------------------------------------------------------------------*/
;*    compile ::Labeled ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Labeled.compile p stmt? toplevel?)
   (with-access::Labeled this (label body)
      (with-access::Label label (id)
	 (template-display p
	    "~a: {\n" (mangle-JS-sym id)
	    "  ~e"    (walk body p #t #f)
	    "}\n"))))

;*---------------------------------------------------------------------*/
;*    compile ::Break ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Break.compile p stmt? toplevel?)
   (with-access::Break this (label val)
      (with-access::Label label (id)
	 (template-display p
	    "{\n"
	    "  ~e" (walk val p #t #f)
	    "  break ~a;\n" (if (eq? label (default-label))
				""
				(mangle-JS-sym id))
	    "}\n"))))

; (define-nmethod (Pragma.compile p stmt?)
;    (with-access::Pragma this (str)
;       (template-display p
; 	 (?@ stmt? "~@;\n")
; 	 "(~a)" str)))


;*---------------------------------------------------------------------*/
;*    compile ::Pragma ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Pragma.compile p stmt? toplevel?)
   
   (define (pragma->str p)
      (with-access::Pragma p (str args)
	 (if (null? args)
	     str
	     (let* ((sport (open-input-string str))
		    (oport (open-output-string))
		    (args (list->vector args))
		    (parser (regular-grammar ()
			       ((: #\$ (+ (in (#\0 #\9))))
				(let* ((str   (the-string))
				       (len   (the-length))
				       (index (string->number
						 (substring str 1 len))))
				   (walk (vector-ref args (-fx index 1))
				      oport #f #f)
				   (ignore)))
			       ("$$"
				  (display "$" oport)
				  (ignore))
			       ((+ (out #\$))
				(display (the-string) oport)
				(ignore))
			       (else
				(the-failure)))))
	     (read/rp parser sport)
	     (close-input-port sport)
	     (close-output-port oport)))))
      
   (with-access::Out-Env env (pp? pragmas)
      (let ((to-be-displayed (pragma->str this)))
	 (template-display p
	    (?@ stmt? "~@;\n")
	    "(~a)" to-be-displayed))))

;*---------------------------------------------------------------------*/
;*    add-pragma! ...                                                  */
;*---------------------------------------------------------------------*/
(define (add-pragma! pragmas::Pragmas p)
   (with-access::Pragmas pragmas (lock last-pragma)
      (synchronize lock
	 (let ((tmp (list p)))
	    (set-cdr! last-pragma tmp)
	    (set! last-pragma tmp)))))

;*---------------------------------------------------------------------*/
;*    consume-next-pragma! ...                                         */
;*---------------------------------------------------------------------*/
(define (consume-next-pragma! pragmas::Pragmas)
   (with-access::Pragmas pragmas (lock pragmas)
      (synchronize lock
	 (when (null? (cdr pragmas))
	    ;; should never happen
	    (error "out"
	       "Internal Error: consume-pragma call without pragma"
	       #f))
	 (let ((res (cadr pragmas)))
	    (set! pragmas (cdr pragmas))
	    res))))


