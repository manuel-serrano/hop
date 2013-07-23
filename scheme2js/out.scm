;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/out.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Tue Jul 23 10:56:42 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
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
	   push-declarations
	   error)
   
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
	      host-compiler::procedure
	      host-register::procedure
	      pp?::bool
	      ;; ::Pragmas or #f
	      pragmas       
	      last-line
	      last-file)
	   
	   (wide-class Out-Lambda::Lambda
	      lvalue))
   
   (export (compile-value ::obj ::output-port ::procedure ::procedure ::obj)
	   (out tree::Module p)))

;*---------------------------------------------------------------------*/
;*    out ...                                                          */
;*---------------------------------------------------------------------*/
(define (out tree p)
   (verbose "Compiling")
   (gen-var-names tree)
   (push-var-declarations tree)
   (cond-expand
      ((not enable-threads)
       (config-set! 'pp #f)
       (when (>=fx (bigloo-debug) 3)
	  (warning "pretty-printing/compression only works when pthreads are enabled"))))
   (if (config 'pp)
       (pp-gen-code tree p)
       (gen-code tree p #f)))

;*---------------------------------------------------------------------*/
;*    pp-gen-code ...                                                  */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    gen-code ...                                                     */
;*---------------------------------------------------------------------*/
(define (gen-code tree p pragmas)
   (verbose "  generating code")
   (let ((env (instantiate::Out-Env
		 (trampoline? (and (config 'trampoline) #t))
		 (max-tail-depth (config 'max-tail-depth))
		 (suspend-resume? (and (config 'suspend-resume) #t))
		 (call/cc? (and (config 'call/cc) #t))
		 (optimize-calls? (and (config 'optimize-calls) #t))
		 (debug? (and (config 'debug) #t))
		 (host-compiler (config 'host-compiler))
		 (host-register (config 'host-register))
		 (pp? (and (config 'pp) #t))
		 (pragmas pragmas)
		 (last-line 0)
		 (last-file ""))))
      (compile tree env p #t *tmp-var*)))

(define *tmp-var* "sc_tmp") ;; can't conflict with generated names.
(define *tmp-let* "sc_let")

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

(define (call/cc-frame-name scope)
   (with-access::Call/cc-Scope-Info scope (id)
      (string-append "'frame-" (symbol->string id) "'")))

(define (call/cc-counter-name nb)
   (string-append "sc_counter_" (number->string nb)))

(define-nmethod (Node.compile p stmt? tmp)
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
			(if (config 'use-strict) "\"use strict\";" ""))
	       p)))
      (compile-value-circle obj p host-compiler loc cache)
      (when (pair? cache)
	 (display ";})" p))))

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
	  (template-display p
	     (?@ (< val 0) "(~@)")
	     "$val"))
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
   
   (compile val p))
   
(define-nmethod (Const.compile p stmt? tmp)
   (with-access::Const this (value)
      (with-access::Out-Env env (host-compiler host-register)
	 (template-display p
	    (?@ stmt? "~@;\n")
	    "~e" (compile-value value p host-compiler host-register this)))))

(define-nmethod (Ref.compile p stmt? tmp)
   (with-access::Ref this (var)
      (with-access::Named-Var var (js-id)
	 (template-display p
	    (?@ stmt? "~@;\n")
	    "$js-id"))))

(define-nmethod (Module.compile p stmt? tmp)
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
		   (with-access::Named-Var var (js-id)
		      (template-display p "var $js-id;\n")))
		declared-vars)

      ;; finally the body.
      (walk body p #t tmp)))

;*---------------------------------------------------------------------*/
;*    compile ::Lambda ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Lambda.compile p stmt? tmp)
   (widen!::Out-Lambda this
      (lvalue #f))
   (walk this p stmt? tmp))

;*---------------------------------------------------------------------*/
;*    compile ::Out-Lambda ...                                         */
;*---------------------------------------------------------------------*/
(define-nmethod (Out-Lambda.compile p stmt? tmp)
   ;; Llvalue/stmt?, if given, indicate, that this lambda should be assigned to
   ;; the given lvalue. stmt? is true, if we should treat this node, as if it was
   ;; a statement-node.
   ;; If there's a lvalue and it's not a stmt?, the return value of the compiled
   ;; code is unspecified. (ie, it doesn't anymore necessarily return the
   ;; lambda).
   
   (define (vaarg-code vaarg nb-args)
      (with-access::Named-Var vaarg (js-id)
	 (template-display p
	    "var $js-id = null;\n"
	    "for (var $tmp = arguments.length - 1;"
	    "     $tmp >= $(number->string nb-args);"
	    "     --$tmp) {\n"
	    "  $js-id = sc_cons(arguments[$tmp], $js-id);\n"
	    "}\n")))

   (define (trampoline-start-code)
      (template-display p
	 "var sc_tailTmp;\n"
	 "var sc_tailObj;\n"
	 "var sc_funTailCalls = (sc_tailObj = ~a).calls;\n"
	 (scm2js-global "TAIL_OBJECT")))

   (define (declare/reset-counter-variables)
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (if (zerofx? call/cc-nb-while-counters)
	     (template-display p "/*do nothing*/\n")
	     (let loop ((i 0))
		(unless (>=fx i call/cc-nb-while-counters)
		   (template-display p
		      "var ~a = 0;\n" (call/cc-counter-name i))
		   (loop (+fx i 1)))))))

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
		    "return ~e" (emit-commands (cdr rev-hoisted))))
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

   (define (decrement-counter-variables)
      ;; by decrementing all counter-variables, we can increment the variable
      ;; unconditionally at the beginning of whiles, thus removing an 'if'.
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (let loop ((i 0))
	    (unless (>=fx i call/cc-nb-while-counters)
	       (template-display p
		  "~a--;\n" (call/cc-counter-name i))
	       (loop (+fx i 1))))))
   
   (define (call/cc-start-code)
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

   (define (call/cc-end-code)
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

   (define (split-formals/vaarg)
      (with-access::Lambda this (vaarg? formals)
	 (if (not vaarg?)
	     (values formals #f)
	     (let ((rev (reverse formals)))
		(values (reverse! (cdr rev))
			(car rev))))))

   (define (debug-name lvalue location)
      (cond
	 ((isa? lvalue Ref)
	  (with-access::Ref lvalue (var)
	     (with-access::Var var (id kind)
		(if (and (eq? kind 'local) location)
		    (format "~a,~a:~a" id (cadr location) (caddr location))
		    id))))
	 (lvalue
	  (let ((op (open-output-string)))
	     (walk lvalue op #f tmp)
	     (close-output-port op)))
	 (location
	  (format "&#955;.~a:~a" (cadr location) (caddr location)))
	 (else
	  "lambda")))
   
   (with-access::Out-Lambda this (lvalue declared-vars body vaarg? formals
					 call/cc? contains-trampoline-call?
					 location)
      (receive (formals-w/o-vaarg vaarg)
	 (split-formals/vaarg)

	 (let* ((debug? (with-access::Out-Env env (debug?) debug?))
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
		(needs-parenthesis? (or (and stmt? (not lvalue))
					(and (not stmt?) lvalue)
					debug?))
		(lvar (when (isa? lvalue Ref)
			 (with-access::Ref lvalue (var)
			    (with-access::Named-Var var (js-id)
			       js-id)))))
				 
	    (template-display p
	       (?@ stmt? "~@;\n")
	       (?@ needs-parenthesis? "(~@)")
	       (?@ (and debug? lvar)
		   "~a=~@,~a.displayName=\"~a\",~a.sc_location=\"~a\",~a.sc_arity=~a,~a"
		   lvar
		   lvar
		   (debug-name lvalue location)
		   lvar location
		   lvar (if vaarg?
			   (negfx (length formals))
			   (length formals))
		   lvar)
	       (?@ (and debug? (not lvar))
		   "~a=~@,~a.displayName=\"~a\",~a.sc_location=\"~a\",~a.sc_arity=~a,~a"
		   tmp
		   tmp
		   (debug-name lvalue location)
		   tmp location
		   tmp (if vaarg?
			   (negfx (length formals))
			   (length formals))
		   tmp)
	       (?@ (and (not debug?) lvar) "~e = ~@" lvar)
	       (?@ #t "function(~e) {~a ~e ~@ }" ;; always.
		   (separated ","
			      (lambda (e) "~e" (walk e p #f tmp))
			      formals-w/o-vaarg)
		   (if (config 'use-strict) "\"use strict\";" "")
		   (when vaarg?
		      (with-access::Ref vaarg (var)
			 (vaarg-code var (- (length formals) 1)))))
	       (?? (or call/cc? contains-trampoline-call?)
		   "~e" (scm2js-globals-init p))
	       (?? contains-trampoline-call? "~e" (trampoline-start-code))
	       (?@ call/cc?
		   "try {\n"
		   "  ~e"   (call/cc-start-code)
		   "  ~@"   ;; body
		   "} ~e\n" (call/cc-end-code))
	       "  ~e" ;; declared-vars
	       "  ~e" ;; body
	       (each (lambda (var)
			"var ~a;\n"
			(with-access::Named-Var var (js-id) js-id))
		     declared-vars)
	       (walk body p #t tmp))))))

;*---------------------------------------------------------------------*/
;*    compile ::Frame-alloc ...                                        */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-alloc.compile p stmt? tmp)
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

;*---------------------------------------------------------------------*/
;*    compile ::Frame-push ...                                         */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-push.compile p stmt? tmp)
   (with-access::Frame-push this (body frame-allocs)
      (with-access::Frame-alloc (car frame-allocs) (storage-var)
	 (with-access::Named-Var storage-var (js-id)
	    (template-display p
	       "with ($js-id) {\n"
	       "  ~e" (walk body p #t tmp)
	       "}\n")))))

;*---------------------------------------------------------------------*/
;*    compile ::If ...                                                 */
;*---------------------------------------------------------------------*/
(define-nmethod (If.compile p stmt? tmp)
   (with-access::If this (test then else)
      (cond
	 (stmt?
	  (template-display p
	     "if (~e) {\n" (compile-boolified p walk test tmp)
	     "  ~e"        (walk then p #t tmp)
	     "}\n"
	     (?? (not (isa? else Const)) ;; avoid common case 'undefined
		 "else {\n"
		 "  ~e"    (walk else p #t tmp)
		 "}\n")))
	 ((and (isa? then Const)
	       (eq? (with-access::Const then (value) value) #t))
	  ;; should nearly never happen...
	  ;; usually (or X Y) is converted into:
	  ;;     (tmp = X, X!==false? tmp: Y)
	  (template-display p
	     "(~e || ~e)"
	     (compile-boolified p walk test tmp)
	     (walk else p #f tmp)))
	 ((and (isa? else Const)
	       (eq? (with-access::Const else (value) value) #f))
	  ;; happens more often.
	  (template-display p
	     "(~e && ~e)"
	     (compile-boolified p walk test tmp)
	     (walk then p #f tmp)))
	 (else
	  (template-display p
	     "(~e? ~e: ~e)"
	     (compile-boolified p walk test tmp)
	     (walk then p #f tmp)
	     (walk else p #f tmp))))))

;*---------------------------------------------------------------------*/
;*    compile ::Case ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Case.compile p stmt? tmp)
   (with-access::Case this (key clauses)
      (template-display p
	 "switch (~e) {\n" (walk key p #f tmp)
	 "  ~e"    (for-each (lambda (clause) (walk clause p #t tmp)) clauses)
	 "}\n")))

;*---------------------------------------------------------------------*/
;*    compile ::Clause ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Clause.compile p stmt? tmp)
   (with-access::Clause this (default-clause? consts expr)
      (if default-clause?
	  (template-display p
	     "default:\n"
	     "~e" (walk expr p #t tmp)
	     "break;\n")
	  (template-display p
	     ;; consts
	     "~e" (for-each (lambda (const)
			       (template-display p
				  "case ~e:\n" (walk const p #f tmp)))
			    consts)
	     ;; body
	     "~e" (walk expr p #t tmp)
	     "break;\n"))))

;*---------------------------------------------------------------------*/
;*    compile ::Decl-Set! ...                                          */
;*---------------------------------------------------------------------*/
(define-nmethod (Decl-Set!.compile p stmt? tmp)
   [assert (stmt?) stmt?]
   (with-access::Set! this (lvalue val)
      (template-display p
	 "var ~e;\n" (compile-unoptimized-set! p walk this tmp))))

;*---------------------------------------------------------------------*/
;*    compile ::Set! ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.compile p stmt? tmp)
   (with-access::Set! this (lvalue val)
      (if (isa? val Lambda)
	  (begin
	     (widen!::Out-Lambda val
		(lvalue lvalue))
	     (walk val p stmt? tmp))
	  (template-display p
	     (?@ stmt? "~@;\n")
	     (?@ (not stmt?) "(~@)") ;; for now...
	     "~e" (compile-set! p walk this tmp)))))

;*---------------------------------------------------------------------*/
;*    compile ::Begin ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Begin.compile p stmt? tmp)
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
			    "~e" (walk expr p #t tmp)))
	       exprs
	       call/cc-ranges))))
   
   (with-access::Begin this (exprs call/cc? call/cc-ranges)
      (cond
	 ((and call/cc? (not (null? call/cc-ranges)))
	  (call/cc-begin-out))
	 (stmt?
	  ;; we do not need { }, as all statements that could contain blocks
	  ;; already have them (or do not need them).
	  (for-each (lambda (n) (walk n p #t tmp)) exprs))
	 (else
	  (template-display p
	     "(~e)"
	     (separated ", "
		(lambda (e) "~e" (walk e p #f tmp))
		exprs))))))

;*---------------------------------------------------------------------*/
;*    compile ::Call/cc-Resume ...                                     */
;*---------------------------------------------------------------------*/
(define-nmethod (Call/cc-Resume.compile p stmt? tmp)
   (template-display p
      (?@ stmt? "~@;\n")
      "sc_callCcIndex = 0"))

;*---------------------------------------------------------------------*/
;*    compile ::Call ...                                               */
;*---------------------------------------------------------------------*/
(define-nmethod (Call.compile p stmt? tmp)
   
   (define (compile-operator tmp)
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
	     (walk operator p #f tmp))))
   
   (define (compile-operands)
      (with-access::Call this (operands)
	 (template-display p
	    "~e"
	    (separated ", "
	       (lambda (operand) "~e" (walk operand p #f tmp))
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
	       "(~a.f = ~e," tail-obj (walk operator p #f tmp)
	       " ~a.f(~e))"  tail-obj (compile-operands))))
      
      (let ((operator (with-access::Call this (operator) operator))
	    (max-tail-depth (with-access::Out-Env env (max-tail-depth) max-tail-depth)))
	 (with-access::Call this (operator)
	    (template-display p
	       "(this === sc_tailObj?\n"
	       "     (!sc_funTailCalls?\n"  ;; == 0
	       "          (this.args = [~e],"   (compile-operands)
	       "           this.f = ~e,"        (walk operator p #f tmp)
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
      (with-access::Out-Env env (debug? call/cc? optimize-calls?)
	 (cond
	    ((not (lambda-call? this))
	     (template-display p
		(?@ stmt? "~@;\n")
		;;  was (not stmt?). now always. even for stmt.
		(?@ #t "(~@)")
		(?@ (and call/cc? call/cc-index)
		   "sc_callCcIndex = ~a, ~@" call/cc-index)
		"~e"
		(cond
		   ((and optimize-calls?
			 (compile-optimized-call p walk operator operands tmp))
		    ;; already printed
		    'do-nothing) 
		   (trampoline?
		    (compile-trampoline-call))
		   ((and debug? (not call/cc?))
		    (let ((len (length operands)))
		       (template-display p
			  "sc_arity_check(~e, ~a)(~e)"
			  (compile-operator tmp) len (compile-operands))))
		   (else
		    (template-display p
		       "~e(~e)" (compile-operator tmp) (compile-operands))))))
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
			       (walk val p #f tmp))
			 scope-vars
			 operands)
		      "~e}\n"
		      (walk body p stmt? tmp)))))
	    (else
	     (template-display p
		(?@ stmt? "~@;\n")
		;;  was (not stmt?). now always. even for stmt.
		(?@ #t "(~@)")
		(?@ (and call/cc? call/cc-index)
		   "sc_callCcIndex = ~a, ~@" call/cc-index)
		"~e(~e)" (compile-operator *tmp-let*) (compile-operands)))))))

(define-nmethod (While.compile p stmt? tmp)
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

   (define (body-out)
      (with-access::While this (body call/cc? call/cc-finally-scopes
				     call/cc-counter-nb)
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
		(for-each finally-outs call/cc-finally-scopes))
	    "~e" (walk body p #t tmp))))

   (with-access::While this (init test body label call/cc?)
      (template-display p
	 ;; init
	 (?? (not (isa? init Const)) "~e" (walk init p #t tmp))
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
	     "while (~e) {\n" (compile-boolified p walk test tmp)
	     "  ~e"           (walk body p #t tmp)
	     "}\n"))
	 (else
	  (template-display p
	     "while (sc_callCcIndex || (~e)) {\n" (walk test p #f tmp)
	     "  ~e"                               (body-out)
	     "}\n")))))
			  
(define-nmethod (Continue.compile p stmt? tmp)
   (with-access::Continue this (label)
      (if (eq? label (default-label))
	  (template-display p
	     "continue;\n")
	  (template-display p
	     "continue ~a;\n" (mangle-JS-sym (with-access::Label label (id) id))))))

(define-nmethod (Return.compile p stmt? tmp)
   (with-access::Return this (val)
      (template-display p
	 "return ~e;\n" (walk val p #f tmp))))

(define-nmethod (Labeled.compile p stmt? tmp)
   (with-access::Labeled this (label body)
      (with-access::Label label (id)
	 (template-display p
	    "~a: {\n" (mangle-JS-sym id)
	    "  ~e"    (walk body p #t tmp)
	    "}\n"))))

(define-nmethod (Break.compile p stmt? tmp)
   (with-access::Break this (label val)
      (with-access::Label label (id)
	 (template-display p
	    "{\n"
	    "  ~e" (walk val p #t tmp)
	    "  break ~a;\n" (if (eq? label (default-label))
				""
				(mangle-JS-sym id))
	    "}\n"))))

; (define-nmethod (Pragma.compile p stmt? tmp)
;    (with-access::Pragma this (str)
;       (template-display p
; 	 (?@ stmt? "~@;\n")
; 	 "(~a)" str)))


(define-nmethod (Pragma.compile p stmt? tmp)
   
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
				      oport #f tmp)
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
      (when pp?
	 (add-pragma! pragmas (pragma->str this)))
      (let ((to-be-displayed (if pp?
				 ;; placeholder
				 #a000
				 (pragma->str this))))
	 (template-display p
	    (?@ stmt? "~@;\n")
	    "(~a)" to-be-displayed))))

(define (add-pragma! pragmas::Pragmas p)
   (with-access::Pragmas pragmas (lock last-pragma)
      (synchronize lock
	 (let ((tmp (list p)))
	    (set-cdr! last-pragma tmp)
	    (set! last-pragma tmp)))))

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


