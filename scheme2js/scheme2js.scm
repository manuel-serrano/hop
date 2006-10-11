(module scheme2js
   (option (loadq "protobject-eval.sch"))
   (include "version.sch")
   (import config
	   compile
	   pobject-conv
	   js-interface
	   expand
	   expanders
	   dot-expand
	   symbol
	   side
	   tail-rec
	   hack-encapsulation
	   statements
	   node-elimination
	   traverse
	   inline
	   constant-propagation
	   var-propagation
	   while
	   trampoline
	   capture
	   protobject
	   liveness
	   rm-unused-vars
	   locations
	   callcc
	   verbose)
   (main my-main)
   (export (scheme2js top-level::pair-nil js-interface::pair-nil config p)
	   (scheme2js-compile-files! in-files::pair
				     out-file::bstring
				     js-interface::pair-nil
				     config)
	   (default-scheme2js-config)))

(define (default-scheme2js-config)
   (let ((ht (make-hashtable)))
      (for-each (lambda (p)
		   (hashtable-put! ht (car p) (cadr p)))
		'((direct-js-object-access #t)
		  (procedures-provide-js-this? #f)
		  (unresolved=JS #f)
		  (optimize-tail-rec #t)
		  (do-inlining #t)
		  (inline-globals #f)
		  (constant-propagation #t)
		  (var-propagation #t)
		  (while #f)
		  (correct-modulo #f)
		  (optimize-calls #t)
		  (optimize-var-number #f)
		  (optimize-boolify #t)
		  (optimize-set! #t)
		  (encapsulate-parts #f)
		  (trampoline #f)
		  (print-locations #f)
		  (return #f)
		  (call/cc #f)))
      ht))

(define (read-rev-port in-port)
   (with-input-from-port in-port
      (lambda ()
	 (let loop ((sexp (read (current-input-port) #t))
		    (rev-res '()))
	    (if (eof-object? sexp)
		rev-res
		(loop (read (current-input-port) #t) (cons sexp rev-res)))))))
   
;; just read all expressions we can get
(define (read-files files)
   (let loop ((files files)
	      (rev-top-level '()))
      (if (null? files)
	  (reverse! rev-top-level)
	  (let ((file (car files)))
	     (verbose "reading: " (if (string=? file "-")
				      "std-in"
				      file))
	     (let ((rev-sexps (if (string=? file "-")
				  (read-rev-port (current-input-port))
				  (let ((port (open-input-file file)))
				     (if port
					 (let ((res (read-rev-port port)))
					    (close-input-port port)
					    res)
					 (error "read-files"
						"couldn't open: "
						file))))))
		(loop (cdr files)
		      (append! rev-sexps rev-top-level)))))))

(define (dot-out p tree)
   (with-output-to-port p
      (lambda ()
	 (pobject-dot-out tree (lambda (id)
				  (not (memq id
					     '(imported traverse traverse0
							traverse1 traverse2
							traverse0! traverse1!
							traverse2! clone
							deep-clone
							already-defined?
							single-value
							var
   ))))))))

(define *out-file* #f)
(define *in-files* '())

(define (handle-args args config-ht)
   (args-parse (cdr args)
      (section "Help")
      (("?")
       (args-parse-usage #f))
      ((("-h" "--help") (help "?,-h,--help" "This help message"))
       (args-parse-usage #f))
      (section "Misc")
      ((("--version") (help "Version number"))
       (print *version*))
      ((("-v" "--verbose") (help "Verbose output"))
       (hashtable-put! config-ht 'verbose #t))
      (("-o" ?file (help "The output file. '-' prints to stdout."))
       (set! *out-file* file))
      (section "Compile-flags")
      (("--no-js-dot-notation"
	(help "disallows the access of JS-fields with dots."))
       (hashtable-put! config-ht 'direct-js-object-access #f))
      (("--mutable-strings"
	(help "use mutable strings."))
       (hashtable-put! config-ht 'mutable-strings #t))
      (("--encapsulate-parts"
	(help "encapsulates subparts, so they don't flood the surrounding scope with local vars."))
       (hashtable-put! config-ht 'encapsulate-parts #t))
      (("--optimize-var-number"
	(help "reduce used variables by reusing existing vars."))
       (hashtable-put! config-ht 'optimize-var-number #t))
      (("--optimize-while"
	(help "unstable while-optim."))
       (hashtable-put! config-ht 'while #t))
      (("--no-inlining"
	(help "don't inline at all"))
       (hashtable-put! config-ht 'do-inlining #f))
      (("--inline-globals"
	(help "inline global constants."))
       (hashtable-put! config-ht 'inline-globals #t))
      (("--unresolved-is-js"
	(help "unresolved vars are supposed to be js-vars."))
       (hashtable-put! config-ht 'unresolved=JS #t))
      (("--js-this"
	(help "procedures may use Javascript's 'this' variable."))
       (hashtable-put! config-ht 'procedures-provide-js-this #t))
      (("--js-return"
	(help "adds the special-form 'return!'."))
       (hashtable-put! config-ht 'return #t))
      (("--no-tailrec"
	(help "don't optimize tail-recs."))
       (hashtable-put! config-ht 'optimize-tail-rec #f))
      (("--trampoline"
	(help "add trampolines around tail-recursive calls"))
       (hashtable-put! config-ht 'trampoline #t))
      (("--max-tail-depth"
	?depth
	(help (string-append "maximum tail-call depth before "
			     "the trampoline is used. Default: "
			     (number->string (default-max-tail-call-depth)))))
       (let ((new-depth (string->number depth)))
	  (if (not new-depth)
	      (error #f
		     "--max-tail-depth requires a number as argument"
		     depth))
	  (hashtable-put! config-ht 'max-tail-depth new-depth)))
      (("--call/cc" (help "allow call/cc"))
       (hashtable-put! config-ht 'call/cc #t))
      (("--no-constant-propagation"
	(help "don't propagate constants."))
       (hashtable-put! config-ht 'constant-propagation #f))
      (("--correct-modulo"
	(help "(module -13 4) will return R5RS's 3 instead of faster -1."))
       (hashtable-put! config-ht 'correct-modulo #t))
      (("--no-optimize-calls"
	(help "don't inline simple runtime-functions."))
       (hashtable-put! config-ht 'optimize-calls #f))
      (("--no-optimize-consts"
	(help "don't factor constants, but recreate them at every use."))
       (hashtable-put! config-ht 'optimize-consts #f))
      (("--no-optimize-boolify"
	(help "always test against false. Even if the test is a bool."))
       (hashtable-put! config-ht 'optimize-boolify #f))
      ((("-l" "--print-locations") (help "print locations"))
       (hashtable-put! config-ht 'print-locations #t))
      (("-d" ?stage (help "debug compiler-stage"))
       (hashtable-put! config-ht 'debug-stage (string->symbol stage)))
      (else
       (set! *in-files* (cons else *in-files*)))))

(define (scheme2js top-level js-interface config-ht p)
   (config-init! config-ht)
   (let* ((tmp (extract-js-interface top-level js-interface))
	  (top-level-s (cons 'begin (car tmp))) ;; top-level-s plitted
	  (extended-js-interface (cdr tmp))
	  (top-level-e (my-expand top-level-s))
	  (dummy (if (eq? (config 'debug-stage) 'expand)
		     (pp top-level-e)))
	  (tree (pobject-conv top-level-e)))
      (if (eq? (config 'debug-stage) 'tree) (dot-out p tree))
      (symbol-resolution tree extended-js-interface)
      (if (eq? (config 'debug-stage) 'symbol) (dot-out p tree))
      (node-elimination! tree)
      (if (eq? (config 'debug-stage) 'node-elim1) (dot-out p tree))
      (tail-rec! tree)
      (if (eq? (config 'debug-stage) 'tail) (dot-out p tree))
      (inline! tree)
      (if (eq? (config 'debug-stage) 'inline) (dot-out p tree))
      (constant-propagation! tree)
      (if (eq? (config 'debug-stage) 'constant-propagation) (dot-out p tree))
      (var-propagation! tree)
      (if (eq? (config 'debug-stage) 'var-propagation) (dot-out p tree))
      (rm-unused-vars! tree)
      (if (eq? (config 'debug-stage) 'rm-unused-vars) (dot-out p tree))
      (capture! tree)
      (if (eq? (config 'debug-stage) 'capture) (dot-out p tree))
      (node-elimination! tree)
      (if (eq? (config 'debug-stage) 'node-elim2) (dot-out p tree))
      (while! tree)
      (if (eq? (config 'debug-stage) 'while) (dot-out p tree))
      (trampoline tree)
      (if (eq? (config 'debug-stage) 'trampoline) (dot-out p tree))

      (hack-encapsulation! tree)

      (callcc-check-points tree)
      (if (eq? (config 'debug-stage) 'call/cc-cp) (dot-out p tree))
      (statements! tree)
      (if (eq? (config 'debug-stage) 'statements) (dot-out p tree))
      (node-elimination! tree)
      (if (eq? (config 'debug-stage) 'node-elim3) (dot-out p tree))
      (callcc-late tree)
      (if (eq? (config 'debug-stage) 'call/cc-late) (dot-out p tree))
      (locations tree)
      (if (eq? (config 'debug-stage) 'locations) (dot-out p tree))
      (let* ((out-p (if (config 'debug-stage)
			(open-output-string)
			p))
	     (compiled (compile tree out-p)))
	 (if (config 'debug-stage) (close-output-port out-p))
	 (if (eq? (config 'debug-stage) 'compiled) (dot-out p tree))
	 (verbose "--- compiled")
	 )))

(define (scheme2js-compile-files! in-files out-file js-interface config-ht)
   ;; we need this for "verbose" outputs.
   (config-init! config-ht)
   (let ((top-level (read-files (reverse! in-files)))
	 (out-port (if (string=? "-" out-file)
		       (current-output-port)
		       (open-output-file out-file))))
      (scheme2js top-level js-interface config-ht out-port)))

(define (my-main args)
   (let ((config-ht (default-scheme2js-config)))
      (handle-args args config-ht)
      (if (or (null? *in-files*)
	      (not *out-file*))
	  (error #f "missing in or output-file. Use --help to see the usage." #f))
      
      (scheme2js-compile-files! *in-files* *out-file* '() config-ht)))
