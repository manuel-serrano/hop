(module scheme2js
   (option (loadq "protobject-eval.sch"))
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
	   statements
	   node-elimination
	   traverse
	   inline
	   capture
	   protobject
	   liveness
	   verbose)
   (main my-main)
   (export (scheme2js top-level::pair-nil js-interface::pair-nil config)
	   (scheme2js-compile-files! in-files::pair out-file::bstring config)
	   (default-scheme2js-config)))

;; TODO: automate this...
(define *version* 0.1)

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
		  (optimize-calls #t)
		  (optimize-var-number #f)
		  (optimize-boolify #t)
		  (encapsulate-parts #f)
		  (print-locations #f)))
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

(define (dot-out tree)
   (let ((imported tree.imported))
      (delete! tree.imported)
      (pobject-dot-out tree)
      (if imported
	  (set! tree.imported imported))))

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
      (("--encapsulate-parts"
	(help "encapsulates subparts, so they don't flood the surrounding scope with local vars."))
       (hashtable-put! config-ht 'encapsulate-parts #t))
      (("--optimize-var-number"
	(help "reduce used variables by reusing existing vars."))
       (hashtable-put! config-ht 'optimize-var-number #t))
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
      (("--no-tailrec"
	(help "don't optimize tail-recs."))
       (hashtable-put! config-ht 'optimize-tail-rec #f))
      (("--no-optimize-calls"
	(help "don't inline simple runtime-functions."))
       (hashtable-put! config-ht 'optimize-calls #f))
      (("--no-optimize-consts"
	(help "don't factor constants, but recreate them at every use."))
       (hashtable-put! config-ht 'optimize-consts #f))
      (("--no-optimize-boolify"
	(help "always test against false. Even if the test is a bool."))
       (hashtable-put! config-ht 'optimize-boolify #f))
      (("-d" ?stage (help "debug stage"))
       (hashtable-put! config-ht 'debug-stage (string->symbol stage)))
      ((("-l" "--print-locs") (help "print locations"))
       (hashtable-put! config-ht 'print-locations #t))
      (else
       (set! *in-files* (cons else *in-files*)))))

(define (scheme2js top-level js-interface config-ht)
   (config-init! config-ht)
   (let* ((tmp (extract-js-interface top-level js-interface))
	  (top-level-s (cons 'begin (car tmp))) ;; top-level-s plitted
	  (extended-js-interface (cdr tmp))
	  (top-level-e (my-expand top-level-s))
	  (dummy (if (eq? (config 'debug-stage) 'expand)
		     (pp top-level-e)))
	  (tree (pobject-conv top-level-e)))
      (if (eq? (config 'debug-stage) 'tree) (dot-out tree))
      (symbol-resolution tree extended-js-interface)
      (if (eq? (config 'debug-stage) 'symbol) (dot-out tree))
      (node-elimination! tree)
      (if (eq? (config 'debug-stage) 'node-elim1) (dot-out tree))
      (side-effect tree)
      (if (eq? (config 'debug-stage) 'side) (dot-out tree))
      (tail-rec! tree)
      (if (eq? (config 'debug-stage) 'tail) (dot-out tree))
      (capture! tree)
      (if (eq? (config 'debug-stage) 'capture) (dot-out tree))
      (inline! tree)
      (if (eq? (config 'debug-stage) 'inline) (dot-out tree))
      (statements! tree)
      (if (eq? (config 'debug-stage) 'statements) (dot-out tree))
      (node-elimination! tree)
      (if (eq? (config 'debug-stage) 'node-elim2) (dot-out tree))
      ;(liveness tree)
      (let ((compiled (compile tree)))
	 (if (eq? (config 'debug-stage) 'compiled) (dot-out tree))
	 (verbose "--- compiled")
	 (unless (config 'debug-stage)
	    (print compiled))
	 )))

(define (scheme2js-compile-files! in-files out-file config-ht)
   ;; we need this for "verbose" outputs.
   (config-init! config-ht)
   (let ((top-level (read-files (reverse! in-files))))
      (if (string=? "-" out-file)
	  (scheme2js top-level '() config-ht)
	  (with-output-to-file out-file
	     (lambda () (scheme2js top-level '() config-ht))))))

(define (my-main args)
   (let ((config-ht (default-scheme2js-config)))
      (handle-args args config-ht)
      (if (or (null? *in-files*)
	      (not *out-file*))
	  (error #f "missing in or output-file. Use --help to see the usage." #f))
      
      (scheme2js-compile-files! *in-files* *out-file* config-ht)))
