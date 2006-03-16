(module scheme2js
   (option (loadq "protobject-eval.sch"))
   (import compile
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
   (export (scheme2js top-level::pair-nil js-interface::pair-nil)
	   (scheme2js-compile-files! in-files::pair out-file::bstring)))

;; TODO: automate this...
(define *version* 0.1)

(define (read-rev-port in-port)
   (with-input-from-port in-port
      (lambda ()
	 (let loop ((sexp (read))
		    (rev-res '()))
	    (if (eof-object? sexp)
		rev-res
		(loop (read) (cons sexp rev-res)))))))
   
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
				  (let* ((port (open-input-file file))
					 (res (read-rev-port port)))
				     (close-input-port port)
				     res))))
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

(define (handle-args args)
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
       (set! *verbose* #t))
      (("-o" ?file (help "The output file. '-' prints to stdout."))
       (set! *out-file* file))
      (section "Compile-flags")
      (("--no-js-dot-notation"
	(help "disallows the access of JS-fields with dots."))
       (set! *direct-js-object-access* #f))
      (("--encapsulate-parts"
	(help "encapsulates subparts, so they don't flood the surrounding scope with local vars."))
       (set! *encapsulate-parts* #t))
      (("--optimize-var-number"
	(help "reduce used variables by reusing existing vars."))
       (set! *optimize-var-number* #t))
      (("--unresolved-is-js"
	(help "unresolved vars are supposed to be js-vars."))
       (set! *unresolved=JS* #t))
      (("--js-this"
	(help "procedures may use Javascript's 'this' variable."))
       (set! *procedures-provide-js-this?* #t))
      (("--no-tailrec"
	(help "don't optimize tail-recs."))
       (set! *optimize-tail-rec* #f))
      (("--no-optimize-calls"
	(help "don't inline simple runtime-functions."))
       (set! *optimize-calls* #f))
      (("--no-optimize-consts"
	(help "don't factor constants, but recreate them at every use."))
       (set! *optimize-consts* #f))
      (("--no-optimize-boolify"
	(help "always test against false. Even if the test is a bool."))
       (set! *optimize-boolify* #f))
      (("-d" ?stage (help "debug stage"))
       (set! *debug-stage* (string->symbol stage)))
      (else
       (set! *in-files* (cons else *in-files*)))))

(define *debug-stage* #f)
(define (scheme2js top-level js-interface)
   (let* ((tmp (extract-js-interface top-level js-interface))
	  (top-level-s (cons 'begin (car tmp))) ;; top-level-s plitted
	  (extended-js-interface (cdr tmp))
	  (top-level-e (my-expand top-level-s))
	  (dummy (if (eq? *debug-stage* 'expand)
		     (pp top-level-e)))
	  (tree (pobject-conv top-level-e)))
      (if (eq? *debug-stage* 'tree) (dot-out tree))
      (symbol-resolution tree extended-js-interface)
      (if (eq? *debug-stage* 'symbol) (dot-out tree))
      (node-elimination! tree)
      (if (eq? *debug-stage* 'node-elim1) (dot-out tree))
      (side-effect tree)
      (if (eq? *debug-stage* 'side) (dot-out tree))
      (tail-rec! tree)
      (if (eq? *debug-stage* 'tail) (dot-out tree))
      (capture! tree)
      (if (eq? *debug-stage* 'capture) (dot-out tree))
      (inline! tree)
      (if (eq? *debug-stage* 'inline) (dot-out tree))
      (statements! tree)
      (if (eq? *debug-stage* 'statements) (dot-out tree))
      (node-elimination! tree)
      (if (eq? *debug-stage* 'node-elim2) (dot-out tree))
      ;(liveness tree)
      (let ((compiled (compile tree)))
	 (if (eq? *debug-stage* 'compiled) (dot-out tree))
	 (verbose "--- compiled")
	 (unless *debug-stage*
	    (print compiled))
	 )))

(define (scheme2js-compile-files! in-files out-file)
   (let ((top-level (read-files (reverse! in-files))))
      (if (string=? "-" out-file)
	  (scheme2js top-level '())
	  (with-output-to-file out-file
	     (lambda () (scheme2js top-level '()))))))

(define (my-main args)
   (handle-args args)
   (if (or (null? *in-files*)
	   (not *out-file*))
       (error #f "missing in or output-file. Use --help to see the usage." #f))

   (scheme2js-compile-files! *in-files* *out-file*))
