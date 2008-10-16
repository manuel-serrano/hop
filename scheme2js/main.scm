(module scheme2js-main
   (import scheme2js
	   config)
   (include "version.sch")
   (main my-main))

(define (default-str help sym)
   (let ((p (assq sym (default-config-alist))))
      (if p
	  (with-output-to-string (lambda ()
				    (display* help
					   " (default: "
					   (if (boolean? (cdr p))
					       (if (cdr p)
						   "true"
						   "false")
					       (cdr p))
					   ")")))
	  help)))
   
(define *out-file* #f)
(define *in-file* #f)

(define (update-conf conf val)
   (set! *config* (extend-config *config* conf val)))

(define *config* #unspecified) ;; will be set in main

(define (handle-args args invert-flag?)
   (args-parse (cdr args)
      (section "Help")
      ((("-h" "--help") (help "This help message."))
       (args-parse-usage #f)
       (exit 0))
      (section "Misc")
      ((("--version") (help "Version number."))
       (print *version*))
      ((("-v" "--verbose") (help "Verbose output."))
       (update-conf 'verbose #t))
      (("-o" ?file (help "The output file. '-' prints to stdout."))
       (set! *out-file* file))
      (("-I" ?path (help "Adds include path."))
       (update-conf 'include-paths
		    (append (read-config *config* 'include-paths)
			    (list path))))
      (("--bigloo-modules" (help (default-str "Use Bigloo-module headers."
				    'bigloo-modules)))
       (update-conf 'bigloo-modules (not invert-flag?)))
      (section "JavaScript Interface")
      (("--infotron" (help (default-str "Activate support for Infotrons." 'infotron)))
       (update-conf 'infotron (not invert-flag?)))
      (("--js-dot-notation"
	(help (default-str "Disallows the access of JS-fields with dots."
		 'direct-js-object-access)))
       (update-conf 'direct-js-object-access (not invert-flag?)))
      (("--encapsulate-modules"
	(help (default-str (string-append "Encapsulates modules, so they don't"
					  " flood the surrounding scope with"
					  " local vars.")
		 'encapsulate-modules)))
       (update-conf 'encapsulate-modules (not invert-flag?)))
      (("--export-globals"
	(help (default-str "Export global variables." 'export-globals)))
       (update-conf 'export-globals (not invert-flag?)))
      (("--allow-unresolved"
	(help (default-str "Unresolved vars are supposed to be js-vars."
		 'unresolved=JS)))
       (update-conf 'unresolved=JS (not invert-flag?)))
      (("--js-this"
	(help (default-str "Procedures may use Javascript's 'this' variable."
		 'procedures-provide-js-this)))
       (update-conf 'procedures-provide-js-this	(not invert-flag?)))
      (("--js-return"
	(help (default-str "Adds the special-form 'return!'."
		 'return)))
       (update-conf 'return (not invert-flag?)))
      (("--constant-runtime"
	(help (default-str "Assume runtime is constant."
		 'runtime-is-constant)))
       (update-conf 'runtime-is-constant (not invert-flag?)))
      (("--statics-suffix" ?suffix
			   (help (string-append "Set the suffix for static (global, but not "
						"exported) variables. (default: _ followed by the"
						" file-name without extension)")))
       (update-conf 'statics-suffix suffix))
      (("--indent" ?width
		   (help (default-str "Set indentation-width of produced code"
			    'indent)))
       (let ((new-width (string->number width)))
	  (if (not new-width)
	      (error #f
		     "--indent requires a number as argument"
		     width))
	  (update-conf 'indent new-width)))
      (section "Optimizations")
      (("-O" ?level
	     (help "Set Optimization Level [0-3,bench]"))
       (set! *config*
	     (set-optim-level *config* (or (string->number level) (string->symbol level)))))
      (("--tailrec"
	(help (default-str "transform tail-recursive calls into while-loops."
		 'optimize-tail-rec)))
       (update-conf 'optimize-tail-rec (not invert-flag?)))
      (("--while"
	(help (default-str "Search for typical Loop-patterns." 'while)))
       (update-conf 'while (not invert-flag?)))
      (("--loop-hoist"
	(help (default-str "hoists functions outside loops"
		 'loop-hoist)))
       (update-conf 'loop-hoist (not invert-flag?)))
      (("--inlining"
	(help (default-str "Enable inlining" 'do-inlining)))
       (update-conf 'do-inlining (not invert-flag?)))
      (("--max-inline-size" ?size
			    (help (default-str "Maximal size of a function to be inlined."
				     'max-inline-size)))
       (let ((new-size (string->number size)))
	  (if (not new-size)
	      (error #f
		     "--max-inline-size requires a number as argument"
		     size))
	  (update-conf 'max-inline-size new-size)))
      (("--rec-inline-nb" ?nb
			  (help (default-str "Maximal nb of nested inlining."
				   'rec-inline-nb)))
       (let ((new-nb (string->number nb)))
	  (if (not new-nb)
	      (error #f
		     "--rec-inline-nb requires a number as argument"
		     nb))
	  (update-conf 'rec-inline-nb new-nb)))
      (("--inline-runtime"
	(help (default-str (string-append "Inline runtime functions like map,"
					  " for-each, filter, ...")
		 'inline-runtime)))
       (update-conf 'inline-runtime (not invert-flag?)))
      (("--var-elimination"
	(help (default-str "Reduce vars by propagating its init-value."
		 'var-elimination)))
       (update-conf 'var-elimination (not invert-flag?)))
      (("--propagation"
	(help (default-str "Propagate constants and var-references."
		 'propagation)))
       (update-conf 'propagation (not invert-flag?)))
      (("--constant-propagation"
	(help (default-str "Propagate constants."
		 'constant-propagation)))
       (update-conf 'constant-propagation (not invert-flag?)))
      (("--correct-modulo"
	(help (default-str "Generate R5RS compliant modulo (slower)."
		 'correct-modulo)))
       (update-conf 'correct-modulo (not invert-flag?)))
      (("--optimize-calls"
	(help (default-str "Inline simple runtime-functions."
		 'optimize-calls)))
       (update-conf 'optimize-calls (not invert-flag?)))
      (("--optimize-boolify"
	(help (default-str "Avoid boolify-tests, if expr types to bool."
		 'optimize-boolify)))
       (update-conf 'optimize-boolify (not invert-flag?)))
      (("--optimize-consts"
	(help (default-str "Factor expensive constants into global variables."
		 'optimize-consts)))
       (update-conf 'optimize-consts (not invert-flag?)))
      
      (section "Trampolines and Call/cc")
      (("--trampoline"
	(help (default-str "Add trampolines around tail-recursive calls"
		 'trampoline)))
       (update-conf 'trampoline (not invert-flag?)))
      (("--max-tail-depth"
	?depth
	(help (default-str (string-append "maximum tail-call depth before "
					  "the trampoline is used.")
		 'max-tail-depth)))
       (let ((new-depth (string->number depth)))
	  (if (not new-depth)
	      (error #f
		     "--max-tail-depth requires a number as argument"
		     depth))
	  (update-conf 'max-tail-depth new-depth)))
      (("--suspend/resume"
	(help (default-str "Allow suspend/resume (weaker call/cc)"
		 'suspend/resume)))
       (update-conf 'suspend/resume (not invert-flag?)))
      (("--call/cc"
	(help (default-str "Allow call/cc" 'call/cc)))
       ;; call/cc implies suspend/resume, encapsulate-modules
       (unless invert-flag?
	  (update-conf 'suspend/resume #t))
       (update-conf 'call/cc (not invert-flag?)))
      (("--extern-invokes-call/cc"
	(help (default-str "Assume imported functions invoke call/cc."
		 'extern-always-call/cc)))
       (update-conf 'extern-always-call/cc (not invert-flag?)))
      (section "Debug")
      ((("-l" "--print-locations") (help "print locations"))
       (update-conf 'print-locations #t))
      (("-d" ?stage (help "debug compiler-stage"))
       (update-conf 'debug-stage (string->symbol stage)))
      (else
       (cond
	  ((substring=? else "-O" 2)
	   (handle-args (list (car args)
			      "-O"
			      (substring else 2 (string-length else)))
			invert-flag?))
	  ((substring=? else "--no-" 5)
	   (handle-args (list (car args)
			      (string-append "--"
					     (substring else
							5
							(string-length else))))
			(not invert-flag?))) ;; invert-flag
	  (*in-file*
	   (error #f
		  "only one input-file allowed"
		  (cons *in-file* else)))
	  (else
	   (set! *in-file* else))))))

(define (my-main args)
   (set! *config* (default-scheme2js-config))
   (handle-args args #f)
   (if (or (not *in-file*)
	   (not *out-file*))
       (error #f
	      "missing in or output-file. Use --help to see the usage."
	      #f))
   (scheme2js-compile-file *in-file* *out-file* '() *config*))
