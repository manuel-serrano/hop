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
					   (if (boolean? (cadr p))
					       (if (cadr p)
						   "true"
						   "false")
					       (cadr p))
					   ")")))
	  help)))
   
(define *out-file* #f)
(define *in-file* #f)

(define (handle-args args config-ht invert-flag?)
   (args-parse (cdr args)
      (section "Help")
      ((("-h" "--help") (help "This help message."))
       (args-parse-usage #f)
       (exit 0))
      (section "Misc")
      ((("--version") (help "Version number."))
       (print *version*))
      ((("-v" "--verbose") (help "Verbose output."))
       (hashtable-put! config-ht 'verbose #t))
      (("-o" ?file (help "The output file. '-' prints to stdout."))
       (set! *out-file* file))
      (("-I" ?path (help "Adds include path."))
       (hashtable-put! config-ht
		       'include-paths
		       (append (hashtable-get config-ht 'include-paths)
			       (list path))))
      (section "JavaScript Interface")
      (("--infotron" (help (default-str "Activate support for Infotrons.") 'infotron))
       (hashtable-put! config-ht 'infotron (not invert-flag?)))
      (("--js-dot-notation"
	(help (default-str "Disallows the access of JS-fields with dots."
		 'direct-js-object-access)))
       (hashtable-put! config-ht 'direct-js-object-access (not invert-flag?)))
      (("--encapsulate-modules"
	(help (default-str (string-append "Encapsulates modules, so they don't"
					  " flood the surrounding scope with"
					  " local vars.")
		 'encapsulate-modules)))
       (hashtable-put! config-ht 'encapsulate-modules (not invert-flag?)))
      (("--export-globals"
	(help (default-str "Export global variables." 'export-globals)))
       (hashtable-put! config-ht 'export-globals (not invert-flag?)))
      (("--allow-unresolved"
	(help (default-str "Unresolved vars are supposed to be js-vars."
		 'unresolved=JS)))
       (hashtable-put! config-ht 'unresolved=JS (not invert-flag?)))
      (("--js-this"
	(help (default-str "Procedures may use Javascript's 'this' variable."
		 'procedures-provide-js-this)))
       (hashtable-put! config-ht
		       'procedures-provide-js-this
		       (not invert-flag?)))
      (("--js-return"
	(help (default-str "Adds the special-form 'return!'."
		 'return)))
       (hashtable-put! config-ht 'return (not invert-flag?)))
      (("--with-closures"
	(help (default-str (string-append "Use the 'with'-keyword to push "
					  "variables on the stack.")
		 'with-closures)))
       (hashtable-put! config-ht 'with-closures (not invert-flag?)))
      (("--constant-runtime"
	(help (default-str "Assume runtime is constant."
		 'runtime-is-constant)))
       (hashtable-put! config-ht 'runtime-is-constant (not invert-flag?)))
      (("--statics-suffix" ?suffix
	(help (string-append "Set the suffix for static (global, but not "
			     "exported) variables. (default: _ followed by the"
			     " file-name without extension)")))
       (hashtable-put! config-ht 'statics-suffix suffix))
      (("--indent" ?width
		   (help (default-str "Set indentation-width of produced code"
			    'indent)))
       (let ((new-width (string->number width)))
	  (if (not new-width)
	      (error #f
		     "--indent requires a number as argument"
		     width))
	  (hashtable-put! config-ht 'indent new-width)))
      (section "Optimizations")
      (("-O" ?level
	     (help "Set Optimization Level [0-3,bench]"))
       (set-optim-level config-ht (or (string->number level) (string->symbol level))))
      (("--tailrec"
	(help (default-str "transform tail-recursive calls into while-loops."
		 'optimize-tail-rec)))
       (hashtable-put! config-ht 'optimize-tail-rec (not invert-flag?)))
      (("--while"
	(help (default-str "Search for typical Loop-patterns." 'while)))
       (hashtable-put! config-ht 'while (not invert-flag?)))
      (("--loop-hoist"
	(help (default-str "hoists functions outside loops"
		 'loop-hoist)))
       (hashtable-put! config-ht 'loop-hoist (not invert-flag?)))
      (("--inlining"
	(help (default-str "Enable inlining" 'do-inlining)))
       (hashtable-put! config-ht 'do-inlining (not invert-flag?)))
      (("--max-inline-size" ?size
       (help (default-str "Maximal size of a function to be inlined."
		'max-inline-size)))
       (let ((new-size (string->number size)))
	  (if (not new-size)
	      (error #f
		     "--max-inline-size requires a number as argument"
		     size))
	  (hashtable-put! config-ht 'max-inline-size new-size)))
      (("--rec-inline-nb" ?nb
	(help (default-str "Maximal nb of nested inlining."
		 'rec-inline-nb)))
       (let ((new-nb (string->number nb)))
	  (if (not new-nb)
	      (error #f
		     "--rec-inline-nb requires a number as argument"
		     nb))
	  (hashtable-put! config-ht 'rec-inline-nb new-nb)))
      (("--inline-runtime"
	(help (default-str (string-append "Inline runtime functions like map,"
					  " for-each, filter, ...")
		 'inline-runtime)))
       (hashtable-put! config-ht 'inline-runtime (not invert-flag?)))
      (("--var-elimination"
	(help (default-str "Reduce vars by propagating its init-value."
		 'var-elimination)))
       (hashtable-put! config-ht 'var-elimination (not invert-flag?)))
      (("--propagation"
	(help (default-str "Propagate constants and var-references."
		 'propagation)))
       (hashtable-put! config-ht 'propagation (not invert-flag?)))
      (("--constant-propagation"
	(help (default-str "Propagate constants."
		 'constant-propagation)))
       (hashtable-put! config-ht 'constant-propagation (not invert-flag?)))
      (("--correct-modulo"
	(help (default-str "Generate R5RS compliant modulo (slower)."
		 'correct-modulo)))
       (hashtable-put! config-ht 'correct-modulo (not invert-flag?)))
      (("--optimize-calls"
	(help (default-str "Inline simple runtime-functions."
		 'optimize-calls)))
       (hashtable-put! config-ht 'optimize-calls (not invert-flag?)))
      (("--optimize-boolify"
	(help (default-str "Avoid boolify-tests, if expr types to bool."
		 'optimize-boolify)))
       (hashtable-put! config-ht 'optimize-boolify (not invert-flag?)))
      (("--optimize-consts"
	(help (default-str "Factor expensive constants into global variables."
		 'optimize-consts)))
       (hashtable-put! config-ht 'optimize-consts (not invert-flag?)))

      (section "Trampolines and Call/cc")
      (("--trampoline"
	(help (default-str "Add trampolines around tail-recursive calls"
		 'trampoline)))
       (hashtable-put! config-ht 'trampoline (not invert-flag?)))
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
	  (hashtable-put! config-ht 'max-tail-depth new-depth)))
      (("--suspend/resume"
	(help (default-str "Allow suspend/resume (weaker call/cc)"
		 'suspend/resume)))
       (hashtable-put! config-ht 'suspend/resume (not invert-flag?)))
      (("--call/cc"
	(help (default-str "Allow call/cc" 'call/cc)))
       ;; call/cc implies suspend/resume, encapsulate-modules
       (unless invert-flag?
	  (hashtable-put! config-ht 'suspend/resume #t))
       (hashtable-put! config-ht 'call/cc (not invert-flag?)))
      (("--extern-invokes-call/cc"
	(help (default-str "Assume imported functions invoke call/cc."
		 'extern-always-call/cc)))
       (hashtable-put! config-ht 'extern-always-call/cc (not invert-flag?)))
      (section "Debug")
      ((("-l" "--print-locations") (help "print locations"))
       (hashtable-put! config-ht 'print-locations #t))
      (("-d" ?stage (help "debug compiler-stage"))
       (hashtable-put! config-ht 'debug-stage (string->symbol stage)))
      (else
       (cond
	  ((substring=? else "-O" 2)
	   (handle-args (list (car args)
			      "-O"
			      (substring else 2 (string-length else)))
			config-ht
			invert-flag?))
	  ((substring=? else "--no-" 5)
	   (handle-args (list (car args)
			      (string-append "--"
					     (substring else
							5
							(string-length else))))
			config-ht
			(not invert-flag?))) ;; invert-flag
	  (*in-file*
	   (error #f
		  "only one input-file allowed"
		  (cons *in-file* else)))
	  (else
	   (set! *in-file* else))))))

(define (my-main args)
   (let ((config-ht (default-scheme2js-config)))
      (handle-args args config-ht #f)
      (if (or (not *in-file*)
	      (not *out-file*))
	  (error #f
		 "missing in or output-file. Use --help to see the usage."
		 #f))
      (scheme2js-compile-file *in-file* *out-file* '() config-ht)))
