;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/src/parseargs.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Tue May 19 12:10:55 2009 (serrano)                */
;*    Copyright   :  2004-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop command line parsing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_parseargs

   (library scheme2js hopscheme hop web)

   (import  hop_param)
   
   (eval    (export hop-load-rc))
   
   (export  (parse-args ::pair-nil)
	    (hop-load-rc ::bstring)))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   
   (let ((loadp #t)
	 (mimep #t)
	 (autoloadp #t)
	 (p (hop-port))
	 (ep #unspecified)
	 (rc-file #unspecified)
	 (mime-file #unspecified)
	 (libraries '())
	 (exprs '())
	 (log-file #f)
	 (be #f)
	 (files '()))
      
      (bigloo-debug-set! 0)
      
      (args-parse (cdr args)
	 ;; Misc
	 (section "Misc")
         ((("-h" "--help") (help "This message"))
          (usage args-parse-usage)
          (exit 0))
         (("--options" (help "Display the Hop options and exit"))
          (usage args-parse-usage)
          (exit 0))
         (("--version" (help "Print the version and exit"))
          (print (hop-name) "-" (hop-version))
          (exit 0))
	 
	 ;; RC
	 (section "RC & Autoload")
	 (("-q" (help "Do not load an init file"))
	  (set! loadp #f))
	 (("-qmime" (help "Do not load any mime file"))
	  (set! mimep #f))
	 (("-qpreferences" (help "Do not load any user preferences file"))
	  (hop-load-preferences-set! #f))
	 (("--rc-file" ?file (help "Load alternate rc file"))
	  (set! rc-file file))
	 (("--rc-dir" ?dir (help "Set rc directory"))
	  (hop-rc-directory-set! dir))
	 (("--var-dir" ?dir (help "Set var directory"))
	  (hop-var-directory-set! dir)
	  (hop-upload-directory-set! (make-file-name dir "upload")))
	 (("--cache-dir" ?dir (help "Set cache directory"))
	  (hop-cache-directory-set! dir))
	 (("--script-file" ?file (help "A file loaded before the main loop"))
	  (hop-script-file-set! file))
	 (("--enable-autoload" (help "Enable autoload (default)"))
	  (set! autoloadp #t))
	 (("--disable-autoload" (help "Disable autoload"))
	  (set! autoloadp #f))
	 (("--add-autoload-dir" ?dir (help "Add autoload directory"))
	  (hop-autoload-directory-add! dir))
	 (("--autoload-dir" ?dir (help "Set autoload directory"))
	  (hop-autoload-directories-set! (list dir)))
	 (("--mime-type" ?file (help "Load aternate user mime-type file"))
	  (set! mime-file file))
	 (("--preload-service" ?svc (help "Preload service"))
	  (hop-preload-services-set! (cons svc (hop-preload-services))))
	 
	 ;; Verbosity and logs
	 (section "Verbosity & Logging")
         (("-v?level" (help "Increase/set verbosity level (-v0 crystal silence)"))
          (if (string=? level "")
	      (hop-verbose-set! (+fx 1 (hop-verbose)))
	      (hop-verbose-set! (string->integer level))))
         (("-g?level" (help "Increase/set debug level"))
          (if (string=? level "")
	      (begin
		 (bigloo-debug-module-set! (+fx 1 (bigloo-debug-module)))
		 (bigloo-debug-set! (+fx 1 (bigloo-debug))))
	      (begin
		 (bigloo-debug-module-set! (string->integer level))
		 (bigloo-debug-set! (string->integer level)))))
	 (("--time" (help "Report execution time"))
	  (hop-report-execution-time-set! #t))
	 (("-w?level" (help "Increase/set warning level (-w0 no warning)"))
          (if (string=? level "")
	      (bigloo-warning-set! (+fx 1 (bigloo-warning)))
	      (bigloo-warning-set! (string->integer level))))
	 (("--no-color" (help "Disable colored traces"))
	  (bigloo-trace-color-set! #f))
	 (("--log-file" ?file (help "Use <FILE> as log file"))
	  (set! log-file file))
	 (("--allow-service-override" (help "Allow service overriding"))
	  (hop-allow-service-override-set! #t))
	 
	 ;; Run
	 (section "Run")
	 ((("-p" "--http-port") ?port (help (format "Port number [~s]" p)))
	  (set! p (string->integer port)))
	 (("--fast-server-event-port" ?port (help (format "Fast Server event Port number [~s]" p)))
	  (set! ep (string->integer port)))
	 ((("-s" "--enable-https") (help (format "Enable HTTPS")))
	  (hop-enable-https-set! #t))
	 (("--disable-https" (help (format "Disable HTTPS")))
	  (hop-enable-https-set! #f))
	 (("--enable-fast-server-event" (help (format "Enable fast Server events")))
	  (hop-enable-fast-server-event-set! #t))
	 (("--disable-fast-server-event" (help (format "Disable fast server events")))
	  (hop-enable-fast-server-event-set! #f))
	 ((("-i" "--session-id") ?session (help "Set session identifier"))
	  (hop-session-set! (string->integer session)))
	 (("--no-job-restore" (help "Don't restore jobs"))
	  (hop-job-restore-set! #f))
	 (("--eval" ?string (help "Evaluate STRING"))
	  (set! exprs (cons string exprs)))
	 (("--repl" (help "Start a repl"))
	  (hop-enable-repl-set! #t))
	 ((("-x" "--xml-backend") ?ident
				  (help (format "Set XML backend [~s]"
						(xml-backend-id (hop-xml-backend)))))
	  (set! be ident))
	 (("--accept-kill" (help "Enable remote kill commands (see -k)"))
	  (hop-accept-kill-set! #t))
	 (("--no-accept-kill" (help "Forbidden remote kill commands"))
	  (hop-accept-kill-set! #f))
	 ((("-k" "--kill") (help "Kill the running local HOP and exit"))
	  (hop-verb 2 "Kill HOP process " (key-filepath p) "...\n")
	  (let ((key (hop-process-key-read p)))
	     (if (string? key)
		 (http :port p :path (format "/hop/shutdown/kill?key=~a" key))
		 (error 'hop-kill "Cannot find process key" (key-filepath p)))
	     (exit 0)))
	 
	 ;; Paths
	 (section "Paths")
	 ((("-I" "--path") ?path (help "Add <PATH> to hop load path"))
	  (hop-path-set! (cons path (hop-path))))
	 ((("-L" "--library-path") ?path (help "Add <PATH> to hop library path"))
	  (bigloo-library-path-set! (cons path (bigloo-library-path))))
	 ((("-l" "--library") ?library (help "Preload additional <LIBRARY>"))
	  (set! libraries (cons library libraries )))
	 
	 ;; Internals
	 (section "Internals")
	 (("--configure" ?config (help "Report HOP configuration"))
	  (hop-configure config)
	  (exit 0))
	 (("--no-thread" (help "Disable multithreading (equiv. to \"--scheduler nothread\")"))
	  (hop-max-threads-set! 1)
	  (hop-enable-keep-alive-set! #f)
	  (hop-scheduling-set! 'nothread))
	 (("--scheduler" ?ident (help (format "Set scheduling policy [~s] (see --help-scheduler)" (hop-scheduling))))
	  (hop-scheduling-set! (string->symbol ident)))
	 (("--help-scheduler" (help "Prints the available schedulers list"))
	  (with-output-to-port (current-error-port)
	     (lambda ()
		(print  "Schedulers:")
		(print "  - queue (split threads but avoid useless switches)")
		(print "  - nothread (single threaded execution)")
		(print "  - one-to-one (one thread per request)")
		(print "  - pool (one thread per request from a pool)")
		(print "  - accept-many (as pool but an accept-many call)")))
	  (exit 0))
	 (("--restore-cache" (help "Restore disk caches"))
	  (hop-restore-disk-cache-set! #t))
	 (("--no-restore-cache" (help "Do not restore disk caches"))
	  (hop-restore-disk-cache-set! #f))
	 (("-psn_?dummy") ;; Macosx sends process serial numbers this way.
	  ;; just ignore it.
	  'do-nothing)
	 (("-?dummy")
	  (args-parse-usage #f)
	  (exit 1))
	 (else
	  (set! files (cons else files))))
      
      ;; http port
      (hop-port-set! p)
      (when (eq? ep #unspecified) (set! ep p))
      
      ;; log
      (when log-file
	 (let ((p (append-output-file log-file)))
	    (unless p
	       (error 'hop "Cannot open log file" log-file))
	    (hop-log-file-set! p)))
      
      ;; mime types
      (when mimep
	 (load-mime-types (hop-mime-types-file))
	 (load-mime-types (if (string? mime-file)
			      mime-file
			      (make-file-name (getenv "HOME") ".mime.types"))))
      
      ;; weblets path
      (hop-autoload-directory-add!
       (make-file-name (hop-rc-directory) "weblets"))
      
      ;; init hss, scm compilers, and services
      (init-hss-compiler! (hop-port))
      (init-hopscheme! :reader (lambda (p v) (hop-read p))
	 :share (hop-share-directory)
	 :verbose (hop-verbose)
	 :eval (lambda (e) (hop->json (eval e) #f #f))
	 :postprocess (lambda (s)
			 (with-input-from-string s
			    (lambda ()
			       (hop-read-javascript
				(current-input-port)
				(hop-charset)))))
	 :features `(hop
		     ,(string->symbol (format "hop-~a" (hop-branch)))
		     ,(string->symbol (format "hop-~a" (hop-version))))
	 :expanders `((labels ,(lambda (x e) (e (expand-once x) e)))
		      (match-case ,(lambda (x e) (e (expand-once x) e)))))
      (init-clientc-compiler! :modulec compile-scheme-module
	 :expressionc compile-scheme-expression
	 :macroc (lambda (form env) env)
	 :filec compile-scheme-file
	 :JS-expression JS-expression
	 :JS-statement JS-statement
	 :JS-return JS-return)
      (init-hop-services!)
      
      ;; hoprc
      (when loadp (parseargs-loadrc rc-file (hop-rc-file)))
      
      ;; default backend
      (when (string? be) (hop-xml-backend-set! (string->symbol be)))

      ;; server event port
      (when (hop-enable-fast-server-event)
	 (if (<fx ep 1024)
	     (error 'fast-server-event-port
		    "Server event port must be greater than 1023. (See `--fast-server-event-port' or `--disable-fast-server-event' options.)"
		    ep)
	     (hop-fast-server-event-port-set! ep)))
      
      (for-each (lambda (expr)
		   (with-input-from-string expr
		      (lambda ()
			 (let ((sexp (hop-read (current-input-port))))
			    (with-handler
			       (lambda (e)
				  (if (&eval-warning? e)
				      (begin
					 (warning-notify e)
					 #unspecified)
				      (raise e)))
			       (eval sexp))))))
		exprs)
      
      (when autoloadp (install-autoload-weblets! (hop-autoload-directories)))
      
      (for-each (lambda (l) (eval `(library-load ',l))) libraries)
      
      (for-each (lambda (f)
		   (let ((path (cond
				  ((string-index f ":")
				   f)
				  ((and (>fx (string-length f) 0)
					(char=? (string-ref f 0)
						(file-separator)))
				   f)
				  (else
				   (file-name-canonicalize!
				    (make-file-name (pwd) f))))))
		      (cond
			 ((string-suffix? ".hz" path)
			  ;; this is a weblet
			  (hop-load-hz path))
			 ((directory? path)
			  ;; load a directory
			  (let ((src (string-append (basename path) ".hop")))
			     (hop-load-weblet (make-file-name path src))))
			 (else
			  ;; this is a plain file
			  (hop-load-weblet path)))))
		(reverse! files))

      ;; write the process key
      (hop-process-key-write (hop-process-key) (hop-port))
      (register-exit-function! (lambda (ret)
				  (hop-process-key-delete (hop-port))
				  ret))
      
      (reverse! files)))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage)
   (print "Hop v" (hop-version))
   (print "usage: hop [options] ...")
   (print "       hop [options] file.hop|file.hz ...")
   (args-parse-usage #f))

;*---------------------------------------------------------------------*/
;*    %hop-load-rc ...                                                 */
;*---------------------------------------------------------------------*/
(define (%hop-load-rc path)
   (if (string? path)
       (when (file-exists? path)
	  (hop-verb 2 "Loading `" path "'...\n")
	  (hop-load path)
	  (hop-rc-loaded!))))
      
;*---------------------------------------------------------------------*/
;*    parseargs-loadrc ...                                             */
;*---------------------------------------------------------------------*/
(define (parseargs-loadrc rc-file default)
   (if (string? rc-file)
       (%hop-load-rc rc-file)
       (let ((path (make-file-name (hop-rc-directory) default)))
	  (if (file-exists? path)
	      (%hop-load-rc path)
	      (%hop-load-rc (make-file-name (hop-etc-directory) default))))))

;*---------------------------------------------------------------------*/
;*    hop-load-rc ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-load-rc file)
   (let ((path (make-file-name (hop-rc-directory) file)))
      (when (file-exists? path)
	 (%hop-load-rc path))))

;*---------------------------------------------------------------------*/
;*    key-filename ...                                                 */
;*---------------------------------------------------------------------*/
(define (key-filename port)
   (format ".process-key.~a" port))

;*---------------------------------------------------------------------*/
;*    key-filepath ...                                                 */
;*---------------------------------------------------------------------*/
(define (key-filepath port)
   (make-file-name (hop-rc-directory) (key-filename port)))

;*---------------------------------------------------------------------*/
;*    hop-process-key-write ...                                        */
;*    -------------------------------------------------------------    */
;*    Write the HOP process for other Hop processes.                   */
;*---------------------------------------------------------------------*/
(define (hop-process-key-write key port)
   (let ((dir (hop-rc-directory)))
      (when (directory? dir)
	 (let ((path (make-file-name dir (key-filename port))))
	    (when (file-exists? path) (delete-file path))
	    (with-output-to-file path (lambda () (display key)))
	    (chmod path #o600)))))

;*---------------------------------------------------------------------*/
;*    hop-process-key-read ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-process-key-read port)
   (let ((dir (hop-rc-directory)))
      (when (directory? dir)
	 (let ((path (make-file-name dir (key-filename port))))
	    (when (file-exists? path)
	       (with-input-from-file path read-string))))))

;*---------------------------------------------------------------------*/
;*    hop-process-key-delete ...                                       */
;*---------------------------------------------------------------------*/
(define (hop-process-key-delete port)
   (let* ((dir (hop-rc-directory))
	  (path (make-file-name dir (key-filename port))))
      (when (file-exists? path) (delete-file path))))
   
