;*=====================================================================*/
;*    serrano/prgm/project/hop/src/parseargs.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Wed Nov 21 06:59:03 2007 (serrano)                */
;*    Copyright   :  2004-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop command line parsing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_parseargs

   (library hop)

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
          (print (hop-name) (hop-version))
          (exit 0))
	 
	 ;; RC
	 (section "RC & Autoload")
	 (("-q" (help "Do not load an init file"))
	  (set! loadp #f))
	 (("-qmime" (help "Do not load an mime file"))
	  (set! mimep #f))
	 (("--rc-file" ?file (help "Load alternate rc file"))
	  (set! rc-file file))
	 (("--rc-dir" ?dir (help "Set rc directory"))
	  (hop-rc-directory-set! dir)
	  (hop-upload-directory-set! (make-file-name dir "upload")))
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
	 (("-w?level" (help "Increase/set warning level (-w0 no warning)"))
          (if (string=? level "")
	      (bigloo-warning-set! (+fx 1 (bigloo-warning)))
	      (bigloo-warning-set! (string->integer level))))
	 (("--no-color" (help "Disable colored traces"))
	  (bigloo-trace-color-set! #f))
	 (("--log-file" ?file (help "Use <FILE> as log file"))
	  (set! log-file file))
	 
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
	 (("--no-thread" (help "Disable multithreading"))
	  (hop-max-accept-thread-set! 1)
	  (hop-max-reply-thread-set! 1)
	  (hop-max-reply-persite-thread-set! 1))
	 (("--thread-scheduler" ?ident (help (format "Set scheduling policy [~s]" (hop-scheduling))))
	  (hop-scheduling-set! (string->symbol ident)))
	 (("-?dummy")
	  (args-parse-usage #f)
	  (exit 1))
	 (else
	  (set! files (cons else files))))
      
      (when log-file
	 (let ((p (append-output-file log-file)))
	    (unless p
	       (error 'hop "Cannot open log file" log-file))
	    (hop-log-file-set! p)))      
      (when mimep
	 (load-mime-types (hop-mime-types-file))
	 (load-mime-types (if (string? mime-file)
			      mime-file
			      (make-file-name (getenv "HOME") ".mime.types"))))
      (hop-autoload-directory-add!
       (make-file-name (hop-rc-directory) "weblets"))
      (if loadp
	  (if (string? rc-file)
	      (%hop-load-rc rc-file)
	      (let ((path (make-file-name (hop-rc-directory) (hop-rc-file))))
		 (if (file-exists? path)
		     (%hop-load-rc path)
		     (%hop-load-rc (make-file-name (hop-etc-directory) (hop-rc-file))))))
	  (%hop-load-rc (make-file-name (hop-etc-directory) (hop-rc-file))))
      (when (string? be) (hop-xml-backend-set! (string->symbol be)))
      ;; http port
      (hop-port-set! p)
      (when (eq? ep #unspecified)
	 (set! ep p))
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
		      (if (string-suffix? ".hz" path)
			  ;; this is a weblet
			  (hop-load-weblet path)
			  ;; this is a plain file
			  (hop-load path))))
		(reverse! files))))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage)
   (print "Hop v" (hop-version))
   (print "usage: hop [options] ...")
   (print "       hop [options] file.hop|file.hz ...")
   (args-parse-usage #f))

;*---------------------------------------------------------------------*/
;*    hop-load-weblet ...                                              */
;*---------------------------------------------------------------------*/
(define (hop-load-weblet path)
   (let ((p (open-input-gzip-file path)))
      (unwind-protect
	 (let* ((tmp (make-file-name (os-tmp) "hop"))
		(file (car (untar p :directory tmp)))
		(base (substring file
				 (+fx (string-length tmp) 1)
				 (string-length file)))
		(dir (dirname base))
		(name (if (string=? dir ".") base dir))
		(src (make-file-path tmp name (string-append name ".hop"))))
	    (if (file-exists? src)
		(hop-load src)
		(error 'hop-load-weblet "Cannot find HOP source" path)))
	 (close-input-port p))))

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
;*    hop-load-rc ...                                                  */
;*---------------------------------------------------------------------*/
(define (hop-load-rc file)
   (let ((path (make-file-name (hop-rc-directory) file)))
      (when (file-exists? path)
	 (%hop-load-rc path))))
