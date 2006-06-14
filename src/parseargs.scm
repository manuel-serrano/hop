;*=====================================================================*/
;*    serrano/prgm/project/hop/src/parseargs.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Mon Jun 12 21:16:12 2006 (eg)                     */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop command line parsing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_parseargs

   (library hop
	    pthread)
   
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
	 (hopp #t)
	 (replp #f)
	 (autoloadp #t)
	 (p (hop-port))
	 (rc-file #unspecified)
	 (mime-file #unspecified)
	 (libraries '())
	 (exprs '())
	 (log-file #f))
      (args-parse (cdr args)
         ((("-h" "--help") (help "This message"))
          (args-parse-usage #f)
          (exit 0))
         (("--options" (help "Display the Hop options and exit"))
          (args-parse-usage #t)
          (exit 0))
         (("--version" (help "Print the version and exit"))
          (print (hop-name) (hop-version))
          (exit 0))
	 (("-q" (help "Do not load an init file"))
	  (set! loadp #f))
	 (("-qmime" (help "Do not load an mime file"))
	  (set! mimep #f))
	 (("--enable-autoload" (help "Enable autoload"))
	  (set! autoloadp #t))
	 (("--disable-autoload" (help "Enable autoload"))
	  (set! autoloadp #f))
	 (("--autoload-dir" ?dir (help "Add autoload directory"))
	  (hop-autoload-directory-add! dir))
	 (("--rc-file" ?file (help "Load alternate rc file"))
	  (set! rc-file file))
	 (("--rc-dir" ?dir (help "Set rc directory"))
	  (hop-rc-directory-set! dir))
	 (("--mime-type" ?file (help "Load aternate user mime-type file"))
	  (set! mime-file file))
         (("-v?level" (help "Increase or set verbosity level (-v0 crystal silence)"))
          (if (string=? level "")
	      (hop-verbose-set! (+fx 1 (hop-verbose)))
	      (hop-verbose-set! (string->integer level))))
         (("-g?level" (help "Increase or set debug level"))
          (if (string=? level "")
	      (bigloo-debug-set! (+fx 1 (bigloo-debug)))
	      (bigloo-debug-set! (string->integer level))))
	 (("-w?level" (help "Increase or set warning level (-w0 no warning)"))
          (if (string=? level "")
	      (bigloo-warning-set! (+fx 1 (bigloo-warning)))
	      (bigloo-warning-set! (string->integer level))))
	 (("--no-color" (help "Disable colored traces"))
	  (bigloo-trace-color-set! #f))
	 ((("-p" "--port") ?port (help (format "Port number (default: ~s)" p)))
	  (set! p (string->integer port)))
	 (("--configure" ?config (help "Report HOP configuration"))
	  (hop-configure config)
	  (exit 0))
	 (("--no-thread" (help "Disable multithreading"))
	  (hop-max-accept-thread-set! 1)
	  (hop-max-reply-thread-set! 1)
	  (hop-max-reply-persite-thread-set! 1))
	 ((("-s" "--session") ?session (help "Set session identifier"))
	  (hop-session-set! (string->integer session)))
	 (("--no-init" (help "Don't load default hop.scm file"))
	  (set! hopp #f))
	 (("--no-job-restore" (help "Don't restore jobs"))
	  (hop-job-restore-set! #f))
	 (("--eval" ?string (help "Evaluate STRING"))
	  (set! exprs (cons string exprs)))
	 (("--repl" (help "Starts a repl"))
	  (set! replp #t))
	 (("--log-file" ?file (help "Use <FILE> as log file"))
	  (set! log-file file))
	 ((("-I" "--path") ?path (help "Add <PATH> to hop load path"))
	  (hop-path-set! (cons path (hop-path))))
	 ((("-l" "--library") ?library (help "Preload additional <LIBRARY>"))
	  (set! libraries (cons library libraries )))
	 (("-?dummy")
	  (args-parse-usage #f)
	  (exit 1))
	 (else
	  (print "Unknown argument: " else)
	  (args-parse-usage #f)
	  (exit 1)))
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
      (when hopp (load-hop))
      (when loadp
	 (if (string? rc-file)
	     (%hop-load-rc rc-file)
	     (hop-load-rc (hop-rc-file))))
      (when replp (hop-repl))
      (hop-port-set! p)
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
      (when autoloadp (autoload-weblets (reverse (hop-autoload-directories))))
      (hop-preload-libraries-set! (append libraries (hop-preload-libraries)))))

;*---------------------------------------------------------------------*/
;*    load-hop ...                                                     */
;*---------------------------------------------------------------------*/
(define (load-hop)
   (let ((file (make-file-name (hop-share-directory) "hop.scm")))
      (if (string? file)
	  (if (file-exists? file)
	      (begin
		 (hop-verb 2 "Loading `" file "'...\n")
		 (hop-load file))
	      (error 'hop "Can't find hop init file, aborting..." file)))))

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
      (if (file-exists? path)
	  (%hop-load-rc path)
	  (%hop-load-rc (make-file-name (hop-etc-directory) (hop-rc-file))))))
      
;*---------------------------------------------------------------------*/
;*    hop-repl ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-repl)
   (hop-verb 1 "Entering repl...\n")
   (thread-start! (make-thread (lambda () (begin (repl) (exit 0))))))

