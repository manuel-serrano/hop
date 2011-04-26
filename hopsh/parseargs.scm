;*=====================================================================*/
;*    serrano/prgm/project/hop/2.2.x/hopsh/parseargs.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Sun Apr  3 20:29:58 2011 (serrano)                */
;*    Copyright   :  2004-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop command line parsing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopsh_parseargs

   (library hop)
   
   (import  hopsh_param
	    hopsh_repl
	    hopsh_login)
   
   (export  (parse-args ::pair-nil)
	    (hopsh-load-rc ::bstring))

   (eval    (export hopsh-load-rc)))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (let ((rc-file #unspecified)
	 (loadp #t)
	 (exprs '())
	 (h "localhost")
	 (p (hop-port))
	 (login #f)
	 (command-string #f))
      (args-parse (cdr args)
         ((("-h" "--help") (help "This message"))
	  (print "HopSh v" (hop-version))
	  (print "usage: hopsh [options]")
	  (newline)
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
	 (("-c" ?string (help "Execute command STRING"))
	  (set! command-string string))
	 (("--rc-file" ?file (help "Load alternate rc file"))
	  (set! rc-file file))
	 (("--rc-dir" ?dir (help "Set rc directory"))
	  (hop-rc-directory-set! dir))
	 (("--var-dir" ?dir (help "Set var directory"))
	  (hop-var-directory-set! dir)
	  (hop-upload-directory-set! (make-file-name dir "upload")))
	 (("--cache-dir" ?dir (help "Set cache directory"))
	  (hop-cache-directory-set! dir))
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
	 (("--eval" ?string (help "Evaluate STRING"))
	  (set! exprs (cons string exprs)))
	 ((("-H" "--host") ?host (help (format "Host name (default: ~s)" h)))
	  (set! h host))
	 ((("-p" "--port") ?port (help (format "Port number (default: ~s)" p)))
	  (set! p (string->integer port)))
	 ((("-s" "--enable-https") (help (format "Enable HTTPS")))
	  (hopsh-enable-https-set! #t))
	 (("--disable-https" (help (format "Disable HTTPS")))
	  (hopsh-enable-https-set! #f))
	 ((("-l" "--login") ?user (help "Specify user log in"))
	  (set! login user))
	 ((("-t" "--timeout") ?timeout (help "Connection timeout (0=no timeout)"))
	  (hopsh-timeout-set! (string->integer timeout)))
	 (("-?dummy")
	  (args-parse-usage #f)
	  (exit 1))
	 (else
	  (print "Unknown argument: " else)
	  (args-parse-usage #f)
	  (exit 1)))
      (when login (login! login))
      (when loadp
	 (if (string? rc-file)
	     (%hopsh-load-rc rc-file)
	     (let ((path (make-file-name (hop-rc-directory) (hopsh-rc-file))))
		(if (file-exists? path)
		    (%hopsh-load-rc path)
		    (%hopsh-load-rc (make-file-name (hop-etc-directory) (hopsh-rc-file)))))))
      (hopsh-host-set! h)
      (hop-port-set! p)
      (for-each hopsh-eval exprs)
      (when (string? command-string)
	 (with-handler
	    (lambda (e)
	       (exception-notify e)
	       (exit 1))
	    (begin
	       (print (hopsh-eval-string command-string))
	       (exit 0))))))

;*---------------------------------------------------------------------*/
;*    %hopsh-load-rc ...                                               */
;*---------------------------------------------------------------------*/
(define (%hopsh-load-rc path)
   (if (string? path)
       (when (file-exists? path)
	  (hop-verb 2 "Loading `" path "'...\n")
	  (hop-load path :menv #unspecified))))
      
;*---------------------------------------------------------------------*/
;*    hopsh-load-rc ...                                                */
;*---------------------------------------------------------------------*/
(define (hopsh-load-rc file)
   (let ((path (make-file-name (hop-rc-directory) file)))
      (when (file-exists? path)
	 (%hopsh-load-rc path))))
      

