;*=====================================================================*/
;*    serrano/prgm/project/hop/2.4.x/hophz/parseargs.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Fri Jun 22 10:22:24 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop command line parsing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hophz_parseargs

   (library hop)
   
   (import  hophz_param
            hophz_login
	    hophz_action)
   
   (export  (parse-args ::pair-nil)
	    (hophz-load-rc ::bstring))

   (eval    (export hophz-load-rc)))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (let ((loadp #t)
	 (rc-file (hophz-rc-file))
	 (exprs '())
	 (actions '()))
      (args-parse (cdr args)
	 ;; Misc
	 (section "Misc")
         ((("-h" "--help") (help "This message (use -v -h for long help)"))
          (usage args-parse-usage)
          (exit 0))
         (("--version" (help "Print the version and exit"))
          (print "hophz-" (hophz-version))
          (exit 0))
	 
	 ;; Commands
	 (section "Commands")
	 (("show" ?weblet (help "show WEBLET [CATEGORY]" "Show a readable record for the weblet"))
	  (set! actions
	     (cons (instantiate::list-action
		      (options 1)
		      (verbose 3)
		      (args (list (string-append "^" (pregexp-quote weblet) "$"))))
		actions)))
	 (("list" (help "list [REGEX [CATEGORY]]" "List the names of all weblets in the system"))
	  (set! actions
	     (cons (instantiate::list-action
		      (options 2))
		actions)))
	 (("depends" ?weblet (help "depends WEBLET [CATEGORY]" "Show raw dependency information for a weblet"))
	  (set! actions
	     (cons (instantiate::depends-action
		      (name weblet)
		      (options 1))
		actions)))
	 (("install" ?weblet (help "install WEBLET [CATEGORY]" "Install new weblet"))
	  (set! actions
	     (cons (instantiate::install-action
		      (options 1)
		      (name weblet))
		actions)))
	 (("download" ?url (help "Install a weblet located at URL"))
	  (set! actions
	     (cons (instantiate::download-action
		      (url url))
		actions)))
	 (("remove" ?weblet (help "remove WEBLET [CATEGORY]" "Remove weblet"))
	  (set! actions
	     (cons (instantiate::uninstall-action
		      (options 1)
		      (name weblet))
		actions)))
	 (("update" (help "Retrieve new lists of weblets"))
	  (set! actions
	     (cons (instantiate::update-action)
		actions)))
	 (("upgrade" (help "Perform an upgrade"))
	  (set! actions
	     (cons (instantiate::upgrade-action)
		actions)))
	 (("clean" (help "Erase downloaded archive files"))
	  (set! actions
	     (cons (instantiate::clean-action)
		actions)))
	 (("config" (help "Print HZ configuration"))
	  (set! actions
	     (cons (instantiate::config-action)
		actions)))
	 (("publisher" ?action (help "publisher [add|remove] URL" "Add/remove publisher"))
	  (set! actions
	     (cons (instantiate::publisher-action
		      (options 1)
		      (action (string->symbol action)))
		actions)))
	 
	 ;; Options
	 (section "Options")
	 ((("-H" "--host") ?host (help (format "Set local server [default ~a]" (hophz-host))))
	  (hophz-host-set! host))
	 ((("-U" "--user") ?user (help "User authentication"))
	  (login! user))
	 ((("-F" "--force") (help "Force action"))
	  (hophz-force-action-set! #t))
	 ((("-Y" "--force-download") (help "Download a fresh copy"))
	  (hophz-force-download-set! #t))
	 (("--noconfirm" (help "Bypass \"are you sure?\" questions"))
	  (hophz-noconfirm-set! #t))
	 (("--publisher" ?publisher (help "Filter displayed weblets"))
	  (hophz-publishers-set! (cons publisher (hophz-publishers))))

	 ;; Verbosity and logs
	 (section "Verbosity")
         (("-v?<level>" (help "Increase/set verbosity level (-v0 crystal silence)"))
          (if (string=? <level> "")
	      (hophz-verbose-set! (+fx 1 (hophz-verbose)))
	      (hophz-verbose-set! (string->integer <level>))))
         (("-g?<level>" (help "Increase/set debug level"))
          (if (string=? <level> "")
	      (begin
		 (bigloo-debug-module-set! (+fx 1 (bigloo-debug-module)))
		 (bigloo-debug-set! (+fx 1 (bigloo-debug))))
	      (begin
		 (bigloo-debug-module-set! (string->integer <level>))
		 (bigloo-debug-set! (string->integer <level>)))))
	 (("--no-color" (help "Disable colored traces"))
	  (bigloo-trace-color-set! #f))
	 
	 ;; RC
	 (section "RC")
	 (("--no-rc" (help "Do not load an init file"))
	  (set! loadp #f))
	 (("--rc-file" ?file (help "Load alternate rc file"))
	  (set! rc-file file))
	 
	 ;; Illegal option
	 (("-?dummy")
	  (usage args-parse-usage)
	  (exit 1))

	 ;; null rule
	 (()
	  (when (null? actions)
	     (usage args-parse-usage)
	     (exit 1)))
	 ;; else rule
	 (else
	  (if (or (null? actions)
		  (with-access::action (car actions) (options)
		     (=fx options 0)))
	      (begin
		 (usage args-parse-usage)
		 (exit 1))
	      (let ((a (car actions)))
		 (with-access::action a (options args)
		    (set! options (-fx options 1))
		    (set! args (append args (list else))))))))
      
      ;; load rc
      (when (and loadp (string? rc-file)) (hophz-load-rc rc-file))
      
      ;; evaluate command line expressions
      (when (pair? exprs) (eval exprs))
      
      actions))

;*---------------------------------------------------------------------*/
;*    %hophz-load-rc ...                                               */
;*---------------------------------------------------------------------*/
(define (%hophz-load-rc path)
   (if (string? path)
       (when (file-exists? path)
	  (hop-verb 2 "Loading `" path "'...\n")
	  (hop-load path :menv #unspecified))))
      
;*---------------------------------------------------------------------*/
;*    hophz-load-rc ...                                                */
;*---------------------------------------------------------------------*/
(define (hophz-load-rc file)
   (let ((path (make-file-name (hop-rc-directory) file)))
      (when (file-exists? path)
	 (%hophz-load-rc path))))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage)
   (print "HopHz v" (hophz-version))
   (print "usage: hophz [options]")
   (args-parse-usage #f))
   
