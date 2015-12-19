;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/src/parseargs.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Fri Dec 18 08:13:24 2015 (serrano)                */
;*    Copyright   :  2004-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hop command line parsing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hop_parseargs

   (include "libraries.sch")

   (import  hop_param
	    hop_init)

   (eval    (export hop-load-rc))

   (export  (parse-args::pair-nil ::pair)
	    (hop-load-rc ::bstring)
	    (hello-world)))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)

   ;; defaults
   (let ((loadp #t)
	 (mimep #t)
	 (autoloadp #t)
	 (p #f)
	 (ep #unspecified)
	 (dp #unspecified)
	 (rc-file #f)
	 (mime-file #unspecified)
	 (libraries '())
	 (exprs '())
	 (exprsjs '())
	 (log-file #f)
	 (be #f)
	 (files '())
	 (killp #f)
	 (webdav #unspecified)
	 (zeroconf #unspecified)
	 (clear-cache #f)
	 (setuser #f)
	 (clientc-source-map #f)
	 (clientc-arity-check #f)
	 (clientc-type-check #f)
	 (clientc-debug #f)
	 (clientc-compress #f)
	 (clientc-inlining #t)
	 (clientc-use-strict #t))
      
      (bigloo-debug-set! 0)

      (bind-exit (stop)
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
	     (hop-rc-directory-set! dir)
	     (hop-cache-directory-set! (make-file-name dir "cache"))
	     (hop-sofile-directory-set! (make-file-path dir "libs")))
	    (("--var-dir" ?dir (help "Set var directory"))
	     (hop-var-directory-set! dir)
	     (hop-upload-directory-set! (make-file-name dir "upload")))
	    (("--cache-dir" ?dir (help "Set cache directory"))
	     (hop-cache-directory-set! dir))
	    (("--icons-dir" ?dir (help "Set Hop icons directory"))
	     (hop-icons-directory-set! dir))
	    (("--no-cache" (help "Disable server caching"))
	     (hop-cache-enable-set! #f))
	    (("--clear-cache" (help "Clear all caches"))
	     (set! clear-cache #t))
	    (("--no-clear-cache" (help "Don't clear any cache"))
	     (hop-hss-clear-cache-set! #f)
	     (hop-clientc-clear-cache-set! #f))
	    (("--no-sofile" (help "Disable loading pre-compiled file"))
	     (hop-sofile-enable-set! #f))
	    (("--autoload" (help "Enable autoload (default)"))
	     (set! autoloadp #t))
	    (("--no-autoload" (help "Disable autoload"))
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
	     (cond
		((string=? level "")
		 (hop-sofile-enable-set! #f)
		 (set! clientc-source-map #t)
		 (hop-clientc-debug-unbound-set! 1)
		 (set! clientc-debug #t)
		 (set! clientc-arity-check #t)
		 (set! clientc-type-check #t)
		 (bigloo-debug-set! (+fx 1 (bigloo-debug))))
		((string=? level "clientc-debug")
		 (set! clientc-debug #t)
		 (set! clientc-inlining #f))
		((string=? level "no-clientc-debug")
		 (set! clientc-debug #t))
		((string=? level "clientc-arity-check")
		 (set! clientc-arity-check #t))
		((string=? level "no-clientc-arity-check")
		 (set! clientc-arity-check #f))
		((string=? level "clientc-type-check")
		 (set! clientc-type-check #t))
		((string=? level "no-clientc-type-check")
		 (set! clientc-type-check #f))
		((string=? level "clientc-use-strict")
		 (set! clientc-use-strict #t))
		((string=? level "no-clientc-use-strict")
		 (set! clientc-use-strict #f))
		((string=? level "clientc-inlining")
		 (set! clientc-inlining #t))
		((string=? level "no-clientc-inlining")
		 (set! clientc-inlining #f))
		((string=? level "clientc-debug-unbound")
		 (hop-clientc-debug-unbound-set! 1))
		((string=? level "no-clientc-debug-unbound")
		 (hop-clientc-debug-unbound-set! 0))
		((string=? level "clientc-source-map")
		 (set! clientc-source-map #t))
		((string=? level "no-clientc-source-map")
		 (set! clientc-source-map #f))
		((string=? level "module")
		 (bigloo-debug-module-set! 2))
		((string=? level "sofile")
		 (hop-sofile-enable-set! #f))
		((string=? level "0")
		 #f)
		(else
		 (hop-sofile-enable-set! #f)
		 (let ((l (string->integer level)))
		    (set! clientc-source-map #t)
		    (set! clientc-debug #t)
		    (set! clientc-inlining (<=fx l 2))
		    (set! clientc-arity-check #t)
		    (set! clientc-type-check #t)
		    (bigloo-debug-set! l)
		    (hop-clientc-debug-unbound-set! l)))))
	    (("--client-output" ?file (help "Client output port [stderr]"))
	     (if (string=? file "-")
		 (hop-client-output-port-set! (current-output-port))
		 (let ((p (open-output-file file)))
		    (if (output-port? p)
			(hop-client-output-port-set! p)
			(error "hop" "Cannot open client port" file)))))
	    (("--devel" (help "Enable devel mode"))
	     (set! clear-cache #t)
	     (hop-cache-enable-set! #f)
	     (hop-allow-redefine-service-set! #t)
	     (hop-force-reload-service-set! #t))
	    (("--time" (help "Report execution time"))
	     (hop-report-execution-time-set! #t))
	    (("-w?level" (help "Increase/set warning level (-w0 no warning)"))
	     (if (string=? level "")
		 (bigloo-warning-set! (+fx 1 (bigloo-warning)))
		 (bigloo-warning-set! (string->integer level))))
	    (("-s?level" (help "Increase/set security level (-s0 no security enforcement)"))
	     (if (string=? level "")
		 (hop-security-set! (+fx 1 (hop-security)))
		 (hop-security-set! (string->integer level)))
	     (cond
		((=fx (hop-security) 0)
		 (hop-allow-redefine-service-set! #t))
		((>=fx (hop-security) 2)
		 (hop-security-manager-set! 'tree))))
	    (("--no-color" (help "Disable colored traces"))
	     (bigloo-trace-color-set! #f))
	    (("--log-file" ?file (help "Use <FILE> as log file"))
	     (set! log-file file))
	    (("--capture-file" ?file (help "Use <FILE> as remote capture file"))
	     (hop-capture-port-set! (open-output-file file)))
	    (("--allow-service-override" (help "Allow service overriding (see -s)"))
	     (hop-security-set! 0))
	    
	    ;; Run
	    (section "Run")
	    ((("-p" "--http-port") ?port (help (format "Port number [~s]" p)))
	     (set! p (string->integer port)))
	    (("--listen-addr" ?addr (help "Server listen hostname or IP"))
	     (hop-server-listen-addr-set! addr))
	    (("--fast-server-event-port" ?port (help (format "Fast Server event port number [~s]" ep)))
	     (set! ep (string->integer port)))
	    (("--https" (help (format "Enable HTTPS")))
	     (hop-enable-https-set! #t))
	    (("--no-https" (help (format "Disable HTTPS")))
	     (hop-enable-https-set! #f))
	    (("--https-pkey" ?pem (help "HTTPS private key file"))
	     (hop-https-pkey-set! pem))
	    (("--https-cert" ?pem (help "HTTPS certificate file"))
	     (hop-https-cert-set! pem))
	    (("--fast-server-event" (help "Enable fast Server events"))
	     (hop-enable-fast-server-event-set! #t))
	    (("--no-fast-server-event" (help "Disable fast server events"))
	     (hop-enable-fast-server-event-set! #f))
	    ((("-i" "--session-id") ?session (help "Set session identifier"))
	     (hop-session-set! (string->integer session)))
	    (("--no-job-restore" (help "Don't restore jobs"))
	     (hop-job-restore-set! #f))
	    ((("-e" "--eval") ?string (help "Evaluate Hop STRING"))
	     (set! exprs (cons string exprs)))
	    ((("-j" "--evaljs") ?string (help "Evaluate JavaScript STRING"))
	     (set! exprsjs (cons string exprsjs)))
	    (("--repl" (help "Start a repl"))
	     (hop-enable-repl-set! 'scm))
	    (("--repljs" (help "Start a JS repl"))
	     (hop-enable-repl-set! 'js))
	    (("--jobs" (help "Enable jobs management"))
	     (hop-enable-jobs-set! #t))
	    (("--no-jobs" (help "Disable jobs management"))
	     (hop-enable-jobs-set! #f))
	    ((("-z" "--zeroconf") (help "Enable zeroconf support"))
	     (set! zeroconf #t))
	    (("--no-zeroconf" (help "Disable zeroconf support (default)"))
	     (set! zeroconf #f))
	    ((("-d" "--webdav") (help "Enable webdav support"))
	     (set! webdav #t))
	    (("--no-webdav" (help "Disable webdav support"))
	     (set! webdav #f))
	    ((("-x" "--xml-backend")
	      ?ident
	      (help (format "Set XML backend [~s]"
		       (with-access::xml-backend (hop-xml-backend)
			     (id) id))))
	     (set! be ident))
	    (("--accept-kill" (help "Enable remote kill commands (see -k)"))
	     (hop-accept-kill-set! #t))
	    (("--no-accept-kill" (help "Forbidden remote kill commands"))
	     (hop-accept-kill-set! #f))
	    ((("-k" "--kill") (help "Kill the running local HOP and exit"))
	     (set! killp #t))
	    (("--user" ?user (help "Set Hop process owner"))
	     (set! setuser user))
	    (("--no-user" (help "Don't attempt to set the Hop process owner"))
	     (hop-user-set! #f))
	    (("--server" (help "Start the Web server (default)"))
	     (hop-run-server-set! #t))
	    (("--no-server" (help "Exit after loading command line files"))
	     (hop-run-server-set! #f)
	     (unless p (set! p 0)))
	    (("--exepath" ?name (help "Set JavaScript executable path"))
	     (if (string=? name "*")
		 (hop-exepath-set! (executable-name))
		 (hop-exepath-set! name)))
	    
	    ;; PATHS
	    (section "Paths")
	    ((("-I" "--path") ?path (help "Add <PATH> to hop load path"))
	     (hop-path-set! (cons path (hop-path)))
	     (nodejs-resolve-extend-path! (list path)))
	    ((("-L" "--library-path") ?path (help "Add <PATH> to hop library path"))
	     (bigloo-library-path-set! (cons path (bigloo-library-path))))
	    ((("-l" "--library") ?library (help "Preload additional <LIBRARY>"))
	     (set! libraries (cons library libraries )))

	    ;; JavaScript
	    (section "JavaScript")
	    (("--js" (help "Enable JavaScript (default)"))
	     (hop-javascript-set! #t))
	    (("--no-js" (help "Disable JavaScript"))
	     (hop-javascript-set! #f))
	    (("--js-return-as-exit" (help "Consider toplevel returns as exits"))
	     (nodejs-compiler-options-add! :return-as-exit #t)) 
	    (("--no-js-return-as-exit" (help "Do not consider toplevel returns as exits"))
	     (nodejs-compiler-options-add! :return-as-exit #f))
	    (("--js-es6" (help "Enable all EcmaScript 6 supports"))
	     (for-each (lambda (ext)
			  (nodejs-compiler-options-add! ext #t))
		'(es6-let: es6-const: es6-arrow-function: es6-default-value:
		  es6-rest-argument:)))
	    (("--js-dsssl" (help "Enable DSSSL like JS services (deprecated)"))
	     (nodejs-compiler-options-add! :dsssl #t))
	    (("--js-option" ?opt ?val (help "Add JavaScript compilation option"))
	     (nodejs-compiler-options-add! (string->keyword opt)
		(cond
		   ((or (string=? val "true") (string=? val "#t")) #t)
		   ((or (string=? val "false") (string=? val "#f")) #f)
		   ((string->number val) => (lambda (val) val))
		   (else val))))
	    (("--js-modules-dir" ?dir (help "Set default node_modules dir"))
	     (nodejs-modules-directory-set! dir))
	    ;; Internals
	    (section "Internals")
	    (("--configure" ?config (help "Report HOP configuration"))
	     (hop-configure config)
	     (exit 0))
	    (("--cond-expand" ?feature (help "Declare cond-expand feature"))
	     (register-srfi! (string->symbol feature)))
	    (("--no-thread" (help "Disable multithreading (equiv. to \"--scheduler nothread\")"))
	     (hop-max-threads-set! 1)
	     (hop-enable-keep-alive-set! #f)
	     (hop-scheduling-set! 'nothread))
	    (("--max-threads" ?m (help "Maximum number of handling HTTP requests"))
	     (hop-max-threads-set! (string->integer m)))
	    (("--scheduler" ?ident (help (format "Set scheduling policy [~s] (see --help-scheduler)" (hop-scheduling))))
	     (hop-scheduling-set! (string->symbol ident)))
	    (("--help-scheduler" (help "Print available schedulers list"))
	     (with-output-to-port (current-error-port)
		(lambda ()
		   (print  "Schedulers:")
		   (print "  - queue (split threads but avoid useless switches)")
		   (print "  - nothread (single threaded execution)")
		   (print "  - one-to-one (one thread per request)")
		   (print "  - pool (one thread per request from a pool)")
		   (print "  - accept-many (as pool but an accept-many call)")))
	     (exit 0))
	    (("--javascript-version" ?version
		(help (format "JavaScript version to generate (default ~s)"
			 (hop-javascript-version))))
	     (hop-javascript-version-set! version))
	    (("-psn_?dummy")
	     ;; Macosx sends process serial numbers this way.
	     ;; just ignore it.
	     'do-nothing)
	    (("--" ?file (help "Ignore next arguments"))
	     (set! files (cons file files))
	     (stop #t))
	    (("-?dummy")
	     (args-parse-usage #f)
	     (exit 1))
	    (else
	     (set! files (cons else files)))))

      ;; http port
      (hop-port-set! (or p (hop-port)))
      
      ;; Hop version
      (hop-verb 1 "Hop " (hop-color 1 "v" (hop-version)) "\n")

      ;; kill
      (when killp
	 (hop-verb 2 "Kill HOP process " (key-filepath p) "...\n")
	 (let ((key (hop-process-key-read p)))
	    (if (string? key)
		(http :port p :path (format "/hop/shutdown/kill?key=~a" key))
		(error "hop-kill" "Cannot find process key" (key-filepath p)))
	    (exit 0)))

      ;; open the server socket before switching to a different process owner
      (init-server-socket!)
      
      ;; set the hop process owner
      (when setuser
	 (hop-user-set! setuser)
	 (set-hop-owner! setuser))

      ;; log
      (when log-file
	 (let ((p (append-output-file log-file)))
	    (unless p
	       (error "hop" "Cannot open log file" log-file))
	    (hop-log-file-set! p)))
      
      ;; mime types
      (when mimep
	 (load-mime-types (hop-mime-types-file))
	 (cond
	    ((string? mime-file)
	     (load-mime-types mime-file))
	    ((getenv "HOME")
	     =>
	     (lambda (p)
		(load-mime-types (make-file-name p ".mime.types"))))))
      
      ;; clear al caches
      (when clear-cache
	 (let ((cache (make-cache-name)))
	    (when (directory? cache)
	       (delete-path cache))))
      
      ;; weblets path
      (hop-autoload-directory-add!
	 (make-file-name (hop-rc-directory) "weblets"))
      
      ;; init hss, scm compilers, and services
      (init-hss-compiler! (hop-port))
      
      (init-hopscheme! :reader (lambda (p v) (hop-read p))
	 :tmp-dir (os-tmp)
	 :share (hop-share-directory)
	 :verbose (hop-verbose)
	 :eval (lambda (e)
		  (let* ((ev (eval e))
			 (op (open-output-string)))
		     (obj->javascript-attr ev op)
		     (close-output-port op)))
	 :hop-compile (lambda (obj op compile)
			 (hop->javascript obj op compile #f))
	 :hop-register hop-register-value
	 :hop-library-path (hop-library-path)
	 :features `(hop
		       hop-client
		       ,(string->symbol (format "hop-~a" (hop-branch)))
		       ,(string->symbol (format "hop-~a" (hop-version))))
	 :javascript-version (hop-javascript-version)
	 :expanders `(labels match-case
			   (define-tag . ,hop-client-define-tag)
			(define-xml-compound . ,hop-client-define-xml-compound))
	 :source-map clientc-source-map
	 :arity-check clientc-arity-check
	 :type-check clientc-type-check
	 :debug clientc-debug
	 :compress clientc-compress
	 :inlining clientc-inlining
	 :module-use-strict clientc-use-strict
	 :function-use-strict clientc-use-strict)

      (init-clientc-compiler! :modulec hopscheme-compile-module
	 :expressionc hopscheme-compile-expression
	 :valuec hopscheme-compile-value
	 :macroe hopscheme-create-empty-macro-environment
	 :filec hopscheme-compile-file
	 :sexp->precompiled sexp->hopscheme
	 :precompiled->sexp hopscheme->sexp
	 :precompiled->JS-expression hopscheme->JS-expression
	 :precompiled->JS-statement hopscheme->JS-statement
	 :precompiled->JS-return hopscheme->JS-return
	 :precompiled-declared-variables hopscheme-declared
	 :precompiled-free-variables hopscheme-free
	 :filename-resolver hop-clientc-filename-resolver
	 :jsc nodejs-compile-file)

      (init-hop-services!)
      (init-hop-widgets!)

      ;; hoprc
      (if loadp
	  (hop-rc-loaded!
	     (or (parseargs-loadrc rc-file (hop-rc-file)) rc-file))
	  (add-user! "anonymous" 
	     :services '(home doc epassword wizard hz/list shutdown)
	     :directories (hop-path)
	     :preferences-filename #f))

      ;; webdav
      (when (boolean? webdav)
	 (hop-enable-webdav-set! webdav))
	 
      ;; zeroconf
      (when (boolean? zeroconf)
	 (hop-enable-zeroconf-set! zeroconf))
	 
      ;; default backend
      (when (string? be) (hop-xml-backend-set! (string->symbol be)))
      
      ;; server event port
      (when (hop-enable-fast-server-event)
	 (cond
	    ((eq? ep #unspecified)
	     (set! ep p))
	    ((and (>fx ep 0) (<fx ep 1024))
	     (error "fast-server-event-port"
		"Server event port must be greater than 1023. (See `--fast-server-event-port' or `--no-fast-server-event' options.)"
		ep))
	    (else
	     (hop-fast-server-event-port-set! ep))))
      
      (when autoloadp (install-autoload-weblets! (hop-autoload-directories)))
      
      (for-each (lambda (l) (eval `(library-load ',l))) libraries)
      
      ;; write the process key
      (hop-process-key-write (hop-process-key) (hop-port))
      (register-exit-function! (lambda (ret)
				  (hop-process-key-delete (hop-port))
				  ret))

      ;; check if a new server socket must be opened
      (when (and (integer? p) (not (=fx p (hop-port))))
	 (init-server-socket!))
      
      (values (reverse files) (reverse! exprs) (reverse! exprsjs))))

;*---------------------------------------------------------------------*/
;*    set-hop-owner! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-hop-owner! user)

   (define (err)
      (error "hop"
	     "Hop is not allowed to be executed as `root'. Create a dedicated Hop user to run Hop on behalf of.\n"
	     "If you know what you are doing and want to run Hop with the
`root' permissions, edit the Hop configuration file and set the appropriate `hop-user' value."))

   (cond
      ((not (=fx (getuid) 0))
       #unspecified)
      ((not (pair? (getpwnam "root")))
       #unspecified)
      ((not user)
       #unspecified)
      ((string? user)
       (if (string=? user "root")
	   (error "hop" "Hop is executed as root (which is forbidden) and fails to switch to the dedicated HOP system user" user)
	   (let ((pw (getpwnam user)))
	      (if (pair? pw)
		  (let ((uid (caddr pw))
			(gid (cadddr pw)))
		     (unless (=fx (getuid) uid)
			(hop-verb 2 "  switch to user: "
			   (hop-color 2 "" user) " (" uid ":" gid ")\n")
			(setgid gid)
			(setuid uid)))
		  (error "hop" "Hop is executed as root (which is forbidden) and fails to switch to the dedicated HOP system user" user)))))
      (user
       (err))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    hello-world ...                                                  */
;*---------------------------------------------------------------------*/
(define (hello-world)
   ;; ports and various configuration
   (hop-verb 1
      (if (hop-enable-https)
	  (format "  https (~a): " (hop-https-protocol)) "  http: ")
      (hop-color 2 "" (hop-port)) "\n")
   (hop-verb 1
      "  hostname: " (hop-color 2 "" (hop-server-hostname)) "\n")
   (hop-verb 1
      "  hostip: " (hop-color 2 "" (hop-server-hostip)) "\n")
   (hop-verb 2
      (if (and (hop-enable-fast-server-event)
	       (not (=fx (hop-port) (hop-fast-server-event-port))))
	  (format "  comet-port: ~a\n"
	     (hop-color 2 "" (hop-fast-server-event-port)))
	  "")
      "  security: "
      (with-access::security-manager (hop-security-manager) (name)
	 (hop-color 2 "" name))
      " [" (hop-security) "]\n")
   (hop-verb 3 "  session: " (hop-color 2 "" (hop-session)) "\n")
   (hop-verb 3 "  backend: " (hop-color 2 "" (hop-backend)) "\n")
   (hop-verb 3 "  scheduler: "
      (hop-color 2 ""
	 (cond-expand
	    (enable-threads (hop-scheduling))
	    (else "single-threaded")))
      "\n"))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage)
   (print "Hop v" (hop-version))
   (print "usage: hop [options] ...")
   (print "       hop [options] file.hop|file.hz|file.js ...")
   (args-parse-usage #f))

;*---------------------------------------------------------------------*/
;*    %hop-load-rc ...                                                 */
;*---------------------------------------------------------------------*/
(define (%hop-load-rc path)
   (when (and (string? path) (file-exists? path))
      (hop-verb 3 "loading \"" (hop-color 3 "" path) "\"...\n")
      (hop-load path)
      path))

;*---------------------------------------------------------------------*/
;*    parseargs-loadrc ...                                             */
;*---------------------------------------------------------------------*/
(define (parseargs-loadrc rc-file default)
   (if (string? rc-file)
       (if (member (suffix rc-file) '("hop" "scm"))
	   (%hop-load-rc rc-file)
	   rc-file)
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
	    (hop-verb 3 "  key process file: " (hop-color 4 "" path) "\n")
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

;*---------------------------------------------------------------------*/
;*    hop-clientc-filename-resolver ...                                */
;*---------------------------------------------------------------------*/
(define (hop-clientc-filename-resolver name context-or-path)
   (cond
      ((or (string-suffix? ".js" name) (not (string? context-or-path)))
       (let ((scope context-or-path))
	  (nodejs-resolve name scope (js-get scope 'module scope) 'head)))
      (else
       (let ((path context-or-path))
	  (find-file/path name path)))))
