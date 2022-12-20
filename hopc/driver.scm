;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopc/driver.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 14 08:13:05 2014                          */
;*    Last change :  Tue Dec 20 13:57:12 2022 (serrano)                */
;*    Copyright   :  2014-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOPC compiler driver                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc_driver
   
   (library scheme2js hopscheme js2scheme hop)

   (import  hopc_parseargs
	    hopc_param
	    hopc_hop)

   (eval    (library hop))

   (export  (jsheap::int)
	    (js-driver->string)
	    (js-drivers-list)
	    (hopc-read p #!key cenv menv)
	    (setup-client-compiler!)
	    (compile-sources::int)))

;*---------------------------------------------------------------------*/
;*    hopc-read ...                                                    */
;*---------------------------------------------------------------------*/
(define (hopc-read p #!key cenv menv)
   (if (=fx (bigloo-debug) 0)
       (begin
	  (bigloo-debug-set! 1)
	  (let ((v (hop-read p :cenv cenv :menv menv)))
	     (bigloo-debug-set! 0)
	     v))
       (hop-read p :cenv cenv :menv menv)))

;*---------------------------------------------------------------------*/
;*    setup-client-compiler! ...                                       */
;*---------------------------------------------------------------------*/
(define (setup-client-compiler!)
   ;; disable cache clearing otherwise parallel
   ;; invocations of hopc are impossible because one removes
   ;; the file of the other.
   (hop-clientc-clear-cache-set! #f)
   (init-hopscheme! :reader (lambda (p v) (hopc-read p))
      :share (hopc-share-directory)
      :verbose (hop-verbose)
      :eval (lambda (e) (let ((op (open-output-string)))
			   (obj->javascript-expr (eval e) op)
			   (close-output-port op)))
      :hop-compile (lambda (obj op compile ctx)
		      (hop->javascript obj op compile #f ctx))
      :hop-register hop-register-value
      :javascript-version (hop-javascript-version)
      :hop-library-path (hop-library-path)
      :features `(hop
		  ,(string->symbol (format "hop-~a" (hop-branch)))
		  ,(string->symbol (format "hop-~a" (hop-version))))
      :expanders `(labels match-case
			(define-tag . ,(eval 'hop-client-define-tag)))
      :source-map (hopc-clientc-source-map)
      :arity-check (hopc-clientc-arity-check)
      :type-check (hopc-clientc-type-check)
      :meta (hopc-clientc-meta)
      :debug (hopc-clientc-debug)
      :module-use-strict (hopc-clientc-use-strict)
      :function-use-strict (hopc-clientc-use-strict))
   (init-clientc-compiler! :modulec hopscheme-compile-module
      :expressionc hopscheme-compile-expression
      :valuec hopscheme-compile-value 
      :macroe hopscheme-create-empty-macro-environment
      :filec hopscheme-compile-file
      :sexp->precompiled sexp->hopscheme
      :precompiled->sexp hopscheme->sexp
      :precompiled->JS-expression hopscheme->JS-expression
      :precompiled->JS-statement hopscheme->JS-statement
      :precompiled->JS-return hopscheme->JS-return))

;*---------------------------------------------------------------------*/
;*    js-driver ...                                                    */
;*---------------------------------------------------------------------*/
(define (js-driver)
   (cond
      ((string? (hopc-js-driver))
       (j2s-make-driver (string-split (hopc-js-driver) ",")))
      ((eq? (hopc-pass) 'client-js)
       (cond
	  ((eq? (hopc-js-target) 'es5) (j2s-ecmascript5-driver))
	  ((>=fx (hopc-optim-level) 1) (j2s-javascript-optim-driver))
	  (else (j2s-javascript-driver))))
      ((>=fx (hopc-optim-level) 1)
       (j2s-optim-driver))
      (else
       (j2s-plain-driver))))

;*---------------------------------------------------------------------*/
;*    js-driver-name ...                                               */
;*---------------------------------------------------------------------*/
(define (js-driver-name)
   (cond
      ((string? (hopc-js-driver))
       (hopc-js-driver))
      ((eq? (hopc-pass) 'client-js)
       (cond
	  ((eq? (hopc-js-target) 'es5) "j2s-ecmascript5-driver")
	  ((>=fx (hopc-optim-level) 1) "j2s-javascript-optim-driver")
	  (else "j2s-javascript-driver")))
      ((>=fx (hopc-optim-level) 1)
       "j2s-optim-driver")
      (else
       "j2s-plain-driver")))

;*---------------------------------------------------------------------*/
;*    js-drivers-list ...                                              */
;*---------------------------------------------------------------------*/
(define (js-drivers-list)
   (j2s-builtin-drivers-list))

;*---------------------------------------------------------------------*/
;*    js-driver->string ...                                            */
;*---------------------------------------------------------------------*/
(define (js-driver->string)
   (reduce (lambda (el rest) (string-append rest "," el))
      '()
      (map (lambda (s) (with-access::J2SStage s (name) name))
	 (js-driver))))

;*---------------------------------------------------------------------*/
;*    compile-sources ...                                              */
;*---------------------------------------------------------------------*/
(define (compile-sources::int)
   
   (define (input-file->module-name path)
      (let* ((dir (dirname path))
	     (mod (prefix (basename path))))
	 (if (string=? dir ".")
	     mod
	     (format "%%~a_~a" dir mod))))
   
   (define (compile-javascript p)
      (if (or (string-suffix? ".js" p) (string-suffix? ".mjs" p))
	  (call-with-input-file p
	     (lambda (in) (compile p in (language p))))
	  (hopscheme-compile-file p
	     (cond
		((string? (hopc-destination)) (hopc-destination))
		((string? (hopc-temp)) (hopc-temp))
		(else "-"))
	     '())))
   
   (define (find-location-dir exp)
      (when (epair? exp)
	 (match-case (cer exp)
	    ((at ?file . ?-) (dirname file)))))

   (define (compile-module exp src dst)
      (match-case exp
	 ((module ?id . ?clauses)
	  ;; this produces a side effect
	  ;;(eval exp)
	  ;; generate the new module clause
	  (let* ((iimports '())
		 (nclauses '()))
	     (for-each (lambda (c)
			  (match-case c
			     ((<TILDE> ??- :src (quote ?import) ??-)
			      (set! iimports (cons import iimports)))
			     (else
			      (set! nclauses
				 (cons (hop-module-rebase c src dst)
				    nclauses)))))
		clauses)
	     (values
		`(module ,id ,@(reverse! nclauses))
		(hopscheme-compile-module (reverse! iimports)
		   (find-location-dir exp)))))
	 (else
	  (error "hopc" "Illegal module" exp))))
   
   (define (generate-bigloo::int fname in lang)
      
      (define (generate-hop out::output-port)
	 (let loop ((cenv #f))
	    (let ((exp (hopc-read in :cenv cenv)))
	       (unless (eof-object? exp)
		  (match-case exp
		     ((module . ?-)
		      (multiple-value-bind (mod env)
			 (compile-module exp fname (output-port-name out))
			 (pp mod out)
			 (pp `(define the-loading-file
				 (let ((file (hop-sofile-rebase ,(sobase-path fname))))
				    (lambda ()
				       file)))
			    out)
			 (pp `(define (the-loading-dir)
				 (dirname (the-loading-file)))
			    out)
			 (loop env)))
		     (else
		      (pp exp out)
		      (loop cenv)))))))
      
      (define (generate-hopscript out::output-port)
	 (let ((mmap (when (and (string? fname) (file-exists? fname))
			(open-mmap fname :read #t :write #f))))
	    (for-each (lambda (exp) (pp exp out))
	       (apply j2s-compile in
		  :return-as-exit (hopc-js-return-as-exit)
		  :filename fname
		  :mmap-src mmap
		  :driver (js-driver)
		  :driver-name (js-driver-name)
		  :worker (hopc-js-worker)
		  :worker-slave (hopc-js-worker-slave)
		  :module-main (if (boolean? (hopc-js-module-main))
				   (hopc-js-module-main)
				   (not (memq (hopc-pass) '(object so))))
		  :module-name (or (hopc-js-module-name) 
				   (input-file->module-name fname))
		  :module-path (hopc-js-module-path)
		  :hopscript-header (hopc-js-header)
		  :type-annotations (hopc-js-type-annotations)
		  :optim (hopc-optim-level)
		  :verbose (hop-verbose)
		  :long-size (hopc-long-size)
		  :int-size (hopc-int-size)
		  :plugins-loader (hopc-plugins-loader)
		  :sofile-dir (hopc-sofile-dir)
		  :debug (bigloo-debug)
		  :warning (bigloo-warning)
		  :node-modules-directory (hopc-node-modules-directory)
		  (hopc-j2s-flags)))))
      
      (define (generate-js out::output-port)
	 (let ((mmap (when (and (string? fname) (file-exists? fname))
			(open-mmap fname :read #t :write #f))))
	    (for-each (lambda (exp)
			 (unless (isa? exp J2SNode)
			    (display exp out)))
	       (apply j2s-compile in
		  :return-as-exit (hopc-js-return-as-exit)
		  :filename fname
		  :mmap-src mmap
		  :driver (js-driver)
		  :driver-name (js-driver-name)
		  :worker (hopc-js-worker)
		  :worker-slave (hopc-js-worker-slave)
		  :module-main (if (boolean? (hopc-js-module-main))
				   (hopc-js-module-main)
				   (not (memq (hopc-pass) '(object so))))
		  :module-name (or (hopc-js-module-name)
				   (input-file->module-name fname))
		  :module-path (hopc-js-module-path)
		  :hopscript-header (hopc-js-header)
		  :type-annotations (hopc-js-type-annotations)
		  :optim (hopc-optim-level)
		  :verbose (hop-verbose)
		  :long-size (hopc-long-size)
		  :int-size (hopc-int-size)
		  :sofile-dir (hopc-sofile-dir)
		  :plugins-loader (hopc-plugins-loader)
		  :debug (bigloo-debug)
		  :warning (bigloo-warning)
		  :node-modules-directory (hopc-node-modules-directory)
		  (hopc-j2s-flags)))))
      
      (define (generate out::output-port lang::symbol)
	 (case lang
	    ((js) (generate-js out))
	    ((hop) (generate-hop out))
	    ((hopscript) (generate-hopscript out))))

      (if (string? (hopc-destination))
	  (call-with-output-file (hopc-destination)
	     (lambda (out) (generate out lang)))
	  (generate (current-output-port) lang))
      0)
   
   (define (compile-bigloo::int fname in lang)

      (define (srfi-hopc)
	 '("-srfi" "hopc"))
      
      (define (srfi-libuv)
	 (cond-expand
	    (enable-libuv (cons* "-srfi" "enable-libuv" (srfi-hopc)))
	    (else (srfi-hopc))))
      
      (define (srfi-tls)
	 (cond-expand
	    (enable-tls (cons* "-srfi" "enable-tls" (srfi-libuv)))
	    (else (srfi-libuv))))

      (define (srfi-opts)
	 (srfi-tls))
      
      (define (compile-file opts file)
         (let* ((opts (append (cons file (srfi-opts)) opts))
                (proc (apply run-process (hopc-bigloo) opts))
                (cmd (format "~a ~l" (hopc-bigloo) opts)))
            (signal sigterm
               (lambda (sig)
                  (process-kill proc)
                  (exit 1)))
	    (hop-verb 4 cmd "\n")
	    (process-wait proc)
            (process-exit-status proc)))
      
      (define (compile-temp opts comp file temp)
	 (call-with-output-file temp comp)
	 (compile-file opts temp))

      (define (compile-temp-ast opts comp file temp)
	 (call-with-output-file temp comp)
	 (let* ((baseopts (cons "-fread-internal-src"
			     (append (srfi-opts) opts)))
		(opts (if (string? file)
			  (cons* "-fread-internal-src-file-name" file baseopts)
			  baseopts))
		(proc (apply run-process (hopc-bigloo)
			 "-suffix" "ast" temp
			 opts))
		(cmd (format "~a -suffix ast ~a ~l" (hopc-bigloo) temp opts))
		(out (process-input-port proc)))
	    (signal sigterm
	       (lambda (sig)
		  (process-kill proc)
		  (exit 1)))
	    (hop-verb 4 cmd "\n")
	    (unwind-protect
	       (begin
		  (process-wait proc)
		  (process-exit-status proc))
	       (delete-file temp))))

      (define (compile-pipe opts comp file)
         (let* ((baseopts (cons "-fread-internal-src"
			     (append (srfi-opts) opts)))
                (opts (if (string? file)
                          (cons* "-fread-internal-src-file-name" file baseopts)
                          baseopts))
                (proc (apply run-process (hopc-bigloo)
                         input: pipe: "-"
                         opts))
                (cmd (format "~a - ~l" (hopc-bigloo) opts))
                (out (process-input-port proc)))
            (signal sigterm
               (lambda (sig)
                  (process-kill proc)
                  (exit 1)))
            (unwind-protect
               (comp out)
               (begin
                  (hop-verb 4 cmd "\n")
                  (close-output-port out)))
            (process-wait proc)
	    (process-exit-status proc)))

      (define (compiler opts comp file temp)
	 (cond
	    ((not comp)
	     (compile-file opts file))
	    ((string? temp)
	     (compile-temp opts comp file temp))
	    ((> (file-size file) (hopc-pipe-filesize-threshold))
	     (compile-temp-ast opts comp file
		(make-file-name (os-tmp)
		   (string-append (basename file) ".ast"))))
	    (else
	     (compile-pipe opts comp file))))
      
      (define (compile-hop-with-deps in opts file temp deps)
	 (let* ((mext "(lambda (exp)
			(match-case exp
			   ((module . ?rest)
			    (let loop ((clauses (cdr rest))
				       (prev rest))
			       (cond
				  ((null? clauses)
				   exp)
				  ((eq? (car clauses) '~)
				   (set-cdr! prev (cddr clauses))
				   (loop (cddr clauses) prev))
				  (else
				   (loop (cdr clauses) clauses)))))
			   (else
			    exp)))")
		(options (append
			    `("-extend-module"
				,mext
				"--force-cc-o"
				"-rpath" ,(make-file-path (hop-lib-directory)
					     "hop" (hop-version))
				"-I" ,(dirname file))
			    deps))
		(mkcomp (lambda (write)
			   (lambda (out)
			      (let loop ((cenv #f))
				 (let* ((exp (hopc-read in :cenv cenv))
					;; (path fname)
					(path file))
				    (unless (eof-object? exp)
				       (match-case exp
					  ((module . ?-)
					   (multiple-value-bind (mod env)
					      (compile-module exp (input-port-name in) (output-port-name out))
					      (write mod out)
					      (write `(define the-loading-file
							 (let ((file (hop-sofile-rebase ,(sobase-path path))))
							    (lambda ()
							       file)))
						 out)
					      (write `(define (the-loading-dir)
							 (dirname (the-loading-file)))
						 out)
					      (loop env)))
					  (else
					   (write exp out)
					   (loop cenv))))))))))
	    (if (string? temp)
		(compiler
		   (append options opts)
		   (mkcomp pp)
		   file temp)
		(compiler
		   (append (cons "-fread-internal-src" options) opts)
		   (mkcomp (lambda (exp port) (write (obj->string exp) port)))
		   file temp))))

      (define (compile-hop in opts file temp)
	 (let* ((imports (hop-module-imports file))
		(deps (map (lambda (import)
			      (let* ((path import)
				     (temp (unless (string-suffix? ".scm" path)
					      (make-file-name (os-tmp)
						 (string-append
						    (prefix (basename path))
						    ".scm"))))
				     (obj (make-file-name (os-tmp)
					      (string-append
						 (prefix (basename path))
						 ".o")))
				     (opts (cons* "-c" "-o" obj (bigloo-options))))
				 (call-with-input-file path
				    (lambda (in)
				       (compile-hop-with-deps in opts path temp '())))
				 obj))
			 imports)))
	    (compile-hop-with-deps in opts file temp deps)))

      (define (compile-scheme in opts file temp)
	 (compiler
	    (append `("-fread-internal-src"
			"--force-cc-o"
			"-rpath" ,(make-file-path (hop-lib-directory)
				     "hop" (hop-version))
			"-I" ,(dirname file))
	       opts)
	    (lambda (out)
	       (let loop ()
		  (let ((exp (hopc-read in)))
		     (unless (eof-object? exp)
			(write (obj->string exp) out)
			(loop)))))
	    file
	    temp))

      (define (compile-hopscript in opts file syn exec temp)
	 (let ((mmap (when (and (string? fname) (file-exists? fname))
			(open-mmap fname :read #t :write #f))))
	    (compiler
	       (append (hopc-js-libraries)
		  `("--force-cc-o"
		      "-rpath" ,(make-file-path (hop-lib-directory)
				   "hop" (hop-version)))
		  opts)
	       (lambda (out)
		  (when (string? temp)
		     (display ";; " out)
		     (display (format "~( )" (command-line)) out)
		     (newline out))
		  ;; compile
		  (map (lambda (e)
			  (if (string? temp)
			      (pp e out)
			      (write (obj->string e) out)))
		     (apply j2s-compile in
			:return-as-exit (hopc-js-return-as-exit)
			:mmap-src mmap
			:filename fname
			:driver (js-driver)
			:driver-name (js-driver-name)
			:worker (hopc-js-worker)
			:worker-slave (hopc-js-worker-slave)
			:module-main (if (boolean? (hopc-js-module-main))
					 (hopc-js-module-main)
					 (not (memq (hopc-pass) '(object so))))
			:module-name (or (hopc-js-module-name)
					 (input-file->module-name fname))
			:module-path (hopc-js-module-path)
			:hopscript-header (hopc-js-header)
			:type-annotations (hopc-js-type-annotations)
			:optim (hopc-optim-level)
			:verbose (hop-verbose)
			:long-size (hopc-long-size)
			:int-size (hopc-int-size)
			:plugins-loader (hopc-plugins-loader)
			:sofile-dir (hopc-sofile-dir)
			:debug (bigloo-debug)
			:warning (bigloo-warning)
			:function-nice-name (string? temp)
			:syntax syn
			:node-modules-directory (hopc-node-modules-directory)
			(hopc-j2s-flags))))
	       file
	       temp)))

      (define (compile-language lang in opts file exec temp)
	 ;;(tprint "compile.lang.1: " (hopc-plugins-loader))
	 (let* ((comp (hopc-language-loader))
		(jsmain (if (boolean? (hopc-js-module-main))
			    (hopc-js-module-main)
			    (not (memq (hopc-pass) '(object so)))))
		(obj (nowarning (lambda ()
				   (let ((tmp (cond
						 ((not (hopc-temp))
						  #f)
						 ((eq? (hopc-pass) 'ast.json)
						  (hopc-temp))
						 (else
						  (string-append
						     (prefix (hopc-temp))
						     ".ast.json")))))
				      (comp (symbol->string lang) file
					 `((tempfile . ,tmp)
					   (no-cache . #t)
					   (module-main . ,jsmain)))))))
		(ty (assq :type obj))
		(val (assq :value obj)))
	    (cond
	       ((or (not (pair? ty)) (not (pair? val)))
		(error (format "hopc:~a" lang) "wrong status" obj))
	       ((equal? (cdr ty) "error")
		(error (format "hopc:~a" lang) "error" (cdr val)))
	       ((equal? (cdr ty) "json-error")
		(let ((proc (format "hopc:~a" lang)))
		   ((hopc-nodejs-language-notify-error) (cdr val) proc)
		   (exit 1)))
	       ((equal? (cdr ty) "filename")
		(let ((fmt (assq :syntax obj)))
		   (cond
		      ((input-port? in)
		       (close-input-port in)
		       (set! in (open-input-file (cdr val)))))
		   (if (eq? (hopc-pass) 'ast.json)
		       0
		       (if (pair? fmt)
			   (compile-hopscript in opts file (cdr fmt) exec temp)
			   (compile-hopscript in opts file (suffix file) exec temp)))))
	       (else
		(error (format "hopc:~a" lang) "don't know what to do with" obj)))))

      (define (bigloo-options)
	 (let ((l (hopc-optim-level)))
	    (let loop ((opts (hopc-bigloo-O-options))
		       (o '()))
	       (cond
		  ((null? opts)
		   (let ((opts (append (hopc-bigloo-options)
				  (hopc-bigloo-profile-options)
				  (reverse! o))))
		      (cond
			 ((pair? (hopc-bigloo-safe-option))
			  (append (hopc-bigloo-safe-option) opts))
			 ((and (>=fx (hopc-optim-level) 1)
			       (=fx (bigloo-debug) 0)
			       (cond-expand
				  ((not devel) #t)
				  (else (>=fx (hopc-optim-level) 100))))
			  (cons "-unsafe" opts))
			 (else
			  opts))))
		  ((>=fx l (caar opts))
		   (loop (cdr opts) (append (cdar opts) o)))
		  (else
		   (loop (cdr opts) o))))))

      (define (dest-opts dest opts)
	 (cons* "-o" dest opts))
      
      (let* ((opts (bigloo-options))
	     (file (when (and (pair? (hopc-sources))
			      (string? (car (hopc-sources))))
		      (car (hopc-sources))))
	     (opts (cond
		      ((string? (hopc-destination))
		       (when (file-exists? (hopc-destination))
			  (delete-file (hopc-destination)))
		       (case (hopc-pass)
			  ((object)
			   (cons "-c" (dest-opts (hopc-destination) opts)))
			  ((so)
			   (cons* "-dload-sym" "-y"
			      (dest-opts (hopc-destination) opts)))
			  (else
			   (cons* "-o" (hopc-destination) opts))))
		      ((string? file)
		       (case (hopc-pass)
			  ((object)
			   (let* ((base (prefix file))
				  (dest (string-append base ".o")))
			      (cons* "-c" (dest-opts dest opts))))
			  ((so)
			   (let* ((base (prefix file))
				  (dest (string-append base "."
					   (bigloo-config 'shared-lib-suffix))))
			      (cons* "-dload-sym" "-y" (dest-opts dest opts))))
			  (else
			   (dest-opts "a.out" opts))))
		      (else
		       (cons "--to-stdout" opts))))
	     (exec (not (eq? (hopc-pass) 'object))))
	 (unwind-protect
	    (case lang
	       ((hop)
		(compile-hop in opts file (hopc-temp)))
	       ((hopscript ast.json)
		(compile-hopscript in opts file (symbol->string lang) exec (hopc-temp)))
	       ((scheme)
		(compile-scheme in opts file (hopc-temp)))
	       (else
		(compile-language lang in opts file exec (hopc-temp))))
	    (when (and (string? file) (eq? (hopc-pass) 'so))
	       (let ((obj (string-append (prefix file) ".o")))
		  (when (file-exists? obj)
		     (delete-file obj)))))))
   
   (define (compile::int filename::obj in::obj lang::symbol)
      (cond
	 ((eq? (hopc-pass) 'client-js)
	  (generate-bigloo filename in 'js))
	 ((eq? (hopc-pass) 'bigloo)
	  (generate-bigloo filename in lang))
	 (else
	  (compile-bigloo filename in lang))))

   (define (language src)
      (if (eq? (hopc-source-language) 'auto)
	  (let ((lang (string->symbol (suffix src))))
	     (if (eq? lang '||)
		 (error "hopc" "bad language" src)
		 (case lang
		    ((hop) 'hop)
		    ((scm) 'scheme)
		    ((js mjs) 'hopscript)
		    ((json) (if (string-suffix? ".ast.json" src) 'ast.json 'json))
		    (else lang))))
	  (hopc-source-language)))

   (cond
      ((eq? (hopc-pass) 'client-js)
       (for-each compile-javascript (hopc-sources))
       0)
      ((and (hopc-source-ast) (pair? (hopc-sources)))
       (compile (car (hopc-sources))
	  (string->obj (string-as-read (hopc-source-ast)))
	  (language (car (hopc-sources)))))
      ((and (hopc-source-ast-file) (pair? (hopc-sources)))
       (compile (car (hopc-sources))
	  (call-with-input-file (hopc-source-ast-file)
	     (lambda (in)
		(string->obj (read-string in))))
	  (language (car (hopc-sources)))))
      (else
       (let ((srcs (hopc-sources)))
	  (cond
	     ((null? srcs)
	      0)
	     ((null? (cdr srcs))
	      (call-with-input-file (car srcs)
		 (lambda (in)
		    (compile (car srcs)
		       in (language (car srcs))))))
	     (else
	      (let* ((files (cdr srcs))
		     (ip (let ((ip (open-input-file (car srcs))))
			    (if (input-port? ip)
				ip
				(error "hopc" "Cannot open file" (car srcs)))))
		     (in (open-input-procedure
			    (lambda ()
			       (let loop ()
				  (let ((buf (read-chars 8192 ip)))
				     (cond
					((string? buf)
					 buf)
					((eof-object? buf)
					 (close-input-port ip)
					 (if (pair? files)
					     (begin
						(set! ip (open-input-file (car files)))
						(set! files (cdr files))
						(loop))
					     #f))
					(else
					 (error "hopc" "Cannot read from port"
					    ip)))))))))
		 (unwind-protect
		    (compile (car srcs)
		       in (language (cadr srcs)))
		    (close-input-port in)))))))))
				 
;*---------------------------------------------------------------------*/
;*    jsheap ...                                                       */
;*---------------------------------------------------------------------*/
(define (jsheap::int)

   (define (load-module mod path)
      (print "   ;; " mod)
      (display "   ")
      (write (with-input-from-file path read))
      (newline)
      (newline))

   (define abase (dirname (car (hopc-sources))))
   
   (define (include-module module)
      (match-case module
	 (((and (? symbol?) ?sym) (and (? string?) ?path))
	  (load-module sym path))
	 ((? symbol?)
	  (let ((resolved ((bigloo-module-resolver) module '() abase)))
	     (if (pair? resolved)
		 (load-module module (car resolved))
		 (error "hopc" "Cannot find module" module))))
	 (else
	  (error "hopc" "Illegal module clause" module))))
	     
   (define (generate)
      (let ((exp (with-input-from-file (car (hopc-sources)) read)))
	 (match-case exp
	    ((module (and (? symbol?) ?name) . ?clauses)
	     (print "(heap " name)
	     (print "  ;; library modules")
	     (print "  (")
	     (for-each (lambda (c)
			  (match-case c
			     ((import . ?modules)
			      (for-each include-module modules))))
		clauses)
	     (print "  )\n")
	     (print "  ;; heap import")
	     (display "  (import  ")
	     (for-each (match-lambda
			  ((import . ?i)
			   (for-each (lambda (i)
					(display " ")
					(display i))
			      i)))
		clauses)
	     (print ")")
	     (print ")")
	     0)
	    (else
	     (error "hopc" "Illegal heap source file" exp)))))

   (if (string? (hopc-destination))
       (with-output-to-file (hopc-destination) generate)
       (generate)))

;*---------------------------------------------------------------------*/
;*    hopc-plugins-loader ...                                          */
;*---------------------------------------------------------------------*/
(define (hopc-plugins-loader)
   (or nodejs-plugins-loader
       (when (or (hopc-j2s-plugins) (hopc-j2s-preprocessor))
	  (hop-sofile-compile-policy-set! 'none)
	  (hopc-nodejs-load)
	  nodejs-plugins-loader)))

;*---------------------------------------------------------------------*/
;*    hopc-language-loader ...                                         */
;*---------------------------------------------------------------------*/
(define (hopc-language-loader)
   (hopc-nodejs-load
      '((@ hopscript-install-expanders! __hopscript_expanders))
      '((@ nodejs-language-toplevel-loader __nodejs_require))))

;*---------------------------------------------------------------------*/
;*    hopc-nodejs-language-notify-error ...                            */
;*---------------------------------------------------------------------*/
(define (hopc-nodejs-language-notify-error)
   (or (hopc-nodejs-load
	  '(@ nodejs-language-notify-error __nodejs_require))
       (lambda (err proc)
	  (display err (current-error-port)))))

;*---------------------------------------------------------------------*/
;*    nodejs-loaded ...                                                */
;*---------------------------------------------------------------------*/
(define nodejs-loaded #f)
(define nodejs-plugins-loader #f)

;*---------------------------------------------------------------------*/
;*    hopc-nodejs-load ...                                             */
;*---------------------------------------------------------------------*/
(define (hopc-nodejs-load . bindings)
   (nowarning
      (lambda ()
	 (hop-sofile-compile-policy-set! 'none)
	 (unless (or nodejs-loaded (hopc-bootstrap-mode))
	    (set! nodejs-loaded #t)
	    (let ((lpath (if (hopc-hop-lib-dir)
			     (cons (hopc-hop-lib-dir) (hop-library-path))
			     (hop-library-path))))
	       (when (library-load-init 'nodejs lpath)
		  (library-load-init 'hopscript lpath)
		  (apply library-load 'hopscript lpath)
		  (apply library-load 'nodejs lpath)
		  (eval '((@ hopscript-install-expanders! __hopscript_expanders)))
		  (set! nodejs-plugins-loader
		     (eval '((@ nodejs-plugins-toplevel-loader __nodejs_require)))))))
	 (let ((res #f))
	    (for-each (lambda (b) (set! res (eval b))) bindings)
	    res))))
   
;*---------------------------------------------------------------------*/
;*    nowarning ...                                                    */
;*---------------------------------------------------------------------*/
(define (nowarning thunk)
   (let ((oldw (bigloo-warning))
	 (oldd (bigloo-debug)))
      (bigloo-warning-set! 0)
      (bigloo-debug-set! 0)
      (unwind-protect
	 (thunk)
	 (begin
	    (bigloo-warning-set! oldw)
	    (bigloo-debug-set! oldd)))))
	 
;*---------------------------------------------------------------------*/
;*    sobase-path ...                                                  */
;*    -------------------------------------------------------------    */
;*    Compute the file name relative to the sobase directory.          */
;*---------------------------------------------------------------------*/
(define (sobase-path path)
   (if (hopc-sobase)
       (make-file-name (relative-file-name (hopc-sobase) (pwd)) path)
       path))
