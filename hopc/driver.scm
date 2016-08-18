;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopc/driver.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 14 08:13:05 2014                          */
;*    Last change :  Tue Aug 16 09:34:14 2016 (serrano)                */
;*    Copyright   :  2014-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOPC compiler driver                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hopc_driver
   
   (library scheme2js hopscheme js2scheme hop)

   (import  hopc_parseargs
	    hopc_param)

   (eval    (library hop))

   (export  (jsheap::int)
	    (js-driver->string)
	    (hopc-read p)
	    (setup-client-compiler!)
	    (compile-sources::int)))

;*---------------------------------------------------------------------*/
;*    hopc-read ...                                                    */
;*---------------------------------------------------------------------*/
(define (hopc-read p)
   (if (=fx (bigloo-debug) 0)
       (begin
	  (bigloo-debug-set! 1)
	  (let ((v (hop-read p)))
	     (bigloo-debug-set! 0)
	     v))
       (hop-read p)))

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
      :hop-compile (lambda (obj op compile)
		      (hop->javascript obj op compile #f))
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

   (define (input-file->module-name in)
      (let* ((path (input-port-name in))
	     (dir (dirname path))
	     (mod (prefix (basename (input-port-name in)))))
	 (if (string=? dir ".")
	     mod
	     (format "%%~a_~a" (basename dir) mod))))
   
   (define (compile-javascript p)
      (if (string-suffix? ".js" p)
	  (call-with-input-file p
	     (lambda (in) (compile in (language p))))
	  (hopscheme-compile-file p
	     (if (string? (hopc-destination)) (hopc-destination) "-")
	     '())))

   (define (compile-module exp)
      (match-case exp
	 ((module ?id . ?clauses)
	  ;; this produces a side effect
	  (hop-module-extension-handler exp)
	  ;; generate the new module clause
	  (let ((nclauses (filter (lambda (c)
				     (match-case c
					((<TILDE> . ?-) #f)
					(else #t)))
			     clauses)))
	     `(module ,id ,@nclauses)))
	 (else
	  (error "hopc" "Illegal module" exp))))
   
   (define (generate-bigloo::int in lang)
      
      (define (generate-hop out::output-port)
	 (let loop ()
	    (let ((exp (hopc-read in)))
	       (unless (eof-object? exp)
		  (match-case exp
		     ((module . ?-)
		      (pp (compile-module exp) out))
		     (else
		      (pp exp out)))
		  (loop)))))
      
      (define (generate-hopscript out::output-port)
	 (let* ((fname (input-port-name in))
		(mmap (when (and (string? fname) (file-exists? fname))
			 (open-mmap fname :read #t :write #f))))
	    (for-each (lambda (exp) (pp exp out))
	       (j2s-compile in
		  :return-as-exit (hopc-js-return-as-exit)
		  :mmap-src mmap
		  :driver (js-driver)
		  :worker (hopc-js-worker)
		  :module-main (if (boolean? (hopc-js-module-main))
				   (hopc-js-module-main)
				   (not (memq (hopc-pass) '(object so))))
		  :module-name (or (hopc-js-module-name)
				   (input-file->module-name in))
		  :module-path (hopc-js-module-path)
		  :hopscript-header (hopc-js-header)
		  :optim (hopc-optim-level)
		  :debug (bigloo-debug)))))

      (define (generate-js out::output-port)
	 (let* ((fname (input-port-name in))
		(mmap (when (and (string? fname) (file-exists? fname))
			 (open-mmap fname :read #t :write #f))))
	    (for-each (lambda (exp)
			 (unless (isa? exp J2SNode)
			    (display exp out)))
	       (j2s-compile in
		  :return-as-exit (hopc-js-return-as-exit)
		  :mmap-src mmap
		  :driver (js-driver)
		  :worker (hopc-js-worker)
		  :module-main (if (boolean? (hopc-js-module-main))
				   (hopc-js-module-main)
				   (not (memq (hopc-pass) '(object so))))
		  :module-name (or (hopc-js-module-name)
				   (input-file->module-name in))
		  :module-path (hopc-js-module-path)
		  :hopscript-header (hopc-js-header)
		  :optim (hopc-optim-level)
		  :debug (bigloo-debug)))))

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

   (define (compile-bigloo::int in lang)

      (define (srfi-opts)
	 (cond-expand
	    (enable-libuv '("-srfi" "enable-libuv" "-srfi" "hopc"))
	    (else '("-srfi" "hopc"))))
      
      (define (compile in opts comp file)
	 (let* ((baseopts (cons "-fread-internal-src" (append (srfi-opts) opts)))
		(opts (if (string? file)
			  (cons* "-fread-internal-src-file-name" file baseopts)
			  baseopts))
		(proc (apply run-process (hopc-bigloo)
			 input: pipe: "-"
			 opts))
		(cmd (format "~a - ~l" (hopc-bigloo) opts))
		(out (process-input-port proc)))
	    (unwind-protect
	       (comp out)
	       (begin
		  (hop-verb 1 cmd "\n")
		  (close-output-port out)))
	    (process-wait proc)
	    (process-exit-status proc)))
      
      (define (compile-hop in opts file)
	 (compile in
	    (append `("-library" "hop"
		      "-library" "hopscheme"
		      "-library" "hopwidget"
		      "-rpath" ,(make-file-path (hop-lib-directory) "hop" (hop-version)))
	       opts)
	    (lambda (out)
	       (let loop ()
		  (let ((exp (hopc-read in)))
		     (unless (eof-object? exp)
			(match-case exp
			   ((module . ?-)
			    (write (obj->string (compile-module exp)) out))
			   (else
			    (write (obj->string exp) out)))
			(loop)))))
	    file))
      
      (define (compile-hopscript in opts file exec)
	 (let* ((fname (input-port-name in))
		(mmap (when (and (string? fname) (file-exists? fname))
			 (open-mmap fname :read #t :write #f))))
	    (compile in
	       (append (hopc-js-libraries)
		  `("-rpath" ,(make-file-path (hop-lib-directory)
				 "hop" (hop-version)))
		  opts)
	       (lambda (out)
		  ;; compile
		  (map (lambda (e) (write (obj->string e) out))
		     (j2s-compile in
			:return-as-exit (hopc-js-return-as-exit)
			:mmap-src mmap
			:driver (js-driver)
			:worker (hopc-js-worker)
			:module-main (if (boolean? (hopc-js-module-main))
					 (hopc-js-module-main)
					 (not (memq (hopc-pass) '(object so))))
			:module-name (or (hopc-js-module-name)
					 (input-file->module-name in))
			:module-path (hopc-js-module-path)
			:hopscript-header (hopc-js-header)
			:optim (hopc-optim-level)
			:debug (bigloo-debug))))
	       file)))
      
      (let* ((opts (hopc-bigloo-options))
	     (file (when (and (pair? (hopc-sources))
			      (string? (car (hopc-sources))))
		      (car (hopc-sources))))
	     (opts (cond
		      ((string? (hopc-destination))
		       (when (file-exists? (hopc-destination))
			  (delete-file (hopc-destination)))
		       (case (hopc-pass)
			  ((object)
			   (cons* "-c" "-o" (hopc-destination) opts))
			  ((so)
			   (cons* "-dload-sym" "-y" "-o" (hopc-destination) opts))
			  (else
			   (cons* "-o" (hopc-destination) opts))))
		      ((and (pair? (hopc-sources))
			    (string? (car (hopc-sources))))
		       (case (hopc-pass)
			  ((object)
			   (let* ((base (prefix (car (hopc-sources))))
				  (dest (string-append base ".o")))
			      (cons* "-c" "-o" dest opts)))
			  ((so)
			   (let* ((base (prefix (car (hopc-sources))))
				  (dest (string-append base "."
					   (bigloo-config 'shared-lib-suffix))))
			      (cons* "-y" "-o" dest opts)))
			  (else
			   (cons* "-o" "a.out" opts))))
		      (else
		       (cons "--to-stdout" opts))))
	     (exec (not (eq? (hopc-pass) 'object))))
	 (case lang
	    ((hop) (compile-hop in opts file))
	    ((hopscript) (compile-hopscript in opts file exec)))))
   
   (define (compile::int in lang::symbol)
      (cond
	 ((eq? (hopc-pass) 'client-js)
	  (generate-bigloo in 'js))
	 ((eq? (hopc-pass) 'bigloo)
	  (generate-bigloo in lang))
	 (else
	  (compile-bigloo in lang))))

   (define (language src)
      (if (eq? (hopc-source-language) 'auto)
	  (case (string->symbol (suffix src))
	     ((hop) 'hop)
	     ((js) 'hopscript)
	     (else (error "hopc" "Unknown language source" src)))
	  (hopc-source-language)))
   
   (cond
      ((eq? (hopc-pass) 'client-js)
       (for-each compile-javascript (hopc-sources))
       0)
      ((pair? (hopc-sources))
       (let loop ((srcs (hopc-sources)))
	  (if (null? srcs)
	      0
	      (let ((r (call-with-input-file (car srcs)
			  (lambda (in) (compile in (language (car srcs)))))))
		 (call-with-output-file "/tmp/ERR"
		    (lambda (p)
		       (display "COMPILE r=" p)
		       (display r p)
		       (newline p)))
		 (if (=fx r 0)
		     (loop (cdr srcs))
		     r)))))
      (else
       (compile (current-input-port)
	  (if (eq? (hopc-source-language) 'auto)
	      'hop
	      (hopc-source-language))))))
				 
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
   

