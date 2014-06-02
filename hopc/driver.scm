;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/hopc/driver.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 14 08:13:05 2014                          */
;*    Last change :  Mon May 26 08:46:25 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
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
;*    compile-sources ...                                              */
;*---------------------------------------------------------------------*/
(define (compile-sources::int)

   (define (input-file->module-name in)
      (let* ((path (input-port-name in))
	     (dir (dirname path))
	     (mod (prefix (basename (input-port-name in)))))
	 (if (string=? dir ".")
	     mod
	     (format "__~a_~a" (basename dir) mod))))
   
   (define (compile-javascript p)
      (hopscheme-compile-file p
	 (if (string? (hopc-destination)) (hopc-destination) "-")
	 '()))

   (define (j2s-driver)
      (cond
	 ((string? (hopc-js-driver))
	  (j2s-make-driver (string-split (hopc-js-driver) ",")))
	 ((>=fx (hopc-optim-level) 1)
	  (j2s-optim-driver))
	 (else
	  (j2s-plain-driver))))
   
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
	 (for-each (lambda (exp) (pp exp out))
	    (j2s-compile in
	       :driver (j2s-driver)
	       :worker (hopc-js-worker)
	       :module-main (if (boolean? (hopc-js-module-main))
				(hopc-js-module-main)
				(not (eq? (hopc-pass) 'object)))
	       :module-name (or (hopc-js-module-name)
				(input-file->module-name in))
	       :module-path (hopc-js-module-path)
	       :hopscript-header (hopc-js-header)
	       :nodejs-header (hopc-js-header))))

      (define (generate out::output-port lang::symbol)
	 (case lang
	    ((hop) (generate-hop out))
	    ((hopscript) (generate-hopscript out))))
      
      (if (string? (hopc-destination))
	  (call-with-output-file (hopc-destination)
	     (lambda (out) (generate out lang)))
	  (generate (current-output-port) lang))
      0)

   (define (compile-bigloo::int in lang)

      (define (compile in opts comp file)
	 (let* ((opts (if (string? file)
			  (cons* "-fread-internal-src"
			     "-fread-internal-src-file-name" file
			     "-srfi" "hopc" opts)
			  (cons* "-fread-internal-src" "-srfi" "hopc" opts)))
		(proc (apply run-process (hopc-bigloo)
			 input: pipe: "-" 
			 opts))
		(cmd (format "~a - ~l" (hopc-bigloo) opts))
		(out (process-input-port proc)))
	    (hop-verb 1 cmd "\n")
	    (unwind-protect
	       (comp out)
	       (close-output-port out))
	    (process-wait proc)
	    (process-exit-status proc)))
      
      (define (compile-hop in opts file)
	 (compile in
	    (append `("-library" "hop"
		      "-library" "hopscheme"
		      "-library" "hopwidget"
		      "-rpath" ,(hop-lib-directory))
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
	 (compile in
	    (append `("-library" "hopscript"
			"-library" "nodejs"
			"-library" "web"
			"-rpath" ,(hop-lib-directory))
	       opts)
	    (lambda (out)
	       ;; compile
	       (map (lambda (e) (write (obj->string e) out))
		  (j2s-compile in
		     :driver (j2s-driver)
		     :worker (hopc-js-worker)
		     :module-main (if (boolean? (hopc-js-module-main))
				      (hopc-js-module-main)
				      (not (eq? (hopc-pass) 'object)))
		     :module-name (or (hopc-js-module-name)
				      (input-file->module-name in))
		     :module-path (hopc-js-module-path)
		     :hopscript-header (hopc-js-header)
		     :nodejs-header (hopc-js-header))))
	    file))
      
      (let* ((opts (hopc-bigloo-options))
	     (file (when (and (pair? (hopc-sources))
			      (string? (car (hopc-sources))))
		      (car (hopc-sources))))
	     (opts (cond
		      ((string? (hopc-destination))
		       (when (file-exists? (hopc-destination))
			  (delete-file (hopc-destination)))
		       (if (eq? (hopc-pass) 'object)
			   (cons* "-c" "-o" (hopc-destination) opts)
			   (cons* "-o" (hopc-destination) opts)))
		      ((and (pair? (hopc-sources))
			    (string? (car (hopc-sources))))
		       (if (eq? (hopc-pass) 'object)
			   (let* ((base (prefix (car (hopc-sources))))
				  (dest (string-append base ".o")))
			      (cons* "-c" "-o" dest opts))
			   (cons* "-o" "a.out" opts)))
		      (else
		       (cons "--to-stdout" opts))))
	     (exec (not (eq? (hopc-pass) 'object))))
	 (case lang
	    ((hop) (compile-hop in opts file))
	    ((hopscript) (compile-hopscript in opts file exec)))))
   
   (define (compile::int in lang::symbol)
      (if (eq? (hopc-pass) 'bigloo)
	  (generate-bigloo in lang)
	  (compile-bigloo in lang)))

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
   
   (define (include-module module)
      (match-case module
	 (((and (? symbol?) ?sym) (and (? string?) ?path))
	  (load-module sym path))
	 ((? symbol?)
	  (load-module module (car ((bigloo-module-resolver) module '() (module-abase)))))
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
   

