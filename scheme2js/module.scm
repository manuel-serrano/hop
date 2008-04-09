(module module-system
   (option (loadq "protobject-eval.sch"))
   (import verbose
	   infotron
	   config)
   (export (create-module-from-file filename module-headers reader)
	   (create-module-from-expr expr module-headers)
	   (class Module
	      name              ;; #f if no module-clause
	      top-level         ;; id or pair-nil
	      macros::pair-nil  ;; of form (define-macro ... )
	      imports::pair-nil
	      exports::pair-nil)))

;; reads an evaluates the module/infotron clauses in the beginning of files.
;; In addition to the (optionel) module-header found in files one can provide
;; additional headers that should either replace or merge with the existing
;; one. It is also possible to provide headers that are only "applied" if the
;; file does not have any.
;; headers are of the form:
;;  '((header . kind) ...)
;; header is a std module-header (the "module name" part can be omitted)
;; kind is one of:
;; - replace: replace original header. can only be used once
;; - provide: use this header if no original header was there. (replace will
;; replace this header as well!)
;; - merge-first: but module-clauses before original header (or the
;; replaced/provided one).
;; - merge-last: but module-clauses after the original header (or ...).
;;
;; Note: additional headers can (currently) only be used through the
;; library. The main-executable has no means to add them (as of 2007/11/02).


(define (read-includes files reader)
   (append-map!
    (lambda (f)
       (with-input-from-file f
	  (lambda ()
	     (let loop ((rev-source '()))
		(let ((sexp (reader (current-input-port) #t)))
		   (if (eof-object? sexp)
		       (reverse! rev-source)
		       (loop (cons sexp rev-source))))))))
    files))

(define (merge-headers file-header other-headers)
   (let ((replace-headers (filter-map (lambda (p)
					 (and (eq? (cdr p) 'replace)
					      (car p)))
				      other-headers))
	 (provide-headers (filter-map (lambda (p)
					 (and (eq? (cdr p) 'provide)
					      (car p)))
				      other-headers))
	 (merge-first-headers (filter-map (lambda (p)
					     (and (eq? (cdr p) 'merge-first)
						  (car p)))
					  other-headers))
	 (merge-last-headers (filter-map (lambda (p)
					    (and (eq? (cdr p) 'merge-last)
						 (car p)))
					 other-headers))
	 (module/infotron (if file-header
			      (car file-header)
			      'module)))
      (if (> (length replace-headers) 1)
	  (error "module-system"
		 "only one replace-module-header allowed"
		 (length replace-headers)))
      (if (and (not (null? replace-headers))
	       (not (null? provide-headers)))
	  (warning "replace-header is always shadowing provide-headers"))
      `(,module/infotron ,(if file-header
			      (cadr file-header)
			      #f)
	  ,@(apply append merge-first-headers)
	  ,@(cond
	       ((not (null? replace-headers))
		(car replace-headers))
	       (file-header
		(cddr file-header))
	       (else
		(apply append provide-headers)))
	  ,@(apply append merge-last-headers))))
      
(define (create-module-from-expr expr module-headers)
   (let ((header (merge-headers #f module-headers))
	 (include-paths (cons "." (config 'include-paths))))
      (receive (name macros imports exports includes)
	 (read-module-header header #f include-paths read)
	 (let ((included (read-includes includes read)))
	    (instantiate::Module
	       (name #f)
	       (top-level (append included (list expr)))
	       (macros macros)
	       (imports imports)
	       (exports exports))))))

;; reads in-file. if first clause is a 'module-clause' then the imports,
;; exports... are established.
(define (create-module-from-file file module-headers reader)
   (define (read-file-exprs in-port first-expr)
      (let loop ((rev-top-level (if (and first-expr
					 (not (eof-object? first-expr)))
				    (list first-expr)
				    '())))
	 (let ((sexp (reader in-port #t)))
	    (if (eof-object? sexp)
		(reverse! rev-top-level)
		(loop (cons sexp rev-top-level))))))

   (verbose "reading: " (if (string=? file "-") "std-in" file))
   (let ((in-port (if (string=? file "-")
		      (current-input-port)
		      (open-input-file file)))
	 (include-paths (if (string=? "-" file)
			    (cons "." (config 'include-paths))
			    (cons (dirname file)
				  (config 'include-paths)))))
      (if (not in-port)
	  (error "read-file" "couldn't open: " file))
      (let* ((sexp (reader in-port #t))
	     (header-sexp? (and (not (eof-object? sexp))
				(pair? sexp)
				(or (eq? (car sexp) 'module)
				    (and (config 'infotron)
					 (eq? (car sexp) 'infotron)))))
	     (infotron? (and header-sexp? (eq? (car sexp) 'infotron)))
	     (header (merge-headers (and header-sexp? sexp)
				    module-headers))
	     (top-level (read-file-exprs in-port (if header-sexp?
						     #f
						     sexp))))
	 (if (not (string=? file "-")) (close-input-port in-port))

	 (receive (name macros imports exports includes)
	    (read-module-header header file include-paths reader)
	    (let ((module (instantiate::Module
			     (name name)
			     (top-level (append (read-includes includes reader)
						top-level))
			     (macros macros)
			     (imports imports)
			     (exports exports))))
	       (if infotron?
		   (module->infotron! header module)
		   module))))))

(define *module-extensions* '("scm" "sch"))


(define mutex (make-mutex "scm2js-mutex"))

(define cache '())
(define cache-len 0)
(define max-cache-size 10)

(define (take! lst size)
   (let loop ((i 0)
	      (l lst))
      (cond
	 ((null? l)
	  lst)
	 ((=fx i size)
	  (set-cdr! l '())
	  lst)
	 (else
	  (loop (+fx i 1) (cdr l))))))

(define (parse-module-header clause filename)
   (define (extract-entries a-list type)
      (append-map cdr
		  (filter (lambda (entry)
			     (eq? type (car entry)))
			  a-list)))
   (when (not (pair? (cdr clause)))
      (error "module" "Missing module-name:"  filename))
   (let* ((module-name (cadr clause))
	  (a-list (filter pair? clause))
	  (macros (extract-entries a-list 'export-macros))
	  (imported-modules (extract-entries a-list 'import))
	  (JS-imports (extract-entries a-list 'JS))
	  (exports (extract-entries a-list 'export))
	  (includes (extract-entries a-list 'include)))
      (when (and module-name ;; we allow #f
		 (not (symbol? module-name)))
	 (error "module" "Bad module/infotron-name" module-name))
      (when (and (eq? (car clause) 'module) ;; exclude infotrons from test
		 filename
		 module-name
		 (not (string=? filename "-"))
		 (or (not (symbol? module-name))
		     (not (eq? (string->symbol (prefix (basename filename)))
			       module-name))))
	 (error "module"
		"Module-name and filename are not equal."
		(prefix (basename filename))))
      (values module-name macros JS-imports
	      imported-modules exports includes)))

(define (get-module-header file reader)
   (with-lock mutex
      (lambda ()
	 (let ((k (cons file reader)))
	    (let ((old (assoc k cache)))
	       (if (pair? old)
		   (cdr old)
		   (let ((clause (with-input-from-file file
				    (lambda ()
				       (reader (current-input-port) #t)))))
		      (match-case clause
			 ((module ?- . ?-)
			  (receive (module-name exported-macros JS-imports
						imported-modules exports
						includes)
			     (parse-module-header clause file)
			     (let ((v (vector module-name exported-macros
					      JS-imports
					      imported-modules exports
					      includes)))
				(if (>=fx cache-len max-cache-size)
				    (set! cache
					  (cons (cons k v)
						(take! cache max-cache-size)))
				    (begin
				       (set! cache-len (+fx cache-len 1))
				       (set! cache (cons (cons k v) cache))))
				v)))
			 (else
			  (error "module-imports"
				 "couldn't read module clause:"
				 file))))))))))

   
(define (read-module-header module-clause filename include-paths reader)
   (receive (module-name exported-macros JS-imports
			 imported-modules exports includes)
      (parse-module-header module-clause filename)
      (let loop ((imported-modules imported-modules)
		 (macros '())
		 (imports '()))
	 (if (null? imported-modules)
	     (values module-name
		     (append macros exported-macros)
		     (append imports JS-imports)
		     exports includes)
	     (let* ((imported-module (car imported-modules))
		    (module-str (symbol->string imported-module))
		    (module-filenames (map (lambda (extension)
					      (string-append module-str
							     "."
							     extension))
					   *module-extensions*))
		    (module-file (any (lambda (file)
					 (find-file/path file include-paths))
				      module-filenames)))
		(if (not module-file)
		    (error "module-imports" "can't find module" imported-module)
		    (let* ((vec (get-module-header module-file reader))
			   (module-macros (vector-ref vec 1))
			   (module-exports (vector-ref vec 4)))
		       (loop (cdr imported-modules)
			     (append macros module-macros)
			     (append imports module-exports)))))))))
