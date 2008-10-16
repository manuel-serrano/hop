(module module-system
   (import verbose
	   export
	   infotron
	   config)
   (export (final-class Compilation-Unit
	      name              ;; #f if no module-clause
	      top-level         ;; id or pair-nil
	      macros::pair-nil  ;; of form (define-macro ... )
	      imports::pair-nil
	      exports::pair-nil)
	   (wide-class WIP-Unit::Compilation-Unit ;; work in progress
	      header)
	   (create-module-from-file file::bstring override-headers::pair-nil
				    reader::procedure)
	   (create-module-from-expr expr override-headers::pair-nil)))

(define *module-extensions* '("scm" "sch"))

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

(define (create-module-from-file file override-headers reader)
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
		      (open-input-file file))))
      (if (not in-port)
	  (error "read-file" "couldn't open: " file))
      (let* ((sexp (reader in-port #t))
	     (header-sexp? (and (not (eof-object? sexp))
				(pair? sexp)
				(eq? (car sexp) 'module)))
	     (header (and header-sexp? sexp))
	     (top-level (read-file-exprs in-port (if header-sexp?
						     #f
						     sexp)))
	     (file-path (if (string=? "-" file) "." (dirname file)))
	     (m (instantiate::Compilation-Unit
		   (name #f)
		   (top-level top-level)
		   (macros '())
		   (imports '())
		   (exports '()))))
	 (widen!::WIP-Unit m
	    (header header))

	 (if (not (string=? file "-")) (close-input-port in-port))

	 (prepare-module! m override-headers file-path reader)

	 (let ((module-name (WIP-Unit-name m)))
	    (when (and (not (config 'infotron)) ;; exclude infotrons from test
		       file
		       module-name
		       (not (string=? file "-"))
		       (or (not (symbol? module-name))
			   (not (eq? (string->symbol (prefix (basename file)))
				     module-name))))
	       (error "module"
		      "Module-name and filename are not equal."
		      (prefix (basename file)))))

	 (shrink! m)
	 m)))

(define (create-module-from-expr expr override-headers)
   (let ((m (instantiate::Compilation-Unit
	       (name #f)
	       (top-level (list expr))
	       (macros '())
	       (imports '())
	       (exports '())))
	 (file-path ".")) ;; filepath is assumed to be "."
      (widen!::WIP-Unit m
	 (header #f))

      (prepare-module! m override-headers file-path read)

      (shrink! m)
      m))

(define (extract-entries header type)
   (append-map cdr
	       (filter (lambda (entry)
			  (and (pair? entry)
			       (eq? type (car entry))))
		       header)))

(define (prepare-module! m::WIP-Unit override-headers::pair-nil
			 file-path::bstring reader::procedure)
   (let ((include-paths (cons file-path (config 'include-paths)))
	 (module-preprocessor (config 'module-preprocessor))
	 (module-postprocessor (config 'module-postprocessor))
	 (bigloo-modules? (config 'bigloo-modules)))
      (when module-preprocessor
	 (module-preprocessor m))
      (merge-headers! m override-headers)
      (set-name! m)
      (read-includes! m include-paths reader)
      (read-imports! m include-paths reader bigloo-modules?) ;; macros too
      (normalize-JS-imports! m)
      (normalize-exports! m bigloo-modules?)
      (when (config 'infotron)
	 (module->infotron! m))
      (when module-postprocessor
	 (module-postprocessor m))))
   
(define (merge-headers! m::WIP-Unit override-headers)
   (with-access::WIP-Unit m (header)
      (cond
	 ((and (null? override-headers)
	       (not header))
	  (set! header '()))
	 ((null? override-headers)
	  'do-nothing)
	 (else
	  (let ((replaces (filter-map (lambda (p)
					 (and (eq? (cdr p) 'replace)
					      (car p)))
				      override-headers))
		(provides (filter-map (lambda (p)
					 (and (eq? (cdr p) 'provide)
					      (car p)))
				      override-headers))
		(merge-firsts (filter-map (lambda (p)
					     (and (eq? (cdr p) 'merge-first)
						  (car p)))
					  override-headers))
		(merge-lasts (filter-map (lambda (p)
					    (and (eq? (cdr p) 'merge-last)
						 (car p)))
					 override-headers)))
	     (if (> (length replaces) 1)
		 (error "module-system"
			"only one replace-module-header allowed"
			(length replaces)))
	     (if (and (not (null? replaces))
		      (not (null? provides)))
		 (warning "replace-header is always shadowing provide-header"))
	     (set! header `(module ,(if header
					(cadr header)
					#f)
			      ,@(apply append merge-firsts)
			      ,@(cond
				   ((not (null? replaces))
				    (car replaces))
				   (header
				    (cddr header))
				   (else
				    (apply append provides)))
			      ,@(apply append merge-lasts))))))))

(define (set-name! m::WIP-Unit)
   (with-access::WIP-Unit m (header name)
      (cond
	 ((null? header)
	  (set! name #f))
	 ((not (and (pair? header)
		    (pair? (cdr header))
		    (or (symbol? (cadr header))
			(not (cadr header))))) ;; we allow #f
	  (error "module-system"
		 "bad module-clause. could not get name"
		 header))
	 (else
	  (set! name (cadr header))))))

(define (read-includes! m::WIP-Unit include-paths reader)
   (define (read-file f)
      (let ((file (find-file/path f include-paths)))
	 (unless file
	    (error "scheme2js module"
		   "can't find include-file"
		   f))
	 (with-input-from-file file
	    (lambda ()
	       (let loop ((rev-source '()))
		  (let ((sexp (reader (current-input-port) #t)))
		     (if (eof-object? sexp)
			 (reverse! rev-source)
			 (loop (cons sexp rev-source)))))))))
      
   (with-access::WIP-Unit m (header top-level)
      (let* ((include-files (extract-entries header 'include)))
	 (set! top-level
	       (append! (append-map! read-file include-files)
			top-level)))))

(define (read-imports! m::WIP-Unit include-paths reader bigloo-modules?)
   (with-access::WIP-Unit m (header imports macros)
      (let loop ((imported-modules (extract-entries header 'import))
		 (new-macros macros)
		 (new-imports imports))
	 (if (null? imported-modules)
	     (begin
		(set! macros new-macros)
		(set! imports new-imports))
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
		    (error "scheme2js module"
			   "can't find imported module"
			   imported-module)
		    (let ((ip (open-input-file module-file)))
		       (unwind-protect
			  (let ((module-clause (reader ip #t)))
			     (unless (and
				      (pair? module-clause)
				      (eq? (car module-clause) 'module)
				      (pair? (cdr module-clause))
				      (or (not (cadr module-clause)) ;; we allow #f
					  (and (symbol? (cadr module-clause))
					       (eq? (cadr module-clause)
						    imported-module))))
				(error "module"
				       "Module-name and filename are not equal"
				       module-file))
			     (let ((im (instantiate::Compilation-Unit ;; import-module
					  (name imported-module)
					  (top-level #f)
					  (imports '())
					  (exports '())
					  (macros '()))))
				(widen!::WIP-Unit im (header module-clause))
				;; normalize-exports might need the 'ip' in
				;; case it needs to search for macros.
				(normalize-exports! im bigloo-modules?
						    #t reader ip) ;; get macros
				(loop (cdr imported-modules)
				      (append new-macros
					      (Compilation-Unit-macros im))
				      (append new-imports
					      (Compilation-Unit-exports im)))))
			  (close-input-port ip)))))))))

(define (normalize-JS-imports! m)
   (with-access::WIP-Unit m (header imports)
      (set! imports (append imports (extract-entries header 'JS)))))

(define (normalize-exports! m bigloo-modules?
			    #!optional get-macros? reader input-p)
   (if bigloo-modules?
       (normalize-bigloo-exports! m get-macros? reader input-p)
       (normalize-scheme2js-exports! m)))

;; the input-port is only used when macros are exported and the module has not
;; yet read its top-level.
(define (normalize-bigloo-exports! m get-macros? reader input-p)
   (define (untype v::symbol)
      (let* ((str (symbol->string v))
	     (pos (string-contains str "::")))
	 (if pos
	     (values (string->symbol (substring str 0 pos))
		     (string->symbol (substring str (+fx pos 2)
						(string-length str))))
	     (values v #f))))
	  
   (define (normalize-var v pragmas)
      (receive (v type)
	 (untype v)
	 (let ((pragma-info (assq v pragmas)))
	    (or pragma-info v))))

   (define (normalize-fun f pragmas)
      (receive (v type)
	 (untype (car f)) ;; the fun-name
	 (let ((pragma-info (assq v pragmas)))
	    (cond
	       ((and (not pragma-info)
		     type)
		(list v `(type ,type) '(constant? #t)))
	       ((not pragma-info)
		(list v '(constant? #t)))
	       (else
		(let ((pragma-type (assq 'type (cdr pragma-info)))
		      (pragma-constant? (assq 'constant?
					      (cdr pragma-info))))
		   (cond
		      ((and pragma-type type
			    (not (eq? type (cadr pragma-type))))
		       (error "scheme2js-module"
			      "variable exported with two different types"
			      f))
		      ((and pragma-constant?
			    (not (cadr pragma-constant?)))
		       (error "scheme2js-module"
			      "exported functions must be constant"
			      f))
		      ((or (and pragma-type pragma-constant?)
			   (and pragma-constant? (not type)))
		       pragma-info)
		      (else
		       (let* ((without-name (cdr pragma-info))
			      (with-type (if pragma-type
					     without-name
					     (cons `(type ,type)
						   without-name)))
			      (with-constant? (if pragma-constant?
						  with-type
						  (cons '(constant? #t)
							with-type))))
			  (cons v with-constant?))))))))))

   (define (find-macros new-macros)
      (let loop ((new-macros new-macros)
		 (rev-res '()))
	 (if (null? new-macros)
	     (reverse! rev-res)
	     (let ((e (reader input-p #t)))
		(cond
		   ((eof-object? e)
		    (error "scheme2js-module"
			   "could not find macro(s):"
			   new-macros))
		   ((and (pair? e)
			 (eq? (car e) 'define-macro)
			 (pair? (cdr e))
			 (pair? (cadr e))
			 (memq (car (cadr e)) new-macros))
		    (loop (filter (lambda (macro)
				     (not (eq? macro (car (cadr e)))))
				  new-macros)
			  (cons e rev-res)))
		   (else
		    (loop new-macros rev-res)))))))
			  
   (with-access::WIP-Unit m (header exports macros top-level)
      (let ((new-exports (extract-entries header 'export))
	    (pragmas (extract-entries header 'scheme2js-pragma)))
	 (let loop ((entries new-exports)
		    (new-macros '()))
	    (if (null? entries)
		(set! macros (append (find-macros new-macros) macros))
		(let ((e (car entries)))
		   (cond
		      ((symbol? e)
		       (set! exports
			     (cons (create-Export (normalize-var e pragmas) #f)
				   exports))
		       (loop (cdr entries) new-macros))
		      ((not (pair? e))
		       (loop (cdr entries) new-macros))
		      ((eq? (car e) 'macro)
		       (if get-macros?
			   (loop (cdr entries)
				 (append (cdr e) new-macros))
			   (loop (cdr entries) new-macros)))
		      (else
		       (set! exports
			     (cons (create-Export (normalize-fun e pragmas) #f)
				   exports))
		       (loop (cdr entries) new-macros)))))))))

(define (normalize-scheme2js-exports! m::WIP-Unit)
   (with-access::WIP-Unit m (header exports macros)
      (set! exports (append exports (map (lambda (ex)
					    (create-Export ex #f))
					 (extract-entries header 'export))))
      (set! macros (append macros (extract-entries header 'export-macros)))))
