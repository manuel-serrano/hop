(module module-system
   (import verbose
	   export-desc
	   infotron
	   config)
   (export (final-class Compilation-Unit
	      name              ;; #f if user has not given module-clause.
	      top-level         ;; id or pair-nil
	      macros::pair-nil  ;; of form (define-macro ... )
	      imports::pair-nil ;; a list of export-lists/hashtables.
	      exports)          ;; list or Export-Table (see export.scm)
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
;;  '((kind header) ...)
;;  '((module-kind module) ...)
;; The kind clauses work on individual headers (such as '(import m)) whereas
;; the module-kind clauses work on complete modules (such
;;  as '(module foo (import ...))
;; The modules for the module-kind may give name==#f - as
;; in '(module #f (import ...)) - in which case the module-name will not be set
;; (and the module is treated as if no module-clause had been given). This is
;; only interesting when configs are set to 'module (for instance
;; 'export-globals).
;;
;; kind is one of:
;; - module-replace: replace original header. can only be used once
;; - module-provide: use this header if no original header was there. (replace will
;; replace this header as well!)
;; - merge-first: put module-clause before original clause (or the
;; replaced/provided one).
;; - merge-last: put module-clause after the original header (or ...).
;;
;; Note: additional headers can (currently) only be used through the
;; library. The main-executable has no means to add them (as of 2008/10/22).
;; Note2: This module deals with user-code. -> we need to do many checks.

;; does _not_ verify if it is a well-formed module-clause.
(define (module-clause? e)
   (and (pair? e)
	(eq? (car e) 'module)))

;; checks that (user-supplied) module-clause is well formed.
(define (check-module-clause clause)
   (unless (and (list? clause)
		(pair? clause)
		(eq? (car clause) 'module)
		(pair? (cdr clause))
		(symbol? (cadr clause))
		(every (lambda (sub-term)
			  (and (pair? sub-term)
			       (list? sub-term)))
		       (cddr clause)))
      (error "scheme2js-module"
	     "bad module-clause"
	     clause)))

;; verify that module-header and file-name are the same.
(define (check-module-name module-name file-name)
   (when (or (not (symbol? module-name))
	     (and (not (string=? file-name "-"))
		  (not (eq? (string->symbol (prefix (basename file-name)))
			    module-name))))
      (error "module"
	     "Module-name and filename are not equal."
	     (prefix (basename file-name))))
   (when (or (eq? module-name '*)
	     (eq? module-name '_))
      (error "module"
	     "Invalid module name"
	     module-name)))
   
(define (create-module-from-file file override-headers reader)
   (define (read-file-exprs in-port first-expr use-first-expr?)
      (let loop ((rev-top-level (if (and use-first-expr?
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
	     (header-sexp? (module-clause? sexp))
	     (header (and header-sexp? sexp))
	     (top-level (read-file-exprs in-port
					 sexp
					 (not header-sexp?)))
	     (file-path (if (string=? "-" file) "." (dirname file)))
	     (m (instantiate::Compilation-Unit
		   (name #f)
		   (top-level top-level)
		   (macros '())
		   (imports '())
		   (exports '()))))
	 (when header-sexp? (check-module-clause header))
	 (widen!::WIP-Unit m
	    (header header))

	 (unless (string=? file "-") (close-input-port in-port))

	 (prepare-module! m override-headers file-path reader)

	 (let ((module-name (WIP-Unit-name m)))
	    (unless (or (config 'infotron)
			(not header-sexp?))
	       (check-module-name module-name file)))

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

;; really inefficient, but the module-headers should not be too big.
(define (extract-entries header type)
   (append-map cdr
	       (filter (lambda (entry)
			  (and (pair? entry)
			       (eq? type (car entry))))
		       header)))

;; precondition: the WIP-Unit's header is either #f or well formed.
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

(define (check-override-headers o-headers)
   (define valid-kinds '(module-replace module-provide merge-first merge-last))

   (unless (list? o-headers)
      (error "scheme2js-module"
	     "Override headers must be a list"
	     o-headers))

   (let loop ((o-headers o-headers)
	      (already-a-replace? #f)
	      (found-provide? #f))
      (if (null? o-headers)
	  (when (and already-a-replace?
		     found-provide?)
	     (warning "replace-header is always shadowing provide-header"))
	  (let ((override (car o-headers)))
	     (cond
		((not (and (pair? override)
			   (pair? (cdr override))
			   (null? (cddr override))))
		 (error "scheme2js-module"
			"invalid override-header - not a list of 2 elements"
			(car o-headers)))
		((not (and (symbol? (car override))
			   (memq (car override) valid-kinds)))
		 (error "scheme2js-module"
			(string-append "car of override-header must be one of "
				       "module-replace, module-provide, "
				       "merge-first, merge-last")
			override))
		((and (or (eq? (car override) 'module-replace)
			  (eq? (car override) 'module-provide))
		      (not (match-case override
			      ((module (? (lambda (n)
					     (or (eq? n #f)
						 (symbol? n))))
				  (? list?))
			       #t)
			      (else #f))))
		 (error "scheme2js-module"
			"invalid override-header"
			override))
		((and (eq? (car override) 'module-replace)
		      already-a-replace?)
		 (error "scheme2js-module"
			"only one replace override header allowed"
			override))
		((and (eq? (car override) 'module-provide)
		      found-provide?)
		 (error "scheme2js-module"
			"only one provide override header allowed"
			override))
		((eq? (car override) 'module-replace)
		 (loop (cdr o-headers) #t found-provide?))
		((eq? (car override) 'module-provide)
		 (loop (cdr o-headers) already-a-replace? #t))
		(else (loop (cdr o-headers)
			    already-a-replace?
			    found-provide?)))))))

;; precondition: the WIP-Unit's header is either #f or well formed.
;;
;; CARE: inefficient, but the override-headers should not be too big.
(define (merge-headers! m::WIP-Unit override-headers)
   (define (select-name header replace provide)
      (cond
	 (replace (cadr replace))
	 (header (cadr header))
	 (provide (cadr provide))
	 (else #f)))
	  
   (with-access::WIP-Unit m (header)
      (cond
	 ((and (null? override-headers)
	       (not header))
	  (set! header '()))
	 ((null? override-headers)
	  'do-nothing)
	 (else
	  (check-override-headers override-headers)
	  (let* ((replace (let ((t (assq 'module-replace override-headers)))
			     (and t (cadr t))))
		 (provide (let ((t (assq 'module-provide override-headers)))
			      (and t (cadr t))))
		 (merge-firsts (filter-map (lambda (p)
					      (and (eq? (car p) 'merge-first)
						   (cadr p)))
					   override-headers))
		 (merge-lasts (filter-map (lambda (p)
					     (and (eq? (car p) 'merge-last)
						  (cadr p)))
					  override-headers))
		 ;; replace and provide are complete module-headers (they
		 ;; must start with (module ...)
		 (new-name (select-name header replace provide)))
	     (set! header `(module ,new-name
			      ,@merge-firsts
			      ,@(cond
				   (replace (cddr replace))
				   (header (cddr header))
				   (provide (cddr header))
				   (else '()))
			      ,@merge-lasts)))))))

(define (set-name! m::WIP-Unit)
   (with-access::WIP-Unit m (header name)
      (cond
	 ((null? header)
	  (set! name #f))
	 (else
	  ;; this might be #f too. But only if it was not supplied by the
	  ;; user. (For instance using replace-overrides.)
	  (set! name (cadr header))))))

(define (read-includes! m::WIP-Unit include-paths reader)
   (define (read-file f)
      (unless (string? f)
	 (error "scheme2js-module"
		"include-parameter must be a string"
		f))
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
   (define (get-import-list header)
      (let ((import-list (extract-entries header 'import)))
	 (unless (every (lambda (im)
			   (or (symbol? im)
			       (Compilation-Unit? im)))
			import-list)
	    (error "scheme2js-module"
		   ;; we allow compilation units too, but this should not
		   ;; appear in error message.
		   "only symbols are allowed in import-list"
		   import-list))
	 import-list))

   (with-access::WIP-Unit m (header imports macros)
      (let loop ((imported-modules (get-import-list header))
		 (new-macros macros)
		 (new-imports imports))
	 (cond
	    ((null? imported-modules)
	     (set! macros new-macros)
	     (set! imports new-imports))
	    ((Compilation-Unit? (car imported-modules))
	     (let ((im (car imported-modules)))
		(loop (cdr imported-modules)
		      (append new-macros
			      (Compilation-Unit-macros im))
		      (if (null? (Compilation-Unit-exports im))
			  new-imports
			  (cons (Compilation-Unit-exports im)
				new-imports)))))
	    (else
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
			     (check-module-clause module-clause)
			     (check-module-name (cadr module-clause) module-file)
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
				      (if (null? (Compilation-Unit-exports im))
					  new-imports
					  (with-access::Compilation-Unit im
						(name exports)
					     ;; qualified exports. that is name
					     ;; followed by exports.
					     (cons (cons name exports)
						   new-imports))))))
			  (close-input-port ip))))))))))

(define (normalize-JS-imports! m)
   (with-access::WIP-Unit m (header imports)
      (let* ((direct-JS-imports (extract-entries header 'JS))
	     (descs (map (lambda (js)
			    (when (not (symbol? js))
			       (error "scheme2js module"
				      "JS clauses can only contain symbols"
				      js))
			    (create-Export-Desc js #f #f))
			 direct-JS-imports)))
	 (unless (null? descs)
	    (set! imports
		  ;; qualified name for JavaScript is '_
		  (cons (cons '_ descs)
			imports))))))

(define (normalize-exports! m bigloo-modules?
			    #!optional get-macros? reader input-p)
   (if bigloo-modules?
       (normalize-bigloo-exports! m get-macros? reader input-p)
       (normalize-scheme2js-exports! m)))

(define (check-pragma pragma)
   (unless (and (list? pragma)
		(pair? pragma)
		(symbol? (car pragma))
		(every? (lambda (p)
			   (list? p)
			   (pair? p)
			   (symbol? (car p)))
			(cdr pragma)))
      (error "scheme2js-module"
	     "invalid pragma clause"
	     pragma)))

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
      (unless (symbol? v)
	 (error "scheme2js-module"
		"bad export-clause. expected symbol"
		v))
      (receive (v type)
	 (untype v)
	 (let ((pragma-info (assq v pragmas)))
	    (or pragma-info v))))

   (define (normalize-fun f pragmas)
      (receive (v type)
	 (untype (car f)) ;; the fun-name
	 (let ((pragma-info (assq v pragmas)))
	    (when pragma-info (check-pragma pragma-info))
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
			 (symbol? (car (cadr e)))
			 (memq (car (cadr e)) new-macros))
		    (loop (filter (lambda (macro)
				     (not (eq? macro (car (cadr e)))))
				  new-macros)
			  (cons e rev-res)))
		   (else
		    (loop new-macros rev-res)))))))
			  
   (with-access::WIP-Unit m (header name exports macros top-level)
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
			     (cons (create-Export-Desc
				    (normalize-var e pragmas) name #f)
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
			     (cons (create-Export-Desc
				    (normalize-fun e pragmas) name #f)
				   exports))
		       (loop (cdr entries) new-macros)))))))))

(define (check-scheme2js-export-clause ex)
   (cond
      ((symbol? ex) 'ok)
      ((and (pair? ex)
	    (list? ex)
	    (symbol? (car ex))
	    (every? (lambda (p)
		       (and (pair? p)
			    (list? p)
			    (symbol? (car p))))
		    (cdr ex)))
       'ok)
      (else
       (error "scheme2js-module"
	      "bad export-clause"
	      ex))))

(define (check-scheme2js-export-macro-clause ex)
   (unless (and (pair? ex)
		(eq? (car ex) 'define-macro)
		(pair? (cdr ex))
		(pair? (cadr ex))
		(symbol? (car (cadr ex))))
      (error "scheme2js-module"
	     "bad macro-export-clause"
	     ex)))

(define (normalize-scheme2js-exports! m::WIP-Unit)
   (with-access::WIP-Unit m (header name exports macros)
      (set! exports (append exports (map (lambda (ex)
					    (check-scheme2js-export-clause ex)
					    (create-Export-Desc ex name #f))
					 (extract-entries header 'export))))
      (let ((exported-macros (extract-entries header 'export-macros)))
	 (for-each check-scheme2js-export-clause exported-macros)
	 (set! macros (append macros exported-macros)))))
