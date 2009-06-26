;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module module-system
   (import verbose
	   error
	   srfi0
	   tools
	   expand
	   export-desc
	   module-resolver
	   infotron
	   config)
   (export (final-class Compilation-Unit
	      name::symbol      ;; module-name
	      top-level         ;; id or pair-nil
	      exported-macros   ;; ht or list of macros.
	                        ;;  with list: of form (define-macro...)
	      exports           ;; list or hashtable (see export.scm)

	      ;; the following two entries will be filled in this module
	      ;;  list of hts/lists of macros that are imported
	      (macros::pair-nil (default '()))
	      ;;  a list of 'Export'-lists/hashtables.
	      (imports::pair-nil (default '()))
	      ;; #t if the file/unit contained a module header.
	      (declared-module?::bool (default #f)))
	   (wide-class WIP-Unit::Compilation-Unit ;; work in progress
	      header)
	   (create-module-from-file file::bstring override-headers::pair-nil
				    reader::procedure)
	   (create-module-from-expr expr override-headers::pair-nil)
	   (read-imported-module-file module-name::symbol file
				      reader
				      #!key
				      (bigloo-modules? #t)
				      (store-exports-in-ht? #f)
				      (store-exported-macros-in-ht? #f))
	   (module-exported-macro-add! m::Compilation-Unit macro::pair)))

(define (module-exported-macro-add! m::Compilation-Unit macro::pair)
   (with-access::Compilation-Unit m (exported-macros)
      (cond
	 ((or (null? exported-macros)
	      (pair? exported-macros))
	  ;; inefficient...
	  (set! exported-macros (append exported-macros (list macro))))
	 ((hashtable? exported-macros)
	  (add-macro-to-ht macro exported-macros))
	 (else
	  (error "scheme2js-module module-exported-macro-add!"
		 "bad Compilation-Unit"
		 exported-macros)))))

;; uses config 'module-resolver (if given). Should be a procedure that, given a
;; module name, returns a list of potential files that could contain the
;; module.

;; reads an evaluates the module/infotron clauses in the beginning of files.
;; In addition to the (optionel) module-header found in files one can provide
;; additional headers that should either replace or merge with the existing
;; one. It is also possible to provide headers that are only "applied" if the
;; file does not have any.
;; headers are of the form:
;;  '((kind header1 ...) ...)
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

;; m.header must be of form (module sym ...)
(define (normalize-module-header! m::WIP-Unit)
   (define (add-new-els! acc new-elements)
      (let loop ((new-els new-elements))
	 (cond
	    ((null? new-els) 'done)
	    ((epair? new-els)
	     ;; don't forget the location
	     (set-cdr! acc
		       (econs (car new-els)
			      (cdr acc)
			      (cer new-els)))
	     (loop (cdr new-els)))
	    ((pair? new-els)
	     (set-cdr! acc
		       (cons (car new-els)
			     (cdr acc)))
	     (loop (cdr new-els)))
	    (else
	     (scheme2js-error
	      "module"
	      "invalid module-clause"
	      new-els
	      new-elements)))))
	     
   (with-access::WIP-Unit m (header)
      (let loop ((h (cddr header))
		 (rev-res '())) ;; only the assq themselves are reversed
	 (cond
	    ((null? h)
	     (for-each (lambda (clause)
			  (when (pair? clause)
			     (set-cdr! clause (reverse! (cdr clause)))))
		       rev-res)
	     (set! header (cons* (car header) (cadr header) rev-res)))
	    ((not (pair? h))
	     (scheme2js-error "module"
			      "invalid module-header"
			      h
			      header))
	    ((or (not (pair? (car h)))
		 (not (symbol? (car (car h)))))
	     (scheme2js-error "module"
			      "invalid module-clause"
			      (car h)
			      h))
	    (else
	     (let* ((clause (car h))
		    (acc (assq (car clause) rev-res)))
		(cond
		   (acc ;; already have an entry for this kind.
		    (add-new-els! acc (cdr clause))
		    (loop (cdr h) rev-res))
		   ((epair? clause)
		    (let ((acc (econs (car clause)
				      '()
				      (cer clause))))
		       (add-new-els! acc (cdr clause))
		       (loop (cdr h)
			     (cons acc rev-res))))
		   (else
		    (let ((acc (list (car clause))))
		       (add-new-els! acc (cdr clause))
		       (loop (cdr h)
			     (cons acc rev-res)))))))))))

;; verifies that (user-supplied) module-clause is well formed.
(define (verify-module-clause clause)
   (and (list? clause)
	(pair? clause)
	(eq? (car clause) 'module)
	(pair? (cdr clause))
	(symbol? (cadr clause))))
   
(define (check-module-clause clause)
   (unless (verify-module-clause clause)
      (scheme2js-error "scheme2js-module"
		       "bad module-clause"
		       clause
		       clause)))

(define (check-module-name module-name file-name header)
   (when (or (eq? module-name '*)
	     (eq? module-name '_))
      (scheme2js-error "module"
		       "Invalid module name"
		       module-name
		       header)))

(define (make-ecopy in-l)
   (let loop ((l in-l)
	      (rev-res '()))
      (cond
	 ((null? l) (reverse! rev-res))
	 ((epair? l) (loop (cdr l)
			   (econs (car l)
				  rev-res
				  (cer l))))
	 ((pair? l) (loop (cdr l)
			  (cons (car l) rev-res)))
	 (else
	  (scheme2js-error
	   "module"
	   "bad list"
	   l
	   in-l)))))
   
(define (cond-expand-headers! m::WIP-Unit)
   (with-access::WIP-Unit m (header)
      (when (any? (lambda (h)
		     (match-case h
			((cond-expand . ?L) #t)
			(else #f)))
		  header)
	 ;; make a copy so we can physically modify the list.
	 (let ((copy (make-ecopy header)))
	    (let loop ((clauses copy)
		       ;; a module always starts with (module xyz ...)
		       ;; therefore we can't have a cond-expand as first el.
		       (last-list-el #f))
	    (cond
	       ((null? clauses)
		(set! header copy))
	       ((not (pair? clauses))
		(scheme2js-error "module"
				 "invalid module-clause"
				 header
				 header))
	       (else
		(match-case (car clauses)
		   ((cond-expand ?clause . ?Lclauses)
		    (let ((new-clauses (srfi0-expand (car clauses))))
		       (cond
			  ((null? new-clauses)
			   (set-cdr! last-list-el (cdr clauses))
			   (loop (cdr clauses) last-list-el))
			  ((list? new-clauses)
			   (let ((new-last-list-el (last-pair new-clauses)))
			      (set-cdr! last-list-el new-clauses)
			      (set-cdr! new-last-list-el (cdr clauses))
			      (loop (cdr clauses) new-last-list-el)))
			  (else
			   (scheme2js-error "module"
					    "invalid cond-expand module-clause"
					    (car clauses)
					    (if (pair? new-clauses)
						new-clauses
						(car clauses)))))))
		   (else
		    (loop (cdr clauses)
			  clauses))))))))))

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
      (when (not in-port)
	 (error "read-file" "couldn't open: " file))
      (let* ((sexp (reader in-port #t))
	     (header-sexp? (module-clause? sexp))
	     (header (and header-sexp? sexp))
	     (top-level (read-file-exprs in-port
					 sexp
					 (not header-sexp?)))
	     (file-path (if (string=? "-" file) "." (dirname file)))
	     (m (instantiate::Compilation-Unit
		   (name 'tmp) ;; will be set later.
		   (top-level top-level)
		   (exported-macros '())
		   (exports '())
		   (declared-module? header-sexp?))))
	 (when header-sexp? (check-module-clause header))
	 (widen!::WIP-Unit m
	    (header header))

	 (unless (string=? file "-") (close-input-port in-port))

	 (prepare-module! m override-headers file-path reader)

	 (let ((module-name (WIP-Unit-name m)))
	    (unless (or (config 'infotron)
			(not header-sexp?))
	       (check-module-name module-name file header)))

	 (shrink! m)
	 m)))

(define (create-module-from-expr expr override-headers)
   (let ((m (instantiate::Compilation-Unit
	       (name (gensym 'module))
	       (top-level (list expr))
	       (exported-macros '())
	       (exports '())
	       (declared-module? #f)))
	 (file-path ".")) ;; filepath is assumed to be "."
      (widen!::WIP-Unit m
	 (header #f))

      (prepare-module! m override-headers file-path read)

      (shrink! m)
      m))

;; header must be #f or of form (module sym ...)
(define (module-entries header type)
   (let ((tmp (assq type (cddr header))))
      (if tmp
	  (cdr tmp)
	  '())))

;; precondition: the WIP-Unit's header is either #f or well formed.
(define (prepare-module! m::WIP-Unit override-headers::pair-nil
			 file-path::bstring reader::procedure)
   (let* ((include-paths (cons file-path (config 'include-paths)))
	  (module-preprocessor (config 'module-preprocessor))
	  (module-postprocessor (config 'module-postprocessor))
	  (bigloo-modules? (config 'bigloo-modules))
	  (module-resolver (or (config 'module-resolver)
			       (extension-resolver include-paths))))
      (when module-preprocessor
	 (module-preprocessor m))
      (merge-headers! m override-headers)
      (set-name! m)
      (with-access::WIP-Unit m (header)
	 (when header
	    (cond-expand-headers! m)
	    (normalize-module-header! m)
	    (read-includes! m include-paths reader)
	    (read-imports! m module-resolver reader bigloo-modules?) ;; macros too
	    (normalize-JS-imports! m)
	    (normalize-exports! m bigloo-modules?)))
      (when (config 'infotron)
	 (module->infotron! m))
      (when module-postprocessor
	 (module-postprocessor m))))

(define (check-override-headers o-headers)
   (define valid-kinds '(module-replace module-provide merge-first merge-last))

   (unless (list? o-headers)
      (scheme2js-error "scheme2js-module"
		       "Override headers must be a list"
		       o-headers
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
		((not (list? override))
		 (scheme2js-error
		  "scheme2js-module"
		  "invalid override-header"
		  override
		  o-headers))
		((not (and (symbol? (car override))
			   (memq (car override) valid-kinds)))
		 (scheme2js-error
		  "scheme2js-module"
		  (string-append "car of override-header must be one of "
				 "module-replace, module-provide, "
				 "merge-first, merge-last")
		  override
		  override))
		((and (or (eq? (car override) 'module-replace)
			  (eq? (car override) 'module-provide))
		      (or (null? (cdr override))
			  (not (match-case (cadr override)
				  ((module (? (lambda (n)
						 (or (eq? n #f)
						     (symbol? n))))
				      (? list?))
				   #t)
				  (else #f)))))
		 (scheme2js-error "scheme2js-module"
				  "invalid override-header"
				  override
				  override))
		((and (eq? (car override) 'module-replace)
		      already-a-replace?)
		 (scheme2js-error
		  "scheme2js-module"
		  "only one replace override header allowed"
		  override
		  override))
		((and (eq? (car override) 'module-provide)
		      found-provide?)
		 (scheme2js-error
		  "scheme2js-module"
		  "only one provide override header allowed"
		  override
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
						   (cdr p)))
					   override-headers))
		 (merge-lasts (filter-map (lambda (p)
					     (and (eq? (car p) 'merge-last)
						  (cdr p)))
					  override-headers))
		 ;; replace and provide are complete module-headers (they
		 ;; must start with (module ...)
		 (new-name (select-name header replace provide)))
	     (set! header `(module ,new-name
			      ,@(apply append merge-firsts)
			      ,@(cond
				   (replace (cddr replace))
				   (header (cddr header))
				   (provide (cddr header))
				   (else '()))
			      ,@(apply append merge-lasts))))))))

(define (set-name! m::WIP-Unit)
   (with-access::WIP-Unit m (header name declared-module?)
      (cond
	 ((not header)
	  (set! name (gensym 'module)))
	 ((eq? (cadr header) #f) ;; can only be a override header
	  (set! declared-module? #f)
	  (set! name (gensym 'module)))
	 ((not declared-module?)
	  ;; override header provides name now.
	  ;; -> declared module becomes true.
	  (set! declared-module? #t)
	  (set! name (cadr header)))
	 (else ;; the typical case. just take the name out of the header.
	  (set! name (cadr header))))))

(define (emap f L)
   (let loop ((L L)
	      (rev-res '()))
      (cond
	 ((null? L)
	  (reverse! rev-res))
	 ((epair? L)
	  (loop (cdr L)
		(cons (f (car L) (cer L))
		      rev-res)))
	 ((pair? L)
	  (loop (cdr L)
		(cons (f (car L) #f)
		      rev-res)))
	 (else
	  (scheme2js-error
	   "module"
	   "bad module clause"
	   L
	   L)))))

(define (read-includes! m::WIP-Unit include-paths reader)
   (define (read-file f loc)
      (unless (string? f)
	 (scheme2js-error
	  "scheme2js-module"
	  "include-parameter must be a string"
	  f
	  loc))
      (let ((file (find-file/path f include-paths)))
	 (unless file
	    (scheme2js-error "scheme2js module"
			     "can't find include-file"
			     f
			     loc))
	 (with-input-from-file file
	    (lambda ()
	       (let loop ((rev-source '()))
		  (let ((sexp (reader (current-input-port) #t)))
		     (if (eof-object? sexp)
			 (reverse! rev-source)
			 (loop (cons sexp rev-source)))))))))
      
   (with-access::WIP-Unit m (header top-level)
      (let* ((include-files (module-entries header 'include))
	     (read-includes (emap read-file include-files)))
	 (unless (null? include-files)
	    (set! top-level
		  (apply append! (append read-includes (list top-level))))))))

;; returns #f if the file is not the correct one.
(define (read-imported-module-file module-name file reader
				   #!key
				   (bigloo-modules? #t)
				   (store-exports-in-ht? #f)
				   (store-exported-macros-in-ht? #f))
   (when (file-exists? file)
      (let ((ip (open-input-file file)))
	 (unwind-protect
	    (let ((module-clause (reader ip #t)))
	       (cond
		  ((or (not (pair? module-clause))
		       (not (eq? (car module-clause) 'module)))
		   #f)
		  ((not (verify-module-clause module-clause))
		   (warning "scheme2js module"
			    "invalid module-clause skipped"
			    file)
		   #f)
		  ((not (eq? (cadr module-clause) module-name))
		   #f)
		  (else
		   (let ((im (instantiate::Compilation-Unit ;; import-module
				(name module-name)
				(top-level #f)
				(exports '())
				(exported-macros '()))))
		      (widen!::WIP-Unit im (header module-clause))
		      (cond-expand-headers! im)
		      (normalize-module-header! im)
		      ;; normalize-exports might need the 'ip' in
		      ;; case it needs to search for macros.
		      (normalize-exports! im bigloo-modules?
					  #t  ;; get macros
					  reader ip)
		      (with-access::Compilation-Unit im (exports exported-macros)
			 (when store-exports-in-ht?
			    (let ((ht (make-eq-hashtable)))
			       (for-each (lambda (desc)
					    (hashtable-put! ht
							    (Export-Desc-id desc)
							    desc))
					 exports)
			       (set! exports ht)))
			 (when store-exported-macros-in-ht?
			    (let ((ht (make-eq-hashtable)))
			       (for-each (lambda (def-macro)
					    (add-macro-to-ht def-macro ht))
					 exported-macros)
			       (set! exported-macros ht))))
		      im))))
	    (close-input-port ip)))))

(define (export-list? l)
   (or (null? l)
       (and (pair? l)
	    (Export-Desc? (car l))
	    (export-list? (cdr l)))))

(define (read-imports! m::WIP-Unit module-resolver reader bigloo-modules?)
   (define (get-import-list header)
      (let ((import-list (module-entries header 'import)))
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
		 (new-macros '())
		 (new-imports imports))
	 (cond
	    ((null? imported-modules)
	     (set! macros new-macros)
	     (set! imports new-imports))
	    ((Compilation-Unit? (car imported-modules))
	     (let ((im (car imported-modules)))
		(with-access::Compilation-Unit im  (exports exported-macros name)
		   (loop (cdr imported-modules)
			 (if (or (null? exported-macros)
				 (and (hashtable? exported-macros)
				      (zerofx? (hashtable-size
						exported-macros))))
			     new-macros
			     (cons exported-macros new-macros))
			 (if (empty-exports? exports)
			     new-imports
			     (cons (cons name exports)
				   new-imports))))))
	    ((symbol? (car imported-modules))
	     (let* ((imported-module (car imported-modules))
		    (module-files (module-resolver imported-module)))
		(let liip ((files module-files))
		   (cond
		      ((null? files)
		       (error "scheme2js module"
			      "can't find imported module"
			      imported-module))
		      ((read-imported-module-file
			imported-module
			(car module-files)
			reader
			:bigloo-modules? bigloo-modules?)
		       =>
		       (lambda (im)
			  ;; just reuse the cond-clause above.
			  (loop (cons im (cdr imported-modules))
				new-macros
				new-imports)))
		      (else
		       (liip (cdr files)))))))
	    (else
	     (error "scheme2js module"
		    "bad import clause"
		    imports))))))

(define (normalize-JS-imports! m)
   (with-access::WIP-Unit m (header imports)
      (let* ((direct-JS-imports (module-entries header 'JS))
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

   (define (analyze-arity L)
      (let loop ((L L)
		 (res 0))
	 (cond
	    ((null? L) res)
	    ((pair? L) (loop (cdr L) (+fx res 1)))
	    (else (negfx (+fx res 1))))))

   (define (analyze-fun f)
      (receive (name type)
	 (untype (car f))
	 (values name type (analyze-arity (cdr f)))))

   (define (normalize-fun f pragmas)
      (receive (fun-name type arity)
	 (analyze-fun f)
	 (let ((pragma-info (assq fun-name pragmas)))
	    (when pragma-info (check-pragma pragma-info))
	    (cond
	       ((and (not pragma-info)
		     type)
		`(,fun-name (type ,type) (arity ,arity) (constant? #t)))
	       ((not pragma-info)
		`(,fun-name (arity ,arity) (constant? #t)))
	       (else
		(let ((pragma-type (assq 'type (cdr pragma-info)))
		      (pragma-arity (assq 'arity (cdr pragma-info)))
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
		      ((and pragma-arity
			    (not (eq? arity (cadr pragma-arity))))
		       (error "scheme2js-module"
			      "variable exported with two different arities"
			      f))
		      ((or (and pragma-constant? pragma-arity pragma-type)
			   (and pragma-constant? pragma-arity (not type)))
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
							with-type)))
			      (with-arity (if pragma-arity
					      with-constant?
					      (cons `(arity ,arity)
						    with-constant?))))
			  (cons fun-name with-arity))))))))))

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
			  
   (with-access::WIP-Unit m (header name exports exported-macros top-level)
      (let ((new-exports (module-entries header 'export))
	    (pragmas (module-entries header 'scheme2js-pragma)))
	 (let loop ((entries new-exports)
		    (new-macros '()))
	    (if (null? entries)
		(set! exported-macros (find-macros new-macros))
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
   (with-access::WIP-Unit m (header name exports exported-macros macros)
      (set! exports (append exports (map (lambda (ex)
					    (check-scheme2js-export-clause ex)
					    (create-Export-Desc ex name #f))
					 (module-entries header 'export))))
      (let ((exported-ms (module-entries header 'export-macros)))
	 (for-each check-scheme2js-export-clause exported-ms)
	 (set! exported-macros exported-ms)
	 ;; HACK: in scheme2js-modules the exported modules are known to the
	 ;; module too.
	 ;; Add them to the macros.
	 (set! macros (append exported-ms macros)))))
