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

(module exporter
   (main my-main))

;; (export #t) will search its name from the exported Js-function.
;; unmarshalling recognizes the following patterns:
;; is-XX, has-YY, ZZ-bang which are converted to XX?, YY? and ZZ!
;;
;; by default both CamlCase and c_underscores are recognized. Both can be
;; disabled with a flag. In either case a '-' is inserted instead (ex
;; Caml-case and c-underscores).
;;
;; prefixes can be ignored: --ignored-prefixes '("pre" "sc_" "SC_" "hop_")'
;;
;; the --constant flag will add a (constant #f) clause to every exported
;; variable. scheme2js will then make sure that none of these are
;; modified.
;;
;; --constant-functions will only add the (constant #f) clause to
;; functions. This is purely syntactic. var f = function() {}; will _not_ be
;; exported as (constant #t). but function f() {}; will.
;;
;; (arity #t) will search the arity from the function.

(define *ignored-prefixes* '())
(define *out-file* #f)
(define *in-files* '())

(define *module-name* #f)

(define *camel-case?* #t)
(define *c_underlines?* #t)

(define *constant?* #f)
(define *constant-functions?* #f)

(define *scheme2js-modules?* #f)

(define (unmarshall s)
   (define (is-has-bang str)
      (cond
	 ((starts-with? str "is-")
	  (is-has-bang (string-append (substring str 3 (string-length str)) "?")))
	 ((starts-with? str "has-")
	  (is-has-bang (string-append str "?")))
	 ((ends-with? str "-bang")
	  (is-has-bang (string-append (substring str 0
						 (- (string-length str) 5))
				       "!")))
	 (else
	  str)))
   
   (let* ((str (symbol->string s))
	  (s-without-ignored-prefixes
	   (remove-prefix str *ignored-prefixes*)))
      (let loop ((chars (string->list s-without-ignored-prefixes))
		 (rev-res '()))
	 (if (null? chars)
	     (let ((res (list->string (reverse! rev-res))))
		(string->symbol (is-has-bang res)))
	     (cond
		((and *camel-case?*
		      (char-upper-case? (car chars))) ;; starting upper-case
		 (loop (cdr chars)
		       (cons (char-downcase (car chars)) (cons #\- rev-res))))
		((and *c_underlines?*
		      (eq? (car chars) #\_))
		 (loop (cdr chars)
		       (cons #\- rev-res)))
		((eq? (car chars) #\2)
		 (loop (cdr chars)
		       (cons #\> (cons #\- rev-res))))
		(else
		 (loop (cdr chars)
		       (cons (car chars) rev-res))))))))

(define (extract-scheme-names var meta)
   (define (my-error msg)
      (let ((loc (if (epair? meta) (cer meta) #f)))
	 (match-case loc
	    ((at ?fname ?loc)
	     (error/location "exporter" msg meta fname loc))
	    (else
	     (error "exporter" msg meta)))))

   (let* ((scheme-funs-entry (assq 'export meta)))
      (cond
	 ((not scheme-funs-entry)
	  (warning "Function has META info but is not exported."
		   var)
	  '())
	 ((null? (cdr scheme-funs-entry))
	  (my-error "(export) -clause without value"))
	 ((and (eq? (cadr scheme-funs-entry) #t)
	       (not var))
	  (let ((var-name-p (assq 'JS meta)))
	     (unless var-name-p
		(my-error "meta without name and without JS"))
	     ;; TODO: more error-handling.
	     (list (unmarshall (cadr var-name-p)))))
	 ((eq? (cadr scheme-funs-entry) #t)
	  (list (unmarshall var)))
	 (else
	  (cdr scheme-funs-entry)))))

(define (construct-scheme2js-module-clause exports-ht macros-ht)
   `(module ,(string->symbol *module-name*)
       (export-macros ,@(hashtable-map macros-ht
				       (lambda (ignored macro)
					  macro)))
       (export ,@(hashtable-map exports-ht
				(lambda (scheme-name export-clause)
				   (cons scheme-name export-clause))))))

(define (construct-bigloo-module-clause exports-ht macros-ht)
   `(module ,(string->symbol *module-name*)
       (export ,@(hashtable-map exports-ht
				(lambda (scheme-name export-clause)
				   scheme-name)))
       (scheme2js-pragma
	,@(hashtable-map exports-ht
			 (lambda (scheme-name export-clause)
			    (cons scheme-name export-clause))))
       (export ,@(hashtable-map macros-ht
				(lambda (ignored macro)
				   (let ((macro-name (car (cadr macro))))
				      `(macro ,macro-name)))))))
(define (print-macros macros-ht)
   (hashtable-for-each macros-ht
		       (lambda (ignored macro)
			  (pp macro))))

(define (fixup-arity! meta js-arity)
   (let ((t (assq 'arity meta)))
      (when (and t (eq? (cadr t) #t))
	 (set-car! (cdr t) js-arity))))

(define (print-module-clause metas)
   (let ((exports-ht (make-hashtable))
	 (macros-ht (make-hashtable)))
      ;; TODO: hard-coded combinations.
      (for-each
       (lambda (var/kind/meta)
	  (let ((var (car var/kind/meta))
		(kind (cadr var/kind/meta))
		(meta (caddr var/kind/meta)))
	     (when (eq? kind 'function)
		(fixup-arity! meta (cadddr var/kind/meta)))
	     (cond
		((eq? kind 'macro)
		 (hashtable-put! macros-ht (caadr meta) meta))
		(else
		 (let* ((scheme-funs (extract-scheme-names var meta))
			(without-export
			 (filter (lambda (assoc)
				    (not (eq? (car assoc) 'export)))
				 meta))
			(with-JS (if (assoc 'JS without-export)
				     ;; there is already a JS-clause
				     without-export
				     (cons (list 'JS var)
					   without-export)))
			(with-constant (if (or *constant?*
					       (and *constant-functions?*
						    (eq? kind 'function)))
					   (cons (list 'constant? #t)
						 with-JS)
					   with-JS)))
		    (for-each (lambda (scheme-fun)
				 (hashtable-put! exports-ht
						 scheme-fun
						 with-constant))
			      scheme-funs))))))
       metas)
      (if *scheme2js-modules?*
	  (pp (construct-scheme2js-module-clause exports-ht macros-ht))
	  (begin
	     (pp (construct-bigloo-module-clause exports-ht macros-ht))
	     (print-macros macros-ht)))))

(define (handle-args args)
   (args-parse (cdr args)
      (section "Help")
      (("?")
       (args-parse-usage #f))
      ((("-h" "--help") (help "?,-h,--help" "This help message"))
       (args-parse-usage #f))
      (section "Misc")
      (("-o" ?file (help "The output file. '-' prints to stdout."))
       (set! *out-file* file))
      (("--module" ?name (help "The module name"))
       (set! *module-name* name))
      (("--ignored-prefixes" ?list
			     (help "scheme-list of ignored prefixes"))
       (set! *ignored-prefixes* (with-input-from-string list read)))
      (("--no-camelCase" (help "Disable camel-case unmarshalling"))
       (set! *camel-case?* #f))
      (("--no-c_underscores" (help "Disable underscore unmarshalling"))
       (set! *c_underlines?* #f))
      (("--constant" (help "Add (constant? #t) clause to every export"))
       (set! *constant?* #t))
      (("--constant-functions"
	(help "Add (constant? #t) clause to exported functions."))
       (set! *constant-functions?* #t))
      (("--scheme2js-modules"
	(help "Create scheme2js-modules and not Bigloo modules."))
       (set! *scheme2js-modules?* #t))
      (else
       (set! *in-files* (append! *in-files* (list else))))))

(define (my-main args)
   (handle-args args)
   (if (not *out-file*)
       (error "exporter" "no out-file given" #f))
   (if (null? *in-files*)
       (error "exporter" "no input-file(s) given" #f))
   (if (not *module-name*)
       (error "exporter" "no module-name given" #f))
   (let ((metas (read-metas (open-multi-file *in-files*))))
      (with-output-to-file *out-file*
	 (lambda () (print-module-clause metas))))
   (exit 0))
