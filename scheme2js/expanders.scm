;*=====================================================================*/
;*    serrano/prgm/project/hop/2.3.x/scheme2js/expanders.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Thu Nov 24 10:52:12 2011                          */
;*    Last change :  Tue Dec 20 18:00:11 2011 (serrano)                */
;*    Copyright   :  2007011-2011 Florian Loitsch, Manuel Serrano      */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*    Scheme2Js is distributed in the hope that it will be useful,     */
;*    but WITHOUT ANY WARRANTY; without even the implied warranty of   */
;*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
;*    LICENSE file for more details.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expanders
   (import expand
	   config
	   error
	   tools
	   (runtime-ref pobject-conv)))

;; reuse the expanders of Bigloo.
(for-each (lambda (expd)
	     (install-expander! expd expand-once-expander))
	  '(labels match-case when unless do cond quasiquote))

;; don't go into quotes.
(install-expander! 'quote identity-expander)

(install-expander! 'lambda lambda-expander)
(install-expander! 'define-macro define-macro-expander)
(install-expander! 'define define-expander)
(install-expander! 'define-inline define-expander)
(install-expander! 'or or-expander)
(install-expander! 'and and-expander)
(install-expander! 'let* let*-expander)
(install-expander! 'let let-expander)
(install-expander! 'define-struct define-struct-expander)
(install-expander! 'delay delay-expander)
(install-expander! 'bind-exit bind-exit-expander)
(install-expander! 'with-handler with-handler-expander)
(install-expander! 'receive receive-expander)
(install-expander! '@ identity-expander)
(install-expander! 'multiple-value-bind multiple-value-bind-expander)
(install-expander! 'define-generic define-generic-expander)
(install-expander! 'define-method define-method-expander)
(install-expander! '-> ->expander)
(install-expander! 'with-trace with-trace-expander)

(define (->expander x e)
   (if (every? symbol? x)
       (loc-attach
	  `(,(car x) ,(e (cadr x) e) ,@(cddr x))
	  x (cdr x))
       (scheme2js-error "->" "bad form" x x)))


(define (lambda-expander x e)
   (match-case x
       ((?- ?formal ?- . (? list?))
	;; do not expand formal
	(loc-attach
	 `(lambda ,(cadr x) ,@(emap1 (lambda (y) (e y e)) (cddr x)))
	 x (cdr x)))
       (else
	(scheme2js-error "lambda-expand"
			 "bad 'lambda'-form"
			 x x))))

(define (define-macro-expander x e)
   (match-case x
      ((?- (?name . ?args) ?e0 . ?body)
       (let ((ht (module-macro-ht)))
	  (hashtable-put! ht
			  name
			  (lazy-macro x ht))
	  #unspecified))
      (else
       (scheme2js-error "define-macro"
			"Illegal 'define-macro' syntax"
			x x))))

(define (define-expander x e)
   (match-case x
      ((?- (?fun . ?formals) . ?body)
       (e
	;; keep the location...
	(loc-attach
	 `(define ,fun (lambda ,formals ,@body))
	 x (cadr x) (cadr x))
	e))
      ((?- ?var ?val)
       (loc-attach
	`(define ,(e var e) ,(e val e))
	x (cdr x) (cddr x)))
      (else
       (scheme2js-error "define-expand"
			"Invalid 'define'-form"
			x x))))

(define (or-expander x e)
   (match-case x
      ((?-) #f)
      ((?- . (and (? pair?) ?tests))
       (e (if (null? (cdr tests)) ; last element
	      (car tests)
	      (let* ((tmp-var (gensym 'tmp))
		     (binding (loc-attach
			       `(,tmp-var ,(car tests))
			       tests tests)))
		 `(let (,binding)
		     (if ,tmp-var
			 ,tmp-var
			 (or ,@(cdr tests))))))
	  e))
      (else
       (scheme2js-error "or-expand"
			"Invalid 'or'-form"
			x x))))

(define (and-expander x e)
   (match-case x
      ((?-) #t)
      ((?- . (and (? pair?) ?tests))
       (e (if (null? (cdr tests)) ; last element
	      (car tests)
	      (loc-attach
	       `(if ,(car tests)
		    (and ,@(cdr tests))
		    #f)
	       x tests x x))
	  e))
      (else
       (scheme2js-error "and-expand"
			"Invalid 'and'-form"
			x x))))

;; transform let* into nested lets
(define (let*-expander x e)
   (match-case x
      ((?- () . ?body)
       (e (loc-attach
	   `(let ()
	       ,@body)
	   x (cdr x))
	  e))
      ((?- ((and (?- ?-) ?binding) . ?bindings) . ?body)
       (e (loc-attach
	   `(let (,binding)
	       ,@(if (null? bindings)
		     body
		     `((let* ,bindings
			  ,@body))))
	   x (cdr x))
	  e))
      (else
       (scheme2js-error "let*-expand"
			"Invalid 'let*'-form"
			x x))))

(define (expand-let x e)
   ;; we know it's of form (?- (? list?) . ?-)
   (let* ((bindings (cadr x))
	  (body (cddr x)))
      (unless (every? (lambda (b)
			 (and (pair? b)
			      (pair? (cdr b))
			      (null? (cddr b))
			      (symbol? (car b))))
		      bindings)
	 (scheme2js-error "let expand"
			  "Invalid 'let' form"
			  x x))
      (for-each (lambda (binding)
		   (set-car! binding (e (car binding) e))
		   (set-car! (cdr binding)
			     (e (cadr binding) e)))
		bindings)

      `(let ,bindings ,@(emap1 (lambda (y) (e y e)) body))))
;*       `(let ,(emap1 (lambda (binding)                               */
;* 		       (loc-attach                                     */
;* 			`(,(e (car binding) e) ,(e (cadr binding) e))  */
;* 			binding (cdr binding)))                        */
;* 		    bindings)                                          */
;* 	  ,@(emap1 (lambda (y) (e y e)) body))))                       */

(define (let-expander x e)
   (match-case x
      ((?- (? symbol?) (? list?) . ?-)
       (e (my-expand-once x) e))
      ((?- (? list?) . ?-)
       (expand-let x e))
      (else
       (scheme2js-error "let expand" "Illegal 'let' form"
			x x))))

(define (define-struct-expander x e)
   (match-case x
      ((?- ?name . ?fields)
       (unless (and (symbol? name)
		    (list? fields)
		    (every (lambda (f)
			      (or (symbol? f)
				  (and (pair? f)
				       (symbol? (car f))
				       (pair? (cdr f))
				       (null? (cddr f)))))
			   fields))
	  (scheme2js-error "define-struct expand"
			   "Illegal 'define-struct' form"
			   x x))
       (let* ((field-ids (emap1 (lambda (f)
				   (if (pair? f) (car f) f))
				fields))
	      (field-getters (emap1 (lambda (field)
				       (symbol-append name '- field))
				    field-ids))
	      (field-setters (emap1 (lambda (field)
				       (symbol-append name '- field '-set!))
				    field-ids))
	      (defaults (emap1 (lambda (f)
				  (if (pair? f) (cadr f) '()))
			       fields))
	      (tmp (gensym)))
	  `(begin
	      (define ,(symbol-append 'make- name)
		 (lambda ()
		    (,name ,@defaults)))
	      (define ,name
		 (lambda ,field-ids
		    (let ((,tmp (make-struct ',name)))
		       ,@(map (lambda (setter val)
				 `(,setter ,tmp ,val))
			      field-setters field-ids)
		       ,tmp)))
	      (define ,(symbol-append name '?)
		 (lambda (s) (struct-named? ',name s)))
	      ,@(map (lambda (field getter setter)
			`(begin
			    (define ,getter
			       (lambda (s)
				  (struct-field s ',name ,(symbol->string field))))
			    (define ,setter
			       (lambda (s val)
				  (struct-field-set! s ',name ,(symbol->string field) val)))))
		     field-ids
		     field-getters
		     field-setters))))
      (else
       (scheme2js-error "define-struct expand"
			"Illegal 'define-struct' form"
			x x))))

(define (delay-expander x e)
   (match-case x
      ((?- ?exp)
       (e `(,(runtime-ref 'make-promise)
	    (lambda () ,exp)) e))
      (else
       (scheme2js-error "delay expand"
			"Illegal 'delay' form"
			x x))))

(define (bind-exit-expander x e)
   (match-case x
      ((?- (?escape) ?expr . ?Lrest)
       (e
	`(,(runtime-ref 'bind-exit-lambda)
	  (lambda (,escape)
	     ,expr
	     ,@Lrest))
	e))
      (else
       (scheme2js-error "bind-exit" "Invalid 'bind-exit' form" x x))))

(define (with-handler-expander x e)
   (match-case x
      ((?- ?handler ?expr . ?Lrest)
       (e
	`(,(runtime-ref 'with-handler-lambda)
	  ,handler
	  ,(if (config 'procedures-provide-js-this)
	       (let ((th (gensym 'this)))
		  `(let ((,th this))
		      (lambda ()
			 (let ((this ,th))
			    ,expr
			    ,@Lrest))))
	       `(lambda ()
		   ,expr
		   ,@Lrest)))
	e))
      (else
       (scheme2js-error "with-handler" "Invalid 'with-handler' form" x x))))

(define (receive-expander x e)
   (match-case x
      ((?- ?vars ?producer ?expr . ?Lrest)
       (e
	`(,(runtime-ref 'call-with-values)
	  (lambda () ,producer)
	  (lambda ,vars
	     ,expr
	     ,@Lrest))
	e))
      (else
       (scheme2js-error "receive"
			"Invalid 'receive' form"
			x x))))

(define (multiple-value-bind-expander x e)
   (match-case x
      ((?- ?vars ?producer ?expr . ?Lrest)
       (e
	`(,(runtime-ref 'call-with-values)
	  (lambda () ,producer)
	  (lambda ,vars
	     ,expr
	     ,@Lrest))
	e))
      (else
       (scheme2js-error "multiple-value-bind"
			"Invalid 'multiple-value-bind' form"
			x x))))

;*---------------------------------------------------------------------*/
;*    map* ...                                                         */
;*---------------------------------------------------------------------*/
(define (map* f lst)
   (cond
      ((null? lst)
       '())
      ((not (pair? lst))
       (list (f lst)))
      (else
       (cons (f (car lst)) (map* f (cdr lst))))))
	  
;*---------------------------------------------------------------------*/
;*    define-generic-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (define-generic-expander x e)
   (match-case x
      ((?- (?id ?a0 . ?args) . ?body)
       (multiple-value-bind (gname gtype)
	  (parse-ident id)
	  (multiple-value-bind (aname atype)
	     (parse-ident a0)
	     (unless (pair? body)
		(set! body `(error ,(symbol->string id) "No default body" ,aname)))
	     `(begin
		 ,(e (loc-attach
			`(define ,(cons* id a0 args)
			    ,(if (list? args)
				 `((-> ,aname ,gname)
				   ,aname ,@(map id-of-id args))
				 `(apply (-> ,aname ,gname)
				     ,aname ,@(map* id-of-id args))))
			x (cadr x) (cadr x))
		     e)
		 ,(e (loc-attach
			`((@ sc_add_method js)
			  ,atype
			  ',gname
			  (lambda ,(cons a0 args) ,@body))
			x x x x)
		     e)))))
      (else
       (scheme2js-error "define-generic" "Illegal form" x x))))
	  
;*---------------------------------------------------------------------*/
;*    define-method-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (define-method-expander x e)
   (match-case x
      ((?- ((and (? symbol?) ?id) (and (? symbol?) ?a0) . ?args) . ?body)
       (multiple-value-bind (gname gtype)
	  (parse-ident id)
	  (multiple-value-bind (aname atype)
	     (parse-ident a0)
	     (e (loc-attach
		   `((@ sc_add_method js)
		     ,atype
		     ',gname
		     (lambda ,(cons a0 args)
			(define (call-next-method)
			   (let* ((super (class-super ,atype))
				  (proto (-> super prototype)))
			      ,(if (list? args)
				   `((-> proto ,gname)
				     ,aname ,@(map id-of-id args))
				   `(apply (-> proto ,gname)
				       ,aname ,@(map* id-of-id args)))))
			,@body))
		   x x x x)
		e))))
      (else
       (scheme2js-error "define-method" "Illegal form" x x))))

;*---------------------------------------------------------------------*/
;*    with-trace ...                                                   */
;*---------------------------------------------------------------------*/
(define (with-trace-expander x e)
   (match-case x
      ((?- ?level ?name . ?body)
       (e (loc-attach
	     `((@ sc_withTrace js) ,level ,name (lambda () ,@body))
	     x (cdr x) (cddr x) (cdddr x))
	  e))
      (else
       (scheme2js-error "with-trace" "Illegal form" x x))))
