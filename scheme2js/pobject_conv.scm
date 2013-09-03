;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/pobject_conv.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Thu Nov 24 07:23:39 2011                          */
;*    Last change :  Thu Aug 15 07:08:12 2013 (serrano)                */
;*    Copyright   :  2007-13 Florian Loitsch, Manuel Serrano           */
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
(module pobject-conv
   
   (import tools
	   nodes
	   error
	   export-desc
	   config
	   verbose)
   
   (export ;;; going to be used in symbol-pass.
           (wide-class Runtime-Ref::Ref)
	   (wide-class Define::Set!))
   
   (export (pobject-conv::Node prog)
	   (runtime-ref id::symbol)))

;*---------------------------------------------------------------------*/
;*    runtime-ref ...                                                  */
;*    -------------------------------------------------------------    */
;*    recognized as "artificial" runtime-reference.                    */
;*---------------------------------------------------------------------*/
(define (runtime-ref id)
   ;; we use a function to differenciate this construct
   ;; from similar user-constructs.
   (list 'runtime-ref id (lambda () 'runtime-ref)))

;*---------------------------------------------------------------------*/
;*    location ...                                                     */
;*---------------------------------------------------------------------*/
(define (location s-expr)
   (or (and (epair? s-expr) (cer s-expr))
       ;; not "really" correct, but an approximate location is better
       ;; than nothing.
       (and (pair? s-expr) (location (car s-expr)))))

;*---------------------------------------------------------------------*/
;*    find-location ...                                                */
;*---------------------------------------------------------------------*/
(define (find-location exp loc)
   (or (location exp) loc))

;*---------------------------------------------------------------------*/
;*    scheme->pobject-map/loc ...                                      */
;*---------------------------------------------------------------------*/
(define (scheme->pobject-map/loc l loc procname)
   (let loop ((l l)
	      (rev-res '())
	      (loc loc))
      (cond
	 ((null? l)
	  (reverse! rev-res))
	 ((not (pair? l))
	  (scheme2js-error "Object-conv" "invalid expression-list" l l))
	 (else
	  (let ((loc (find-location l loc)))
	     (loop (cdr l)
		(cons (scheme->pobject/loc (car l) loc procname) rev-res)
		loc))))))

;*---------------------------------------------------------------------*/
;*    attach-location! ...                                             */
;*---------------------------------------------------------------------*/
(define (attach-location!::Node n::Node loc)
   (when loc
      (with-access::Node n (location)
	 (set! location loc)))
   n)
   
(define (pobject-conv prog)
   (verbose "list->pobject")
   (instantiate::Module
      (body (scheme->pobject prog (location prog)))))

(define (expr-list->Begin expr-list loc procname)
   (instantiate::Begin
      (exprs (scheme->pobject-map/loc expr-list
		(find-location expr-list loc) procname))))

;*---------------------------------------------------------------------*/
;*    type-check ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-check var tname loc procname)
   (let ((pred (case tname
		  ((pair) 'pair?)
		  ((vector) 'vector?)
		  ((symbol) 'symbol?)
		  ((char) 'char?)
		  ((int bint) 'integer?)
		  ((real breal) 'real?)
		  ((bool) 'boolean?)
		  ((struct) 'struct?)
		  ((class) 'class?)
		  ((string bstring) 'string?)
		  (else `(lambda (o)
			    (let ((c (class-exists ',tname)))
			       (if c (isa? o c) #t)))))))
      (when pred
	 (epairify
	    `(if (not (,pred ,var))
		 (type-error
		    ,(when (symbol? procname) (symbol->string procname))
		    ,(symbol->string tname) ,var))
	    loc))))

;*---------------------------------------------------------------------*/
;*    type-checks ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-checks vars body loc procname)
   (if (config 'type-check)
       body
       (let loop ((vars vars))
	  (if (null? vars)
	      body
	      (multiple-value-bind (id tname)
		 (parse-ident (car vars))
		 (if tname
		     (let ((tcheck (type-check id tname (find-location vars loc) procname)))
			(if tcheck 
			    (cons tcheck (loop (cdr vars)))
			    (loop (cdr vars))))
		     (loop (cdr vars))))))))

;*---------------------------------------------------------------------*/
;*    epairify ...                                                     */
;*---------------------------------------------------------------------*/
(define (epairify obj loc)
   (if (and (pair? obj) loc)
       (econs (car obj) (cdr obj) loc)
       obj))

;*---------------------------------------------------------------------*/
;*    lambda->object ...                                               */
;*---------------------------------------------------------------------*/
(define (lambda->pobject type arguments body loc procname)
   
   ;; if there's a vaarg, make it a the last element of the list and return
   ;; (the list . #t)
   (define (vaarg-list! arguments)
      (cond
	 ((null? arguments)
	  (values arguments #f))
	 ((not (pair? arguments))
	  (values (list arguments) #t))
	 (else
	  (let* ((p (last-pair arguments))
		 (vaarg (cdr p)))
	     (cond
		((null? vaarg)
		 (values arguments #f))
		(else
		 (set-cdr! p (list vaarg)) ;; physically attach the vaarg
		 (values arguments #t)))))))
   
   (define (location-map f l loc)
      (let loop ((l l)
		 (rev-res '()))
	 (if (null? l)
	     (reverse! rev-res)
	     (let ((loc (find-location l loc)))
		(loop (cdr l)
		   (cons (f (car l) loc) rev-res))))))

   (define (type-result type body)
      (if type
	  (let ((tmp (gensym 'tmp)))
	     (list
		`(let ((,(string->symbol (format "~a::~a" tmp type))
			(begin ,@body)))
		    ,tmp)))
	  body))
   
   (define (analyze-arity L)
      (let loop ((L L)
		 (res 0))
	 (cond
	    ((null? L) res)
	    ((pair? L) (loop (cdr L) (+fx res 1)))
	    (else (negfx (+fx res 1))))))
   
   (multiple-value-bind (formals vaarg?)
      (vaarg-list! arguments)
      
      (unless (and (list? formals) (every symbol? formals))
	 (scheme2js-error "Object-conv"
	    "Invalid arguments-clause"
	    arguments
	    arguments))

      (let ((formal-decls (location-map (lambda (formal loc)
					   (instantiate::Ref
					      (location loc)
					      (id (id-of-id formal))))
			     formals
			     (find-location arguments loc)))
	    (loc (find-location body loc)))
	 (instantiate::Lambda
	    (location loc)
	    (formals formal-decls)
	    (vaarg? vaarg?)
	    (arity (analyze-arity arguments))
	    (body (instantiate::Return
		     (location loc)
		     (val (expr-list->Begin
			     (type-checks formals
				(type-result type body)
				loc procname)
			     loc procname))))))))

;*---------------------------------------------------------------------*/
;*    let-form->pobject ...                                            */
;*---------------------------------------------------------------------*/
(define (let-form->pobject bindings body kind loc procname)
   
   (define (binding->pobject binding)
      (when (or (null? binding)
		(null? (cdr binding))
		(not (symbol? (car binding)))
		(not (null? (cddr binding))))
	 (scheme2js-error "pobject-conversion"
	    "Bad Let-form binding"
	    binding
	    binding))
      (let ((var (id-of-id (car binding)))
	    (val (cadr binding))
	    (loc (find-location (car binding) loc)))
	 (instantiate::Set!
	    (location loc)
	    (lvalue (instantiate::Ref
		       (id var)
		       (location loc)))
	    (val (scheme->pobject/loc val
		    (find-location (cdr binding) loc) var)))))
   
   (let ((pobject-bindings (map binding->pobject bindings)))
      (instantiate::Let
	 (location loc)
	 (bindings pobject-bindings)
	 (body (expr-list->Begin
		  (type-checks (map car bindings) body loc procname)
		  loc procname))
	 (kind kind))))

;*---------------------------------------------------------------------*/
;*    case->object ...                                                 */
;*---------------------------------------------------------------------*/
(define (case->pobject key clauses loc procname)
   
   (define (clause->pobject clause last?)
      (match-case clause
	 ((?consts . ?raw-exprs)
	  (let* ((loc (find-location raw-exprs (find-location clause loc)))
		 (exprs (scheme->pobject-map/loc raw-exprs loc procname))
		 (begin-expr (instantiate::Begin
				(exprs exprs)
				(location loc))))
	     (if (and last? (eq? consts 'else))
		 (instantiate::Clause
		    (consts '())
		    (location loc)
		    (expr begin-expr)
		    (default-clause? #t))
		 (begin
		    (unless (list? consts)
		       (scheme2js-error "Object-conv"
			  "bad constants in case-clause"
			  consts
			  consts))
		    (instantiate::Clause
		       (location loc)
		       (consts (map (lambda (const)
				       (instantiate::Const (value const)))
				  consts))
		       (expr begin-expr)
		       (default-clause? #f))))))
	 (else
	  (scheme2js-error "Object-conv"
	     "bad Case-clause"
	     clause
	     clause))))
   
   (define (clauses->pobjects clauses rev-result)
      (cond
	 ((null? clauses) ;; should never happen
	  (reverse! rev-result))
	 ((not (pair? clauses)) ;; dotted form (x . y)
	  (scheme2js-error "Object-conv"
	     "bad case-form"
	     clauses
	     clauses))
	 ((null? (cdr clauses))
	  (let ((rev-all-clauses (cons (clause->pobject (car clauses) #t)
				    rev-result)))
	     ;; if there was no default clause, we add one.
	     (if (with-access::Clause (car rev-all-clauses) (default-clause?)
		    default-clause?)
		 (reverse! rev-all-clauses)
		 (reverse! (cons (clause->pobject '(else #unspecified) #t)
			      rev-all-clauses)))))
	 (else
	  (clauses->pobjects (cdr clauses)
	     (cons (clause->pobject (car clauses) #f)
		rev-result)))))
   
   (instantiate::Case
      (key (scheme->pobject/loc key loc procname))
      (clauses (clauses->pobjects clauses '()))))

;*---------------------------------------------------------------------*/
;*    js-field-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-field-ref l)
   (let loop ((l (cdr l))
	      (n (car l)))
      (let ((n `(js-field ,n ,(symbol->string (car l)))))
	 (if (null? (cdr l))
	     n
	     (loop (cdr l) n)))))

;*---------------------------------------------------------------------*/
;*    js-field-set ...                                                 */
;*---------------------------------------------------------------------*/
(define (js-field-set l exp)
   (let loop ((l (cdr l))
	      (n (car l)))
      (if (null? (cdr l))
	  `(js-field-set! ,n ,(symbol->string (car l)) ,exp)
	  (loop (cdr l) `(js-field ,n ,(symbol->string (car l)))))))

;*---------------------------------------------------------------------*/
;*    js-method-call ...                                               */
;*---------------------------------------------------------------------*/
(define (js-method-call l args)
   (let loop ((l (cdr l))
	      (n (car l)))
      (if (null? (cdr l))
	  `(js-method-call ,n ,(symbol->string (car l)) ,@args)
	  (loop (cdr l) `(js-field ,n ,(symbol->string (car l)))))))

;*---------------------------------------------------------------------*/
;*    epairify ...                                                     */
;*---------------------------------------------------------------------*/
(define (epairfy dest src)
   (if (epair? src)
       (econs (car dest) (cdr dest) (cer src))
       dest))

;*---------------------------------------------------------------------*/
;*    typed-lambda? ...                                                */
;*---------------------------------------------------------------------*/
(define (typed-lambda? id)
   (when (symbol? id)
      (multiple-value-bind (id type-name)
	 (parse-ident id)
	 (and type-name (eq? id 'lambda)))))

;*---------------------------------------------------------------------*/
;*    scheme->pobject/loc ...                                          */
;*---------------------------------------------------------------------*/
(define (scheme->pobject/loc exp loc procname)
   (cond
      ((pair? exp)
       (let ((loc (find-location exp loc)))
	  (match-case exp
	     ((quote ?datum)
	      (instantiate::Const
		 (location loc)
		 (value datum)))
	     ((lambda ?formals . ?body)
	      (lambda->pobject #f formals body loc procname))
	     ((if ?test ?then)
	      (set-cdr! (cddr exp) '(#f))
	      (scheme->pobject/loc exp loc procname))
	     ((if ?test ?then ?else)
	      (instantiate::If
		 (location loc)
		 (test (scheme->pobject test (find-location (cdr exp) loc)))
		 (then (scheme->pobject then (find-location (cddr exp) loc)))
		 (else (scheme->pobject else (find-location (cdddr exp) loc)))))
	     ((if . ?L)
	      (scheme2js-error #f "bad if-form" exp exp))
	     ((case ?key . ?clauses)
	      (case->pobject key clauses loc procname))
	     ((set! (and ?var (? symbol?)) ?expr)
	      (instantiate::Set!
		 (location loc)
		 (lvalue (instantiate::Ref
			    (id var)
			    (location (find-location (cdr exp) loc))))
		 (val (scheme->pobject expr (find-location (cddr exp) loc)))))
	     ((set! (-> . ?l) ?val)
	      (if (and (pair? l) (pair? (cdr l)) (every symbol? (cdr l)))
		  ;; field set
		  (scheme->pobject/loc (epairfy (js-field-set l val) exp)
		     loc procname)
		  (error "scheme2js" "bad set!-form" exp)))
	     ((set! (@ ?sym ?qualifier) ?expr)
	      (let ((id (cadr exp)))
		 (instantiate::Set!
		    (location loc)
		    (lvalue (attach-location!
			       (instantiate::Ref (id id))
			       (location (find-location (cdr exp) loc))))
		    (val (scheme->pobject expr (find-location (cddr exp) loc))))))
	     ((set! . ?L)
	      (scheme2js-error #f "bad set!-form" exp exp))
	     ((let ?bindings . ?body)
	      (let-form->pobject bindings body 'let loc procname))
	     ((letrec ?bindings . ?body)
	      (let-form->pobject bindings body 'letrec loc procname))
	     ((begin . ?body)
	      (instantiate::Begin
		 (location loc)
		 (exprs (scheme->pobject-map/loc body loc procname))))
	     ((define ?var ?expr)
	      (let ((id (id-of-id var)))
		 (instantiate::Define
		    (location loc)
		    (lvalue (instantiate::Ref
			       (location (find-location (cdr exp) loc))
			       (id id)))
		    (val (scheme->pobject/loc expr
			    (find-location (cddr exp) loc) id)))))
	     ((pragma ?str . ?args)
	      (if (string? str)
		  (instantiate::Pragma
		     (str str)
		     (location loc)
		     (args (map (lambda (a)
				   (scheme->pobject/loc a
				      (find-location args loc) procname))
			      args)))
		  (scheme2js-error #f "bad pragma-form" exp exp)))
	     ((runtime-ref ?id (? procedure?))
	      (instantiate::Runtime-Ref
		 (location loc)
		 (id id)))
	     ((-> . ?l)
	      (if (and (pair? l) (pair? (cdr l)) (every symbol? (cddr l)))
		  (scheme->pobject/loc (epairfy (js-field-ref l) exp)
		     loc procname)
		  (error "scheme2js" "bad field access" exp)))
	     ((@ ?sym ?qualifier)
	      (instantiate::Ref
		 (location loc)
		 (id (cdr exp))))
	     (((-> . ?l) . ?args)
	      (if (and (pair? l) (pair? (cdr l)) (every symbol? (cdr l)))
		  (scheme->pobject/loc (epairfy (js-method-call l args) exp)
		     loc procname)
		  (error "scheme2js" "illegal method invocation" exp)))
	     (((and ?tlambda (? typed-lambda?)) ?formals . ?body)
	      (multiple-value-bind (id type-name)
		 (parse-ident tlambda)
		 (lambda->pobject type-name formals body loc procname)))
	     ((?operator . ?operands)
	      (if (and (config 'return) (eq? operator 'return!))
		  (if (or (null? operands) (not (null? (cdr operands))))
		      (scheme2js-error #f "bad return! form: " exp exp)
		      (instantiate::Return
			 (location loc)
			 (val (scheme->pobject (car operands)
				 (find-location operands loc)))))
		  (instantiate::Call
		     (location loc)
		     (operator (scheme->pobject operator
				  (find-location operator loc)))
		     (operands (scheme->pobject-map/loc operands
				  loc procname))))))))
      ((eq? exp #unspecified)
       (instantiate::Const
	  (location loc)
	  (value #unspecified)))
      ;; unquoted symbols must be var-refs
      ((symbol? exp)
       (instantiate::Ref
	  (location loc)
	  (id exp)))
      ((vector? exp)
       (scheme2js-error #f "vectors must be quoted" exp exp))
      (else
       (instantiate::Const
	  (location loc)
	  (value exp)))))
      
;*---------------------------------------------------------------------*/
;*    scheme->pobject ...                                              */
;*---------------------------------------------------------------------*/
(define (scheme->pobject exp loc)
   (attach-location! (scheme->pobject/loc exp (find-location exp loc) #f) loc))
