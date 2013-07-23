;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/pobject_conv.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Thu Nov 24 07:23:39 2011                          */
;*    Last change :  Tue Jul 23 09:52:57 2013 (serrano)                */
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
;*    scheme->pobject-map ...                                          */
;*---------------------------------------------------------------------*/
(define (scheme->pobject-map l)
   (let loop ((l l)
	      (rev-res '()))
      (cond
	 ((null? l)
	  (reverse! rev-res))
	 ((not (pair? l))
	  (scheme2js-error "Object-conv" "invalid expression-list" l l))
	 (else
	  (let ((loc (location l)))
	     (loop (cdr l)
		(cons (scheme->pobject (car l) loc)
		   rev-res)))))))

;*---------------------------------------------------------------------*/
;*    location-map ...                                                 */
;*---------------------------------------------------------------------*/
(define (location-map f l)
   (let loop ((l l)
	      (rev-res '()))
      (if (null? l)
	  (reverse! rev-res)
	  (let ((loc (location l)))
	     (loop (cdr l)
		(cons (f (car l) loc) rev-res))))))

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

(define (expr-list->Begin expr-list)
   (instantiate::Begin
      (exprs (scheme->pobject-map expr-list))))

;*---------------------------------------------------------------------*/
;*    lambda->object ...                                               */
;*---------------------------------------------------------------------*/
(define (lambda->pobject arguments body)
   
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
   
   (receive (formals vaarg?)
      (vaarg-list! arguments)
      
      (unless (and (list? formals) (every symbol? formals))
	 (scheme2js-error "Object-conv"
	    "Invalid arguments-clause"
	    arguments
	    arguments))
      
      (let ((formal-decls
	       (location-map (lambda (formal loc)
				(attach-location!
				   (instantiate::Ref
				      (id (id-of-id formal)))
				   loc))
		  formals)))
	 (instantiate::Lambda
	    (formals formal-decls)
	    (vaarg? vaarg?)
	    (body (instantiate::Return
		     (val (expr-list->Begin body))))))))

;*---------------------------------------------------------------------*/
;*    let-form->pobject ...                                            */
;*---------------------------------------------------------------------*/
(define (let-form->pobject bindings body kind)
   
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
	    (val (cadr binding)))
	 (instantiate::Set!
	    (location -1)
	    (lvalue (attach-location!
		       (instantiate::Ref (id var))
		       (location binding)))
	    (val (scheme->pobject val (location (cdr binding)))))))
   
   (let ((pobject-bindings (map binding->pobject bindings)))
      (instantiate::Let
	 (bindings pobject-bindings)
	 (body (expr-list->Begin body))
	 (kind kind))))

;*---------------------------------------------------------------------*/
;*    case->object ...                                                 */
;*---------------------------------------------------------------------*/
(define (case->pobject key clauses)
   
   (define (clause->pobject clause last?)
      (match-case clause
	 ((?consts . ?raw-exprs)
	  (let* ((exprs (scheme->pobject-map raw-exprs))
		 (begin-expr (instantiate::Begin (exprs exprs))))
	     (if (and last?
		      (eq? consts 'else))
		 (instantiate::Clause
		    (consts '())
		    (expr begin-expr)
		    (default-clause? #t))
		 (begin
		    (unless (list? consts)
		       (scheme2js-error "Object-conv"
			  "bad constants in case-clause"
			  consts
			  consts))
		    (instantiate::Clause
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
      (key (scheme->pobject-no-loc key))
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
;*    scheme->pobject-no-loc ...                                       */
;*---------------------------------------------------------------------*/
(define (scheme->pobject-no-loc exp)
   (cond
      ((pair? exp)
       (match-case exp
	  ((quote ?datum) (instantiate::Const (value datum)))
	  ((lambda ?formals . ?body) (lambda->pobject formals body))
	  ((if ?test ?then)
	   ;(scheme->pobject `(if ,test ,then #f))
	   (set-cdr! (cddr exp) '(#f))
	   (scheme->pobject-no-loc exp))
	  ((if ?test ?then ?else)
	   (instantiate::If
	      (test (scheme->pobject test (location (cdr exp))))
	      (then (scheme->pobject then (location (cddr exp))))
	      (else (scheme->pobject else (location (cdddr exp))))))
	  ((if . ?L) (scheme2js-error #f "bad if-form" exp exp))
	  ((case ?key . ?clauses)
	   (case->pobject key clauses))
	  ((set! (and ?var (? symbol?)) ?expr)
	   (instantiate::Set!
	      (location -2)
	      (lvalue (attach-location!
			 (instantiate::Ref (id var))
			 (location (cdr exp))))
	      (val (scheme->pobject expr (location (cddr exp))))))
	  ((set! (-> . ?l) ?val)
	   (if (and (pair? l) (pair? (cdr l)) (every symbol? (cdr l)))
	       ;; field set
	       (scheme->pobject-no-loc (epairfy (js-field-set l val) exp))
	       (error "scheme2js" "bad set!-form" exp)))
	  ((set! (@ ?sym ?qualifier) ?expr)
	   (let ((id (cadr exp)))
	      (instantiate::Set!
		 (location -3)
		 (lvalue (attach-location!
			    (instantiate::Ref (id id))
			    (location (cdr exp))))
		 (val (scheme->pobject expr (location (cddr exp)))))))
	  ((set! . ?L) (scheme2js-error #f "bad set!-form" exp exp))
	  ((let ?bindings . ?body) (let-form->pobject bindings body 'let))
	  ((letrec ?bindings . ?body) (let-form->pobject bindings body 'letrec))
	  ((begin . ?body) (instantiate::Begin (exprs (scheme->pobject-map body))))
	  ((define ?var ?expr)
	   (instantiate::Define
	      (lvalue (attach-location! (instantiate::Ref
					   (id (id-of-id var)))
			 (location (cdr exp))))
	      (val (scheme->pobject expr (location (cddr exp))))))
	  ((pragma ?str . ?args)
	   (if (string? str)
	       (instantiate::Pragma
		  (str str)
		  (args (map (lambda (a)
				(scheme->pobject a (location args)))
			   args)))
	       (scheme2js-error #f "bad pragma-form" exp exp)))
	  ((runtime-ref ?id (? procedure?))
	   (instantiate::Runtime-Ref
	      (id id)))
	  ((-> . ?l)
	   (if (and (pair? l) (pair? (cdr l)) (every symbol? (cddr l)))
	       (scheme->pobject-no-loc (epairfy (js-field-ref l) exp))
	       (error "scheme2js" "bad field access" exp)))
	  ((@ ?sym ?qualifier)
	   (instantiate::Ref (id (cdr exp))))
	  (((-> . ?l) . ?args)
	   (if (and (pair? l) (pair? (cdr l)) (every symbol? (cdr l)))
	       (scheme->pobject-no-loc (epairfy (js-method-call l args) exp))
	       (error "scheme2js" "illegal method invocation" exp)))
	  ((?operator . ?operands)
	   (if (and (config 'return)
		    (eq? operator 'return!))
	       (if (or (null? operands) (not (null? (cdr operands))))
		   (scheme2js-error #f "bad return! form: " exp exp)
		   (instantiate::Return
		      (val (scheme->pobject (car operands)
			      (location operands)))))
	       (instantiate::Call
		  (operator (scheme->pobject operator (location exp)))
		  (operands (scheme->pobject-map operands)))))))
      ((eq? exp #unspecified)
       (instantiate::Const (value #unspecified)))
      ;; unquoted symbols must be var-refs
      ((symbol? exp)
       (instantiate::Ref
	  (id exp)))
      ((vector? exp)
       (scheme2js-error #f "vectors must be quoted" exp exp))
      (else
       (instantiate::Const (value exp)))))

;*---------------------------------------------------------------------*/
;*    scheme->pobject ...                                              */
;*---------------------------------------------------------------------*/
(define (scheme->pobject exp loc)
   (attach-location! (scheme->pobject-no-loc exp) loc))
