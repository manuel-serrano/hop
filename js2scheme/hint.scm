;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/hint.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 19 10:13:17 2016                          */
;*    Last change :  Fri Dec 10 11:44:57 2021 (serrano)                */
;*    Copyright   :  2016-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hint typing.                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_type-hint

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_use
	   __js2scheme_alpha
	   __js2scheme_node-size
	   __js2scheme_usage
	   __js2scheme_scheme-utils
	   __js2scheme_classutils)

   (static (class FunHintInfo
	      hinted
	      unhinted
	      types::pair-nil))

   (export (j2s-hint!::pair-nil ::J2SProgram ::obj)
	   (generic j2s-call-hint!::J2SNode ::J2SNode ::bool conf)
	   (generic j2s-hint-block!::J2SNode ::J2SNode conf)
	   (j2s-hint-meta-noopt! ::J2SDecl)
	   (j2s-known-type ::obj)
	   (j2s-hint-type ::obj)))

;*---------------------------------------------------------------------*/
;*    *j2s-hint-block-node-size-factor* ...                            */
;*---------------------------------------------------------------------*/
(define *j2s-hint-block-node-size-factor* 30)
(define *j2s-hint-block-node-expansion-ratio* .00007)

;*---------------------------------------------------------------------*/
;*    j2s-hint! ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-hint! prgm conf)
   (with-access::J2SProgram prgm (decls nodes)
      ;; reset previously collected hints
      (for-each j2s-reset-hint decls)
      (for-each j2s-reset-hint nodes)
      ;; first collect all possible hints...
      (for-each (lambda (n) (j2s-hint n '())) decls)
      (for-each (lambda (n) (j2s-hint n '())) nodes)
      ;; then, for each function whose parameters are "hinted", generate
      ;; an ad-hoc typed version
      (if (config-get conf :optim-hint)
	  (let ((dups (append-map (lambda (d) (j2s-hint-function* d conf))
			 decls)))
	     (when (pair? dups)
		(set! decls
		   (append (filter (lambda (dup::J2SDeclFun)
				      (with-access::J2SDeclFun dup (usecnt scope id)
					 (and (>fx usecnt 0)
					      (not (memq scope '(letblock inner))))))
			      dups)
		      decls)))
	     (for-each (lambda (n) (j2s-call-hint! n #f conf)) decls)
	     (for-each (lambda (n) (j2s-call-hint! n #f conf)) nodes)
	     (when (config-get conf :optim-hintloop #f)
		(for-each (lambda (n) (j2s-hint-loop! n #f 0)) decls)
		(for-each (lambda (n) (j2s-hint-loop! n #f 0)) nodes))
	     dups)
	  '())))

;*---------------------------------------------------------------------*/
;*    j2s-known-type ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-known-type ty)
   (cond
      ((memq ty
	  '(any unknown magic
	    integer real number bool string
	    regexp array jsvector date arguments function arrow service
	    procedure
	    record class object promise
	    null undefined void
	    cmap scmstring tilde pair
	    int8array uint8array bigint
	    map weakmap set weakset))
       ty)
      ((memq ty '(index indexof length)) 'integer)
      ((memq ty '(ureal1 real1 real4)) 'real)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    j2s-hint-type ...                                                */
;*    -------------------------------------------------------------    */
;*    Maps to hint/tyflow understood types.                            */
;*---------------------------------------------------------------------*/
(define (j2s-hint-type ty)
   (if (symbol? ty)
       (let ((kty (j2s-known-type ty)))
	  (or kty (error "js2scheme" "Illegal tyflow/hint type" ty)))
       ty))

;*---------------------------------------------------------------------*/
;*    j2s-reset-hint ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-reset-hint this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-reset-hint ::J2SDecl ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-reset-hint this::J2SDecl)
   (with-access::J2SDecl this (hint)
      (set! hint (filter (lambda (h) (=fx (cdr h) (minvalfx))) hint))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    add-hints! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (add-hints! this::J2SNode hints::pair-nil)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    add-hints! ::J2SExpr ...                                         */
;*---------------------------------------------------------------------*/
(define-method (add-hints! expr::J2SExpr hints::pair-nil)
   
   (define (add-hint! expr type inc)
      (with-access::J2SExpr expr (hint)
	 (let ((c (assq type hint)))
	    (cond
	       ((not (pair? c))
		(set! hint (cons (cons type inc) hint)))
	       ((=fx inc (minvalfx))
		(set-cdr! c  inc))
	       ((>=fx (cdr c) 0)
		(set-cdr! c (+fx inc (cdr c))))))))

   (with-access::J2SExpr expr (hint)
      (when (pair? hints)
	 (for-each (lambda (hi)
		      (let ((ty (car hi))
			    (inc (cdr hi)))
			 (unless (memq ty '(unknown any))
			    (add-hint! expr ty inc))))
	    hints))))
   
;*---------------------------------------------------------------------*/
;*    add-hints! ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-method (add-hints! decl::J2SDecl hints::pair-nil)
   
   (define (add-hint! decl type inc)
      (with-access::J2SDecl decl (id hint vtype)
	 (let ((c (assq type hint)))
	    (cond
	       ((not (pair? c))
		(set! hint (cons (cons type inc) hint)))
	       ((=fx inc (minvalfx))
		(set-cdr! c  inc))
	       ((>=fx (cdr c) 0)
		(set-cdr! c (+fx inc (cdr c))))))))
   
   (with-access::J2SDecl decl (id hint itype vtype)
      (when (and (pair? hints)
		 (memq itype '(unknown any number))
		 (memq vtype '(unknown any number)))
	 (for-each (lambda (hi)
		      (let ((ty (car hi))
			    (inc (cdr hi)))
			 (unless (memq ty '(unknown any))
			    (add-hint! decl ty inc))))
	    hints))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SNode hints::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SIf ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SIf hints::pair-nil)
   (with-access::J2SIf this (test then else)
      (j2s-hint test (cons '(bool . 2) hints))
      (j2s-hint then hints)
      (j2s-hint else hints)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SRef hints)
   (with-access::J2SRef this (decl loc type)
      (let ((dh (if (isa? decl J2SThis) (cons '(object . 100) hints) hints)))
	 (add-hints! decl dh))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SExpr hints)
   (multiple-value-bind (op decl type ref loc)
      (j2s-expr-type-test this)
      (if op
	  (add-hints! decl `((,type . 2)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-binary ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-hint-binary op lhs rhs hints)
   (case op
      ((<< >> >>> ^ & BIT_OR)
       (j2s-hint rhs  '((integer . 5)))
       (j2s-hint lhs '((integer . 5))))
      ((&& OR OR*)
       (j2s-hint rhs  (cons '(bool . 10) hints))
       (j2s-hint lhs  (cons '(bool . 10) hints)))
      ((- * /)
       (case (j2s-type lhs)
	  ((real)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((real . 5) (string . ,(minvalfx)))))
	  ((integer)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((integer . 3) (string . ,(minvalfx)))))
	  (else
	   (j2s-hint lhs `((string . ,(minvalfx))))
	   (j2s-hint rhs `((integer . 2) (real . 2) (string . ,(minvalfx))))))
       (case (j2s-type rhs)
	  ((real)
	   (j2s-hint lhs `((real . 5) (string . ,(minvalfx))))
	   (j2s-hint rhs '()))
	  ((integer)
	   (j2s-hint lhs `((integer . 3) (string . ,(minvalfx))))
	   (j2s-hint rhs '()))
	  (else
	   (j2s-hint lhs `((integer . 2) (real . 2) (string . ,(minvalfx))))
	   (j2s-hint rhs `((string . ,(minvalfx)))))))
      ((< <= >= >)
       
       (case (j2s-type lhs)
	  ((real)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((real . 5) (string . ,(minvalfx)))))
	  ((integer)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((integer . 3) (string . ,(minvalfx)))))
	  (else
	   (j2s-hint rhs '((integer . 2) (real . 2)))))
       (case (j2s-type rhs)
	  ((real)
	   (j2s-hint lhs `((real . 5) (string . ,(minvalfx))))
	   (j2s-hint rhs '()))
	  ((integer)
	   (j2s-hint lhs `((integer . 3) (string . ,(minvalfx))))
	   (j2s-hint rhs '()))
	  (else
	   (j2s-hint lhs '((integer . 2) (real . 2))))))
      ((%)
       (case (j2s-type lhs)
	  ((real)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((real . 5) (string . ,(minvalfx)))))
	  ((integer)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((integer . 5) (string . ,(minvalfx)))))
	  (else
	   (j2s-hint lhs `((string . ,(minvalfx))))
	   (j2s-hint rhs `((integer . 4) (string . ,(minvalfx))))))
       (case (j2s-type rhs)
	  ((real)
	   (j2s-hint lhs `((real . 5) (string . ,(minvalfx))))
	   (j2s-hint rhs '()))
	  ((integer)
	   (j2s-hint lhs `((integer . 5) (string . ,(minvalfx))))
	   (j2s-hint rhs '()))
	  (else
	   (j2s-hint lhs `((integer . 4) (string . ,(minvalfx))))
	   (j2s-hint rhs `((string . ,(minvalfx)))))))
      ((+)
       (cond
	  ((eq? (j2s-type lhs) 'real)
	   (j2s-hint rhs '((real . 5))))
	  ((eq? (j2s-type rhs) 'real)
	   (j2s-hint rhs '((real . 5))))
	  ((eq? (j2s-type lhs) 'integer)
	   (j2s-hint rhs '((integer . 5) (real . 4))))
	  ((eq? (j2s-type rhs) 'integer)
	   (j2s-hint rhs '((integer . 5) (real . 4))))
	  ((eq? (j2s-type lhs) 'number)
	   (j2s-hint rhs '((integer . 3) (real . 3))))
	  ((eq? (j2s-type rhs) 'number)
	   (j2s-hint rhs '((integer . 3) (real . 3))))
	  ((eq? (j2s-type lhs) 'string)
	   (j2s-hint rhs '((string . 5))))
	  ((eq? (j2s-type rhs) 'string)
	   (j2s-hint lhs '((string . 5))))
	  (else
	   (j2s-hint lhs '((string . 2) (integer . 2) (real . 1)))
	   (j2s-hint rhs '((string . 2) (integer . 2) (real . 1))))))
      ((== === != !== eq?)
       (cond
	  ((isa? lhs J2SNull)
	   (j2s-hint lhs '())
	   (j2s-hint rhs '((null . 1))))
	  ((isa? rhs J2SNull)
	   (j2s-hint rhs '())
	   (j2s-hint lhs '((null . 1))))
	  ((isa? lhs J2SUndefined)
	   (j2s-hint lhs '())
	   (j2s-hint rhs '((undefined . 1))))
	  ((isa? rhs J2SUndefined)
	   (j2s-hint lhs '((undefined . 1)))
	   (j2s-hint rhs '()))
	  ((eq? (j2s-type lhs) 'integer)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((integer . 3) (string . ,(minvalfx)))))
	  ((eq? (j2s-type rhs) 'integer)
	   (j2s-hint lhs `((integer . 3) (string . ,(minvalfx))))
	   (j2s-hint rhs '()))
	  ((eq? (j2s-type lhs) 'real)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((real . 5) (string . ,(minvalfx)))))
	  ((eq? (j2s-type lhs) 'real)
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((real . 5) (string . ,(minvalfx)))))
	  ((not (memq (j2s-type lhs) '(any unknown)))
	   (j2s-hint lhs '())
	   (j2s-hint rhs `((,(j2s-type lhs) . 5))))
	  ((not (memq (j2s-type rhs) '(any unknown)))
	   (j2s-hint lhs `((,(j2s-type lhs) . 5)))
	   (j2s-hint rhs '()))
	  (else
	   (j2s-hint lhs hints)
	   (j2s-hint rhs hints))))
      ((instanceof)
       (j2s-hint rhs '((function . 5)))
       (j2s-hint lhs '((object . 2) (function . 1) (array . 1))))
      (else
       (j2s-hint lhs hints)
       (j2s-hint rhs hints))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SBinary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SBinary hints)
   (with-access::J2SBinary this (op lhs rhs)
      (j2s-hint-binary op lhs rhs hints)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SSwitch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SSwitch hints)
   
   (define (cases-hints cases)
      (let ((ty 'unknown))
	 (for-each (lambda (c)
		      (when ty
			 (unless (isa? c J2SDefault)
			    (with-access::J2SCase c (expr)
			       (case (j2s-type expr)
				  ((integer)
				   (case ty
				      ((unknown) (set! ty 'integer))
				      ((integer) #unspecified)
				      (else (set! ty #f))))
				  ((string)
				   (case ty
				      ((unknown) (set! ty 'string))
				      ((string) #unspecified)
				      (else (set! ty #f)))))))))
	    cases)
	 (if (and ty (not (memq ty '(unknown any))))
	     `((,ty . 5))
	     `((integer . 4) (string . 3)))))
      
   (with-access::J2SSwitch this (key cases)
      (j2s-hint key (cases-hints cases))
      (for-each (lambda (cases)
		   (j2s-hint cases hints))
	 cases)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SDeclInit hints)
   (with-access::J2SDeclInit this (val vtype hint loc id)
      (let ((ty (j2s-type val)))
	 (when (and (symbol? ty) (not (memq ty '(unknown any))))
	    (add-hints! this `((,ty . 3))))
	 (when (isa? val J2SRef)
	    (with-access::J2SRef val (decl)
	       (with-access::J2SDecl decl (hint)
		  (add-hints! this hint)))))
      (let ((bc (if (decl-usage-has? this '(get))
		    100
		    (multiple-value-bind (bt bc)
		       (best-hint-type this #t)
		       (if (eq? bt 'string) 0 bc)))))
	 (when (> bc 0)
	    ;; if the variable is not a string, neither is the rhs
	    (with-access::J2SExpr val (hint)
	       (add-hints! this `((string . ,(minvalfx)))))))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    j2s-hint-access ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-hint-access this maybe-string)
   (with-access::J2SAccess this (obj field loc)
      (let loop ((field field))
	 (cond
	    ((isa? field J2SString)
	     (with-access::J2SString field (val loc)
		(if (string=? val "length")
		    (if maybe-string
			(j2s-hint obj `((array . 5) (string . 5) (object . 2) (integer . ,(minvalfx))))
			(j2s-hint obj `((array . 5) (string . ,(minvalfx)) (object . 2) (integer . ,(minvalfx)))))
		    (j2s-hint obj '((object . 5))))))
	    ((or (isa? field J2SNumber) (type-number? (j2s-type field)))
	     (if maybe-string
		 (j2s-hint obj '((array . 5) (string . 5)))
		 (j2s-hint obj `((array . 5) (string . ,(minvalfx))))))
	    ((isa? field J2SLiteralCnst)
	     (with-access::J2SLiteralCnst field (val)
		(loop val)))
	    (else
	     (j2s-hint field '((string . 2) (integer . 2)))
	     (if maybe-string
		 (j2s-hint obj '((object . 5) (array . 5)))
		 (j2s-hint obj `((object . 5) (array . 5) (string . ,(minvalfx))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAccess hints)
   (add-hints! this hints)
   (j2s-hint-access this #t))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAssig ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAssig hints)
   (with-access::J2SAssig this (lhs rhs loc)
      (when (isa? lhs J2SAccess) (j2s-hint-access lhs #f))
      (j2s-hint rhs '())))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAssigOp ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAssigOp hints)
   (with-access::J2SAssigOp this (op lhs rhs)
      (j2s-hint-binary op lhs rhs hints)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPostfix ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPostfix hints)
   (with-access::J2SPostfix this (lhs)
      (if (isa? lhs J2SAccess)
	  (j2s-hint-access lhs #f)
	  (j2s-hint lhs '((integer . 5))))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPrefix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPrefix hints)
   (with-access::J2SPrefix this (lhs)
      (if (isa? lhs J2SAccess)
	  (j2s-hint-access lhs #f)
	  (j2s-hint lhs '((integer . 5))))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SNew ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SNew hints)
   (with-access::J2SNew this (clazz args type)
      (j2s-hint clazz '((function . 5)))
      (when (and (eq? type 'array) (pair? args) (null? (cdr args)))
	 (unless (eq? (j2s-type (car args)) 'integer)
	    (j2s-hint (car args) '((integer . 2)))))
      (for-each (lambda (a) (j2s-hint a '())) args)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SFun hints)
   (with-access::J2SFun this (body loc name)
      (j2s-hint body hints)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SCall hints)

   (define (hint-unknown-call callee args)
      (for-each (lambda (a) (j2s-hint a '())) args)
      (j2s-hint callee '((function . 5))))
   
   (define (hint-known-call callee args)
      (with-access::J2SFun callee (params)
	 (let loop ((args args)
		    (params params))
	    (when (pair? args)
	       (if (pair? params)
		   (with-access::J2SDecl (car params) (vtype)
		      (let ((hints (if (memq vtype '(any unknown))
				       '()
				       `((,vtype . 2)))))
			 (j2s-hint (car args) hints)
			 (loop (cdr args) (cdr params))))
		   (for-each (lambda (a) (j2s-hint a '())) args))))))

   (define (hint-fun-call callee args)
      (with-access::J2SFun callee (body)
	 (j2s-hint body '())
	 (hint-known-call callee args)))
   
   (define (hint-ref-call callee args)
      (with-access::J2SRef callee (decl)
	 (cond
	    ((isa? decl J2SDeclFun)
	     (with-access::J2SDeclFun decl (val)
		(if (and (not (decl-usage-has? decl '(assig))) (isa? val J2SFun))
		    (hint-known-call val args)
		    (hint-unknown-call callee args))))
	    ((isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (val)
		(if (and (not (decl-usage-has? decl '(assig))) (isa? val J2SFun))
		    (hint-known-call val args)
		    (hint-unknown-call callee args))))
	    (else
	     (hint-unknown-call callee args)))))

   (define (receiver-hint-object? rtys)
      ;; a receiver is hintted object if and only if it has several
      ;; possible possible hints
      (when (pair? rtys)
	 (let loop ((rtys rtys)
		    (armed #f))
	    (cond
	       ((null? rtys) #f)
	       (armed #t)
	       (else (loop (cdr rtys) #t))))))
		    
   (define (hint-access-call callee args)
      (with-access::J2SAccess callee (obj field loc)
	 (let* ((fn (j2s-field-name field))
		(tys (if (string? fn)
			 (guess-builtin-method-type obj fn)
			 '(any any))))
	    ;; hint the receiver object
	    (let ((hints (cond
			    ((eq? (cadr tys) 'any)
			     '((object . 5)))
			    ((receiver-hint-object? (cadr tys))
			     (cons '(object . 5)
				(map (lambda (t)
					`(,(j2s-hint-type t) . 2))
				   (cadr tys))))
			    ((not (pair? (cadr tys)))
			     `((object . 2)
			       (,(j2s-hint-type (cadr tys)) . 5)))
			    (else
			     `((object . 3)
			       ,@(map (lambda (t)
					 `(,(j2s-hint-type t) . 5))
				    (cadr tys)))))))
	       (j2s-hint obj hints))
	    ;; hint the arguments
	    (let loop ((args args)
		       (tys (cddr tys)))
	       (when (pair? args)
		  (if (pair? tys)
		      (let ((hints (if (pair? (car tys))
				       (map (lambda (t)
					       `(,(j2s-hint-type t) . 2))
					  (car tys))
				       `((,(j2s-hint-type (car tys)) . 4)))))
			 (j2s-hint (car args) hints)
			 (loop (cdr args) (cdr tys)))
		      (for-each (lambda (a) (j2s-hint a '())) args)))))))
   
   (with-access::J2SCall this (fun args)
      (cond
	 ((isa? fun J2SFun) (hint-fun-call fun args))
	 ((isa? fun J2SRef) (hint-ref-call fun args))
	 ((isa? fun J2SAccess) (hint-access-call fun args))
	 ((isa? fun J2SGlobalRef) (hint-unknown-call fun args))
	 (else (hint-unknown-call fun args)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SFor hints)
   (with-access::J2SFor this (init test incr body)
      (j2s-hint init '())
      (j2s-hint test '((bool . 2)))
      (j2s-hint incr '())
      (j2s-hint body '())))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SWhile ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SWhile hints)
   (with-access::J2SWhile this (test body)
      (j2s-hint test '((bool . 2)))
      (j2s-hint body '())))
   
;*---------------------------------------------------------------------*/
;*    j2s-hint-function* ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-function* ::J2SBlock ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SBlock conf)
   (with-access::J2SBlock this (nodes)
      (let ((ndecls (append-map (lambda (node)
				   (if (isa? node J2SDeclFun)
				       (j2s-hint-function* node conf)
				       '()))
		       nodes)))
	 (set! nodes (append ndecls nodes))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-function* ::J2SLetBlock ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SLetBlock conf)
   (with-access::J2SLetBlock this (decls nodes)
      (let ((ndecls (append-map (lambda (decl)
				   (j2s-hint-function* decl conf))
		       decls)))
	 (set! decls (append ndecls decls))
	 (call-next-method))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-function* ::J2SDeclFun ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SDeclFun conf)
   
   (define (call-hinted orig idthis params types)
      (with-access::J2SDeclFun this (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (protocol 'bounce)
	    (fun (instantiate::J2SRef
		    (decl orig)
		    (loc loc)))
	    (thisargs (list (instantiate::J2SHopRef
			      (loc loc)
			      (type 'any)
			      (id idthis))))
	    (args (map (lambda (p::J2SDecl type)
			  (with-access::J2SDecl p (hint)
			     (instantiate::J2SRef
				(loc loc)
				(type type)
				(decl p))))
		     params types)))))

   (define (call-error orig idthis params types)
      (with-access::J2SDeclFun this (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (fun (instantiate::J2SHopRef
		    (id 'js-raise-utype-error)
		    (rtype 'magic)
		    (loc loc)))
	    (thisargs (list (instantiate::J2SHopRef
			      (loc loc)
			      (type 'any)
			      (id '%this))))
	    (args (list
		     (J2SArray* (length params)
			(map (lambda (p::J2SDecl type)
				(with-access::J2SDecl p (loc id)
				   (J2SArray
				      (J2SString (symbol->string type))
				      (J2SString (symbol->string id))
				      (J2SString (cadr loc))
				      (J2SNumber (caddr loc)))))
			   params types))
		     (J2SArray* (length params)
			(map (lambda (p::J2SDecl)
				(with-access::J2SDecl p (hint)
				   (instantiate::J2SRef
				      (loc loc)
				      (type 'any)
				      (decl p))))
			   params)))))))

   (define (dispatch-body body pred callt callu fun::J2SFun)
      (with-access::J2SBlock body (loc endloc)
	 (instantiate::J2SBlock
	    (loc loc)
	    (endloc endloc)
	    (nodes (list (instantiate::J2SIf
			    (loc loc)
			    (test pred)
			    (then (instantiate::J2SReturn
				     (from fun)
				     (loc loc)
				     (expr callt)))
			    (else (instantiate::J2SReturn
				     (from fun)
				     (loc loc)
				     (expr callu)))))))))
   
   (define (unparen expr::J2SExpr)
      (if (isa? expr J2SParen)
	  (with-access::J2SParen expr (expr)
	     (unparen expr))
	  expr))
   
   (define (typeof? expr::J2SExpr)
      (let ((ue (unparen expr)))
	 (when (isa? ue J2SUnary)
	    (with-access::J2SUnary ue (op)
	       (eq? op 'typeof)))))
   
   (define (type-checker? fun::J2SFun)
      
      (define (check-node? node)
	 (when (isa? node J2SReturn)
	    (with-access::J2SReturn node (expr)
	       (let ((ue (unparen expr)))
		  (when (isa? ue J2SBinary)
		     (with-access::J2SBinary ue (op lhs rhs)
			(or (eq? op 'instanceof)
			    (when (memq op '(== != === !== eq?))
			       (or (typeof? lhs) (typeof? rhs))))))))))
      
      (define (block-check-node? node)
	 (when (isa? node J2SBlock)
	    (with-access::J2SBlock node (nodes)
	       (match-case nodes
		  (((? check-node?)) #t)
		  (else #f)))))
      
      (with-access::J2SFun fun (body)
	 (with-access::J2SBlock body (nodes)
	    (match-case nodes
	       (((? profile-node?) (? block-check-node?)) #t)
	       (((? check-node?)) #t)
	       (else #f)))))

   (define (hopscript-utype? val::J2SFun)
      (with-access::J2SFun val (params)
	 (when (config-get conf :mode 'hopscript)
	    (any (lambda (p)
		    (with-access::J2SDecl p (utype)
		       (not (eq? utype 'unknown))))
	       params))))

   (define (imprecise-params-type? params)
      (any (lambda (p)
	      (with-access::J2SDecl p (vtype itype)
		 ;; at least one parameter is not precisely typed
		 (and (memq vtype '(unknown number any))
		      (memq itype '(unknown number any)))))
	 params))
   
   (define (fun-duplicable? decl::J2SDeclFun)
      ;; returns #t iff the function is duplicable, returns #f otherwise
      (with-access::J2SDeclFun this (val id %info hintinfo)
	 (let ((val (j2sdeclinit-val-fun this)))
	    (when (isa? val J2SFun)
	       (with-access::J2SFun val (params vararg body)
		  (let ((dup (and (not (isa? %info J2SDecl))
				  (not (isa? hintinfo FunHintInfo))
				  (not (isa? hintinfo J2SDeclFun))
				  (not vararg)
				  (not (isa? val J2SSvc))
				  (not (type-checker? val))
				  (or (hopscript-utype? val)
				      (imprecise-params-type? params)))))
		     (when (>=fx (config-get conf :verbose 0) 6)
			(with-output-to-port (current-error-port)
			   (lambda ()
			      (display* " [" id 
				 (if dup " dup " " no-dup ")
				 (not (isa? %info J2SDecl)) " " 
				 (not (isa? hintinfo FunHintInfo)) " " 
				 (not (isa? hintinfo J2SDeclFun)) " " 
				 (not vararg) " " 
				 (not (isa? val J2SSvc)) " "
				 (map (lambda (p)
					 (with-access::J2SDecl p (vtype itype)
					    (cons (type->sexp vtype) (type->sexp itype))))
				    params) " " 
				 (not (type-checker? val))
				 "]"))))
		     (and dup (config-get conf :optim-hintfun #f))))))))
   
   (define (typed? decl::J2SDeclFun)
      ;; return #t iff the function's arguments are all typed
      (with-access::J2SDeclFun this (val id %info hintinfo)
	 (when (isa? val J2SFun)
	    (with-access::J2SFun val (params vararg)
	       (and (not (isa? %info J2SDecl))
		    (not (isa? hintinfo FunHintInfo))
		    (not vararg)
		    (not (isa? val J2SSvc))
		    (pair? params)
		    (any (lambda (p::J2SDecl)
			    (with-access::J2SDecl p (hint usecnt vtype)
			       (when (>fx usecnt 0)
				  (not (memq vtype '(unknown any))))))
		       params))))))
   
   (define (fun-dispatch! fun::J2SDecl htypes::pair-nil ft vtypes::pair-nil fu)
      (with-access::J2SDeclFun this (id)
	 (let ((val (j2sdeclinit-val-fun this)))
	    (with-access::J2SFun val (params body idthis loc)
	       (let* ((newparams (map j2sdecl-duplicate params vtypes))
		      (pred (test-hint-decls newparams htypes loc))
		      (callt (call-hinted ft idthis newparams htypes))
		      (callu (call-hinted fu idthis newparams vtypes))
		      (disp (dispatch-body body pred callt callu val)))
		  (set! params newparams)
		  (set! body disp)
		  (when (config-get conf :profile-hint)
		     (profile-hint! this id 'dispatch)))))))

   (define (fun-utype-dispatch! fun::J2SDecl htypes::pair-nil ft fu)
      (with-access::J2SDeclFun this (id)
	 (let ((val (j2sdeclinit-val-fun this)))
	    (with-access::J2SFun val (params body idthis loc)
	       (let* ((newparams (map j2sdecl-duplicate-as-any params))
		      (pred (test-hint-decls newparams htypes loc))
		      (callt (call-hinted ft idthis newparams htypes))
		      (calle (call-error ft idthis newparams htypes))
		      (disp (dispatch-body body pred callt calle val)))
		  (with-access::J2SFun (j2sdeclinit-val-fun fu) (rtype rutype)
		     (set! rtype 'any)
		     (set! rutype 'unknown))
		  (set! params newparams)
		  (set! body disp)
		  (when (config-get conf :profile-hint)
		     (profile-hint! this id 'dispatch)))))))
   
   (define (duplicated? fun::J2SDecl)
      (with-access::J2SDeclFun fun (%info)
	 (when (isa? %info J2SDeclFun)
	    (with-access::J2SDeclFun %info (hintinfo)
	       (when (isa? hintinfo FunHintInfo)
		  (with-access::FunHintInfo hintinfo (unhinted hinted)
		     (or (eq? hinted fun) (eq? unhinted fun))))))))

   (define (megamorphic-hint? hint)
      ;; a hint is megamorphic if more than N=4 types are hinted
      ;; and if the maximal type is no greater than twice the hint mean
      (and (>=fx (length hint) 4)
	   (let* ((w (map cdr hint))
		  (max (apply max w))
		  (mean (/ (apply + w) (length w))))
	      (< max (* mean 2)))))

   (define (interesting-hint-type? hint)
      ;; only use positive hint for duplication
      (>fx (cdr hint) 0))
   
   (define (param-best-hint-type p::J2SDecl)
      (with-access::J2SDecl p (hint usecnt useinloop vtype id)
	 (let ((hint (filter interesting-hint-type? hint)))
	    (cond
	       ((null? hint)
		'(any 0 0))
	       ((megamorphic-hint? hint)
		;; a megamorphic parameter, don't specialize it
		'(any 0 0))
	       (else
		(multiple-value-bind (bt bc)
		   (best-hint-type p #t)
		   (let ((c (if useinloop (*fx 2 (* bc usecnt)) (* bc usecnt))))
		      (list bt c usecnt))))))))
   
   (define (score-duplicate? score besthints body)
      (or (>fx score (*fx 5 (length besthints)))
	  (>fx (*fx 20 (apply + (map caddr besthints))) (node-size body))
	  (> (/ (length (filter (lambda (d) (> (cadr d) 0)) besthints))
		(length besthints))
	     0.4)))
   
   (with-access::J2SDeclFun this (id %info hintinfo)
      (let loop ((dup (fun-duplicable? this)))
	 (cond
	    (dup
	     (with-access::J2SFun (j2sdeclinit-val-fun this) (params body)
		(let* ((besthints (map param-best-hint-type params))
		       (score (apply max (map cadr besthints))))
		   (when (>=fx (config-get conf :verbose 0) 5)
		      (with-output-to-port (current-error-port)
			 (lambda ()
			    (display* " [[" id
			       " besthints:" besthints
			       " score:" score
			       "]]"))))
		   (cond
		      ((hopscript-utype? (j2sdeclinit-val-fun this))
		       (let* ((htypes (map (lambda (p)
					      (with-access::J2SDecl p (utype vtype)
						 (cond
						    ((not (eq? utype 'unknown))
						     utype)
						    ((not (eq? vtype 'unknown))
						     vtype)
						    (else
						     'any))))
					 params))
			      (fu (fun-duplicate-untyped this htypes conf))
			      (ft (fun-duplicate-typed this htypes fu conf)))
			  (fun-utype-dispatch! this htypes ft fu)
			  (list ft fu)))
		      ((not (score-duplicate? score besthints body))
		       ;; no benefit in duplicating this function because:
		       ;;   - the hintted parameters are not used frequently
		       ;;     enough;
		       ;;   - or because their hints are unlikely to
		       ;;     improve the overall function compilation
		       ;;   - or because the ratio of hinted parameters is
		       ;;     too low
		       (loop #f))
		      (else
		       (let ((htypes (map (lambda (bh p)
					     (if (< (cadr bh) 3)
						 (with-access::J2SDecl p (vtype)
						    vtype)
						 (car bh)))
					besthints params)))
			  (if (or (not (every (lambda (t) (memq t '(object any)))
					  htypes))
				  (not (self-recursive? this)))
			      ;; only hints non-recursive or
			      ;; non-object functions
			      (let* ((vtypes (map (lambda (p::J2SDecl)
						     (with-access::J2SDecl p (vtype)
							vtype))
						params))
				     (fu (fun-duplicate-untyped this htypes conf))
				     (ft (fun-duplicate-typed this htypes fu conf)))
				 (fun-dispatch! this htypes ft vtypes fu)
				 (list ft fu))
			      (loop #f))))))))
	    ((typed? this)
	     (when (config-get conf :profile-hint #f)
		(unless (profile-hint? this)
		   (profile-hint! this id 'type)))
	     '())
	    ((not (duplicated? this))
	     (when (config-get conf :profile-hint #f)
		(unless (profile-hint? this)
		   (profile-hint! this id 'notype)))
	     '())
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    best-hint-type ...                                               */
;*---------------------------------------------------------------------*/
(define (best-hint-type decl::J2SDecl normalize)
   
   (define (normalize-hint hint)
      (let loop ((l hint)
		 (r '()))
	 (cond
	    ((null? l)
	     r)
	    ((memq (caar l) '(number integer))
	     (let ((c (assq 'num r)))
		(if (pair? c)
		    (begin
		       (set-cdr! c (+fx (cdr c) (cdar l)))
		       (loop (cdr l) r))
		    (loop (cons (cons 'num (cdar l)) (cdr l)) r))))
	    (else
	     (loop (cdr l) (cons (car l) r))))))
   
   (define (return decl t c)
      (with-access::J2SDecl decl (hint)
	 (cond
	    ((eq? t 'object)
	     (cond
		((not (decl-usage-has? decl '(assig))) (values 'object c))
		((or (assq 'undefined hint) (assq 'null hint)) (values 'any 0))
		(else (values 'object c))))
	    ((not (eq? t 'num)) (values t c))
	    ((assq 'integer hint) (values 'integer c))
	    (else (values 'number c)))))
   
   (with-access::J2SDecl decl (hint)
      (let loop ((l (if normalize (normalize-hint hint) hint))
		 (t 'any)
		 (c 0))
	 (cond
	    ((null? l)
	     (return decl t c))
	    ((<=fx (cdar l) 0)
	     (loop (cdr l) t c))
	    ((>fx (cdar l) c)
	     (loop (cdr l) (caar l) (cdar l)))
	    ((and (=fx (cdar l) c) (eq? t 'string))
	     ;; in doubt, prefer arrays over strings
	     (loop (cdr l) (caar l) (cdar l)))
	    ((and (=fx (cdar l) c) (eq? t 'any))
	     ;; ... and prefer everything over "any"
	     (loop (cdr l) (caar l) (cdar l)))
	    (else
	     (loop (cdr l) t c))))))

;*---------------------------------------------------------------------*/
;*    hint-type-predicate ...                                          */
;*---------------------------------------------------------------------*/
(define (hint-type-predicate::symbol type)
   (cond
      ((eq? type 'number) 'number?)
      ((eq? type 'integer) 'fixnum?)
      ((eq? type 'string) 'js-jsstring?)
      ((eq? type 'array) 'js-array?)
      ((eq? type 'jsvector) 'js-vector?)
      ((eq? type 'object) 'js-object?)
      ((eq? type 'function) 'js-function?)
      ((eq? type 'service) 'js-service?)
      ((eq? type 'arrow) 'js-procedure?)
      ((eq? type 'bool) 'boolean?)
      ((eq? type 'undefined) 'js-undefined?)
      ((eq? type 'null) 'js-null?)
      ((eq? type 'regexp) 'js-regexp?)
      ((eq? type 'real) 'flonum?)
      ((isa? type J2SRecord) (class-predicate-id type))
      ((isa? type J2SClass) 'js-object?)
      (else
       (error "hint-type-predicate"
	  (format "Unknown hint type predicate (~a)" (typeof type))
	  (type->sexp type)))))

;*---------------------------------------------------------------------*/
;*    test-hint-decls ...                                              */
;*---------------------------------------------------------------------*/
(define (test-hint-decls decls::pair htypes::pair loc)
   
   (define (test-hint-decl param htype)
      (with-access::J2SDecl param (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (type 'bool)
	    (fun (instantiate::J2SHopRef
		    (loc loc)
		    (type 'function)
		    (rtype 'bool)
		    (id (hint-type-predicate htype))))
	    (thisargs '())
	    (args (list (instantiate::J2SRef
			   (loc loc)
			   (decl param)))))))

   (let loop ((decls decls)
	      (htypes htypes))
      (let ((decl (car decls))
	    (htype (car htypes)))
	 (with-access::J2SDecl decl (vtype)
	    (cond
	       ((null? (cdr decls))
		(if (memq htype '(unknown any))
		    (instantiate::J2SBool
		       (loc loc)
		       (val #t))
		    (test-hint-decl (car decls) htype)))
	       ((memq htype '(unknown any))
		(loop (cdr decls) (cdr htypes)))
	       (else
		(instantiate::J2SBinary
		   (loc loc)
		   (op '&&)
		   (type 'bool)
		   (lhs (test-hint-decl (car decls) htype))
		   (rhs (loop (cdr decls) (cdr htypes))))))))))

;*---------------------------------------------------------------------*/
;*    profile-hint! ...                                                */
;*---------------------------------------------------------------------*/
(define (profile-hint! fun::J2SDeclFun id::symbol attr::symbol)
   
   (define (prof val::J2SFun)
      (with-access::J2SFun val (body loc)
	 (unless (profile-node? body)
	    (let ((prof (J2SStmtExpr
			   (J2SPragma
			      `(profile-hint ,(format "~a" id) ',attr)))))
	       (set! body
		  (duplicate::J2SBlock body
		     (nodes (list prof body))))))))
   
   (with-access::J2SDeclFun fun (val loc)
      (if (isa? val J2SFun)
	  (prof val)
	  (with-access::J2SMethod val (function method)
	     (prof function)
	     (prof method)))))

;*---------------------------------------------------------------------*/
;*    profile-hint? ...                                                */
;*---------------------------------------------------------------------*/
(define (profile-hint? fun::J2SDeclFun)
   (with-access::J2SDeclFun fun (val id %info loc)
      (when (isa? val J2SFun)
	 (with-access::J2SFun val (body)
	    (with-access::J2SBlock body (nodes)
	       (when (pair? nodes)
		  (profile-node? (car nodes))))))))

;*---------------------------------------------------------------------*/
;*    profile-node? ...                                                */
;*---------------------------------------------------------------------*/
(define (profile-node? node)
   (let loop ((node node))
      (cond
	 ((isa? node J2SPragma)
	  (with-access::J2SPragma node (expr)
	     (match-case expr
		((profile-hint . ?-) #t)
		(else #f))))
	 ((isa? node J2SStmtExpr)
	  (with-access::J2SStmtExpr node (expr)
	     (loop expr)))
	 ((isa? node J2SBlock)
	  (with-access::J2SBlock node (nodes)
	     (loop (car nodes))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    fun-duplicate-untyped ...                                        */
;*---------------------------------------------------------------------*/
(define (fun-duplicate-untyped::J2SDeclFun fun::J2SDeclFun htypes conf)
   (with-access::J2SDeclFun fun (id)
      (let ((val (j2sdeclinit-val-fun fun)))
	 (with-access::J2SFun val (params body name generator idthis loc)
	    (let* ((nbody (duplicate::J2SBlock body
			     (nodes (list (J2SMeta 'hint 0 1
					     (J2SSeq body))))))
		   (nfun (duplicate::J2SDeclFun fun
			    (parent fun)
			    (key (ast-decl-key))
			    (id (symbol-append id '%%))
			    (scope 'global)
			    (usage (usage '()))
			    (writable #f)
			    (binder 'let)
			    (usecnt 1)
			    ;; MS CARE UTYPE
			    (vtype 'function)
			    (%info fun)
			    (hintinfo fun)
			    (val (duplicate::J2SFun val
				    (generator #f)
				    (optimize #f)
				    (idgen generator)
				    (idthis (if (this? body) idthis #f))
				    (name (when (symbol? name)
					     (symbol-append name '%%)))
				    (params params)
				    (rtype 'unknown)
				    (body nbody))))))
	       (with-access::J2SDeclFun nfun ((nval val))
		  (with-access::J2SFun nval (body decl)
		     ;; MS 26aug2020: previous version were missing the
		     ;; assigment so untyped functions were always generated
		     ;; as scheme closures (see beautiful-define in
		     ;; scheme-fun.scm) and called as such!
		     (set! decl nfun)
		     ;; force a copy of the three to avoid sharing with the
		     ;; typed version
		     (set! body
			(return-patch! (j2s-alpha body '() '()) val nval)))
		  (with-access::J2SFun nval (params)
		     (for-each (lambda (p h)
				  (add-hints! p `((,h . ,(minvalfx)))))
			params htypes)))
	       (when (config-get conf :profile-hint)
		  (profile-hint! nfun id 'nohint))
	       nfun)))))

;*---------------------------------------------------------------------*/
;*    fun-duplicate-typed ...                                          */
;*---------------------------------------------------------------------*/
(define (fun-duplicate-typed::J2SDeclFun fun::J2SDeclFun types unhinted conf)
   
   (define (type-initial t)
      (cond
	 ((isa? t J2SRecord)
	  #\R)
	 ((isa? t J2SClass)
	  #\C)
	 (else
	  (case t
	     ((integer) #\I)
	     ((number) #\N)
	     ((array) #\A)
	     ((jsvector) #\J)
	     ((string) #\S)
	     ((unknown) #\X)
	     ((any) #\_)
	     ((arrow) #\F)
	     (else (string-ref (symbol->string t) 0))))))
   
   (with-access::J2SDeclFun fun (id hintinfo)
      (let ((val (j2sdeclinit-val-fun fun)))
	 (with-access::J2SFun val (params body idthis generator thisp rtype)
	    (let* ((newparams (map j2sdecl-duplicate params types))
		   (newthisp (when thisp
				(with-access::J2SDecl thisp (itype)
				   (j2sdecl-duplicate thisp itype))))
		   (typeid (string->symbol
			      (string-upcase!
				 (apply string
				    (map type-initial types)))))
		   (nbody (if thisp
			      (j2s-alpha body
				 (cons thisp params)
				 (cons newthisp newparams))
			      (j2s-alpha body params newparams)))
		   (unbody (reset-type! nbody newparams))
		   (newfun (duplicate::J2SFun val
			      (generator #f)
			      (idgen generator)
			      (%info #unspecified)
			      (idthis (if (this? body) idthis #f))
			      (thisp newthisp)
			      (params newparams)
			      (rtype 'unknown)
			      (body unbody)))
		   (newdecl (duplicate::J2SDeclFun fun
			       (parent fun)
			       (key (ast-decl-key))
			       (scope 'global)
			       (id (symbol-append id '%% typeid))
			       (usage (usage '()))
			       (writable #f)
			       (binder 'let)
			       (usecnt 1)
			       (%info fun)
			       ;; MS CARE UTYPE
			       (vtype 'function)
			       (hintinfo fun)
			       (val newfun))))
	       (with-access::J2SFun newfun (body)
		  (set! body (return-patch! body val newfun)))
	       (with-access::J2SFun newfun (body)
		  (set! body (call-patch! body fun newdecl)))
	       (use-count nbody +1 0)
	       (with-access::J2SFun newfun (decl)
		  (set! decl newdecl))
	       (when (config-get conf :profile-hint)
		  (profile-hint! newdecl id 'hint))
	       (set! hintinfo
		  (instantiate::FunHintInfo
		     (hinted newdecl)
		     (unhinted unhinted)
		     (types types)))
	       newdecl)))))

;*---------------------------------------------------------------------*/
;*    j2sdecl-duplicate ...                                            */
;*---------------------------------------------------------------------*/
(define (j2sdecl-duplicate p::J2SDecl type)
   (with-access::J2SDecl p (vtype id)
      (let ((nvtype (if (eq? vtype type) vtype 'unknown)))
	 (if (isa? p J2SDeclInit)
	     (duplicate::J2SDeclInit p
		(key (ast-decl-key))
		(hint '())
		(vtype nvtype)
		(itype type))
	     (duplicate::J2SDecl p
		(key (ast-decl-key))
		(exports '())
		(hint '())
		(vtype nvtype)
		(itype type))))))

;*---------------------------------------------------------------------*/
;*    j2sdecl-duplicate-as-any ...                                     */
;*    -------------------------------------------------------------    */
;*    This function is used to duplicate an utype parameter for        */
;*    the dispatch function.                                           */
;*---------------------------------------------------------------------*/
(define (j2sdecl-duplicate-as-any p::J2SDecl)
   (if (isa? p J2SDeclInit)
       (duplicate::J2SDeclInit p
	  (key (ast-decl-key))
	  (hint '()))
       (duplicate::J2SDecl p
	  (key (ast-decl-key))
	  (exports '())
	  (hint '()))))

(define (j2sdecl-duplicate-as-any-TBR-12jul21 p::J2SDecl)
   (if (isa? p J2SDeclInit)
       (duplicate::J2SDeclInit p
	  (key (ast-decl-key))
	  (hint '())
	  (utype 'unknown)
	  (vtype 'any)
	  (itype 'any))
       (duplicate::J2SDecl p
	  (key (ast-decl-key))
	  (exports '())
	  (hint '())
	  (utype 'unknown)
	  (vtype 'any)
	  (itype 'any))))

;*---------------------------------------------------------------------*/
;*    j2s-call-hint! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-call-hint! this::J2SNode concrete-type::bool conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-call-hint! ::J2SProgram ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-call-hint! this::J2SProgram concrete-type::bool conf)
   (with-access::J2SProgram this (decls nodes)
      ;; the range has improved type precision that might have created
      ;; new hint call opportunities
      (for-each (lambda (n) (j2s-call-hint! n concrete-type conf)) decls)
      (for-each (lambda (n) (j2s-call-hint! n concrete-type conf)) nodes)
      this))

;*---------------------------------------------------------------------*/
;*    j2s-call-hint! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-call-hint! this::J2SCall concrete-type conf)

   (define (type-number? t)
      (memq t '(integer real number)))

   (define (fun-rtype decl::J2SDeclInit)
      (with-access::J2SDeclInit decl (val)
	 (with-access::J2SFun val (rtype)
	    rtype)))
   
   (define (fun-hint-info fun)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (when (isa? decl J2SDeclFun)
	       (with-access::J2SDeclFun decl (val hintinfo)
		  (cond
		     ((isa? val J2SFun)
		      (with-access::J2SFun val (generator)
			 (unless generator
			    (when (isa? hintinfo FunHintInfo)
			       hintinfo))))
		     ((isa? val J2SMethod)
		      (with-access::J2SMethod val (function)
			 (with-access::J2SFun function (generator)
			    (unless generator
			       (when (isa? hintinfo FunHintInfo)
				  hintinfo)))))))))))
   
   (with-access::J2SCall this (fun args thisargs)
      (set! args (map! (lambda (n) (j2s-call-hint! n concrete-type conf)) args))
      (set! fun (j2s-call-hint! fun concrete-type conf))
      (let ((hinfo (fun-hint-info fun)))
	 (if hinfo
	     (with-access::J2SRef fun (decl)
		(with-access::FunHintInfo hinfo (hinted unhinted types)
		   (with-access::J2SDeclFun hinted (val)
		      (with-access::J2SFun val (generator)
			 (cond
			    ((not (=fx (length args) (length types)))
			     (with-access::J2SFun val (idthis)
				(duplicate::J2SCall this
				   (thisargs thisargs)
				   (type (fun-rtype unhinted))
				   (fun (duplicate::J2SRef fun
					   (type 'function)
					   (decl unhinted))))))
			    ((every (lambda (a t)
				       (or (eq? t 'unknown)
					   (eq? t 'any)
					   (let ((tya (j2s-type a)))
					      (or (and (eq? t 'number)
						       (type-number? tya))
						  (eq? tya t)
						  
						  (and (m64? conf)
						       (eq? t 'integer)
						       (memq tya '(int53 uint32 int32)))
						  (and (eq? t 'integer)
						       (memq tya '(int53 uint32 int32))
						       (inrange-int30? a))))))
				args types)
			     (with-access::J2SFun val (idthis)
				;; adjust the usecnt count
				(with-access::J2SDecl hinted (usecnt)
				   (set! usecnt (+fx usecnt 1)))
				(with-access::J2SDecl decl (usecnt)
				   (set! usecnt (-fx usecnt 1)))
				(duplicate::J2SCall this
				   (thisargs thisargs)
				   (type (fun-rtype hinted))
				   (fun (duplicate::J2SRef fun
					   (type 'function)
					   (decl hinted))))))
			    ((every (lambda (a t)
				       (or (eq? t 'unknown)
					   (eq? t 'any)
					   (let ((tya (j2s-type a)))
					      (or (memq tya '(any unknown))
						  (eq? tya t)
						  (and (type-number? tya)
						       (type-number? t))
						  (and (m64? conf)
						       (eq? t 'integer)
						       (memq tya '(int53 uint32 int32)))
						  (and (eq? t 'integer)
						       (memq tya '(int53 uint32 int32))
						       (inrange-int30? a))))))
				args types)
			     this)
			    (else
			     (with-access::J2SFun val (idthis)
				(duplicate::J2SCall this
				   (thisargs thisargs)
				   (type (fun-rtype unhinted))
				   (fun (duplicate::J2SRef fun
					   (type 'function)
					   (decl unhinted)))))))))))
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SNode inloop inc)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ::J2SLoop ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SLoop inloop inc)
   (with-access::J2SLoop this (body)
      (set! body (j2s-hint-loop! body #t (+ 3 inc)))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ::J2SLetBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SLetBlock inloop inc)
   
   (define hint-loop-threshold 0.5)
   
   (define (loop-duplicable? this decls)
      ;; returns #t iff it is worth duplicating this loop
      (any (lambda (p::J2SDecl)
	      (with-access::J2SDecl p (hint usecnt vtype id)
		 (when (pair? hint)
		    (when (or (eq? vtype 'unknown)
			      (eq? vtype 'any)
			      (and (eq? vtype 'number)
				   (assq 'integer hint)))
		       (let ((hintcnt (apply + (map cdr hint))))
			  (> (/ hintcnt usecnt) hint-loop-threshold))))))
	 decls))
   
   (define (dispatch-body this pred then otherwise)
      (with-access::J2SBlock this (loc)
	 (instantiate::J2SIf
	    (loc loc)
	    (test pred)
	    (then then)
	    (else otherwise))))
   
   (define (loop-dispatch this::J2SLetBlock decls::pair-nil htypes::pair-nil)
      (with-access::J2SLetBlock this (nodes loc)
	 (let* ((pred (test-hint-decls decls htypes loc))
		(then (duplicate::J2SBlock this
			 (nodes (map (lambda (n)
					(let ((an (j2s-alpha n '() '())))
					   (reset-type! an decls)))
				   nodes))))
		(otherwise (J2SMeta 'hint 0 0
			      (duplicate::J2SBlock this))))
	    (dispatch-body this pred then otherwise))))
   
   (with-access::J2SLetBlock this (decls nodes loc)
      (let ((decls (filter (lambda (d::J2SDecl)
			      (with-access::J2SDecl d (usecnt)
				 (>=fx usecnt 2)))
		      decls)))
	 (if (and inloop (loop-duplicable? this decls))
	     (let ((htypes (map (lambda (p)
				   (multiple-value-bind (bt _)
				      (best-hint-type p #t)
				      bt))
			      decls)))
		(set! nodes (list (loop-dispatch this decls htypes)))
		this)
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-block! ::J2SNode ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-block! this::J2SNode conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-block! ::J2SLetBlock ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-block! this::J2SLetBlock conf)
   
   (define (hint-decl-gain decl::J2SDecl)
      (with-access::J2SDecl decl (%info)
	 %info))
   
   (define (block-duplicableold? this decls::pair-nil)
      ;; returns #t iff it is worth duplicating this loop
      (when (pair? decls)
	 (let ((gains (map hint-decl-gain decls)))
	    (when (>fx (apply + gains) 5)
	       (or (>fx (*fx *j2s-hint-block-node-size-factor* (apply + gains))
		      (node-size this))
		   (>fx (apply max gains) 12))))))

   (define (block-duplicable? this decls::pair-nil)
      ;; returns #t iff it is worth duplicating this block
      (when (pair? decls)
	 (let* ((gains (map hint-decl-gain decls))
		(gain (apply + gains)))
	    (when (>fx gain 10)
	       (let ((sz (node-size this)))
		  (>= (/ gain (* sz sz))
		     *j2s-hint-block-node-expansion-ratio*))))))
   
   (define (dispatch-body this pred then otherwise)
      (with-access::J2SBlock this (loc)
	 (instantiate::J2SIf
	    (loc loc)
	    (test pred)
	    (then then)
	    (else otherwise))))
   
   (define (decl-duplicate p::J2SDecl type)
      (if (isa? p J2SDeclInit)
	  (with-access::J2SDeclInit p (loc)
	     (duplicate::J2SDeclInit p
		(key (ast-decl-key))
		;; (hint '())
		(vtype type)
		(itype type)
		(val (J2SRef p loc :type type))))
	  (duplicate::J2SDecl p
	     (key (ast-decl-key))
	     (hint '())
	     (vtype type)
	     (itype type))))

   (define (block-dispatch this::J2SLetBlock decls::pair-nil htypes::pair-nil)
      (with-access::J2SLetBlock this (nodes loc)
	 (let* ((pred (test-hint-decls decls htypes loc))
		(ndecls (map decl-duplicate decls htypes))
		(then (duplicate::J2SLetBlock this
			 (decls ndecls)
			 (nodes (map (lambda (n)
					(j2s-alpha n decls ndecls))
				   nodes))))
		(otherwise (duplicate::J2SBlock
				 (unhint-block this decls htypes))))
	    (dispatch-body this pred then otherwise))))
   
   (with-access::J2SLetBlock this (nodes decls loc)
      (let ((decls (filter (lambda (d::J2SDecl)
			      (with-access::J2SDecl d (vtype)
				 (and (memq vtype '(any unknown))
				      (not (decl-usage-has? d '(assig)))
				      (multiple-value-bind (bt bc)
					 (best-hint-type d #t)
					 (and (memq bt '(array object))
					      (>fx bc 4))))))
		      decls)))
	 (if (pair? decls)
	     ;; we have found potential candidates for duplication let's
	     ;; dig in the block body to check how these variables are
	     ;; actually used (typed or untyped access and used on loop or not)
	     (let ((decls (hint-block-access-usages! this decls)))
		(if (block-duplicable? this decls)
		    (let ((htypes (map (lambda (p)
					  (multiple-value-bind (bt _)
					     (best-hint-type p #t)
					     bt))
				     decls)))
		       (when (>=fx (config-get conf :verbose 0) 4)
			  (with-output-to-port (current-error-port)
			     (lambda ()
				(display* "{" (cadr loc) ":" (caddr loc) "," (apply + (map hint-decl-gain decls)) "}"))))
		       (set! nodes (list (block-dispatch this decls htypes)))
		       this)
		    (call-default-walker)))
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-block! ::J2SMeta ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-block! this::J2SMeta conf)
   (with-access::J2SMeta this (meta optim)
      (if (eq? meta 'hint-block)
	  (if (=fx optim 0)
	      this
	      (call-default-walker))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SNode decls)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SRef ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SRef decls)
   (with-access::J2SRef this (decl type)
      (when (or (eq? type 'any) (memq decl decls))
	 (set! type 'unknown)))
   this)

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SDecl ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SDecl decls)
   (with-access::J2SDecl this (decl vtype)
      (when (eq? vtype 'any)
	 (set! vtype 'unknown)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SExpr decls)
   (with-access::J2SExpr this (type)
      (when (eq? type 'any)
	 (set! type 'unknown)))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-meta-noopt! ...                                         */
;*    -------------------------------------------------------------    */
;*    When a function is duplicated and when both hinted and unhinted  */
;*    versions are used, mark the unhinted function as noopt.          */
;*---------------------------------------------------------------------*/
(define (j2s-hint-meta-noopt! d::J2SDecl)
   (when (and (isa? d J2SDeclFun) #f)
      (with-access::J2SDeclFun d (%info id)
	 (when (isa? %info J2SDeclFun)
	    (with-access::J2SDeclFun %info (hintinfo)
	       (when (isa? hintinfo FunHintInfo)
		  (with-access::FunHintInfo hintinfo (unhinted hinted)
		     (when (and unhinted (eq? hinted d))
			;; the two hinted and unhinted function are used
			(with-access::J2SDeclFun unhinted (val)
			   (with-access::J2SFun val (body)
			      (with-access::J2SNode body (loc)
				 (let ((mbody (instantiate::J2SMeta
						 (loc loc)
						 (optim 0)
						 (stmt body))))
				    (set! body
				       (duplicate::J2SBlock body
					  (nodes (list mbody))))))))))))))))

;*---------------------------------------------------------------------*/
;*    return-patch! ::J2SNode ...                                      */
;*    -------------------------------------------------------------    */
;*    After duplicating functions' body, the return nodes must         */
;*    be adjusted so that the FROM fields point to the new function.   */
;*---------------------------------------------------------------------*/
(define-walk-method (return-patch! this::J2SNode old new)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    return-patch! ::J2SReturn ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (return-patch! this::J2SReturn old new)
   (with-access::J2SReturn this (from)
      (when (eq? from old)
	 (set! from new)))
   this)

;*---------------------------------------------------------------------*/
;*    call-patch! ::J2SNode ...                                        */
;*    -------------------------------------------------------------    */
;*    After duplicating functions' body, the return nodes must         */
;*    be adjusted so that the FROM fields point to the new function.   */
;*---------------------------------------------------------------------*/
(define-walk-method (call-patch! this::J2SNode old new)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    call-patch! ::J2SCall ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (call-patch! this::J2SCall old new)
   (call-default-walker)
   (with-access::J2SCall this (fun type)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (when (eq? decl old)
	       (set! decl new)
	       (set! type 'unknown)))))
   this)

;*---------------------------------------------------------------------*/
;*    self-recursive? ...                                              */
;*---------------------------------------------------------------------*/
(define (self-recursive? this::J2SDeclFun)
   (with-access::J2SDeclFun this (val)
      (let ((cell (make-cell #f)))
	 (self-recursive val this cell)
	 (cell-ref cell))))

;*---------------------------------------------------------------------*/
;*    self-recursive ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (self-recursive this::J2SNode self::J2SDecl cell)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    self-recursive ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (self-recursive this::J2SCall self::J2SDecl cell)
   (call-default-walker)
   (with-access::J2SCall this (fun)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (when (eq? decl self)
	       (cell-set! cell #t))))))

;*---------------------------------------------------------------------*/
;*    decl-duplicated? ...                                             */
;*---------------------------------------------------------------------*/
(define (decl-duplicated? fun::J2SDecl)
   (with-access::J2SDeclFun fun (val %info)
      (when (isa? %info J2SDeclFun)
	 (with-access::J2SDeclFun %info (hintinfo)
	    (when (isa? hintinfo FunHintInfo)
	       (with-access::FunHintInfo hintinfo (unhinted hinted)
		  (or (eq? hinted fun) (eq? unhinted fun))))))))


;*---------------------------------------------------------------------*/
;*    hint-block-access-usages! ...                                    */
;*---------------------------------------------------------------------*/
(define (hint-block-access-usages! this::J2SLetBlock decls)
   (for-each (lambda (d)
		(with-access::J2SDecl d (%info)
		   (set! %info 0)))
      decls)
   (hint-block-access-usages this #f)
   (filter (lambda (d)
		(with-access::J2SDecl d (%info)
		   (>fx %info 0)))
      decls))

;*---------------------------------------------------------------------*/
;*    hint-block-access-usages ::J2SNode ...                           */
;*    -------------------------------------------------------------    */
;*    Count the number of untyped access to variables                  */
;*---------------------------------------------------------------------*/
(define-walk-method (hint-block-access-usages this::J2SNode inloop)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    hint-block-access-usages ::J2SLoop ...                           */
;*---------------------------------------------------------------------*/
(define-walk-method (hint-block-access-usages this::J2SLoop inloop)
   (with-access::J2SLoop this (body)
      (hint-block-access-usages body #t)))

;*---------------------------------------------------------------------*/
;*    hint-block-access-usages ::J2SAccess ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (hint-block-access-usages this::J2SAccess inloop)
   (with-access::J2SAccess this (obj field)
      (if (isa? obj J2SRef)
	  (with-access::J2SRef obj (decl type)
	     (with-access::J2SDecl decl (%info)
		(when (and (integer? %info) (memq type '(any unknown)))
		   (set! %info (+fx %info (if inloop 3 1))))))
	  (hint-block-access-usages obj inloop))
      (hint-block-access-usages field inloop)))

;*---------------------------------------------------------------------*/
;*    unhint-block ...                                                 */
;*    -------------------------------------------------------------    */
;*    Remove reference hints (of else branches of hinted blocks)       */
;*---------------------------------------------------------------------*/
(define (unhint-block node decls htypes)
   (for-each (lambda (d t)
		(with-access::J2SDecl d (%info)
		   (set! %info (cons 'unhint t))))
      decls htypes)
   (unhint! node)
   (for-each (lambda (d)
		(with-access::J2SDecl d (%info)
		   (set! %info #unspecified)))
      decls)
   node)

;*---------------------------------------------------------------------*/
;*    unhint! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (unhint! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unhint ::J2SRef ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (unhint! this::J2SRef)
   (with-access::J2SRef this (decl hint)
      (with-access::J2SDecl decl (%info)
	 (when (and (pair? %info) (eq? (car %info) 'unhint))
	    (set! hint
	       (cons `(,(cdr %info) . ,(minvalfx))
		  (filter (lambda (h)
			     (not (eq? (car h) (cdr %info))))
		     hint))))))
   this)
   
