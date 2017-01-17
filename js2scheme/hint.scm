;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/hint.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 19 10:13:17 2016                          */
;*    Last change :  Thu Jan 19 08:46:09 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Hint typping.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_type-hint

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_use
	   __js2scheme_alpha)

   (static (class FunHintInfo
	      hinted
	      types::pair-nil))

   (export (j2s-hint!::bool ::J2SProgram ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-hint! ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-hint! prgm args)
   (with-access::J2SProgram prgm (decls nodes)
      ;; reset previously collected hints
      (for-each j2s-reset-hint decls)
      (for-each j2s-reset-hint nodes)
      ;; first we collect all possible hints...
      (for-each (lambda (n) (j2s-hint n '() 'number)) decls)
      (for-each (lambda (n) (j2s-hint n '() 'number)) nodes)
      ;; then, for each function whose parameters are "hinted", we generate
      ;; an ad-hoc typed version
      (when (>=fx (config-get args :optim 0) 3)
	 (let ((dups (append-map j2s-hint-function* decls)))
	    (when (pair? dups)
	       (set! decls
		  (append (filter (lambda (dup::J2SDeclFun)
				     (with-access::J2SDeclFun dup (usecnt)
					(>fx usecnt 0)))
			     dups)
		     decls)))
	    (for-each call-hint! decls)
	    (for-each call-hint! nodes)
	    (pair? dups)))))

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
      (set! hint '())
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    j2s-add-hint! ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-add-hint! decl::J2SDecl types::pair-nil)
   (with-access::J2SDecl decl (hint itype)
      (when (or (eq? itype 'unknown)
		(eq? itype 'any)
		(and (eq? itype 'number)
		     (or (memq 'integer types) (memq 'index types))))
	 (for-each (lambda (type)
		      [assert (type) (symbol? type)]
		      (unless (memq type '(unknown any))
			 (let ((c (assq type hint)))
			    (if (pair? c)
				(set-cdr! c (+fx 1 (cdr c)))
				(set! hint (cons (cons type 1) hint))))))
	    types))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SNode types numctx::symbol)
   [assert (types) (every symbol? types)]
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SRef types numctx)
   (with-access::J2SRef this (decl loc)
      (j2s-add-hint! decl types)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SExpr types numctx)
   (multiple-value-bind (op decl type ref)
      (j2s-expr-type-test this)
      (if op
	  (j2s-add-hint! decl (list type))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SBinary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SBinary types numctx)
   [assert (types) (every symbol? types)]
   (with-access::J2SBinary this (op lhs rhs)
      (case op
	 ((<< >> >>> ^ & BIT_OR)
	  (cond
	     ((eq? (j2s-type lhs) 'integer)
	      (j2s-hint rhs '(integer) numctx))
	     ((eq? (j2s-type lhs) 'index)
	      (j2s-hint rhs '(index) 'index))
	     (else
	      (j2s-hint rhs (list numctx) numctx)))
	  (cond
	     ((eq? (j2s-type rhs) 'integer)
	      (j2s-hint lhs '(integer) numctx))
	     ((eq? (j2s-type rhs) 'index)
	      (j2s-hint lhs '(index) 'index))
	     (else
	      (j2s-hint lhs (list numctx) numctx))))
	 ((< <= >= > - * /)
	  (cond
	     ((eq? (j2s-type lhs) 'index)
	      (j2s-hint rhs '(index) 'index))
	     (else
	      (j2s-hint rhs (list numctx) numctx)))
	  (cond
	     ((eq? (j2s-type rhs) 'index)
	      (j2s-hint lhs '(index) 'index))
	     (else
	      (j2s-hint lhs (list numctx) numctx))))
	 ((%)
	  (if (and (memq (j2s-type lhs) '(integer index))
		   (memq (j2s-type rhs) '(integer index)))
	      (begin
		 (j2s-hint lhs '(integer) 'integer)
		 (j2s-hint rhs '(integer) 'integer))
	      (begin
		 (j2s-hint lhs (list numctx) numctx)
		 (j2s-hint rhs (list numctx) numctx))))
	 ((+)
	  (cond
	     ((eq? (j2s-type lhs) 'integer)
	      (j2s-hint rhs '(integer) numctx))
	     ((eq? (j2s-type lhs) 'index)
	      (j2s-hint rhs '(index) 'index))
	     ((eq? (j2s-type lhs) 'number)
	      (j2s-hint rhs (list numctx) numctx))
	     ((eq? (j2s-type rhs) 'integer)
	      (j2s-hint lhs '(integer) numctx))
	     ((eq? (j2s-type rhs) 'index)
	      (j2s-hint lhs '(index) 'index))
	     ((eq? (j2s-type rhs) 'number)
	      (j2s-hint lhs (list numctx) numctx))
	     ((eq? (j2s-type lhs) 'string)
	      (j2s-hint rhs '(string) 'number))
	     ((eq? (j2s-type rhs) 'string)
	      (j2s-hint lhs '(string) 'number))
	     (else
	      (j2s-hint lhs (list numctx 'string) 'number)
	      (j2s-hint rhs (list numctx 'string) 'number))))
	 ((== === != !==)
	  (cond
	     ((not (eq? (j2s-type lhs) 'any))
	      (j2s-hint rhs (list (j2s-type lhs)) numctx))
	     ((not (eq? (j2s-type rhs) 'any))
	      (j2s-hint lhs (list (j2s-type rhs)) numctx))))
	 ((instanceof)
	  (j2s-hint lhs '(object) 'number)
	  (j2s-hint rhs '(function) 'number))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SSwitch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SSwitch types numctx)
   
   (define (cases-type cases)
      (let ((ty 'unknown))
	 (for-each (lambda (c)
		      (when ty
			 (unless (isa? c J2SDefault)
			    (with-access::J2SCase c (expr)
			       (case (j2s-type expr)
				  ((integer)
				   (case ty
				      ((unknown) (set! ty 'integer))
				      ((integer index) #unspecified)
				      (else (set! ty #f))))
				  ((index)
				   (case ty
				      ((unknown) (set! ty 'index))
				      ((integer index) #unspecified)
				      (else (set! ty #f))))
				  ((string)
				   (case ty
				      ((unknown) (set! ty 'string))
				      ((integer index) #unspecified)
				      (else (set! ty #f)))))))))
	    cases)
	 (if (and ty (not (eq? ty 'unknown)))
	     (list ty)
	     '(integer string))))
      
   (with-access::J2SSwitch this (key cases)
      (let ((ctype (cases-type cases)))
	 (when ctype (j2s-hint key ctype 'integer))
	 (call-default-walker))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SDeclInit types numctx)
   (with-access::J2SDeclInit this (val type hint)
      (let ((ty (j2s-type val)))
	 (when (symbol? ty)
	    (j2s-add-hint! this (list ty))))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    j2s-string-methods ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-string-methods
   '("charAt" "charCodeAt" "localeCompare" "naturalCompare"
     "match" "replace" "search" "split" "substring" "toLowerCase"
     "toLocaleLowerCase" "toUpperCase" "toLocaleUpperCase"
     "trim" "substr"))

;*---------------------------------------------------------------------*/
;*    j2s-string-array-methods ...                                     */
;*---------------------------------------------------------------------*/
(define j2s-string-array-methods
   '("indexOf" "lastIndexOf" "concat"))

;*---------------------------------------------------------------------*/
;*    j2s-array-methods ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-array-methods
   '("push" "pop" "join" "reverse" "shift" "slice" "sort" "splice" "unshift"
     "every" "some" "forEach" "map" "filter" "find" "reduce" "reduceRight"
     "iterator"))

;*---------------------------------------------------------------------*/
;*    j2s-hint-access ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-hint-access this types hints::pair-nil numctx)
   (with-access::J2SAccess this (obj field)
      (cond
	 ((not (isa? obj J2SRef))
	  (j2s-hint obj '(object array string) numctx))
	 ((memq (j2s-type field) '(index integer number))
	  (with-access::J2SRef obj (decl)
	     (j2s-add-hint! decl hints)))
	 ((isa? field J2SString)
	  (with-access::J2SString field (val)
	     (cond
		((string=? val "length")
		 (with-access::J2SRef obj (decl)
		    (j2s-add-hint! decl hints)))
		((member val j2s-string-methods)
		 (with-access::J2SRef obj (decl)
		    (j2s-add-hint! decl '(string))))
		((member val j2s-string-array-methods)
		 (with-access::J2SRef obj (decl)
		    (j2s-add-hint! decl '(string array))))
		((member val j2s-array-methods)
		 (with-access::J2SRef obj (decl)
		    (j2s-add-hint! decl '(array))))
		(else
		 (j2s-hint obj hints numctx)))))
	 ((isa? field J2SLiteralCnst)
	  (with-access::J2SLiteralCnst field (val)
	     (if (isa? val J2SString)
		 (with-access::J2SString val (val)
		    (if (string=? val "length")
			(with-access::J2SRef obj (decl)
			   (j2s-add-hint! decl hints))
			(j2s-hint obj hints numctx)))
		 (j2s-hint obj hints numctx))))
	 (else
	  (j2s-hint obj hints numctx)))
      (j2s-hint field '(index) 'number)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAccess types numctx)
   (cond
      ((and (pair? types) (member (car types) '(index integer number)))
       (j2s-hint-access this types '(array) 'number))
      ((and (pair? types) (member (car types) '(string)))
       (j2s-hint-access this types '(string) 'number))
      (else
       (j2s-hint-access this types '(array string) 'number))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAssig ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAssig types numctx)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SAccess)
	 (j2s-hint-access lhs types '(array) 'number))
      (j2s-hint rhs types 'number)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPostfix ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPostfix types numctx)
   (with-access::J2SPostfix this (lhs)
      (if (isa? lhs J2SAccess)
	  (j2s-hint-access lhs types '(array) 'number)
	  (j2s-hint lhs '(number) 'number))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPrefix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPrefix types numctx)
   (with-access::J2SPrefix this (lhs)
      (if (isa? lhs J2SAccess)
	  (j2s-hint-access lhs types '(array) 'number)
	  (j2s-hint lhs '(number) 'number))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SNew ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SNew types numctx)
   (with-access::J2SNew this (clazz args)
      (j2s-hint clazz '(function) 'number)
      (for-each (lambda (a) (j2s-hint a '() 'number)) args)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SFun ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SFun types numctx)
   (with-access::J2SFun this (body)
      (j2s-hint body '() 'number)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SCall types numctx)
   
   (define (j2s-hint-fun fun args)
      (with-access::J2SFun fun (params)
	 (let loop ((args args)
		    (params params))
	    (when (and (pair? args) (pair? params))
	       (let ((ty (j2s-type (car args))))
		  (when (symbol? ty)
		     (j2s-add-hint! (car params) (list ty))))
	       (loop (cdr args) (cdr params))))))
   
   (define (j2s-hint-unknown-function fun args)
      (j2s-hint fun '(function) 'number)
      (for-each (lambda (a) (j2s-hint a '() 'number)) args))
   
   (with-access::J2SCall this (fun args)
      (cond
	 ((isa? fun J2SFun)
	  (j2s-hint-fun fun args))
	 ((isa? fun J2SRef)
	  (with-access::J2SRef fun (decl)
	     (cond
		((isa? decl J2SDeclFunCnst)
		 (with-access::J2SDeclFunCnst decl ((fun val))
		    (j2s-hint-fun fun args)))
		((isa? decl J2SDeclFun)
		 (with-access::J2SDeclFun decl (ronly (fun val))
		    (when ronly
		       (j2s-hint-fun fun args))))
		(else
		 (j2s-hint-unknown-function fun args)))))
	 (else
	  (j2s-hint-unknown-function fun args)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SFor ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SFor types numctx)
   (with-access::J2SFor this (init test incr body)
      (j2s-hint init types 'number)
      (j2s-hint test types 'index)
      (j2s-hint incr types 'index)
      (j2s-hint body types 'number)))
   
;*---------------------------------------------------------------------*/
;*    j2s-hint-function ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-function ::J2SDeclFun ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SDeclFun)
   
   (define (call-hinted orig idthis params types)
      (with-access::J2SDeclFun this (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (protocol 'bounce)
	    (fun (instantiate::J2SRef
		    (decl orig)
		    (loc loc)))
	    (this (instantiate::J2SHopRef
		     (loc loc)
		     (itype 'function)
		     (id idthis)))
	    (args (map (lambda (p::J2SDecl type::symbol)
			  (with-access::J2SDecl p (hint)
			     (instantiate::J2SRef
				(loc loc)
				(type type)
				(decl p))))
		     params types)))))
   
   (define (dispatch-body body pred callt callu)
      (with-access::J2SBlock body (loc endloc)
	 (instantiate::J2SBlock
	    (loc loc)
	    (endloc endloc)
	    (nodes (list (instantiate::J2SIf
			    (loc loc)
			    (test pred)
			    (then (instantiate::J2SReturn
				     (loc loc)
				     (expr callt)))
			    (else (instantiate::J2SReturn
				     (loc loc)
				     (expr callu)))))))))
   
   (define (duplicable? decl::J2SDecl)
      ;; returns #t iff the function is duplicable, returns #f otherwise
      (with-access::J2SDeclFun this (val id %info)
	 (with-access::J2SFun val (params vararg)
	    (and (not (isa? %info J2SDecl))
		 (not (isa? %info FunHintInfo))
		 (not vararg)
		 (not (isa? val J2SSvc))
		 (pair? params)
		 (any (lambda (p::J2SDecl)
			 (with-access::J2SDecl p (hint usecnt itype)
			    (when (and (>fx usecnt 0) (pair? hint))
			       (or (eq? itype 'unknown)
				   (eq? itype 'any)
				   (and (eq? itype 'number)
					(or (assq 'integer hint)
					    (assq 'index hint)))))))
		    params)))))
   
   (define (fun-dispatch! fun::J2SDecl htypes::pair-nil ft itypes::pair-nil fu)
      (with-access::J2SDeclFun this (val %info)
	 (with-access::J2SFun val (params body idthis loc)
	    (let* ((newparams (map j2sdecl-duplicate params itypes))
		   (pred (test-hint-decls newparams htypes loc))
		   (callt (call-hinted ft idthis newparams htypes))
		   (callu (call-hinted fu idthis newparams itypes)))
	       (set! params newparams)
	       (set! body (dispatch-body body pred callt callu))))))

   (if (duplicable? this)
       (with-access::J2SDeclFun this (val %info)
	  (with-access::J2SFun val (params body idthis)
	     (let* ((htypes (map (lambda (p)
				    (with-access::J2SDecl p (hint itype %info)
				       (with-access::J2SDeclFun this (id)
					  (tprint "F=" id " "
					     (j2s->list p) " hint=" hint
					     " itype=" itype " %info=" %info))
				       (best-hint-type hint)))
			       params))
		    (itypes (map (lambda (p::J2SDecl)
				    (with-access::J2SDecl p (itype)
				       itype))
			       params))
		    (ft (fun-duplicate-typed this htypes))
		    (fu (fun-duplicate-untyped this)))
		(fun-dispatch! this htypes ft itypes fu)
		(list ft fu))))
       '()))

;*---------------------------------------------------------------------*/
;*    best-hint-type ...                                               */
;*---------------------------------------------------------------------*/
(define (best-hint-type::symbol hint)

   (define (normalize-hint hint)
      (let loop ((l hint)
		 (r '()))
	 (cond
	    ((null? l)
	     r)
	    ((memq (caar l) '(number integer index))
	     (let ((c (assq 'num r)))
		(if (pair? c)
		    (begin
		       (set-cdr! c (+fx (cdr c) (cdar l)))
		       (loop (cdr l) r))
		    (loop (cons (cons 'num (cdar l)) (cdr l)) r))))
	    (else
	     (loop (cdr l) (cons (car l) r))))))
	     
   (let loop ((l (normalize-hint hint))
	      (t 'any)
	      (c 0))
      (cond
	 ((null? l)
	  (cond
	     ((not (eq? t 'num)) t)
	     ((assq 'index hint) 'index)
	     ((assq 'integer hint) 'integer)
	     (else 'number)))
	 ((>fx (cdr (car l)) c)
	  (loop (cdr l) (caar l) (cdar l)))
	 ((and (=fx (cdr (car l)) c) (eq? t 'string))
	  ;; in doubt, prefer arrays over strings
	  (loop (cdr l) (caar l) (cdar l)))
	 ((and (=fx (cdr (car l)) c) (eq? t 'any))
	  (loop (cdr l) (caar l) (cdar l)))
	 (else
	  (loop (cdr l) t c)))))

;*---------------------------------------------------------------------*/
;*    hint-type-predicate ...                                          */
;*---------------------------------------------------------------------*/
(define (hint-type-predicate::symbol type::symbol)
   (case type
      ((number) 'number?)
      ((integer) 'fixnum?)
      ((index) 'js-index?)
      ((string) 'js-jsstring?)
      ((array) 'js-array?)
      ((object) 'js-object?)
      ((function) 'js-function?)
      ((bool) 'boolean?)
      ((undefined) 'js-undefined?)
      ((null) 'js-null?)
      (else (error "hint-type-predicate" "Unknown hint type predicate" type))))

;*---------------------------------------------------------------------*/
;*    test-hint-decls ...                                              */
;*---------------------------------------------------------------------*/
(define (test-hint-decls decls::pair htypes::pair loc)
   
   (define (test-hint-decl param htype)
      (with-access::J2SDecl param (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (fun (instantiate::J2SHopRef
		    (loc loc)
		    (itype 'function)
		    (rtype 'bool)
		    (id (hint-type-predicate htype))))
	    (args (list (instantiate::J2SRef
			   (loc loc)
			   (decl param)))))))

   (let loop ((decls decls)
	      (htypes htypes))
      (let ((decl (car decls))
	    (htype (car htypes)))
	 (with-access::J2SDecl decl (itype)
	    (cond
	       ((null? (cdr decls))
		(if (eq? htype 'any)
		    (instantiate::J2SBool
		       (loc loc)
		       (val #t))
		    (test-hint-decl (car decls) htype)))
	       ((eq? htype 'any)
		(loop (cdr decls) (cdr htypes)))
	       (else
		(instantiate::J2SBinary
		   (loc loc)
		   (op '&&)
		   (lhs (test-hint-decl (car decls) htype))
		   (rhs (loop (cdr decls) (cdr htypes))))))))))

;*---------------------------------------------------------------------*/
;*    fun-duplicate-untyped ...                                        */
;*---------------------------------------------------------------------*/
(define (fun-duplicate-untyped::J2SDeclFun fun::J2SDeclFun)
   (with-access::J2SDeclFun fun (val id %info)
      (with-access::J2SFun val (params body name generator)
	 (duplicate::J2SDeclFun fun
	    (parent fun)
	    (key (ast-decl-key))
	    (id (symbol-append id '$$))
	    (ronly #t)
	    (writable #f)
	    (binder 'const)
	    (scope 'none)
	    (usecnt 1)
	    (itype 'function)
	    (utype 'function)
	    (%info fun)
	    (val (duplicate::J2SFun val
		    (generator #f)
		    (optimize #f)
		    (idgen generator)
		    (name (when (symbol? name) (symbol-append name '$$)))
		    (params params)
		    (body body)))))))

;*---------------------------------------------------------------------*/
;*    fun-duplicate-typed ...                                          */
;*---------------------------------------------------------------------*/
(define (fun-duplicate-typed::J2SDeclFun fun::J2SDeclFun types)
   
   (define (type-initial t)
      (case t
	 ((index) #\U)
	 ((integer) #\I)
	 ((number) #\N)
	 ((array) #\A)
	 ((string) #\S)
	 (else (string-ref (symbol->string t) 0))))
   
   (with-access::J2SDeclFun fun (val id %info)
      (with-access::J2SFun val (params body idthis generator)
	 (let* ((newparams (map j2sdecl-duplicate params types))
		(typeid (string->symbol
			   (string-upcase!
			      (apply string
				 (map type-initial types)))))
		(nbody (j2s-alpha body params newparams))
		(unbody (reset-type! nbody newparams))
		(newfun (duplicate::J2SFun val
			   (generator #f)
			   (idgen generator)
			   (%info #unspecified)
			   (rtype 'unknown)
			   (idthis (if (this? body) idthis #f))
			   (params newparams)
			   (body unbody)))
		(newdecl (duplicate::J2SDeclFun fun
			    (parent fun)
			    (key (ast-decl-key))
			    (id (symbol-append id '$$ typeid))
			    (ronly #t)
			    (writable #f)
			    (binder 'const)
			    (scope 'none)
			    (usecnt 1)
			    (%info fun)
			    (itype 'function)
			    (utype 'function)
			    (val newfun))))
	    (use-count nbody +1)
	    (with-access::J2SFun newfun (decl)
	       (set! decl newdecl))
	    (set! %info
	       (instantiate::FunHintInfo
		  (hinted newdecl)
		  (types types)))
	    ;; compute the new hints for the duplicated function
	    ;;(tprint ">>> --- HINT loop ---------------------")
	    ;;(j2s-hint newbody '() 'number)
	    ;;(tprint "~~~ --- HINT loop ---------------------")
	    ;(j2s-hint-loop! newbody #f)
	    ;;(tprint "<<< --- HINT loop ---------------------")
	    newdecl))))

;*---------------------------------------------------------------------*/
;*    j2sdecl-duplicate ...                                            */
;*---------------------------------------------------------------------*/
(define (j2sdecl-duplicate p::J2SDecl type::symbol)
   (if (isa? p J2SDeclInit)
       (duplicate::J2SDeclInit p
	  (key (ast-decl-key))
	  (hint '())
	  (itype type))
       (duplicate::J2SDecl p
	  (key (ast-decl-key))
	  (hint '())
	  (itype type))))

;*---------------------------------------------------------------------*/
;*    call-hint! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (call-hint! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    call-hint! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (call-hint! this::J2SCall)

   (define (normalize-type t)
      (case t
	 ((integer index) 'number)
	 (else t)))
   
   (define (normalize-expr-type e)
      (normalize-type (j2s-type e)))
   
   (define (fun-hint-info fun)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (when (isa? decl J2SDeclFun)
	       (with-access::J2SDeclFun decl (val %info)
		  (with-access::J2SFun val (generator)
		     (unless generator
			(when (isa? %info FunHintInfo)
			   %info))))))))
   
   (with-access::J2SCall this (fun args (callthis this))
      (set! args (map! call-hint! args))
      (set! fun (call-hint! fun))
      (let ((%info (fun-hint-info fun)))
	 (if %info
	     (with-access::J2SRef fun (decl)
		(with-access::FunHintInfo %info (hinted types)
		   (with-access::J2SDeclFun hinted (val)
		      (with-access::J2SFun val (generator)
			 (if (or (equal? (map j2s-type args) types)
				 (equal? (map normalize-expr-type args) types))
			     (with-access::J2SFun val (idthis)
				;; adjust the usecnt count
				(with-access::J2SDecl hinted (usecnt)
				   (set! usecnt (+fx usecnt 1)))
				(with-access::J2SDecl decl (usecnt)
				   (set! usecnt (-fx usecnt 1)))
				(duplicate::J2SCall this
				   (this (when idthis callthis))
				   (fun (duplicate::J2SRef fun
					   (decl hinted)))))
			     this)))))
	     this))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SNode inloop)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ::J2SLoop ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SLoop inloop)
   (with-access::J2SLoop this (body)
      (set! body (j2s-hint-loop! body #t))
      this))

;*---------------------------------------------------------------------*/
;*    j2s-hint-loop! ::J2SLetBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-loop! this::J2SLetBlock inloop)
   
   (define hint-threshold 0.5)
   
   (define (duplicable? this decls)
      ;; returns #t iff it is worth duplicating this loop
      (any (lambda (p::J2SDecl)
	      (with-access::J2SDecl p (hint usecnt itype id)
		 (when (pair? hint)
		    (when (or (eq? itype 'unknown)
			      (eq? itype 'any)
			      (and (eq? itype 'number)
				   (or (assq 'integer hint)
				       (assq 'index hint))))
		       (let ((hintcnt (apply + (map cdr hint))))
			  (> (/ hintcnt usecnt) hint-threshold))))))
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
		(otherwise (duplicate::J2SBlock this)))
	    (dispatch-body this pred then otherwise))))
   
   (with-access::J2SLetBlock this (decls nodes loc)
      (let ((decls (filter (lambda (d::J2SDecl)
			      (with-access::J2SDecl d (usecnt)
				 (>=fx usecnt 2)))
		      decls)))
	 (if (and inloop (duplicable? this decls))
	     (let ((htypes (map (lambda (p)
				   (with-access::J2SDecl p (hint)
				      (best-hint-type hint)))
			      decls)))
		(set! nodes (list (loop-dispatch this decls htypes)))
		this)
	     (call-default-walker)))))

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
;*    reset-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (reset-type! this::J2SExpr decls)
   (with-access::J2SExpr this (type)
      (when (eq? type 'any)
	 (set! type 'unknown)))
   (call-default-walker))


