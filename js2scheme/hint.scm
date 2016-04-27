;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/hint.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 19 10:13:17 2016                          */
;*    Last change :  Sun Apr 24 13:22:04 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
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

   (export (j2s-hint! ::J2SProgram ::obj)
	   (j2s-call-hint! ::J2SProgram ::obj)))

;*---------------------------------------------------------------------*/
;*    j2s-hint! ...                                                    */
;*---------------------------------------------------------------------*/
(define (j2s-hint! prgm args)
   (with-access::J2SProgram prgm (decls nodes)
      ;; first we collect all possible hints...
      (for-each (lambda (n) (j2s-hint n '())) decls)
      (for-each (lambda (n) (j2s-hint n '())) nodes)
      ;; then, for each function whose parameters are "hinted", we generate
      ;; an ad-hoc typed version
      (let ((dups (append-map j2s-hint-function* decls)))
	 (set! decls
	    (append (filter (lambda (dup::J2SDeclFun)
			       (with-access::J2SDeclFun dup (usecnt val id)
				  (>fx usecnt 0)))
		       dups)
	       decls))
	 (pair? dups))))

;*---------------------------------------------------------------------*/
;*    j2s-call-hint! ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-call-hint! prgm args)
   (with-access::J2SProgram prgm (decls nodes)
      ;; for each function whose parameters are "hinted", we generate
      ;; an ad-hoc typed version
      (for-each call-hint! decls)
      (for-each call-hint! nodes))
   prgm)

;*---------------------------------------------------------------------*/
;*    j2s-add-hint! ...                                                */
;*---------------------------------------------------------------------*/
(define (j2s-add-hint! decl types)
   (with-access::J2SDecl decl (hint)
      (for-each (lambda (type)
		   [assert (type) (symbol? type)]
		   (let ((c (assq type hint)))
		      (if (pair? c)
			  (set-cdr! c (+fx 1 (cdr c)))
			  (set! hint (cons (cons type 1) hint)))))
	 types)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SNode ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SNode types)
   [assert (types) (every symbol? types)]
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SExpr ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SExpr types)
   (if (pair? types)
       (j2s-hint this '())
       (multiple-value-bind (op decl type expr)
	  (j2s-expr-type-test this)
	  (if type
	      (j2s-add-hint! decl (list type))
	      (call-default-walker)))))
   
;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SRef ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SRef types)
   (with-access::J2SRef this (decl loc)
      (j2s-add-hint! decl types)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SBinary ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SBinary types)
   (with-access::J2SBinary this (op lhs rhs)
      (case op
	 ((< <= >= > - * / << >> >>> ^ & BIT_OR)
	  (j2s-hint lhs '(number))
	  (j2s-hint rhs '(number)))
	 ((+)
	  (j2s-hint lhs '(number string))
	  (j2s-hint rhs '(number string)))
	 ((== === != !==)
	  (cond
	     ((eq? (j2s-type lhs) 'number) (j2s-hint rhs '(number)))
	     ((eq? (j2s-type rhs) 'number) (j2s-hint lhs '(number)))))
	 ((instanceof)
	  (j2s-hint lhs '(object))
	  (j2s-hint rhs '(function)))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SSwitch ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SSwitch types)
   
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
				      ((integer) #unspecified)
				      (else (set! ty #f))))
				  ((string)
				   (case ty
				      ((unknown) (set! ty 'string))
				      ((integer) #unspecified)
				      (else (set! ty #f)))))))))
	    cases)
	 (when (and ty (not (eq? ty 'unknown))) (list ty))))
      
   (with-access::J2SSwitch this (key cases)
      (let ((ctype (cases-type cases)))
	 (when ctype (j2s-hint key ctype))
	 (call-default-walker))))
	 
;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPostfix ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPostfix types)
   (with-access::J2SPostfix this (lhs)
      (j2s-hint lhs '(number))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SPrefix ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SPrefix types)
   (with-access::J2SPrefix this (lhs)
      (j2s-hint lhs '(number))))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SDeclInit ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SDeclInit types)
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
   '("push" "pop"))

;*---------------------------------------------------------------------*/
;*    j2s-hint-access ...                                              */
;*---------------------------------------------------------------------*/
(define (j2s-hint-access this types hints::pair-nil)
   (with-access::J2SAccess this (obj field)
      (cond
	 ((not (isa? obj J2SRef))
	  (j2s-hint obj '(object)))
	 ((memq (j2s-type field) '(integer number))
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
		 (j2s-hint obj '(object))))))
	 ((isa? field J2SLiteralCnst)
	  (with-access::J2SLiteralCnst field (val)
	     (if (isa? val J2SString)
		 (with-access::J2SString val (val)
		    (if (string=? val "length")
			(with-access::J2SRef obj (decl)
			   (j2s-add-hint! decl hints))
			(j2s-hint obj '(object))))
		 (j2s-hint obj '(object)))))
	 (else
	  (j2s-hint obj '(object))))
      (j2s-hint field '())))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAccess ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAccess types)
   (j2s-hint-access this types '(array string)))

;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SAssig ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SAssig types)
   (with-access::J2SAssig this (lhs rhs)
      (when (isa? lhs J2SAccess)
	 (j2s-hint-access lhs types '(array)))
      (j2s-hint rhs '())))
   
;*---------------------------------------------------------------------*/
;*    j2s-hint ::J2SCall ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint this::J2SCall types)
   
   (define (j2s-hint-fun fun args)
      (with-access::J2SFun fun (params)
	 (let loop ((args args)
		    (params params))
	    (when (and (pair? args) (pair? params))
	       (let ((ty (j2s-type (car args))))
		  (when (symbol? ty)
		     (j2s-add-hint! (car params) (list ty))))
	       (loop (cdr args) (cdr params))))))

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
		    (if ronly
			(j2s-hint-fun fun args)
			(call-default-walker))))
		(else
		 (call-default-walker)))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    j2s-hint-function ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-hint-function ::J2SDeclFun ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-hint-function* this::J2SDeclFun)
   
   (define (call-orig orig idthis params)
      (with-access::J2SDeclFun this (loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (protocol 'bounce)
	    (fun (instantiate::J2SRef
		    (decl orig)
		    (loc loc)))
	    (this (instantiate::J2SHopRef
		     (loc loc)
		     (id idthis)))
	    (args (map (lambda (p::J2SDecl)
			  (with-access::J2SDecl p (hint)
			     (instantiate::J2SRef
				(loc loc)
				(decl p))))
		     params)))))

   (define (call-hint dup idthis params)
      (with-access::J2SDeclFun this (loc val)
	 (with-access::J2SDeclFun dup ((fun val))
	    (with-access::J2SFun fun ((fthis idthis))
	       (instantiate::J2SCall
		  (loc loc)
		  (protocol 'bounce)
		  (fun (instantiate::J2SRef
			  (decl dup)
			  (loc loc)))
		  (this (when fthis
			   (instantiate::J2SHopRef
			      (loc loc)
			      (id idthis))))
		  (args (map (lambda (p::J2SDecl)
				(with-access::J2SDecl p (hint)
				   (instantiate::J2SRef
				      (loc loc)
				      (type (best-hint-type hint))
				      (decl p))))
			   params)))))))

   (define (predicate type)
      (case type
	 ((number) 'number?)
	 ((integer) 'fixnum?)
	 ((string) 'js-jsstring?)
	 ((array) 'js-array?)
	 ((object) 'js-object?)
	 ((function) 'js-function?)
	 ((bool) 'boolean?)
	 (else (error "hint" "Unknown hint type predicate" type))))
   
   (define (test-hint-param param)
      (with-access::J2SDecl param (type loc)
	 (instantiate::J2SCall
	    (loc loc)
	    (type 'bool)
	    (fun (instantiate::J2SHopRef
		    (loc loc)
		    (type 'bool)
		    (id (predicate type))))
	    (args (list (instantiate::J2SRef
			   (loc loc)
			   (decl param)))))))
   
   (define (test-hint-params dup params)
      (with-access::J2SDeclFun this (loc)
	 (let loop ((params params))
	    (let ((param (car params)))
	       (with-access::J2SDecl param (type)
		  (cond
		     ((null? (cdr params))
		      (if (eq? type 'obj)
			  (instantiate::J2SBool
			     (loc loc)
			     (val #t))
			  (test-hint-param (car params))))
		     ((eq? type 'obj)
		      (loop (cdr params)))
		     (else
		      (instantiate::J2SBinary
			 (loc loc)
			 (op '&&)
			 (lhs (test-hint-param (car params)))
			 (rhs (loop (cdr params)))))))))))

   (define (dispatch-body body pred callhint callorig)
      (with-access::J2SBlock body (loc endloc)
	 (instantiate::J2SBlock
	    (loc loc)
	    (endloc endloc)
	    (nodes (list (instantiate::J2SIf
			    (loc loc)
			    (test pred)
			    (then (instantiate::J2SReturn
				     (loc loc)
				     (expr callhint)))
			    (else (instantiate::J2SReturn
				     (loc loc)
				     (expr callorig)))))))))
   
   (with-access::J2SDeclFun this (val id %info)
      (with-access::J2SFun val (params body idthis vararg)
	 (if (and (not (eq? %info 'duplicate))
		  (not (isa? %info FunHintInfo))
		  (pair? params)
		  (any (lambda (p::J2SDecl)
			  (with-access::J2SDecl p (hint usecnt id)
			     (and (pair? hint)
				  (or (pair? (cdr hint))
				      (not (eq? (caar hint) 'obj)))
				  (>fx usecnt 0))))
		     params)
		  (not vararg))
	     (let* ((dup (fun-duplicate this))
		    (orig (fun-orig this))
		    (types (map (lambda (p)
				   (with-access::J2SDecl p (hint)
				      (best-hint-type hint)))
			      params))
		    (newparams (map j2sdecl-duplicate params types))
		    (pred (test-hint-params dup newparams))
		    (callhint (call-hint! (call-hint dup idthis newparams)))
		    (callorig (call-orig orig idthis newparams)))
		(set! params (map j2sdecl-duplicate-sans-type params))
		(set! body (dispatch-body body pred callhint callorig))
		(erase-type! body)
		(list dup orig))
	     '()))))

;*---------------------------------------------------------------------*/
;*    best-hint-type ...                                               */
;*---------------------------------------------------------------------*/
(define (best-hint-type::symbol hint)
   (let ((r (let loop ((l hint)
		       (t 'obj)
		       (c 0))
	       (cond
		  ((null? l)
		   t)
		  ((eq? (caar l) 'obj)
		   (loop (cdr l) t c))
		  ((>fx (cdr (car l)) c)
		   (loop (cdr l) (caar l) (cdar l)))
		  ((and (=fx (cdr (car l)) c)
			(or (eq? t 'obj) (memq (caar l) '(integer number))))
		   (loop (cdr l) (caar l) (cdar l)))
		  (else
		   (loop (cdr l) t c))))))
      (if (not (symbol? r))
	  (tprint "PAS R: " hint))
      r))

;*---------------------------------------------------------------------*/
;*    fun-orig ...                                                     */
;*---------------------------------------------------------------------*/
(define (fun-orig fun::J2SDeclFun)
   (with-access::J2SDeclFun fun (val id %info)
      (with-access::J2SFun val (params body name generator)
	 (let ((newdecl (duplicate::J2SDeclFun fun
			   (parent fun)
			   (id (symbol-append id '$$))
			   (ronly #t)
			   (writable #f)
			   (binder 'const)
			   (scope 'none)
			   (usecnt 1)
			   (val (duplicate::J2SFun val
				   (generator #f)
				   (idgen generator)
				   (name (when (symbol? name)
					    (symbol-append name '$$)))
				   (params params)
				   (body body))))))
	    newdecl))))

;*---------------------------------------------------------------------*/
;*    j2sdecl-duplicate ...                                            */
;*---------------------------------------------------------------------*/
(define (j2sdecl-duplicate p::J2SDecl type)
   (if (isa? p J2SDeclInit)
       (duplicate::J2SDeclInit p
	  (key (ast-decl-key))
	  (hint '())
	  (type type))
       (duplicate::J2SDecl p
	  (key (ast-decl-key))
	  (hint '())
	  (type type))))

;*---------------------------------------------------------------------*/
;*    j2sdecl-duplicate-sans-type ...                                  */
;*---------------------------------------------------------------------*/
(define (j2sdecl-duplicate-sans-type p::J2SDecl)
   (if (isa? p J2SDeclInit)
       (duplicate::J2SDeclInit p
	  (key (ast-decl-key))
	  (hint '())
	  (type #f))
       (duplicate::J2SDecl p
	  (key (ast-decl-key))
	  (hint '())
	  (type #f))))

;*---------------------------------------------------------------------*/
;*    fun-duplicate ...                                                */
;*---------------------------------------------------------------------*/
(define (fun-duplicate fun::J2SDeclFun)
   (with-access::J2SDeclFun fun (val id %info)
      (with-access::J2SFun val (params body idthis generator)
	 (let* ((types (map (lambda (p)
			       (with-access::J2SDecl p (hint)
				  (best-hint-type hint)))
			  params))
		(newparams (map j2sdecl-duplicate params types))
		(typeid (string->symbol
			   (string-upcase!
			      (apply string
				 (map (lambda (t)
					 (string-ref (symbol->string t) 0))
				    types)))))
		(newbody (j2s-alpha body params newparams))
		(newfun (duplicate::J2SFun val
			   (generator #f)
			   (idgen generator)
			   (%info #unspecified)
			   (type #f)
			   (idthis (if (this? body) idthis #f))
			   (params newparams)
			   (body newbody)))
		(newdecl (duplicate::J2SDeclFun fun
			    (parent fun)
			    (key (ast-decl-key))
			    (id (symbol-append id '$$ typeid))
			    (ronly #t)
			    (writable #f)
			    (binder 'const)
			    (scope 'none)
			    (usecnt 1)
			    (%info 'duplicate)
			    (val newfun))))
	    (use-count newbody +1)
	    (with-access::J2SFun newfun (decl)
	       (set! decl newdecl))
	    (set! %info
	       (instantiate::FunHintInfo
		  (hinted newdecl)
		  (types types)))
	    newdecl))))

;*---------------------------------------------------------------------*/
;*    call-hint! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (call-hint! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    call-hint! ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (call-hint! this::J2SCall)
   
   (define (normalize-type e)
      (let ((t (j2s-type e)))
	 (if (eq? t 'integer) 'number t)))
   
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
      (let ((%info (fun-hint-info fun)))
	 (if %info
	     (with-access::J2SRef fun (decl)
		(with-access::FunHintInfo %info (hinted types)
		   (with-access::J2SDeclFun hinted (val)
		      (with-access::J2SFun val (generator)
			 (if (equal? (map normalize-type args) types)
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
;*    erase-type! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (erase-type! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    erase-type! ::J2SExpr ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (erase-type! this::J2SExpr)
   (with-access::J2SExpr this (type)
      (when (eq? type 'obj) (set! type #f))
      (call-default-walker)))
   
;*---------------------------------------------------------------------*/
;*    erase-type! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (erase-type! this::J2SFun)
   (with-access::J2SFun this (rettype)
      (when (eq? rettype 'obj) (set! rettype #f))
      (call-default-walker)))
   
;*---------------------------------------------------------------------*/
;*    erase-type! ::J2SHopRef ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (erase-type! this::J2SHopRef)
   this)
   
