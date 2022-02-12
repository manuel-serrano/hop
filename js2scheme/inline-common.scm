;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline-common.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 12 08:55:39 2022                          */
;*    Last change :  Sat Feb 12 12:08:15 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Inlining common files                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_inline-common

   (library web)
   
   (include "ast.sch"
	    "usage.sch"
	    "inline.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_alpha
	   __js2scheme_use
	   __js2scheme_node-size
	   __js2scheme_freevars
	   __js2scheme_classutils
	   __js2scheme_inline-profile)

   (export (class J2SMetaInl::J2SMeta
	      (inlstack::pair-nil (info '("notraverse"))))

	   inline-global-expansion
	   inline-max-function-size
	   inline-max-method-size
	   inline-min-dispatch-percentage
	   inline-dispatch-function-test-size
	   inline-dispatch-closure-test-size
	   inline-arity-expansion-size-factor
	   inline-method-check-size
	   inline-blacklist
	   inline-whitelist
	   inline-blacklistloc
	   inline-whitelistloc
	   (inline-check-id id)

	   (inline-verb loc fun targets-cnt fsize limit allcnt conf)
	   (ptable-verb pms)
	   
	   (j2s-inline-cleanup! this::J2SProgram conf)
	   (used-decl?::bool ::J2SDecl)
	   
	   (generic unmetainl! ::J2SNode)
	   (generic collect-funs*::pair-nil this parent::J2SNode)
	   (generic collect-calls*::pair-nil this parent::J2SNode owner)
	   (generic collect-proto-methods*::pair-nil this::J2SNode)
	   (yield?::bool ::J2SNode)

	   (ptable::struct alist)

	   (ronly-variable?::bool obj)
	   (simple-argument?::bool ::J2SExpr)
	   (simple-literal?::bool ::J2SExpr)
	   (private-field?::bool ::J2SExpr)
	   
	   (function-rutype ::obj)
	   (function-freevars? ::J2SFun)
	   (function-arguments? ::J2SFun)
	   (function-varargs? ::J2SFun)
	   (function-generator? ::J2SFun)
	   (function-newtarget? ::J2SFun)
	   (function-delete-argument? ::J2SFun)
	   (function-size ::J2SFun)
	   (method-size ::J2SFun)
	   (function-size-reset! ::J2SFun)
	   (function-mode ::J2SFun)
	   (function-arity ::J2SFun)
	   (function-fxarg? ::J2SFun)
	   (function-leaf? ::J2SFun)
	   (function-self-recursive? ::J2SFun)
	   (function-max-expansion ::J2SFun)
	   (function-glodecl ::J2SFun ::J2SProgram)

	   (filter-rutype ::pair-nil ::J2SProgram)

	   (inline-stmt->expr::J2SExpr loc ::J2SStmt ::symbol)
	   (LetBlock loc t body)
	   (j2s-count-calls! this::J2SProgram conf)
	   ))

;*---------------------------------------------------------------------*/
;*    inline-default-factor ...                                        */
;*---------------------------------------------------------------------*/
(define inline-global-expansion
   ;; the overall program global expansion factor
   2)

(define inline-max-function-size
   ;; the maximum body size of inlined function candidates
   160)

(define inline-max-method-size
   ;; the maximum body size of inlined method candidates
   32)

(define inline-min-dispatch-percentage
   ;; the minimum percentage of call
   0.33)

(define inline-dispatch-function-test-size
   ;; the test size approximation of function dispatch
   8)

(define inline-dispatch-closure-test-size
   ;; the test size approximation of closure dispatch
   12)

(define inline-arity-expansion-size-factor
   ;; the size of the function body 
   15)

(define inline-method-check-size
   ;; the size of a method check
   10)

;*---------------------------------------------------------------------*/
;*    dev                                                              */
;*---------------------------------------------------------------------*/
(define inline-blacklist
      (if (string? (getenv "BLACKLIST"))
	  (map string->symbol
	     (call-with-input-string (getenv "BLACKLIST") port->string-list))
	  '()))
(define inline-whitelist
   (if (string? (getenv "WHITELIST"))
       (map string->symbol
	  (call-with-input-string (getenv "WHITELIST") port->string-list))
       '()))
(define inline-blacklistloc
   (if (string? (getenv "BLACKLISTLOC"))
       (map string->number
	  (call-with-input-string (getenv "BLACKLISTLOC") port->string-list))
       '()))
(define inline-whitelistloc
   (if (string? (getenv "WHITELISTLOC"))
       (map string->number 
	  (call-with-input-string (getenv "WHITELISTLOC") port->string-list))
       '()))

(define checked '())

(define (inline-check-id id)
   (cond
      ((not (or (null? inline-blacklist) (not (memq id inline-blacklist))))
       (unless (memq id checked)
	  (set! checked (cons id checked))
	  (tprint "black " id))
       #f)
      ((not (or (null? inline-whitelist) (memq id inline-whitelist)))
       (unless (memq id checked)
	  (set! checked (cons id checked))
	  (tprint "not white " id))
       #f)
      (else
       #t)))

;*---------------------------------------------------------------------*/
;*    inline-verb ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-verb loc fun targets-cnt fsize limit allcnt conf)
   
   (define (loc->string loc)
      (match-case loc
	 ((at ?file ?pos) (format "[~a:~a]" file pos))
	 (else (format "[~s]" loc))))
   
   (when (>=fx (config-get conf :verbose 0) 3)
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (display "\n      ")
	    (cond
	       ((isa? fun J2SRef)
		(with-access::J2SRef fun (decl)
		   (with-access::J2SDecl decl (id)
		      (display id))))
	       ((isa? fun J2SAccess)
		(with-access::J2SAccess fun (obj field)
		   (with-access::J2SString field (val)
		      (let loop ((obj obj))
			 (cond
			    ((isa? obj J2SRef)
			     (with-access::J2SRef obj (decl)
				(with-access::J2SDecl decl (id)
				   (display id))))
			    ((isa? obj J2SAccess)
			     (with-access::J2SAccess obj (obj field)
				(loop obj)
				(display ".")
				(if (isa? field J2SString)
				    (with-access::J2SString field (val)
				       (display val))
				    (display "..."))))))
		      (display ".")
		      (display val)))))
	    (display "() ")
	    (display (loc->string loc))
	    (when (>=fx (config-get conf :verbose 0) 4)
	       (display " size: ")
	       (display fsize)
	       (display "/")
	       (display limit)
	       (when (> allcnt 0)
		  (display " cnt: ")
		  (display (format "~(, )"
			      (map (lambda (c)
				      (format "~a ~a%" c
					 (inexact->exact
					    (round (* 100 (/ c allcnt))))))
				 targets-cnt)))))))))

;*---------------------------------------------------------------------*/
;*    ptable-verb ...                                                  */
;*---------------------------------------------------------------------*/
(define (ptable-verb pms)
   (when (string-contains (or (getenv "HOPTRACE") "") "j2s:inline")
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (print "\n      proto methods:")
	    (hashtable-for-each pms
	       (lambda (k b)
		  (let ((first #t))
		     (for-each (lambda (p)
				  (when (protoinfo-svar p)
				     (when (isa? (protoinfo-assig p) J2SAssig)
					(when first
					   (set! first #f)
					   (print "        " k ": "))
					(with-access::J2SNode (protoinfo-assig p) (loc)
					   (print "         " (cadr loc)
					      ":" (caddr loc) " size="
					      (node-size (protoinfo-method p)))))))
			b))))
	    (print "\n      class proto methods:")
	    (hashtable-for-each pms
	       (lambda (k b)
		  (let ((first #t))
		     (for-each (lambda (p)
				  (when (protoinfo-svar p)
				     (when (isa? (protoinfo-assig p) J2SMethodPropertyInit)
					(when first
					   (set! first #f)
					   (print "        " k ": "))
					(with-access::J2SNode (protoinfo-assig p) (loc)
					   (print "         " (cadr loc)
					      ":" (caddr loc) " size="
					      (node-size (protoinfo-method p))
					      " owner="
					      (if (isa? (protoinfo-owner p) J2SClass)
						  (type->sexp (protoinfo-owner p))
						  (typeof (protoinfo-owner p))))))))
			b))))))))

;*---------------------------------------------------------------------*/
;*    j2s-inline-cleanup! ...                                          */
;*---------------------------------------------------------------------*/
(define (j2s-inline-cleanup! this::J2SProgram conf)
   (with-access::J2SProgram this (decls nodes)
      ;; cleanup use count
      (reinit-use-count! this)
      (set! decls (filter used-decl? decls))
      ;; cleanup metainl
      (unmetainl! this)
      this))

;*---------------------------------------------------------------------*/
;*    used-decl? ...                                                   */
;*---------------------------------------------------------------------*/
(define (used-decl? decl)
   (with-access::J2SDecl decl (usecnt)
      (when (or (>fx usecnt 0)
		(not (isa? decl J2SDeclFun))
		(and (isa? decl J2SDeclFun)
		     (with-access::J2SDeclFun decl (val)
			(isa? val J2SSvc)))
		(decl-usage-has? decl '(eval)))
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val)
	       (set! val (dead-inner-decl! val))))
	 #t)))

;*---------------------------------------------------------------------*/
;*    dead-inner-decl! ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-inner-decl! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    dead-inner-decl! ::J2SDeclInit ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-inner-decl! this::J2SDeclInit)
   
   (define (simple-expr? val::J2SExpr)
      (or (simple-argument? val)
	  (isa? val J2SFun)
	  (isa? val J2SRef)))
   
   (with-access::J2SDeclInit this (usecnt loc id key val)
      (if (and (=fx usecnt 0) (simple-expr? val))
	  (J2SNop)
	  (begin
	     (call-default-walker)
	     this))))

;*---------------------------------------------------------------------*/
;*    dead-inner-decl! ::J2SLetBlock ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (dead-inner-decl! this::J2SLetBlock)
   (call-default-walker)
   (with-access::J2SLetBlock this (decls)
      (set! decls (filter (lambda (d) (isa? d J2SDecl)) decls))
      this))

;*---------------------------------------------------------------------*/
;*    ronly-variable? ...                                              */
;*---------------------------------------------------------------------*/
(define (ronly-variable? obj)
   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (decl)
	  (decl-ronly? decl)))
      ((isa? obj J2SDecl)
       (decl-ronly? obj))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    simple-argument? ...                                             */
;*---------------------------------------------------------------------*/
(define (simple-argument? this::J2SExpr)
   (or (simple-literal? this)
       (when (isa? this J2SRef)
	  (with-access::J2SRef this (decl)
	     (with-access::J2SDecl decl (escape)
		(unless escape
		   (not (decl-usage-has? decl '(assig eval)))))))))


;*---------------------------------------------------------------------*/
;*    simple-literal? ...                                              */
;*---------------------------------------------------------------------*/
(define (simple-literal?::bool this::J2SExpr)
   (or (isa? this J2SNull)
       (isa? this J2SUndefined)
       (and (isa? this J2SLiteralValue)
	    (not (isa? this J2SRegExp))
	    (not (isa? this J2SCmap)))
       (isa? this J2SLiteralCnst)))

;*---------------------------------------------------------------------*/
;*    private-field? ...                                               */
;*---------------------------------------------------------------------*/
(define (private-field? field)
   (when (isa? field J2SString)
      (with-access::J2SString field (private)
	 private)))

;*---------------------------------------------------------------------*/
;*    unmetainl! ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (unmetainl! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unmetainl! ::J2SMetaInl ...                                      */
;*---------------------------------------------------------------------*/
(define-method (unmetainl! this::J2SMetaInl)
   (with-access::J2SMetaInl this (stmt)
      (unmetainl! stmt)))

;*---------------------------------------------------------------------*/
;*    collect-funs* ::J2SNode ...                                      */
;*    -------------------------------------------------------------    */
;*    Collect all the function expressions.                            */
;*---------------------------------------------------------------------*/
(define-generic (collect-funs*::pair-nil this parent::J2SNode)
   (if (pair? this)
       (append-map (lambda (n) (collect-funs* n parent)) this)
       '()))

;*---------------------------------------------------------------------*/
;*    collect-funs*::pair-nil ::J2SNode ...                            */
;*---------------------------------------------------------------------*/
(define-method (collect-funs*::pair-nil this::J2SNode parent)
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1))
		 (c '()))
	 (if (=fx i -1)
	     c
	     (let* ((f (vector-ref fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (loop (-fx i 1)
		       (append
			  (collect-funs* ((class-field-accessor f) this) this)
			  c))
		    (loop (-fx i 1) c)))))))

;*---------------------------------------------------------------------*/
;*    collect-funs* ::J2SDeclFun ...                                   */
;*---------------------------------------------------------------------*/
(define-method (collect-funs* this::J2SDeclFun parent)
   (let ((val (j2sdeclinit-val-fun this)))
      (with-access::J2SFun val (%info body)
	 (set! %info
	    (funinfo #unspecified #unspecified this parent
	       (j2s-alpha body '() '())))
	 (cons val (collect-funs* body parent)))))

;*---------------------------------------------------------------------*/
;*    collect-funs* ::J2SAssig ...                                     */
;*---------------------------------------------------------------------*/
(define-method (collect-funs* this::J2SAssig parent)
   (with-access::J2SAssig this (lhs rhs loc)
      (cond
	 ((isa? rhs J2SFun)
	  (with-access::J2SFun rhs (%info body loc)
	     (set! %info
		(funinfo #unspecified #unspecified #unspecified this
		   (j2s-alpha body '() '())))
	     (cons rhs (collect-funs* body parent))))
	 ((isa? rhs J2SMethod)
	  (with-access::J2SMethod rhs (method)
	     (with-access::J2SFun method (%info body loc)
		(set! %info
		   (funinfo #unspecified #unspecified #unspecified this
		      (j2s-alpha body '() '())))
		(cons method (collect-funs* body parent)))))
	 (else
	  (call-next-method)))))
   
;*---------------------------------------------------------------------*/
;*    collect-calls* ::J2SNode ...                                     */
;*    -------------------------------------------------------------    */
;*    Link the call nodes to their parents and return the list of      */
;*    all calls.                                                       */
;*---------------------------------------------------------------------*/
(define-generic (collect-calls*::pair-nil this parent::J2SNode owner)
   (if (pair? this)
       (append-map (lambda (n) (collect-calls* n parent owner)) this)
       '()))

;*---------------------------------------------------------------------*/
;*    collect-calls*::pair-nil ::J2SMeta ...                           */
;*---------------------------------------------------------------------*/
(define-method (collect-calls*::pair-nil this::J2SMeta parent owner)
   (with-access::J2SMeta this (optim stmt)
      (if (>=fx optim 2)
	  (collect-calls* stmt this owner)
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-calls* ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-method (collect-calls* this::J2SNode parent::J2SNode owner)
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1))
		 (c '()))
	 (if (=fx i -1)
	     c
	     (let* ((f (vector-ref fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (loop (-fx i 1)
		       (append
			  (collect-calls*
			     ((class-field-accessor f) this) this owner)
			  c))
		    (loop (-fx i 1)
		       c)))))))

;*---------------------------------------------------------------------*/
;*    collect-calls* ::J2SMethod ...                                   */
;*---------------------------------------------------------------------*/
(define-method (collect-calls* this::J2SMethod parent::J2SNode owner)
   (with-access::J2SMethod this (method)
      (collect-calls* method parent owner)))

;*---------------------------------------------------------------------*/
;*    collect-calls* ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (collect-calls* this::J2SFun parent owner)
   (set! owner this)
   (set! parent this)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    collect-calls* ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-method (collect-calls* this::J2SCall parent::J2SNode owner)
   (with-access::J2SCall this (fun thisargs args %info loc)
      (let ((next (append
		     (collect-calls* fun this owner)
		     (collect-calls* thisargs this owner)
		     (collect-calls* args this owner))))
	 (if (and loc
		  ;; dev
		  (not (memq (caddr loc) inline-blacklistloc))
		  (or (null? inline-whitelistloc) (memq (caddr loc) inline-whitelistloc)))
	     (begin
		(set! %info (callinfo parent owner))
		(cons this next))
	     next))))

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ...                                       */
;*    -------------------------------------------------------------    */
;*    Collect the inline methods candidates, which are the top level   */
;*    methods assigned to constructor prototypes.                      */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SNode)
   '())

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ::J2SSeq ...                              */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SSeq)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ::J2SStmtExpr ...                         */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SStmtExpr)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ::J2SAssig ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SAssig)

   (define (method-of rhs)
      (if (isa? rhs J2SMethod)
	  (with-access::J2SMethod rhs (method) method)
	  rhs))

   (with-access::J2SAssig this (lhs rhs)
      (if (and (isa? lhs J2SAccess) (or (isa? rhs J2SFun) (isa? rhs J2SMethod)))
	  (with-access::J2SAccess lhs (obj (metname field))
	     (if (and (isa? obj J2SAccess) (isa? metname J2SString))
		 (with-access::J2SAccess obj (obj field)
		    (if (and (isa? obj J2SRef) (isa? field J2SString))
			(with-access::J2SString field (val)
			   (if (string=? val "prototype")
			       (with-access::J2SString metname (val)
				  (if (inline-check-id (string->symbol val))
				      (with-access::J2SRef obj (decl)
					 (list (cons val
						  (protoinfo this (method-of rhs) #f decl))))
				      '()))
			       '()))
			'()))
		 '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-proto-methods* ::J2SClass ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-proto-methods* this::J2SDeclClass)
   
   (define (element-method el)
      (with-access::J2SClassElement el (prop rtwin)
	 (if rtwin
	     (with-access::J2SClassElement rtwin (prop)
		(with-access::J2SMethodPropertyInit prop (val)
		   val))
	     (with-access::J2SMethodPropertyInit prop (val)
		val))))
   
   (with-access::J2SDeclClass this (val)
      (if (isa? val J2SClass)
	  (filter-map (lambda (el)
			 (with-access::J2SClassElement el (prop)
			    (with-access::J2SMethodPropertyInit prop (name (met val))
			       (when (isa? name J2SString)
				  (with-access::J2SString name ((str val))
				     (when (inline-check-id (string->symbol str))
					(let ((met (element-method el)))
					   (cons str (protoinfo prop met #f val)))))))))
	     (j2s-class-methods val :super #f))
	  '())))

;*---------------------------------------------------------------------*/
;*    yield? ...                                                       */
;*---------------------------------------------------------------------*/
(define (yield? this)
   (let ((res (make-cell #f)))
      (use-yield this res)
      (cell-ref res)))

;*---------------------------------------------------------------------*/
;*    use-yield ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (use-yield this::J2SNode res)
   (or (cell-ref res) (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    use-yield ::J2SYield ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (use-yield this::J2SYield res)
   (cell-set! res #t))

;*---------------------------------------------------------------------*/
;*    ptable ...                                                       */
;*    -------------------------------------------------------------    */
;*    Store all inline candidate methods into a global hashtable.      */
;*---------------------------------------------------------------------*/
(define (ptable::struct alist)
   (let ((table (create-hashtable)))
      (for-each (lambda (e)
		   (hashtable-add! table (car e) cons (cdr e) '()))
	 alist)
      table))

;*---------------------------------------------------------------------*/
;*    node-leaf ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-leaf node::J2SNode cell)
   (when (cell-ref cell)
      (call-default-walker))
   (cell-ref cell))

;*---------------------------------------------------------------------*/
;*    node-leaf ::J2SCall ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (node-leaf node::J2SCall cell)
   (cell-set! cell #f)
   #f)

;*---------------------------------------------------------------------*/
;*    node-leaf ::J2SNew ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-leaf node::J2SNew cell)
   (cell-set! cell #f)
   #f)

;*---------------------------------------------------------------------*/
;*    node-self-recursive ::J2SNode ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (node-self-recursive node::J2SNode self cell)
   (unless (cell-ref cell)
      (call-default-walker))
   (cell-ref cell))

;*---------------------------------------------------------------------*/
;*    node-self-recursive ::J2SCall ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (node-self-recursive node::J2SCall self cell)
   (with-access::J2SCall node (fun)
      (when (isa? fun J2SRef)
	 (with-access::J2SRef fun (decl)
	    (when (isa? decl J2SDeclInit)
	       (with-access::J2SDeclInit decl (val)
		  (when (eq? val self)
		     (cell-set! cell #t))))))))

;*---------------------------------------------------------------------*/
;*    function-rutype ...                                              */
;*---------------------------------------------------------------------*/
(define (function-rutype fun)
   (cond
      ((isa? fun J2SFun)
       (with-access::J2SFun fun (rutype)
	  rutype))
      ((protoinfo? fun)
       (function-rutype (protoinfo-method fun)))
      (else
       (error "function-rutype" "bad value" fun))))

;*---------------------------------------------------------------------*/
;*    function-freevars? ...                                           */
;*---------------------------------------------------------------------*/
(define (function-freevars? this::J2SFun)
   (with-access::J2SFun this (%info body loc)
      (cond
	 ((not (funinfo? %info))
	  (set! %info
	     (funinfo #unspecified (free-vars? this) #f #f
		(j2s-alpha body '() '()))))
	 ((eq? (funinfo-freevars %info) #unspecified)
	  (funinfo-freevars-set! %info (free-vars? this))))
      (funinfo-freevars %info)))

;*---------------------------------------------------------------------*/
;*    function-arguments? ...                                          */
;*---------------------------------------------------------------------*/
(define (function-arguments? this::J2SFun)
   (with-access::J2SFun this (argumentsp)
      argumentsp))

;*---------------------------------------------------------------------*/
;*    function-varargs? ...                                            */
;*---------------------------------------------------------------------*/
(define (function-varargs? this::J2SFun)
   (with-access::J2SFun this (vararg)
      vararg))

;*---------------------------------------------------------------------*/
;*    function-generator? ...                                          */
;*---------------------------------------------------------------------*/
(define (function-generator? this::J2SFun)
   (with-access::J2SFun this (generator)
      generator))

;*---------------------------------------------------------------------*/
;*    function-newtarget? ...                                          */
;*---------------------------------------------------------------------*/
(define (function-newtarget? this::J2SFun)
   (with-access::J2SFun this (new-target)
      (memq new-target '(global argument))))

;*---------------------------------------------------------------------*/
;*    function-delete-argument? ...                                    */
;*---------------------------------------------------------------------*/
(define (function-delete-argument? this::J2SFun)
   (with-access::J2SFun this (params)
      (any (lambda (d) (decl-usage-has? d '(delete))) params)))

;*---------------------------------------------------------------------*/
;*    function-size ...                                                */
;*---------------------------------------------------------------------*/
(define (function-size this::J2SFun)
   
   (define (size this::J2SFun body)
      ;; give a bonus to leaves
      (let ((nsz (node-size body)))
	 (if (function-leaf? this)
	     (/fx (*fx nsz 3) 4)
	     nsz)))
   
   (with-access::J2SFun this (%info body)
      (cond
	 ((not (funinfo? %info))
	  (set! %info
	     (funinfo (size this body) #unspecified #f #f
		(j2s-alpha body '() '()))))
	 ((eq? (funinfo-size %info) #unspecified)
	  (funinfo-size-set! %info
	     (size this (funinfo-initial-body %info)))))
      (funinfo-size %info)))

;*---------------------------------------------------------------------*/
;*    method-size ...                                                  */
;*---------------------------------------------------------------------*/
(define (method-size this::J2SFun)
   (+fx inline-method-check-size (function-size this)))

;*---------------------------------------------------------------------*/
;*    function-size-reset! ...                                         */
;*---------------------------------------------------------------------*/
(define (function-size-reset! this::J2SFun)
   (with-access::J2SFun this (%info)
      (when (funinfo? %info)
	 (funinfo-size-set! %info #unspecified))))

;*---------------------------------------------------------------------*/
;*    function-mode ...                                                */
;*---------------------------------------------------------------------*/
(define (function-mode this::J2SFun)
   (with-access::J2SFun this (mode)
      mode))

;*---------------------------------------------------------------------*/
;*    function-arity ...                                               */
;*---------------------------------------------------------------------*/
(define (function-arity fun::J2SFun)
   (with-access::J2SFun fun (params)
      (length params)))

;*---------------------------------------------------------------------*/
;*    function-fxarg? ...                                              */
;*---------------------------------------------------------------------*/
(define (function-fxarg? fun::J2SFun)
   (with-access::J2SFun fun (vararg)
      (not vararg)))

;*---------------------------------------------------------------------*/
;*    function-leaf? ...                                               */
;*---------------------------------------------------------------------*/
(define (function-leaf? fun::J2SFun)
   (with-access::J2SFun fun (body)
      (let ((cell (make-cell #t)))
	 (node-leaf body cell)
	 (cell-ref cell))))
			       
;*---------------------------------------------------------------------*/
;*    function-self-recursive? ...                                     */
;*---------------------------------------------------------------------*/
(define (function-self-recursive? fun::J2SFun)
   (with-access::J2SFun fun (body)
      (let ((cell (make-cell #f)))
	 (node-self-recursive body fun cell)
	 (cell-ref cell))))

;*---------------------------------------------------------------------*/
;*    function-max-expansion ...                                       */
;*---------------------------------------------------------------------*/
(define (function-max-expansion this::J2SFun)
   (with-access::J2SFun this (params)
      (max (*fx (length params) inline-arity-expansion-size-factor)
	 (/fx inline-max-function-size 3))))

;*---------------------------------------------------------------------*/
;*    function-glodecl ...                                             */
;*---------------------------------------------------------------------*/
(define (function-glodecl this::J2SFun prgm)
   (with-access::J2SFun this (%info loc)
      (unless (isa? (funinfo-decl %info) J2SDecl)
	 (let* ((id (string-append "%met:" (number->string (caddr loc))))
		(decl (instantiate::J2SDeclExtern
			 (loc loc)
			 (id (string->symbol id))
			 (binder 'let-opt)
			 (bind #t)
			 (val (J2SUndefined)))))
	    (cond
	       ((isa? (funinfo-parent %info) J2SAssig)
		(with-access::J2SAssig (funinfo-parent %info) (rhs loc)
		   (set! rhs (J2SAssig (J2SRef decl) rhs))))
	       (else
		(error "function-glodecl" "Not support"
		   (j2s->sexp  (funinfo-parent %info)))))
	    (funinfo-decl-set! %info decl)
	    (with-access::J2SProgram prgm (decls)
	       (set! decls (cons decl decls)))))
      (funinfo-decl %info)))

;*---------------------------------------------------------------------*/
;*    filter-rutype ...                                                */
;*    -------------------------------------------------------------    */
;*    Filter the functions to retain only those of the same rutype.    */
;*---------------------------------------------------------------------*/
(define (filter-rutype funs::pair-nil prgm::J2SProgram)
   (with-access::J2SProgram prgm (mode)
      (cond
	 ((not (eq? mode 'hopscript))
	  funs)
	 ((or (null? funs) (null? (cdr funs)))
	  funs)
	 (else
	  (let ((rut (function-rutype (car funs))))
	     (filter (lambda (f)
			(eq? (function-rutype f) rut))
		funs))))))

;*---------------------------------------------------------------------*/
;*    inline-stmt->expr::J2SExpr ...                                   */
;*    -------------------------------------------------------------    */
;*    Transforms an inline function body statement into an expression. */
;*---------------------------------------------------------------------*/
(define (inline-stmt->expr::J2SExpr loc body::J2SStmt utype::symbol)
   
   (define (stmt->expr node)
      (cond
	 ((isa? node J2SStmtExpr)
	  (with-access::J2SStmtExpr node (expr)
	     expr))
	 ((isa? node J2SReturn)
	  (with-access::J2SReturn node (expr)
	     expr))
	 ((and (isa? node J2SBindExit)
	       (with-access::J2SBindExit node (stmt)
		  (isa? stmt J2SReturn)))
	  (with-access::J2SBindExit node (stmt)
	     (with-access::J2SReturn stmt (expr)
		expr)))
	 ((isa? node J2SLetBlock)
	  (with-access::J2SLetBlock node (loc decls nodes)
	     (when (and (null? decls) (pair? nodes) (null? (cdr nodes)))
		(stmt->expr (cadr nodes)))))
	 ((isa? node J2SBlock)
	  (with-access::J2SBlock node (nodes loc)
	     (cond
		((null? nodes)
		 (J2SUndefined))
		((null? (cdr nodes))
		 (stmt->expr (car nodes)))
		(else
		 #f))))
	 (else
	  #f)))
   
   (let* ((lbl (gensym '%return))
	  (cell (make-cell '()))
	  (bd (bind-exit! body lbl cell '())))
      (cond
	 ((pair? (cell-ref cell))
	  (let ((be (J2SBindExit/utype utype lbl bd)))
	     (for-each (lambda (ret::J2SReturn)
			  (with-access::J2SReturn ret (from)
			     (set! from be)))
		(cell-ref cell))
	     be))
	 ((stmt->expr bd)
	  =>
	  (lambda (expr) expr))
	 (else
	  (J2SBindExit/utype utype #f bd)))))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SNode ...                                         */
;*    -------------------------------------------------------------    */
;*    Replace untail return (those of the inlined function) with       */
;*    an exit.                                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SNode l cell env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SReturn ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SReturn l cell env)
   (with-access::J2SReturn this (tail exit from expr loc from)
      (set! expr (bind-exit! expr l cell env))
      (when (and (not exit) (not (memq from env)))
	 (unless exit
	    ;; (set! tail #f)
	    (cell-set! cell (cons this (cell-ref cell)))))
      this))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SBindExit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SBindExit l cell env)
   (with-access::J2SBindExit this (stmt)
      (set! stmt (bind-exit! stmt l cell (cons this env)))
      this))

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SFun l cell env)
   this)

;*---------------------------------------------------------------------*/
;*    bind-exit! ::J2SMethod ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (bind-exit! this::J2SMethod l cell env)
   this)

;*---------------------------------------------------------------------*/
;*    LetBlock ...                                                     */
;*---------------------------------------------------------------------*/
(define (LetBlock loc t body)
   (if (pair? t)
       (let ((endloc (node-endloc body)))
	  (J2SLetRecBlock #f t body))
       body))

;*---------------------------------------------------------------------*/
;*    j2s-count-calls! ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-count-calls! this::J2SProgram conf)
   (with-access::J2SProgram this (call-size)
      (let ((count (make-cell 0)))
	 (count-call this count)
	 (set! call-size (cell-ref count)))))

;*---------------------------------------------------------------------*/
;*    count-call ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (count-call this::J2SNode count)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    count-call ::J2SCall ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (count-call this::J2SCall count)
   (with-access::J2SCall this (profid)
      (let ((id (cell-ref count)))
	 (cell-set! count (+fx id 1))
	 (call-default-walker)
	 (set! profid id))))

