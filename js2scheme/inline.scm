;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Mon Nov 25 05:11:55 2019 (serrano)                */
;*    Copyright   :  2017-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Method inlining optimization                                     */
;*    -------------------------------------------------------------    */
;*    The method inlining proceeds as follows:                         */
;*      1- the AST is traversed to find all toplevel methods assigned  */
;*         to prototype objects.                                       */
;*      2- the AST is traversed to detect all the method calls, that   */
;*         might concern one the prototype methods, and to inline      */
;*         the call, protecting it with a hidden class check.          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_inline

   (library web)
   
   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_syntax
	   __js2scheme_utils
	   __js2scheme_type-hint
	   __js2scheme_alpha
	   __js2scheme_use
	   __js2scheme_node-size)

   (static (class J2SMetaInl::J2SMeta
	      (inlstack::pair-nil (info '("notraverse")))))

   (export j2s-inline-stage))

;*---------------------------------------------------------------------*/
;*    j2s-inline-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-inline-stage
   (instantiate::J2SStageProc
      (name "inline")
      (comment "Method inlining optimization")
      (optional :optim-inline)
      (proc j2s-inline!)))

;*---------------------------------------------------------------------*/
;*    J2SMetaInl ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (J2SMetaInl stack optim stmt)
   `(instantiate::J2SMetaInl
       (loc loc)
       (inlstack ,stack)
       (debug 0)
       (optim ,optim)
       (stmt ,stmt)))

;*---------------------------------------------------------------------*/
;*    inline-default-factor ...                                        */
;*---------------------------------------------------------------------*/
(define inline-global-expansion
   ;; the overall program global expansion factor
   3)

(define inline-max-function-size
   ;; the maximum body size of inlined candidates
   80)

(define inline-min-call-occurrence
   ;; to be removed
   100000)

(define inline-min-dispatch-percentage
   ;; the minimum percentage of call
   0.33)

(define inline-dispatch-function-test-size
   ;; the test size approximation of function dispatch
   8)

(define inline-dispatch-closure-test-size
   ;; the test size approximation of closure dispatch
   12)

;*---------------------------------------------------------------------*/
;*    dev                                                              */
;*---------------------------------------------------------------------*/
(define blacklist
      (if (string? (getenv "BLACKLIST"))
	  (map string->symbol
	     (call-with-input-string (getenv "BLACKLIST") port->string-list))
	  '()))
(define whitelist
   (if (string? (getenv "WHITELIST"))
       (map string->symbol
	  (call-with-input-string (getenv "WHITELIST") port->string-list))
       '()))

(define checked '())

(define (check-id id)
   (cond
      ((not (or (null? blacklist) (not (memq id blacklist))))
       (unless (memq id checked)
	  (set! checked (cons id checked))
	  (tprint "black " id))
       #f)
      ((not (or (null? whitelist) (memq id whitelist)))
       (unless (memq id checked)
	  (set! checked (cons id checked))
	  (tprint "not white " id))
       #f)
      (else
       #t)))

;*---------------------------------------------------------------------*/
;*    iinfo                                                            */
;*---------------------------------------------------------------------*/
(define-struct protoinfo assig method svar)
(define-struct targetinfo decl fun)

(define-struct callloginfo call counter targets)
(define-struct callinfo parent owner)
(define-struct funinfo size freevars decl parent)
   
(define (callloginfo->list cli)
   `(,(callloginfo-counter cli)
     ,(with-access::J2SNode (callloginfo-call cli) (loc) loc)
     ,(map (lambda (n)
	      (with-access::J2SNode (cdr n) (loc)
		 (cons (car n) loc)))
	 (callloginfo-targets cli))))
   
;*---------------------------------------------------------------------*/
;*    j2s-inline! ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-inline! this args)
   (cond
      ((not (isa? this J2SProgram))
       this)
      ((config-get args :profile-call #f)
       (j2s-count-calls! this args)
       this)
      ((config-get args :profile-log #f)
       =>
       (lambda (logfile)
	  (j2s-inline-profile this args logfile)))
      (else
       (j2s-inline-noprofile this args))))

;*---------------------------------------------------------------------*/
;*    j2s-inline-profile ...                                           */
;*    -------------------------------------------------------------    */
;*    Pofile based inlining is as follows:                             */
;*      1- sort all calls by occurrence number                         */
;*      2- filter out call to large functions                          */
;*      3- filter out call to functions using free variables           */
;*      4- filter out self recursive calls                             */
;*      5- inline until the global program expansion reaches the limit */
;*---------------------------------------------------------------------*/
(define (j2s-inline-profile prgm::J2SProgram conf logfile)

   (define (replace-call! parent old new)
      (vector-for-each (lambda (f)
			  (let ((info (class-field-info f)))
			     (when (and (pair? info) (member "ast" info))
				(let ((v ((class-field-accessor f) parent)))
				   (cond
				      ((eq? v old)
				       ((class-field-mutator f) parent new))
				      ((pair? v)
				       (map! (lambda (n)
						(if (eq? n old) new n))
					  v)))))))
	 (class-all-fields (object-class parent))))
   
   (define (inline-filter-dynamic-targets cli)
      ;; select the function to be inlined on a dynamic call site.
      ;; used to select the inline targets of method and closure calls.
      (let* ((targets (callloginfo-targets cli))
	     (targets (filter (lambda (t)
				 ;; filter out targets larger than
				 ;; INLINE-max-function-size
				 (and (<fx (function-size (cdr t))
					 inline-max-function-size)
				      (not (function-freevars? (cdr t)))))
			 targets))
	     (targets (filter (lambda (t)
				 ;; filter out targets that are called less than
				 ;; INLINE-MIN-DISPATCH-PERCENTAGE times
				 (> (/ (car t) (callloginfo-counter cli))
				    inline-min-dispatch-percentage))
			 targets)))
	 targets))

   (define (inline-call-guard-kind cli)
      ;; the kind of guard to protect the inlined code
      (let ((call (callloginfo-call cli)))
	 (with-access::J2SCall call (fun)
	    (with-access::J2SAccess fun (cspecs loc)
	       (case (length cspecs)
		  ((0)
		   (values 'array (/fx inline-max-function-size 3)))
		  ((1)
		   (values 'pmap inline-max-function-size))
		  (else
		   (values 'function (/fx inline-max-function-size 2))))))))

   (define (inline-call-unknown::long cli fuel inliner)
      ;; generic inline of unknown calls
      (let ((targets (inline-filter-dynamic-targets cli)))
	 (if (null? targets)
	     fuel
	     (let* ((callszs (map (lambda (t) (node-size (cdr t))) targets))
		    (dispatchsz (map (lambda (t)
					(if (isa? (cdr t) J2SDeclFun)
					    inline-dispatch-function-test-size
					    inline-dispatch-closure-test-size))
				   targets))
		    (sz (+ (apply + callszs) (apply + dispatchsz))))
		(multiple-value-bind (guard maxfunsz)
		   (inline-call-guard-kind cli)
		   (cond
		      ((or (>fx sz maxfunsz) (>fx sz fuel))
		       fuel)
		      (else
		       (let* ((call (callloginfo-call cli))
			      (node (inliner call guard targets prgm conf)))
			  (with-access::J2SCall call (%info loc fun)
			     (inline-verb loc fun (length targets) sz fuel conf)
			     (replace-call! (callinfo-parent %info) call
				(inline-stmt->expr loc node)))
			  (- fuel (node-size node))))))))))
   
   (define (inline-call-method!::long cli fuel)
      ;; inline a method invocation
      (inline-call-unknown cli fuel inline-method-call-profile))
   
   (define (inline-call-closure!::long cli fuel)
      ;; inline a closure invocation
      (inline-call-unknown cli fuel inline-closure-call-profile))
   
   (define (inline-call-function!::long cli fuel::long)
      (let ((call (callloginfo-call cli)))
	 (with-access::J2SCall call (fun loc)
	    (with-access::J2SRef fun (decl)
	       (with-access::J2SDeclFun decl (val)
		  (with-access::J2SFun val (body)
		     (let ((sz (node-size body)))
			(if (or (>fx sz inline-max-function-size) (>fx sz fuel))
			    (begin
			       (when (>=fx (config-get conf :verbose 0) 4)
				  (with-output-to-port (current-error-port)
				     (lambda ()
					(printf "(~a fun too-large ~a/~a) "
					   (caddr loc) sz fuel))))
			       fuel)
			    (let* ((node (inline-function-call-profile
					    call val
					    prgm conf)))
			       (inline-verb loc fun 1 sz fuel conf)
			       (with-access::J2SCall call (%info)
				  (replace-call! (callinfo-parent %info) call
				     (inline-stmt->expr loc node)))
			       (- fuel (node-size node)))))))))))
   
   (define (inline-profile-call!::long cli fuel::long)
      (let ((call (callloginfo-call cli)))
	 (with-access::J2SCall call (fun loc)
	    (cond
	       ((isa? fun J2SAccess)
		(inline-call-method! cli fuel))
	       ((not (isa? fun J2SRef))
		fuel)
	       (else
		(with-access::J2SRef fun (decl)
		   (if (isa? decl J2SDeclFun)
		       (inline-call-function! cli fuel)
		       (inline-call-closure! cli fuel))))))))
   
   (define (find-node-by-loc loc::long nodes)
      (let loop ((nodes nodes))
	 (if (null? nodes)
	     (error "find-node-by-loc" "Cannot find node" loc)
	     (with-access::J2SNode (car nodes) ((nloc loc))
		(cond
		   ((=fx loc (caddr nloc))
		    (car nodes))
		   (else
		    (loop (cdr nodes))))))))
   
   (define (cli log calls funs)
      (when (and (> (cadr log) 0) (caddr log))
	 (let ((call (find-node-by-loc (car log) calls)))
	    (callloginfo call (cadr log)
	       (map (lambda (target)
		       (cons (car target)
			  (find-node-by-loc (cdr target) funs)))
		  (vector->list (caddr log)))))))
   
   (let ((log (call-profile-log prgm logfile conf)))
      (if log
	  (with-access::J2SProgram prgm (nodes)
	     (let* ((calls (collect-calls* prgm prgm #f))
		    (funs (collect-funs* prgm prgm))
		    (size (node-size prgm))
		    (fuel (- (* size inline-global-expansion) size))
		    (clis (filter-map (lambda (log) (cli log calls funs))
			     (vector->list log))))
		(let loop ((clis clis)
			   (fuel fuel))
		   (if (or (<= fuel 0) (null? clis))
		       (j2s-inline-profile-cleanup! prgm conf)
		       (let* ((cli (car clis))
			      (call (callloginfo-call cli)))
			  (with-access::J2SCall call (%info)
			     (if (not (callinfo-parent %info))
				 (loop (cdr clis)
				    fuel)
				 (loop (cdr clis)
				    (inline-profile-call! cli fuel)))))))))
	  (j2s-inline-noprofile prgm conf))))

;*---------------------------------------------------------------------*/
;*    j2s-inline-noprofile ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-inline-noprofile this::J2SProgram conf)
   (with-access::J2SProgram this (decls nodes)
      ;; count and mark all the calls
      (j2s-count-calls! this conf)
      ;; mark all the function sizes
      (let ((size (node-size this))
	    (pms (ptable (append-map collect-proto-methods* nodes))))
	 (when (string-contains (or (getenv "HOPTRACE") "") "j2s:inline")
	    (with-output-to-port (current-error-port)
	       (lambda ()
		  (print "\n      proto methods:")
		  (hashtable-for-each pms
		     (lambda (k b)
			(print "       " k ": ")
			(for-each (lambda (p)
				     (with-access::J2SAssig (protoinfo-assig p) (loc)
					(print "         " (cadr loc)
					   ":" (caddr loc) " size="
					   (node-size (protoinfo-method p)))))
			   b))))))
	 (let liip ((leaf #f))
	    (let loop ((limit 20))
	       (inline! this #f leaf limit '() pms this conf)
	       (let ((nsize (node-size this)))
		  (when (and (<fx limit inline-max-function-size)
			     (< nsize (* size inline-global-expansion)))
		     (loop (+fx limit 5)))))
	    (unless leaf
	       (liip #t))))
      (j2s-inline-profile-cleanup! this conf)))

;*---------------------------------------------------------------------*/
;*    j2s-inline-profile-cleanup! ...                                  */
;*---------------------------------------------------------------------*/
(define (j2s-inline-profile-cleanup! this::J2SProgram conf)
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
		(decl-usage? decl '(eval)))
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (val)
	       (set! val (dead-inner-decl! val))))
	 #t)))

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
;*    function-size ...                                                */
;*---------------------------------------------------------------------*/
(define (function-size this::J2SFun)
   (with-access::J2SFun this (%info body)
      (cond
	 ((not (funinfo? %info))
	  (set! %info (funinfo (node-size this) #unspecified #f #f)))
	 ((eq? (funinfo-size %info) #unspecified)
	  (funinfo-size-set! %info (node-size this))))
      (funinfo-size %info)))

;*---------------------------------------------------------------------*/
;*    function-freevars? ...                                           */
;*---------------------------------------------------------------------*/
(define (function-freevars? this::J2SFun)
   (with-access::J2SFun this (%info body)
      (cond
	 ((not (funinfo? %info))
	  (set! %info (funinfo #unspecified (free-vars? this '()) #f #f)))
	 ((eq? (funinfo-freevars %info) #unspecified)
	  (funinfo-freevars-set! %info (free-vars? this '()))))
      (funinfo-freevars %info)))

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
		   (j2s->list  (funinfo-parent %info)))))
	    (funinfo-decl-set! %info decl)
	    (with-access::J2SProgram prgm (decls)
	       (set! decls (cons decl decls)))))
      (funinfo-decl %info)))

;*---------------------------------------------------------------------*/
;*    function-generator? ...                                          */
;*---------------------------------------------------------------------*/
(define (function-generator? this::J2SFun)
   (with-access::J2SFun this (generator)
      generator))

;*---------------------------------------------------------------------*/
;*    invalidate-function-size! ...                                    */
;*---------------------------------------------------------------------*/
(define (invalidate-function-size! this::J2SFun)
   (with-access::J2SFun this (%info body)
      (set! %info #f)))
			     
;*---------------------------------------------------------------------*/
;*    ronly-variable? ...                                              */
;*---------------------------------------------------------------------*/
(define (ronly-variable? obj)
   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (decl)
	  (with-access::J2SDecl decl (ronly)
	     (or ronly (not (decl-usage? decl '(assig)))))))
      ((isa? obj J2SDecl)
       (with-access::J2SDecl obj (ronly)
	  (or ronly (not (decl-usage? obj '(assig))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    refonly-variable? ...                                            */
;*---------------------------------------------------------------------*/
(define (refonly-variable? obj)
   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (decl)
	  (with-access::J2SDecl decl (ronly)
	     (or ronly (decl-only-usage? decl '(ref))))))
      ((isa? obj J2SDecl)
       (with-access::J2SDecl obj (ronly)
	  (or ronly (decl-only-usage? obj '(ref)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    LetBlock ...                                                     */
;*---------------------------------------------------------------------*/
(define (LetBlock loc t body)
   (if (pair? t)
       (J2SLetRecBlock #f t body)
       body))

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
				  (if (check-id (string->symbol val))
				      (list (cons val
					       (protoinfo this (method-of rhs) #f)))
				      '()))
			       '()))
			'()))
		 '()))
	  '())))

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
;*    inline!* ...                                                     */
;*---------------------------------------------------------------------*/
(define (inline!* lst targets leaf::bool limit::long stack::pair-nil
	   pmethods prgm conf)
   (map (lambda (o)
	   (inline! o targets leaf limit stack pmethods prgm conf))
      lst))
		       
;*---------------------------------------------------------------------*/
;*    inline! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SNode
		       targets leaf limit::long stack::pair-nil
		       pmethods prgm conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SMeta ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMeta
		       targets leaf limit::long stack::pair-nil
		       pmethods prgm conf)
   (with-access::J2SMeta this (optim debug)
      (if (or (=fx optim 0) (>fx debug 0))
	  this
	  (call-default-walker))))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SMetaInl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMetaInl
		       targets leaf limit::long stack::pair-nil
		       pmethods prgm conf)
   (with-access::J2SMetaInl this (inlstack stmt loc)
      (set! stmt
	 (inline! stmt
	    targets leaf limit (append inlstack stack) pmethods prgm conf))
      this))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SDeclFun
		       targets leaf limit::long stack::pair-nil
		       pmethods prgm conf)
   (with-access::J2SDeclFun this (val id)
      (inline! val targets leaf limit stack pmethods prgm conf)
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFun
		       targets leaf limit::long stack::pair-nil
		       pmethods prgm conf)
   (with-access::J2SFun this (optimize body generator)
      (when (and optimize (not generator))
	 (set! body
	    (inline! body
	       targets leaf limit (cons this stack) pmethods prgm conf)))
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SCall
		       targets leaf limit::long stack::pair-nil
		       pmethods prgm conf)
   
   (define (find-inline-decl-function this::J2SCall fun arity limit stack)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (id)
	       (let ((val (j2sdeclinit-val-fun decl)))
		  (when (and (>=fx (function-arity val) arity)
			     (function-fxarg? val)
			     (not (function-generator? val))
			     (or (not leaf) (function-leaf? val))
			     (not (memq val stack))
			     (<=fx (function-size val) limit)
			     (check-id id)
			     (not (function-self-recursive? val)))
		     val))))))

   (define (find-inline-methods this fun arity)
      (with-access::J2SAccess fun (obj field)
	 (when (isa? field J2SString)
	    (with-access::J2SString field (val)
	       (let* ((mets (filter (lambda (m::struct)
				       (let ((f (protoinfo-method m)))
					  (and (=fx (function-arity f) arity)
					       (function-fxarg? f)
					       (or (not leaf) (function-leaf? f))
					       (not (memq f stack))
					       (not (function-self-recursive? f)))))
			       (or (hashtable-get pmethods val) '())))
		      (sz (apply +
			     (map (lambda (m)
				     (function-size (protoinfo-method m)))
				mets))))
		  (when (or (<fx sz limit)
			    (and (<fx sz inline-max-function-size)
				 (every (lambda (m)
					   (let ((f (protoinfo-method m)))
					      (function-leaf? f)))
				    mets)))
		     mets))))))

   (define (inline-access-call this::J2SCall fun::J2SAccess args loc)
      (let ((mets (find-inline-methods this fun (length args))))
	 (when (pair? mets)
	    (let* ((mets (if (pair? targets)
			     (filter (lambda (t) (memq t mets)) targets)
			     mets))
		   (vals (map protoinfo-method mets))
		   (sz (apply + (map function-size vals))))
	       (inline-verb loc fun (length mets) sz limit conf)
	       (when (pair? stack) 
		  (invalidate-function-size! (car stack)))
	       (let ((e (inline-method-call fun mets args loc
			   '() leaf limit stack pmethods prgm conf)))
		  (inline-stmt->expr loc
		     (inline! e
			'() leaf 0 (append vals stack) pmethods prgm conf)))))))
   
   (define (inline-ref-call this::J2SCall fun::J2SRef thisarg args loc)
      (cond
	 ((find-inline-decl-function this fun (length args) limit stack)
	  =>
	  (lambda (target)
	     (inline-verb loc fun 0 (function-size target) limit conf)
	     (when (pair? stack)
		(invalidate-function-size! (car stack)))
	     (inline-stmt->expr loc
		(inline-function-call target thisarg args loc
		   targets leaf (if targets 0 limit) stack pmethods prgm conf))))
	 ((pair? targets)
	  (when (pair? stack)
	     (invalidate-function-size! (car stack)))
	  (inline-stmt->expr loc
	     (inline-unknown-call fun thisarg  args loc
		targets leaf limit stack pmethods prgm conf)))
	 (else
	  #f)))

   (define (inline-expr-call this fun thisarg args loc)
      (let ((decl (J2SDeclInit '(ref) (gensym '%fun) fun)))
	 (inline-stmt->expr loc
	    (J2SLetBlock (list decl)
	       (inline-ref-call this (J2SRef decl) thisarg args loc)))))
   
   (with-access::J2SCall this (fun thisarg args type loc cache protocol)
      (cond
	 ((null? stack)
	  ;; don't inline at toplevel
	  (call-default-walker))
	 (cache
	  (call-default-walker))
	 ((eq? protocol 'spread)
	  (call-default-walker))
	 ((isa? fun J2SAccess)
	  (or (inline-access-call this fun args loc) this))
	 ((isa? fun J2SRef)
	  (or (inline-ref-call this fun thisarg args loc)
	      (call-default-walker)))
	 ((pair? targets)
	  (or (inline-expr-call this fun thisarg args loc)
	      (call-default-walker)))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    inline-function-call ...                                         */
;*---------------------------------------------------------------------*/
(define (inline-function-call val::J2SFun thisarg args::pair-nil loc
	   targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (with-access::J2SFun val (body thisp params (floc loc))
      (let* ((vals (inline-args (cons thisp params)
		      (if (pair? thisarg)
			  (append thisarg args)
			  (cons (J2SUndefined) args))
		      #f leaf limit stack pmethods prgm conf loc))
	     (nbody (j2s-alpha body (cons thisp params) vals)))
	 (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
	    (J2SMetaInl (cons val stack)
	       (config-get conf :optim 0)
	       (if (> limit 0)
		   (inline! nbody
		      #f leaf limit (cons val stack) pmethods prgm conf)
		   nbody))))))

;*---------------------------------------------------------------------*/
;*    inline-function-call-profile ...                                 */
;*---------------------------------------------------------------------*/
(define (inline-function-call-profile node::J2SCall target::J2SFun prgm conf)

   (define (inline-function-args args)
      (map (lambda (a)
	      (if (isa? a J2SLiteral)
		  a
		  (let ((id (gensym 'a)))
		     (with-access::J2SNode a (loc)
			(J2SLetOpt '(ref) id a)))))
	 args))
   
   (with-access::J2SCall node (thisargs args loc)
      (with-access::J2SFun target (body thisp params (floc loc))
	 (let* ((vals (inline-function-args args))
		(nbody (j2s-alpha body (cons thisp params) vals)))
	    (LetBlock loc (filter (lambda (b) (isa? b J2SDecl)) vals)
	       nbody)))))
   
;*---------------------------------------------------------------------*/
;*    inline-unknown-call ...                                          */
;*---------------------------------------------------------------------*/
(define (inline-unknown-call ref::J2SRef thisarg args::pair-nil loc
	   targets leaf limit::long stack::pair-nil pmethods prgm conf)
   (let loop ((targets targets))
      (if (null? targets)
	  (J2SStmtExpr (J2SCall* ref args))
	  (let* ((target (car targets))
		 (fun (targetinfo-fun target)))
	     (if (< (function-size fun) limit)
		 (begin
		    (inline-verb loc fun 0 (function-size fun) limit conf)
		    (J2SIf (J2SHopCall (J2SHopRef/rtype 'eq? 'bool)
			      (J2SRef (with-access::J2SRef ref (decl) decl))
			      (J2SRef (targetinfo-decl target)))
		       (inline-function-call fun thisarg args loc
			  #f leaf 0 stack pmethods prgm conf)
		       (loop (cdr targets))))
		 (loop '()))))))
   
;*---------------------------------------------------------------------*/
;*    inline-closure-call-profile ...                                  */
;*---------------------------------------------------------------------*/
(define (inline-closure-call-profile node::J2SCall guard targets::pair prgm conf)
   
   (define (j2sref-ronly? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (with-access::J2SDecl decl (ronly)
	       ronly))))
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))
   
   (define (cache-check c loc obj field kont inline::J2SStmt)
      (J2SIf (J2SCacheCheck 'proto-method c obj field)
	 inline
	 (kont)))
   
   (define (get-svar callee)
      (if (protoinfo-svar callee)
	  (protoinfo-svar callee)
	  (let ((fun (gensym '%met)))
	     (protoinfo-svar-set! callee fun)
	     (with-access::J2SProgram prgm (globals)
		(set! globals (cons `(define ,fun #unspecified) globals))
		(with-access::J2SAssig (protoinfo-assig callee) (rhs loc)
		   (set! rhs
		      (J2SSequence
			 (J2SAssig (J2SHopRef fun) rhs)
			 (J2SHopRef fun)))))
	     fun)))
   
   (define (inline-closure-args args)
      (map (lambda (a)
	      (cond
		 ((isa? a J2SLiteral)
		  a)
		 ((j2sref-ronly? a)
		  a)
		 (else
		  (let ((id (gensym 'a)))
		     (with-access::J2SNode a (loc)
			(J2SLetOpt '(ref) id a))))))
	 args))
   
   (define (inline-method obj::J2SRef field target::pair args cache loc kont)
      (with-access::J2SFun (cdr target) (body thisp params (floc loc))
	 (with-access::J2SRef obj (decl)
	    (cache-check cache loc obj field kont
	       (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) args)
		  (j2s-alpha body
		     (cons thisp params) (cons decl args)))))))
   
   (define (inline-object-method-call-pmap fun obj::J2SDecl args loc)
      (with-access::J2SAccess fun (field cspecs)
	 (let loop ((targets targets)
		    (funcaches '()))
	    (if (null? targets)
		(let* ((c (get-cache prgm))
		       (f (duplicate::J2SAccess fun
			     (obj (J2SRef obj))))
		       (r (J2SLetOpt '(call) (gensym 'r)
			     (J2SMethodCall/cache* f (list (J2SRef obj)) args
				'(pmap-inline vtable-inline) c))))
		   (J2SLetRecBlock #f (list r)
		      (let loop ((cs funcaches))
			 (if (null? cs)
			     (J2SSeq*
				(map (lambda (c)
					(J2SStmtExpr
					   (J2SCacheUpdate 'proto-reset
					      (car c) (J2SRef obj))))
				   funcaches))
			     (let* ((target (cdar cs))
				    (v (function-glodecl (cdr target) prgm)))
				;; cs: cache x callee
				;; callee: decl-or-id x fun
				(J2SIf
				   (J2SCacheCheck 'method
				      c (if (isa? v J2SDecl)
					    (J2SRef v)
					    (J2SHopRef v)))
				   (J2SSeq*
				      (map (lambda (c)
					      (J2SStmtExpr
						 (if (eq? c (car cs))
						     (J2SCacheUpdate 'proto-method
							(car c) (J2SRef obj))
						     (J2SCacheUpdate 'proto-reset
							(car c) (J2SRef obj)))))
					 funcaches))
				   (loop (cdr cs))))))
		      (J2SReturn #t (J2SRef r))))
		(let ((cache (get-cache prgm)))
		   (inline-method (J2SRef obj)
		      field (car targets) args cache loc
		      (lambda ()
			 (loop (cdr targets)
			    (cons (cons cache (car targets)) funcaches)))))))))
   
   (define (inline-closure-call fun::J2SExpr args loc)
      (let ((fun (J2SLetOpt '(ref) (gensym 'fun) fun)))
	 (J2SLetBlock (list fun)
	    (let loop ((targets targets))
	       (cond
		  ((null? targets)
		   (J2SStmtExpr (J2SCall* (J2SRef fun) args)))
		  ((and (isa? (caar targets) J2SDecl)
			(with-access::J2SDecl (caar targets) (scope)
			   (eq? scope '%scope)))
		   (let ((callee (car targets)))
		      (J2SIf (J2SBinary 'eq?
				(J2SRef fun) (J2SRef (car callee)))
			 (with-access::J2SFun (cdr callee) (body thisp params (floc loc))
			    (LetBlock floc (filter (lambda (b)
						      (isa? b J2SDecl))
					      args)
			       (j2s-alpha body
				  (cons thisp params) (cons obj args))))
			 (loop (cdr targets)))))
		  (else
		   (loop (cdr targets))))))))
   
   (with-access::J2SCall node (fun args loc)
      ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
      ;; shape of the test that suits the tyflow analysis
      (let* ((vals (inline-closure-args args))
	     (t (filter (lambda (b) (isa? b J2SDecl)) vals))
	     (args (map (lambda (v)
			   (if (isa? v J2SDecl)
			       (with-access::J2SDecl v (loc)
				  (J2SRef v))
			       v))
		      vals)))
	 (inline-closure-call fun args log))))

;*---------------------------------------------------------------------*/
;*    inline-method-call-profile ...                                   */
;*---------------------------------------------------------------------*/
(define (inline-method-call-profile node::J2SCall guard targets::pair prgm conf)
   
   (define (j2sref-ronly? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (with-access::J2SDecl decl (ronly)
	       ronly))))
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))
   
   (define (cache-check c loc obj field kont inline::J2SStmt)
      (J2SIf (J2SCacheCheck 'proto-method c obj field)
	 inline
	 (kont)))
   
   (define (get-svar callee)
      (if (protoinfo-svar callee)
	  (protoinfo-svar callee)
	  (let ((fun (gensym '%met)))
	     (protoinfo-svar-set! callee fun)
	     (with-access::J2SProgram prgm (globals)
		(set! globals (cons `(define ,fun #unspecified) globals))
		(with-access::J2SAssig (protoinfo-assig callee) (rhs loc)
		   (set! rhs
		      (J2SSequence
			 (J2SAssig (J2SHopRef fun) rhs)
			 (J2SHopRef fun)))))
	     fun)))
   
   (define (inline-method-args args)
      (map (lambda (a)
	      (cond
		 ((isa? a J2SLiteral)
		  a)
		 ((j2sref-ronly? a)
		  a)
		 (else
		  (let ((id (gensym 'a)))
		     (with-access::J2SNode a (loc)
			(J2SLetOpt '(ref) id a))))))
	 args))
   
   (define (inline-method obj::J2SRef field target::pair args cache loc kont)
      (with-access::J2SFun (cdr target) (body thisp params (floc loc))
	 (with-access::J2SRef obj (decl)
	    (cache-check cache loc obj field kont
	       (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) args)
		  (j2s-alpha body
		     (cons thisp params) (cons decl args)))))))
   
   (define (inline-object-method-call-pmap fun obj::J2SDecl args loc)
      (with-access::J2SAccess fun (field cspecs)
	 (let loop ((targets targets)
		    (funcaches '()))
	    (if (null? targets)
		(let* ((c (get-cache prgm))
		       (f (duplicate::J2SAccess fun
			     (obj (J2SRef obj))))
		       (r (J2SLetOpt '(call) (gensym 'r)
			     (J2SMethodCall/cache* f (list (J2SRef obj)) args
				'(pmap-inline vtable-inline) c))))
		   (J2SLetRecBlock #f (list r)
		      (let loop ((cs funcaches))
			 (if (null? cs)
			     (J2SSeq*
				(map (lambda (c)
					(J2SStmtExpr
					   (J2SCacheUpdate 'proto-reset
					      (car c) (J2SRef obj))))
				   funcaches))
			     (let* ((target (cdar cs))
				    (v (function-glodecl (cdr target) prgm)))
				;; cs: cache x callee
				;; callee: decl-or-id x fun
				(J2SIf
				   (J2SCacheCheck 'method
				      c (if (isa? v J2SDecl)
					    (J2SRef v)
					    (J2SHopRef v)))
				   (J2SSeq*
				      (map (lambda (c)
					      (J2SStmtExpr
						 (if (eq? c (car cs))
						     (J2SCacheUpdate 'proto-method
							(car c) (J2SRef obj))
						     (J2SCacheUpdate 'proto-reset
							(car c) (J2SRef obj)))))
					 funcaches))
				   (loop (cdr cs))))))
		      (J2SReturn #t (J2SRef r))))
		(let ((cache (get-cache prgm)))
		   (inline-method (J2SRef obj)
		      field (car targets) args cache loc
		      (lambda ()
			 (loop (cdr targets)
			    (cons (cons cache (car targets)) funcaches)))))))))
   
   (define (inline-object-method-call-function fun obj::J2SDecl args loc)
      (with-access::J2SAccess fun (field cspecs loc)
	 (let ((met (J2SLetOpt '(ref) (gensym 'met)
		       (duplicate::J2SAccess fun
			  (obj (J2SRef obj))))))
	    (J2SLetBlock (list met)
	       (let loop ((targets targets))
		  (cond
		     ((null? targets)
		      (J2SStmtExpr
			 (J2SMethodCall* (J2SRef met)
			    (list (J2SRef obj)) args)))
		     ((and (isa? (caar targets) J2SDecl)
			   (with-access::J2SDecl (caar targets) (scope)
			      (eq? scope '%scope)))
		      (let ((callee (car targets)))
			 (J2SIf (J2SBinary 'eq?
				   (J2SRef met) (J2SRef (car callee)))
			    (with-access::J2SFun (cdr callee) (body thisp params (floc loc))
			       (LetBlock floc (filter (lambda (b)
							 (isa? b J2SDecl))
						 args)
				  (j2s-alpha body
				     (cons thisp params) (cons obj args))))
			    (loop (cdr targets)))))
		     (else
		      (loop (cdr targets)))))))))
   
   (define (inline-object-method-call fun::J2SAccess obj::J2SDecl args loc guard)
      (case guard
	 ((pmap)
	  (inline-object-method-call-pmap fun obj args loc))
	 ((function array)
	  (inline-object-method-call-function fun obj args loc))
	 (else
	  (inline-object-method-call-pmap fun obj args loc))))
   
   (with-access::J2SCall node (fun args loc)
      (with-access::J2SAccess fun (obj field)
	 ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
	 ;; shape of the test that suits the tyflow analysis
	 (let* ((vals (inline-method-args args))
		(t (filter (lambda (b) (isa? b J2SDecl)) vals))
		(args (map (lambda (v)
			      (if (isa? v J2SDecl)
				  (with-access::J2SDecl v (loc)
				     (J2SRef v))
				  v))
			 vals)))
	    (cond
	       ((not (eq? (j2s-type obj) 'object))
		(if (j2sref-ronly? obj)
		    (with-access::J2SRef obj (decl)
		       (LetBlock loc t
			  (J2SIf (J2SHopCall
				    (if (eq? guard 'array)
					(J2SHopRef/rtype 'js-array? 'bool)
					(J2SHopRef/rtype 'js-object? 'bool))
				    (J2SRef decl))
			     (inline-object-method-call fun decl args loc guard)
			     (J2SMeta 0 0
				(J2SStmtExpr
				   (J2SCall* (J2SAccess (J2SRef decl) field)
				      args))))))
		    (let* ((id (gensym 'this))
			   (decl (J2SLetOpt '(get) id obj)))
		       (LetBlock loc (cons decl t)
			  (J2SIf (J2SHopCall
				    (if (eq? guard 'array)
					(J2SHopRef/rtype 'js-array? 'bool)
					(J2SHopRef/rtype 'js-object? 'bool))
				    (J2SRef decl))
			     (inline-object-method-call fun decl args loc guard)
			     (J2SMeta 0 0
				(J2SStmtExpr
				   (J2SCall* (J2SAccess (J2SRef decl) field)
				      args))))))))
	       ((not (j2sref-ronly? obj))
		(let* ((id (gensym 'this))
		       (decl (J2SLetOpt '(get) id obj)))
		   (LetBlock loc (cons decl t)
		      (inline-object-method-call fun decl args loc guard))))
	       (else
		(with-access::J2SRef obj (decl)
		   (inline-object-method-call fun decl args loc guard))))))))

;*---------------------------------------------------------------------*/
;*    inline-method-call ...                                           */
;*---------------------------------------------------------------------*/
(define (inline-method-call fun::J2SAccess callees::pair args::pair-nil loc
	   targets leaf limit::long stack::pair-nil pmethods prgm conf)
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))

   (define (cache-check c loc obj field kont inline::J2SStmt)
      (J2SIf (J2SCacheCheck 'proto-method c obj field)
	 inline
	 (kont)))

   (define (get-svar callee)
      (if (protoinfo-svar callee)
	  (protoinfo-svar callee)
	  (let ((fun (gensym '%met)))
	     (protoinfo-svar-set! callee fun)
	     (with-access::J2SProgram prgm (globals)
		(set! globals (cons `(define ,fun #unspecified) globals))
		(with-access::J2SAssig (protoinfo-assig callee) (rhs loc)
		   (set! rhs
		      (J2SSequence
			 (J2SAssig (J2SHopRef fun) rhs)
			 (J2SHopRef fun)))))
	     fun)))

   (define (inline-method-args args)
      (map (lambda (a)
	      (if (isa? a J2SLiteral)
		  a
		  (let ((id (gensym 'a)))
		     (with-access::J2SNode a (loc)
			(J2SLetOpt '(ref) id
			   (inline! a
			      #f leaf limit stack pmethods prgm conf))))))
	 args))

   (define (inline-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (let ((vals (inline-args params args
			   #f leaf limit stack pmethods prgm conf loc)))
	       (with-access::J2SRef obj (decl)
		  (cache-check cache loc obj field kont
		     (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (inline!
			      (j2s-alpha body
				 (cons thisp params) (cons decl vals))
			      #f leaf limit
			      (cons val stack) pmethods prgm conf)))))))))
   
   (define (inline-object-method-call fun obj args)
      (with-access::J2SAccess fun (field cspecs)
	 (let loop ((callees callees)
		    (caches '()))
	    (if (null? callees)
		(let* ((c (get-cache prgm))
		       (f (duplicate::J2SAccess fun
			     (obj obj)))
		       (r (J2SLetOpt '(call) (gensym 'r)
			     (J2SMethodCall/cache* f (list obj) args
				'(pmap-inline vtable-inline) c))))
		   (J2SLetRecBlock #f (list r)
		      (let loop ((cs caches))
			 (if (null? cs)
			     (J2SSeq*
				(map (lambda (c)
					(J2SStmtExpr
					   (J2SCacheUpdate 'proto-reset
					      (car c) obj)))
				   caches))
			     (let ((v (get-svar (cdar cs))))
				(J2SIf
				   (J2SCacheCheck 'method
				      c (J2SHopRef v))
				   (J2SSeq*
				      (map (lambda (c)
					      (J2SStmtExpr
						 (if (eq? c (car cs))
						     (J2SCacheUpdate 'proto-method
							(car c) obj)
						     (J2SCacheUpdate 'proto-reset
							(car c) obj))))
					 caches))
				   (loop (cdr cs))))))
		      (J2SReturn #t (J2SRef r))))
		(let ((cache (get-cache prgm)))
		   (inline-method obj field (car callees) args cache loc
		      (lambda ()
			 (loop (cdr callees)
			    (cons (cons cache (car callees)) caches)))))))))
   
   (with-access::J2SAccess fun (obj field loc)
      ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
      ;; shape of the test that suits the tyflow analysis
      (let* ((vals (inline-method-args args))
	     (t (filter (lambda (b) (isa? b J2SDecl)) vals))
	     (args (map (lambda (v)
			   (if (isa? v J2SDecl)
			       (with-access::J2SDecl v (loc)
				  (J2SRef v))
			       v))
		      vals)))
	 (cond
	    ((not (eq? (j2s-type obj) 'object))
	     (let* ((id (gensym 'this))
		    (d (J2SLetOpt '(get) id obj)))
		(LetBlock loc (cons d t)
		   (J2SIf (J2SHopCall (J2SHopRef/rtype 'js-object? 'bool)
			     (J2SRef d))
		      (inline-object-method-call fun (J2SRef d) args)
		      (J2SMeta 0 0
			 (J2SStmtExpr
			    (J2SCall* (J2SAccess (J2SRef d) field)
			       args)))))))
	    ((not (isa? obj J2SRef))
	     (let* ((id (gensym 'this))
		    (d (J2SLetOpt '(get) id obj)))
		(LetBlock loc (cons d t)
		   (inline-object-method-call fun (J2SRef d) args))))
	    (else
	     (inline-object-method-call fun obj args))))))

;*---------------------------------------------------------------------*/
;*    inline-args ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-args params args targets leaf limit stack pmethods prgm conf loc)
   (let ((lena (length args))
	 (lenp (length params)))
      (map (lambda (p a)
	      (cond
		 ((and (ronly-variable? p) (isa? a J2SLiteral))
		  a)
		 (else
		  (with-access::J2SDecl p (usage id writable)
		     (with-access::J2SNode a (loc)
			(let ((d (J2SLetOptRo usage (gensym id)
				    (inline! a
				       targets leaf limit stack pmethods prgm conf))))
			   (with-access::J2SDecl d ((w writable))
			      (set! w writable))
			   d))))))
	 params
	 (if (<fx lena lenp)
	     (append args
		;; complement with missing args
		(map! (lambda (i) (J2SUndefined)) (iota (-fx lenp lena))))
	     args))))

;*---------------------------------------------------------------------*/
;*    inline-stmt->expr::J2SExpr ...                                   */
;*    -------------------------------------------------------------    */
;*    Transforms an inline function body statement into an expression. */
;*---------------------------------------------------------------------*/
(define (inline-stmt->expr::J2SExpr loc body::J2SStmt)
   
   (define (stmt->expr node)
      (cond
	 ((isa? node J2SStmtExpr)
	  (with-access::J2SStmtExpr node (expr)
	     expr))
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
	  (let ((be (J2SBindExit lbl bd)))
	     (for-each (lambda (ret::J2SReturn)
			  (with-access::J2SReturn ret (from)
			     (set! from be)))
		(cell-ref cell))
	     be))
	 ((stmt->expr bd)
	  =>
	  (lambda (expr) expr))
	 (else
	  (J2SBindExit #f bd)))))
   
;*---------------------------------------------------------------------*/
;*    inline-expr! ...                                                 */
;*---------------------------------------------------------------------*/
(define (inline-expr!::J2SExpr expr::J2SExpr call stmt)
   (inline-node! expr call stmt))

;*---------------------------------------------------------------------*/
;*    inline-expr*! ...                                                */
;*---------------------------------------------------------------------*/
(define (inline-expr*! exprs::pair-nil call stmt)
   (map! (lambda (e) (inline-expr! e call stmt)) exprs))
   
;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SNode ...                                       */
;*    -------------------------------------------------------------    */
;*    Replace expression CALL with statement STMT.                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SNode call::J2SCall inl::J2SNode)
   (call-default-walker)
   this)

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SCall call inl)
   (if (eq? this call)
       inl
       (with-access::J2SCall this (fun args loc)
	  (set! fun (inline-expr! fun call inl))
	  (set! args (inline-expr*! args call inl))
	  this)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SNew ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SNew call inl)
   (with-access::J2SNew this (clazz args loc)
      (set! clazz (inline-expr! clazz call inl))
      (set! args (inline-expr*! args call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-mode! ::J2SParen ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SParen call inl)
   (with-access::J2SParen this (expr)
      (set! expr (inline-node! expr call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SUnary ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SUnary call inl)
   (with-access::J2SUnary this (expr loc)
      (set! expr (inline-expr! expr call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SBinary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SBinary call inl)
   (with-access::J2SBinary this (rhs lhs loc)
      (set! lhs (inline-expr! lhs call inl))
      (set! rhs (inline-expr! rhs call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAccess ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAccess call inl)
   (with-access::J2SAccess this (obj field)
      (set! obj (inline-expr! obj call inl))
      (set! field (inline-expr! field call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SStmtExpr ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SStmtExpr call inl)
   (with-access::J2SStmtExpr this (expr loc)
      (let ((node (inline-node! expr call inl)))
	 (if (isa? node J2SExpr)
	     (begin
		(set! expr node)
		this)
	     (unreturn! node
		(lambda (n::J2SReturn)
		   (with-access::J2SReturn n (expr)
		      (J2SStmtExpr expr))))))))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SIf ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SIf call inl)
   (with-access::J2SIf this (test then else loc)
      (set! then (inline-node! then call inl))
      (set! else (inline-node! else call inl))
      (set! test (inline-expr! test call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SReturn ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SReturn call inl)
   (with-access::J2SReturn this (expr tail)
      (set! expr (inline-expr! expr call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SAssig ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SAssig call inl)
   ;; for now, only the rhs part is considered for inlining
   (with-access::J2SAssig this (lhs rhs loc)
      (set! rhs (inline-expr! rhs call inl))
      (if (isa? lhs J2SRef)
	  (let ((node (inline-node! rhs call inl)))
	     (if (isa? node J2SExpr)
		 (begin
		    (set! rhs node)
		    this)
		 (unreturn! node
		    (lambda (n::J2SReturn)
		       (with-access::J2SReturn n (expr loc)
			  (set! expr (J2SAssig lhs expr))
			  n)))))
	  this)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SLoop ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SLoop call inl)
   (with-access::J2SLoop this (body)
      (set! body (inline-node! body call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SFor ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SFor call inl)
   (with-access::J2SFor this (init test incr body)
      (set! test (inline-expr! test call inl))
      (set! incr (inline-expr! incr call inl))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SWhile ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SWhile call inl)
   (with-access::J2SWhile this (test body)
      (set! test (inline-expr! test call inl))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SDeclInit ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SDeclInit call inl)
   (with-access::J2SDeclInit this (val)
      (set! val (inline-expr! val call inl))
      this))

;*---------------------------------------------------------------------*/
;*    inline-node! ::J2SLetBlock ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (inline-node! this::J2SLetBlock call inl)
   (with-access::J2SLetBlock this (decls nodes)
      (set! decls (map! (lambda (d) (inline-node! d call inl)) decls))
      (set! nodes (map! (lambda (n) (inline-node! n call inl)) nodes))
      this))
   
;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SNode ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SNode conv)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SReturn ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SReturn conv)
   (with-access::J2SReturn this (tail exit expr loc)
      (if (or tail exit)
	  (conv this)
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SFun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SFun conv)
   this)

;*---------------------------------------------------------------------*/
;*    unreturn! ::J2SMethod ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (unreturn! this::J2SMethod conv)
   this)

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
	    (set! tail #f)
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
;*    node-return-count ...                                            */
;*---------------------------------------------------------------------*/
(define (node-return-count this::J2SNode max)
   (let ((cell (make-cell 0)))
      (return-count this cell max)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    return-count ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (return-count this::J2SNode cnt max)
   (if (< (cell-ref cnt) max)
       (call-default-walker)
       (cell-ref cnt)))

;*---------------------------------------------------------------------*/
;*    return-count ::J2SReturn ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (return-count this::J2SReturn cnt max)
   (cell-set! cnt (+fx 1 (cell-ref cnt))))

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
;*    inline-verb ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-verb loc fun targets fsize limit conf)
   
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
	    (display " (")
	    (display fsize)
	    (display "/")
	    (display limit)
	    (display ")")
	    (when (>=fx targets 1)
	       (display " [")
	       (display targets)
	       (display "]"))))))


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
      (or (isa? val J2SLiteralCnst)
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
	 (set! %info (funinfo #unspecified #unspecified this parent))
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
		(funinfo #unspecified #unspecified #unspecified this))
	     (cons rhs (collect-funs* body parent))))
	 ((isa? rhs J2SMethod)
	  (with-access::J2SMethod rhs (method)
	     (with-access::J2SFun method (%info body loc)
		(set! %info
		   (funinfo #unspecified #unspecified #unspecified this))
		(cons method (collect-funs* body parent)))))
	 (else
	  (call-next-method)))))
   
;* {*---------------------------------------------------------------------*} */
;* {*    collect-funs* ::J2SFun ...                                       *} */
;* {*---------------------------------------------------------------------*} */
;* (define-walk-method (collect-funs* this::J2SFun)                    */
;*    (cons (cons this #f) (call-default-walker)))                     */

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
   (with-access::J2SCall this (fun thisarg args %info loc)
      (let ((next (append
		     (collect-calls* fun this owner)
		     (collect-calls* thisarg this owner)
		     (collect-calls* args this owner))))
	 (if loc
	     (begin
		(set! %info (callinfo parent owner))
		(cons this next))
	     next))))

;*---------------------------------------------------------------------*/
;*    free-vars? ...                                                   */
;*    -------------------------------------------------------------    */
;*    A predicate that is true IFF the ast uses free variables.        */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars? this::J2SNode env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    free-vars? ::J2SRef ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars? this::J2SRef env)
   (with-access::J2SRef this (decl loc)
      (with-access::J2SDecl decl (scope)
	 (when (and (not (eq? scope '%scope)) (not (memq decl env))) (memq decl env)
	    (tprint "FREE " loc))
	 (and (not (eq? scope '%scope)) (not (memq decl env))))))

;*---------------------------------------------------------------------*/
;*    free-vars? ::J2SFun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars? this::J2SFun env)
   (with-access::J2SFun this (decl params thisp body)
      (free-vars? body (cons* decl thisp params env))))

;*---------------------------------------------------------------------*/
;*    free-vars? ::J2SLetBlock ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars? this::J2SLetBlock env)
   (with-access::J2SLetBlock this (decls nodes)
      (let ((env (append decls env)))
	 (any (lambda (n) (free-vars? n env)) nodes))))
   
;*---------------------------------------------------------------------*/
;*    free-vars? ::J2SBlock ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (free-vars? this::J2SBlock env)
   (with-access::J2SBlock this (decls nodes)
      (let loop ((env env)
		 (nodes nodes))
	 (cond
	    ((null? nodes)
	     #f)
	    ((isa? (car nodes) J2SDeclInit)
	     (with-access::J2SDeclInit (car nodes) (val)
		(let ((env (cons (car nodes) env)))
		   (or (free-vars? val (cons (car nodes) env))
		       (loop env (cdr nodes))))))
	    ((isa? (car nodes) J2SDecl)
	     (loop (cons (car nodes) env) (cdr nodes)))
	    (else
	     (any (lambda (n) (free-vars? n env) nodes)))))))
   
;*---------------------------------------------------------------------*/
;*    update-parent! ...                                               */
;*---------------------------------------------------------------------*/
(define (update-parent! this::J2SNode old new)
   
   (define (update-pair! f old new)
      (let loop ((f f))
	 (cond
	    ((not (pair? f))
	     #f)
	    ((eq? (car f) old)
	     (set-car! f new)
	     f)
	    (else
	     (loop (cdr f))))))
      
   (let ((fields (class-all-fields (object-class this))))
      (let loop ((i (-fx (vector-length fields) 1)))
	 (if (=fx i -1)
	     (error "update-parent!" "Cannot find node" (j2s->list old))
	     (let* ((f (vector-ref fields i))
		    (info (class-field-info f)))
		(if (and (pair? info) (member "ast" info))
		    (let ((val ((class-field-accessor f) this)))
		       (cond
			  ((eq? val old)
			   ((class-field-mutator f) this new))
			  ((update-pair! val old new)
			   =>
			   (lambda (newp)
			      ((class-field-mutator f) this newp)))
			  (else
			   (loop (-fx i 1)))))
		    (loop (-fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    call-profile-log ...                                             */
;*---------------------------------------------------------------------*/
(define (call-profile-log this::J2SProgram logfile conf)
   
   (define (get key lst)
      (let ((c (assq key lst)))
	 (when (pair? c) (cdr c))))

   (define (sum-method-count c)
      (let loop ((i (-fx (vector-length c) 1))
		 (s #l0))
	 (if (<fx i 0)
	     s
	     (let ((e (vector-ref c i)))
		(let ((cnt (get 'cnt e)))
		   (loop (-fx i 1) (+ s cnt)))))))

   (let* ((log (load-profile-log logfile))
	  (srcs (get 'calls log))
	  (file (config-get conf :filename)))
      (when (vector? srcs)
	 (when (>=fx (config-get conf :verbose 0) 3)
	    (with-output-to-port (current-error-port)
	       (lambda ()
		  (display "\n      ")
		  (display* "loading log file "
		     (string-append "\"" logfile "\"...")))))
	 (let loop ((i (-fx (vector-length srcs) 1)))
	    (when (>=fx i 0)
	       (let ((filename (get 'filename (vector-ref srcs i))))
		  (if (string=? file filename)
		      (let ((verb (make-cell 0))
			    (cvec (get 'calls (vector-ref srcs i))))
			 (vector-map! (lambda (c)
					 (let ((cnt (get 'cnt c)))
					    (list (get 'point c)
					       (if (vector? cnt)
						   (sum-method-count cnt)
						   cnt)
					       (if (vector? cnt)
						   (vector-map! (lambda (c)
								   (cons (get 'cnt c)
								      (get 'point c)))
						      cnt)
						   #f))))
			    cvec)
			 (sort (lambda (x y) (>= (cadr x) (cadr y))) cvec))
		      (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    load-profile-log ...                                             */
;*---------------------------------------------------------------------*/
(define (load-profile-log logfile)

   (define (counts-val val)
      (vector-map! (lambda (v)
		      (cond
			 ((vector? v)
			  (vector-map! (lambda (c)
					  (match-case c
					     (((loc . ?loc) (cnt . ?cnt))
					      (cons cnt loc))
					     (((cnt . ?cnt) (loc . ?loc))
					      (cons cnt loc))
					     (else
					      (error "load-profile-log"
						 "bad count value" c))))
			     v))
			 ((number? v)
			  v)
			 (else
			  (error "load-profile-log" "bad count value" v))))
	 val))

   (call-with-input-file logfile
      (lambda (ip)
	 (let ((fprofile #f))
	    (json-parse ip
	       :array-alloc (lambda () (make-cell '()))
	       :array-set (lambda (a i val)
			     (cell-set! a (cons val (cell-ref a))))
	       :array-return (lambda (a i)
				(list->vector (reverse! (cell-ref a))))
	       :object-alloc (lambda ()
				(make-cell '()))
	       :object-set (lambda (o p val)
			      (cond
				 ((string=? p "calls")
				  (unless fprofile
				     (error "fprofile" "Wrong log format" logfile))
				  (cell-set! o
				     (cons (cons 'calls val)
					(cell-ref o))))
				 ((string=? p "format")
				  (set! fprofile (equal? val "fprofile")))
				 ((string=? p "counts")
				  (cell-set! o
				     (cons (cons 'counts (counts-val val))
					(cell-ref o))))
				 (else
				  (cell-set! o
				     (cons (cons (string->symbol p) val)
					(cell-ref o))))))
	       :object-return (lambda (o)
				 (reverse! (cell-ref o)))
	       :parse-error (lambda (msg fname loc)
			       (error/location "fprofile" "Wrong JSON file" msg
				  fname loc)))))))
