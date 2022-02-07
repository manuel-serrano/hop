;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Mon Feb  7 08:37:47 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function/Method inlining optimization                            */
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
   
   (include "ast.sch"
	    "usage.sch")
   
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
	   __js2scheme_classutils)

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
   2)

(define inline-max-function-size
   ;; the maximum body size of inlined function candidates
   160)

(define inline-max-method-size
   ;; the maximum body size of inlined method candidates
   64)

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
(define blacklistloc
   (if (string? (getenv "BLACKLISTLOC"))
       (map string->number
	  (call-with-input-string (getenv "BLACKLISTLOC") port->string-list))
       '()))
(define whitelistloc
   (if (string? (getenv "WHITELISTLOC"))
       (map string->number 
	  (call-with-input-string (getenv "WHITELISTLOC") port->string-list))
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
(define-struct protoinfo assig method svar owner)
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
;*    Profile based inlining is as follows:                            */
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
					 (min inline-max-function-size
					    (function-max-expansion (cdr t))))
				      (not (function-arguments? (cdr t)))
				      (not (function-varargs? (cdr t)))
				      (not (function-newtarget? (cdr t)))
				      (not (function-delete-argument? (cdr t)))
				      (not (function-generator? (cdr t)))
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
	    (if (isa? fun J2SAccess)
		(with-access::J2SAccess fun (cspecs loc)
		   (case (length cspecs)
		      ((0)
		       (values 'array (/fx inline-max-function-size 3)))
		      ((1)
		       (values 'pmap inline-max-function-size))
		      (else
		       (values 'function (/fx inline-max-function-size 2)))))
		(values 'function inline-max-function-size)))))
   
   (define (inline-call-unknown::long cli fuel allcnt inliner)
      ;; generic inline of unknown calls
      (let ((targets (filter-rutype (inline-filter-dynamic-targets cli) prgm)))
	 (if (null? targets)
	     0
	     (let* ((call (callloginfo-call cli))
		    (callszs (map (lambda (t) (function-size (cdr t))) targets))
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
		       0)
		      ((and (< (callloginfo-counter cli) (* allcnt 0.001))
			    (>fx sz (node-size call)))
		       0)
		      (else
		       (let ((node (inliner call guard targets #f prgm conf)))
			  (if (isa? node J2SNode)
			      (with-access::J2SCall call (%info loc fun)
				 (inline-verb loc fun (map car targets) sz fuel
				    allcnt conf)
				 (replace-call! (callinfo-parent %info) call
				    (inline-stmt->expr loc node
				       (function-rutype (car targets))))
				 (function-size-reset! (callinfo-owner %info))
				 (node-size node))
			      0)))))))))
   
   (define (inline-call-method!::long cli fuel allcnt)
      ;; inline a method invocation
      (inline-call-unknown cli fuel allcnt inline-method-call-profile))
   
   (define (inline-call-closure!::long cli fuel allcnt)
      ;; inline a closure invocation
      (inline-call-unknown cli fuel allcnt inline-closure-call-profile))
   
   (define (inline-call-function!::long cli fuel::long allcnt)
      (let ((call (callloginfo-call cli)))
	 (with-access::J2SCall call (fun loc)
	    (with-access::J2SRef fun (decl)
	       (with-access::J2SDeclFun decl (val)
		  (let ((sz (function-size val)))
		     (cond
			((or (>fx sz inline-max-function-size)
			     (>fx sz (function-max-expansion val))
			     (>fx sz fuel))
			 0)
			((and (< (callloginfo-counter cli) (* allcnt 0.001))
			      (>fx sz (node-size call)))
			 0)
			((or (function-arguments? val)
			     (function-varargs? val)
			     (function-newtarget? val)
			     (function-delete-argument? val)
			     (function-generator? val))
			 0)
			(else
			 (let* ((node (inline-function-call-profile
					 call val
					 #f prgm conf)))
			    (inline-verb loc fun
			       (list (callloginfo-counter cli))
			       sz fuel allcnt conf)
			    (with-access::J2SFun val (rutype)
			       (with-access::J2SCall call (%info)
				  (replace-call! (callinfo-parent %info) call
				     (inline-stmt->expr loc node rutype))))
			    (node-size node))))))))))
   
   (define (inline-profile-call!::long cli fuel::long allcnt::llong)
      (let ((call (callloginfo-call cli)))
	 (with-access::J2SCall call (fun loc)
	    (cond
	       ((and (isa? fun J2SAccess)
		     (config-get conf :optim-inline-method #f))
		(inline-call-method! cli fuel allcnt))
	       ((not (isa? fun J2SRef))
		0)
	       (else
		(with-access::J2SRef fun (decl)
		   (if (isa? decl J2SDeclFun)
		       (inline-call-function! cli fuel allcnt)
		       (inline-call-closure! cli fuel allcnt))))))))
   
   (define (find-node-by-loc loc::long nodes)
      (let loop ((nodes nodes))
	 (unless (null? nodes)
	    (with-access::J2SNode (car nodes) ((nloc loc))
	       (cond
		  ((=fx loc (caddr nloc))
		   (car nodes))
		  (else
		   (loop (cdr nodes))))))))
   
   (define (find-nodes-by-loc::pair-nil loc::long nodes)
      ;; because of inlining, it might be that a node is duplicatec
      ;; and then a source file location is found multiple times
      ;; in the AST
      (filter (lambda (node)
		 (with-access::J2SNode node ((nloc loc))
		    (=fx loc (caddr nloc))))
	 nodes))
   
   (define (clis::pair-nil log calls funs)
      (cond
	 ((and (> (cadr log) 0) (caddr log))
	  (map (lambda (call)
		  (callloginfo call (cadr log)
		     (filter-map (lambda (target)
				    (let ((node (find-node-by-loc (cdr target) funs)))
				       (when node
					  (cons (car target) node))))
			(vector->list (caddr log)))))
	     (find-nodes-by-loc (car log) calls)))
	 ((> (cadr log) 0)
	  (map (lambda (call)
		  (with-access::J2SCall call (fun)
		     (with-access::J2SRef fun (decl)
			(with-access::J2SDeclInit decl (val)
			   (callloginfo call (cadr log)
			      (list
				 (cons (cadr log) (list val))))))))
	     (find-nodes-by-loc (car log) calls)))
	 (else
	  '())))
   
   (define (inline-program::long prgm funs fuel log::vector)
      ;; returns the remaining fuel
      (let* ((calls (collect-calls* prgm prgm #f))
	     (logs (vector->list log))
	     (allcnt (apply +
			(map (lambda (n)
				(cond
				   ((fixnum? (cadr n))
				    (fixnum->llong (cadr n)))
				   ((elong? (cadr n))
				    (elong->llong (cadr n)))
				   ((real? (cadr n))
				    (flonum->llong (cadr n)))
				   (else
				    (cadr n))))
			   logs))))
	 (let loop ((logs logs)
		    (fuel fuel))
	    (if (and (>fx fuel 0) (pair? logs))
		(let ((clis (clis (car logs) calls funs)))
		   (if (null? clis)
		       (loop (cdr logs) fuel)
		       (let liip ((clis clis)
				  (fuel fuel))
			  (if (null? clis)
			      (loop (cdr logs) fuel)
			      (let* ((cli (car clis))
				     (call (callloginfo-call cli)))
				 (with-access::J2SCall call (%info loc)
				    (if (not (callinfo-parent %info))
					(liip (cdr clis)
					   fuel)
					(liip (cdr clis)
					   (-fx fuel
					      (inline-profile-call!
						 cli fuel allcnt))))))))))
		fuel))))
   
   (let ((log (call-profile-log prgm logfile conf)))
      (cond
	 ((with-access::J2SProgram prgm (mode) (not (eq? mode 'strict)))
	  (j2s-inline-noprofile prgm conf))
	 (log
	  (with-access::J2SProgram prgm (nodes)
	     (let* ((size (node-size prgm))
		    (funs (collect-funs* prgm prgm)))
		(let loop ((fuel (- (* size inline-global-expansion) size)))
		   (if (>fx fuel 0)
		       (let ((nfuel (inline-program prgm funs fuel log)))
			  (if (<fx nfuel fuel)
			      (begin
				 (when (>=fx (config-get conf :verbose 0) 3)
				    (with-output-to-port (current-error-port)
				       (lambda ()
					  (display "\n     ")
					  (display* "fuel " nfuel " (" fuel ")"))))
				 (loop nfuel))
			      (loop 0)))
		       (j2s-inline-profile-cleanup! prgm conf))))))
	 (else
	  (j2s-inline-noprofile prgm conf)))))

;*---------------------------------------------------------------------*/
;*    j2s-inline-noprofile ...                                         */
;*---------------------------------------------------------------------*/
(define (j2s-inline-noprofile this::J2SProgram conf)

   (define (verbose pms)
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
						 (node-size (protoinfo-method p)))))))
			   b))))))))
   
   (with-access::J2SProgram this (decls nodes)
      ;; count and mark all the calls
      (j2s-count-calls! this conf)
      (let ((maxsize (*fx (node-size this) inline-global-expansion))
	    (pms (ptable
		    (append
		       (append-map collect-proto-methods* nodes)
		       (if (config-get conf :optim-inline-class-method)
			   (append-map collect-proto-methods* decls)
			   '())))))
	 (let loop ((limit 10))
	    (inline! this #f limit '() pms #f this conf)
	    (let ((nsize (node-size this)))
	       (when (and (<fx limit inline-max-function-size)
			  (<fx nsize maxsize))
		  (loop (*fx limit 2)))))
	 (verbose pms))
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
		(and (isa? decl J2SDeclFun)
		     (with-access::J2SDeclFun decl (val)
			(isa? val J2SSvc)))
		(decl-usage-has? decl '(eval)))
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
;*    function-max-expansion ...                                       */
;*---------------------------------------------------------------------*/
(define (function-max-expansion this::J2SFun)
   (with-access::J2SFun this (params)
      (max (*fx (length params) inline-arity-expansion-size-factor)
	 (/fx inline-max-function-size 3))))

;*---------------------------------------------------------------------*/
;*    function-size ...                                                */
;*---------------------------------------------------------------------*/
(define (function-size this::J2SFun)
   
   (define (size fun)
      ;; give a bonus to leaves
      (with-access::J2SFun this (body)
	 (let ((nsz (node-size body)))
	    (if (function-leaf? fun)
		(/fx (*fx nsz 3) 4)
		nsz))))
   
   (with-access::J2SFun this (%info)
      (cond
	 ((not (funinfo? %info))
	  (set! %info (funinfo (size this) #unspecified #f #f)))
	 ((eq? (funinfo-size %info) #unspecified)
	  (funinfo-size-set! %info (size this))))
      (funinfo-size %info)))

;*---------------------------------------------------------------------*/
;*    function-size-reset! ...                                         */
;*---------------------------------------------------------------------*/
(define (function-size-reset! this::J2SFun)
   (with-access::J2SFun this (%info)
      (when (funinfo? %info)
	 (funinfo-size-set! %info #unspecified))))

;*---------------------------------------------------------------------*/
;*    function-freevars? ...                                           */
;*---------------------------------------------------------------------*/
(define (function-freevars? this::J2SFun)
   (with-access::J2SFun this (%info body loc)
      (cond
	 ((not (funinfo? %info))
	  (set! %info (funinfo #unspecified (free-vars? this) #f #f)))
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
;*    function-newtarget? ...                                          */
;*---------------------------------------------------------------------*/
(define (function-newtarget? this::J2SFun)
   (with-access::J2SFun this (new-target)
      (memq new-target '(global argument))))

;*---------------------------------------------------------------------*/
;*    function-mode ...                                                */
;*---------------------------------------------------------------------*/
(define (function-mode this::J2SFun)
   (with-access::J2SFun this (mode)
      mode))

;*---------------------------------------------------------------------*/
;*    function-delete-argument? ...                                    */
;*---------------------------------------------------------------------*/
(define (function-delete-argument? this::J2SFun)
   (with-access::J2SFun this (params)
      (any (lambda (d) (decl-usage-has? d '(delete))) params)))

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
	  (decl-ronly? decl)))
      ((isa? obj J2SDecl)
       (decl-ronly? obj))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    refonly-variable? ...                                            */
;*---------------------------------------------------------------------*/
(define (refonly-variable? obj)
   (cond
      ((isa? obj J2SRef)
       (with-access::J2SRef obj (decl)
	  (refonly-variable? decl)))
      ((isa? obj J2SDecl)
       (or (decl-ronly? obj)
	   (not (decl-usage-has? obj
		   '(assig init new get set call delete
		     instanceof uninit rest eval)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    LetBlock ...                                                     */
;*---------------------------------------------------------------------*/
(define (LetBlock loc t body)
   (if (pair? t)
       (let ((endloc (node-endloc body)))
	  (J2SLetRecBlock #f t body))
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
				     (when (check-id (string->symbol str))
					(let ((met (element-method el)))
					   (cons str (protoinfo prop met #f val)))))))))
	     (j2s-class-methods val :super #f))
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
;*    inline! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SNode
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SMeta ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMeta
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SMeta this (optim debug meta)
      (if (and (eq? meta 'inline) (or (=fx optim 0) (>fx debug 0)))
	  this
	  (call-default-walker))))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SMetaInl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMetaInl
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SMetaInl this (inlstack stmt loc)
      (set! stmt
	 (inline! stmt
	    targets limit (append inlstack stack) pmethods ingen prgm conf))
      this))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SDeclFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SDeclFun
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SDeclFun this (val id)
      (inline! val targets limit stack pmethods ingen prgm conf)
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFun
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SFun this (optimize body generator)
      (when optimize
	 (set! body
	    (inline! body
	       targets limit (cons this stack) pmethods
	       generator prgm conf)))
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SCall
		       targets limit::long stack::pair-nil
		       pmethods ingen prgm conf)
   
   (define (find-inline-decl-function this::J2SCall fun arity limit stack)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (id)
	       (let ((val (j2sdeclinit-val-fun decl)))
		  (when (and (>=fx (function-arity val) arity)
			     (function-fxarg? val)
			     (not (function-generator? val))
			     (not (function-newtarget? val))
			     (not (function-delete-argument? val))
			     (not (memq val stack))
			     (<=fx (function-size val) limit)
			     (check-id id)
			     (not (function-self-recursive? val))
			     (eq? (function-mode val) (function-mode (car stack)))
			     (not (isa? val J2SSvc)))
		     val))))))

   (define (find-inline-methods this fun arity)
      (with-access::J2SAccess fun (obj field loc)
	 (when (and (isa? field J2SString)
		    (config-get conf :optim-inline-method #f)
		    (not (isa? obj J2SSuper)))
	    (with-access::J2SString field (val)
	       (let* ((mets (filter (lambda (m::struct)
				       (let ((f (protoinfo-method m)))
					  (and (=fx (function-arity f) arity)
					       (function-fxarg? f)
					       (not (memq f stack))
					       (not (function-self-recursive? f)))))
			       (or (hashtable-get pmethods val) '())))
		      (sz (apply +
			     (map (lambda (m)
				     (+fx inline-method-check-size
					(function-size (protoinfo-method m))))
				mets))))
		  (when (and (<fx sz (*fx limit (length mets)))
			     (and (<fx sz inline-max-method-size)
				  (every (lambda (m)
					    (let ((f (protoinfo-method m)))
					       (function-leaf? f)))
				     mets)))
		     (sort (lambda (f1 f2)
			      (>=fx (function-size (protoinfo-method f1))
				 (function-size (protoinfo-method f2))))
			mets)))))))

   (define (inline-access-call this::J2SCall fun::J2SAccess args loc)
      (let ((mets (find-inline-methods this fun (length args))))
	 (when (pair? mets)
	    (let* ((mets (filter-rutype
			    (if (pair? targets)
				(filter (lambda (t) (memq t mets)) targets)
				mets)
			    prgm))
		   (vals (map protoinfo-method mets))
		   (sz (apply + (map function-size vals))))
	       (when (pair? mets)
		  (inline-verb loc fun
		     (map (lambda (x) '-) mets) sz limit 0 conf)
		  (when (pair? stack) 
		     (invalidate-function-size! (car stack)))
		  (let ((e (inline-method-call fun mets args
			      loc (node-endloc this)
			      '() limit stack pmethods ingen prgm conf)))
		     (inline-stmt->expr loc e
			(function-rutype
			   (protoinfo-method (car mets))))))))))
   
   (define (inline-ref-call this::J2SCall fun::J2SRef thisargs args loc)
      (cond
	 ((find-inline-decl-function this fun (length args) limit stack)
	  =>
	  (lambda (target)
	     (inline-verb loc fun '(-) (function-size target) limit 0 conf)
	     (when (pair? stack)
		(invalidate-function-size! (car stack)))
	     (inline-stmt->expr loc
		(inline-function-call target thisargs args loc
		   targets (if targets 0 limit) stack pmethods ingen prgm conf)
		(function-rutype target))))
	 ((pair? targets)
	  (let ((targets (filter-rutype targets prgm)))
	     (when (pair? targets)
		(when (pair? stack)
		   (invalidate-function-size! (car stack)))
		(inline-stmt->expr loc
		   (inline-unknown-call fun thisargs  args loc
		      targets limit stack pmethods ingen prgm conf)
		   (function-rutype (car targets))))))
	 (else
	  #f)))

   (define (inline-expr-call this fun thisargs args loc)
      (let ((decl (J2SDeclInit '(ref) (gensym '%fun) fun))
	    (endloc (node-endloc this)))
	 (inline-stmt->expr loc
	    (J2SLetBlock (list decl)
	       (inline-ref-call this (J2SRef decl) thisargs args loc))
	    'unknown)))
   
   (with-access::J2SCall this (fun thisargs args type loc cache protocol)
      (cond
	 ((memq (caddr loc) blacklistloc)
	  (call-default-walker))
	 ((and (pair? whitelistloc) (not (memq (caddr loc) whitelistloc)))
	  (call-default-walker))
	 ((null? stack)
	  ;; don't inline at toplevel
	  (call-default-walker))
	 (cache
	  (call-default-walker))
	 ((eq? protocol 'spread)
	  (call-default-walker))
	 ((and ingen (any yield? args))
	  (call-default-walker))
	 ((isa? fun J2SAccess)
	  (or (inline-access-call this fun args loc)
	      (call-default-walker)))
	 ((isa? fun J2SRef)
	  (or (inline-ref-call this fun thisargs args loc)
	      (call-default-walker)))
	 ((pair? targets)
	  (or (inline-expr-call this fun thisargs args loc)
	      (call-default-walker)))
	 (else
	  (call-default-walker)))))

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
;*    inline-function-call ...                                         */
;*---------------------------------------------------------------------*/
(define (inline-function-call val::J2SFun thisargs args::pair-nil loc
	   targets limit::long stack::pair-nil pmethods ingen prgm conf)
   (with-access::J2SFun val (body thisp params (floc loc))
      (let* ((vals (inline-args (cons thisp params)
		      (if (pair? thisargs)
			  (append thisargs args)
			  (cons (J2SUndefined) args))
		      #f limit stack pmethods ingen prgm conf loc))
	     (nbody (j2s-alpha body (cons thisp params) vals)))
	 (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
	    (J2SMetaInl (cons val stack)
	       (config-get conf :optim 0)
	       (if (> limit 0)
		   (inline! nbody
		      #f limit (cons val stack) pmethods ingen prgm conf)
		   nbody))))))

;*---------------------------------------------------------------------*/
;*    inline-function-call-profile ...                                 */
;*---------------------------------------------------------------------*/
(define (inline-function-call-profile node::J2SCall target::J2SFun ingen prgm conf)

   (define (inline-function-args args)
      (map (lambda (a)
	      (let ((id (gensym 'iarg)))
		 (with-access::J2SNode a (loc)
		    (J2SLetOpt '(ref assig) id a))))
	 args))

   (with-access::J2SCall node (thisargss args loc)
      (with-access::J2SFun target (body thisp params (floc loc))
	 (let* ((vals (cons (J2SUndefined) (inline-function-args args)))
		(nbody (j2s-alpha body (cons thisp params) vals)))
	    (LetBlock loc (filter (lambda (a) (isa? a J2SDecl)) vals)
	       nbody)))))
   
;*---------------------------------------------------------------------*/
;*    inline-unknown-call ...                                          */
;*---------------------------------------------------------------------*/
(define (inline-unknown-call ref::J2SRef thisargs args::pair-nil loc
	   targets limit::long stack::pair-nil pmethods ingen prgm conf)
   (let loop ((targets targets))
      (if (null? targets)
	  (J2SStmtExpr (J2SCall* ref args))
	  (let* ((target (car targets))
		 (fun (targetinfo-fun target)))
	     (if (< (function-size fun) limit)
		 (begin
		    (inline-verb loc fun '(-) (function-size fun) limit 0 conf)
		    (J2SIf (J2SHopCall (J2SHopRef/rtype 'eq? 'bool)
			      (J2SRef (with-access::J2SRef ref (decl) decl))
			      (J2SRef (targetinfo-decl target)))
		       (inline-function-call fun thisargs args loc
			  #f 0 stack pmethods ingen prgm conf)
		       (loop (cdr targets))))
		 (loop '()))))))
   
;*---------------------------------------------------------------------*/
;*    inline-closure-call-profile ...                                  */
;*---------------------------------------------------------------------*/
(define (inline-closure-call-profile node::J2SCall guard targets::pair ingen prgm conf)
   
   (define (j2sref-ronly? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (decl-ronly? decl))))
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))
   
   (define (cache-check c loc obj field kont inline::J2SStmt)
      (if (private-field? field)
	  inline
	  (J2SIf (J2SCacheCheck 'proto-method c #f obj field)
	     inline
	     (kont))))
   
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
	      (let ((id (gensym 'iarg)))
		 (with-access::J2SNode a (loc)
		    (J2SLetOpt '(ref assig) id a))))
	 args))
   
   (define (inline-closure-call fun::J2SExpr args::pair-nil loc)
      (let ((fun (J2SLetOpt '(ref) (gensym 'fun) fun))
	    (endloc (node-endloc fun)))
	 (J2SLetBlock (list fun)
	    (LetBlock loc args
	       (let loop ((targets targets))
		  (if (null? targets)
		      (J2SMetaInl '() 0
			 (J2SStmtExpr
			    (J2SCall* (J2SRef fun)
			       (map (lambda (d) (J2SRef d)) args))))
		      (let* ((target (car targets))
			     (v (function-glodecl (cdr target) prgm)))
			 (J2SIf (J2SBinary 'eq? (J2SRef fun) (J2SRef v))
			    (with-access::J2SFun (cdr target) (mode body thisp params (floc loc))
			       (j2s-alpha body
				  (cons thisp params)
				  args))
			    (loop (cdr targets))))))))))
   
   (with-access::J2SCall node (fun thisargs args loc)
      ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
      ;; shape of the test that suits the tyflow analysis
      (let ((vals (inline-closure-args (append thisargs args))))
	 (inline-closure-call fun vals loc))))

;*---------------------------------------------------------------------*/
;*    inline-method-call-profile ...                                   */
;*---------------------------------------------------------------------*/
(define (inline-method-call-profile node::J2SCall guard targets::pair ingen prgm conf)
   
   (define (j2sref-ronly? obj)
      (when (isa? obj J2SRef)
	 (with-access::J2SRef obj (decl)
	    (decl-ronly? decl))))
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))
   
   (define (cache-check c loc obj field kont inline::J2SStmt)
      (if (private-field? field)
	  inline
	  (J2SIf (J2SCacheCheck 'proto-method c (J2SUndefined) obj field)
	     inline
	     (kont))))
   
   (define (get-svar callee)
      (if (protoinfo-svar callee)
	  (protoinfo-svar callee)
	  (let ((fun (gensym '%met)))
	     (protoinfo-svar-set! callee fun)
	     (with-access::J2SProgram prgm (globals)
		(set! globals (cons `(define ,fun #unspecified) globals))
		(let ((as (protoinfo-assig callee)))
		   (cond
		      ((isa? as J2SAssig)
		       (with-access::J2SAssig as (rhs loc)
			  (set! rhs
			     (J2SSequence
				(J2SAssig (J2SHopRef fun) rhs)
				(J2SHopRef fun)))))
		      ((isa? as J2SMethodPropertyInit)
		       (with-access::J2SMethodPropertyInit as (inlinecachevar)
			  (set! inlinecachevar fun)))
		      (else
		       (error "inline-method-call" "bad protoinfo" callee)))))
	     fun)))
   
   (define (inline-method-args args)
      (map (lambda (a)
	      (let ((id (gensym 'iarg)))
		 (with-access::J2SNode a (loc)
		    (J2SLetOpt '(ref assig) id a))))
	 args))
   
   (define (inline-method obj::J2SRef field target::pair args cache loc kont)
      (with-access::J2SFun (cdr target) (body thisp params (floc loc))
	 (with-access::J2SRef obj (decl)
	    (cache-check cache loc obj field kont
	       (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) args)
		  (j2s-alpha body
		     (cons thisp params) (cons decl args)))))))

   (define (inline-cspecs cspecs)
      (append (replace cspecs 'vtable 'mvtable) '(mmiss)))
   
   (define (inline-object-method-call-pmap fun obj::J2SDecl args loc)
      (with-access::J2SAccess fun (field cspecs)
	 (let loop ((targets targets)
		    (funcaches '()))
	    (if (null? targets)
		(let* ((c (get-cache prgm))
		       (f (duplicate::J2SAccess fun
			     (cspecs (inline-cspecs cspecs))
			     (obj (J2SRef obj))))
		       (r (J2SLetOpt '(call) (gensym 'r)
			     (J2SMethodCall/cache* f (list (J2SRef obj)) args
				'(vtable-inline pmap-inline) c)))
		       (endloc (node-endloc node)))
		   (J2SMetaInl '() 0
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
					 c (J2SRef obj)
					 (if (isa? v J2SDecl) (J2SRef v) (J2SHopRef v)))
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
			 (J2SReturn #t (J2SRef r)))))
		(let ((cache (get-cache prgm)))
		   (inline-method (J2SRef obj)
		      field (car targets) args cache loc
		      (lambda ()
			 (loop (cdr targets)
			    (cons (cons cache (car targets)) funcaches)))))))))

   (define (replace lst from to)
      (let loop ((lst lst))
	 (cond
	    ((null? lst) '())
	    ((eq? (car lst) from) (cons to (cdr lst)))
	    (else (cons (car lst) (loop (cdr lst)))))))
	  
   (define (inline-object-method-call-function fun obj::J2SDecl args loc)
      (with-access::J2SAccess fun (field cspecs loc)
	 (let ((endloc (node-endloc fun))
	       (met (J2SLetOpt '(ref) (gensym 'met)
		       (duplicate::J2SAccess fun
			  (cspecs (inline-cspecs cspecs))
			  (obj (J2SRef obj))))))
	    (J2SLetBlock (list met)
	       (let loop ((targets targets))
		  (if (null? targets)
		      (J2SStmtExpr
			 (J2SMethodCall* (J2SRef met)
			    (list (J2SRef obj)) args))
		      (let* ((target (car targets))
			     (v (function-glodecl (cdr target) prgm)))
			 (J2SIf (J2SBinary 'eq?
				   (J2SRef met)
				   (if (isa? v J2SDecl)
				       (J2SRef v)
				       (J2SHopRef v)))
			    (with-access::J2SFun (cdr target) (body thisp params (floc loc))
			       (LetBlock floc (filter (lambda (b)
							 (isa? b J2SDecl))
						 args)
				  (j2s-alpha body
				     (cons thisp params) (cons obj args))))
			    (loop (cdr targets))))))))))
   
   (define (inline-object-method-call fun::J2SAccess obj::J2SDecl args loc guard)
      (case guard
	 ((pmap)
	  (inline-object-method-call-pmap fun obj args loc))
	 ((array)
	  (inline-object-method-call-function fun obj args loc))
	 ((function)
	  (inline-object-method-call-function fun obj args loc))
	 (else
	  (inline-object-method-call-pmap fun obj args loc))))
   
   (with-access::J2SCall node (fun args loc)
      (with-access::J2SAccess fun (obj field)
	 ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
	 ;; shape of the test that suits the tyflow analysis
	 (let* ((vals (inline-method-args args))
		(tmps (filter (lambda (b) (isa? b J2SDecl)) vals))
		(args (map (lambda (v)
			      (if (isa? v J2SDecl)
				  (with-access::J2SDecl v (loc)
				     (J2SRef v))
				  v))
			 vals)))
	    (cond
	       ((not (type-object? (j2s-type obj)))
		(if (j2sref-ronly? obj)
		    (with-access::J2SRef obj (decl)
		       (LetBlock loc tmps
			  (J2SIf (J2SHopCall
				    (if (eq? guard 'array)
					(J2SHopRef/rtype 'js-array? 'bool)
					(J2SHopRef/rtype 'js-object? 'bool))
				    (J2SRef decl))
			     (inline-object-method-call fun decl args loc guard)
			     (J2SMeta 'inline 0 0
				(J2SStmtExpr
				   (J2SCall* (J2SAccess (J2SRef decl) field)
				      args))))))
		    (let* ((id (gensym 'this))
			   (decl (J2SLetOpt '(get) id obj)))
		       (LetBlock loc (cons decl tmps)
			  (J2SIf (J2SHopCall
				    (if (eq? guard 'array)
					(J2SHopRef/rtype 'js-array? 'bool)
					(J2SHopRef/rtype 'js-object? 'bool))
				    (J2SRef decl))
			     (inline-object-method-call fun decl args loc guard)
			     (J2SMeta 'inline 0 0
				(J2SStmtExpr
				   (J2SCall* (J2SAccess (J2SRef decl) field)
				      args))))))))
	       ((not (j2sref-ronly? obj))
		(let* ((id (gensym 'this))
		       (decl (J2SLetOpt '(get) id obj)))
		   (LetBlock loc (cons decl tmps)
		      (inline-object-method-call fun decl args loc guard))))
	       (else
		(with-access::J2SRef obj (decl)
		   (inline-object-method-call fun decl args loc guard))))))))

;*---------------------------------------------------------------------*/
;*    inline-method-call ...                                           */
;*---------------------------------------------------------------------*/
(define (inline-method-call fun::J2SAccess callees::pair args::pair-nil loc endloc
	   targets limit::long stack::pair-nil pmethods ingen prgm conf)
   
   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))

   (define proto-method
      (if (pair? (cdr callees)) 'proto-method-mono 'proto-method-poly))
   
   (define (cache-check c loc owner obj field kont inline::J2SStmt)
      (if (private-field? field)
	  inline
	  (J2SIf (J2SCacheCheck proto-method c #f obj field)
	     inline
	     (kont))))

   (define (get-svar callee)
      (if (protoinfo-svar callee)
	  (protoinfo-svar callee)
	  (let ((fun (gensym '%inlmet)))
	     (protoinfo-svar-set! callee fun)
	     (with-access::J2SProgram prgm (globals)
		(set! globals (cons `(define ,fun #unspecified) globals))
		(let ((as (protoinfo-assig callee)))
		   (cond
		      ((isa? as J2SAssig)
		       (with-access::J2SAssig as (rhs loc)
			  (set! rhs
			     (J2SSequence
				(J2SAssig (J2SHopRef fun) rhs)
				(J2SHopRef fun)))))
		      ((isa? as J2SMethodPropertyInit)
		       (with-access::J2SMethodPropertyInit as (inlinecachevar)
			  (set! inlinecachevar fun)))
		      (else
		       (error "inline-method-call" "bad protoinfo" callee)))))
	     fun)))

   (define (inline-method-args args)
      (map (lambda (a)
	      (if (simple-argument? a)
		  a
		  (let ((id (gensym 'iarg)))
		     (with-access::J2SNode a (loc)
			(J2SLetOpt '(ref assig) id
			   (inline! a
			      #f limit stack pmethods ingen prgm conf))))))
	 args))

   (define (inline-record-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee))
	    (clazz (protoinfo-owner callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (with-access::J2SRef obj (decl)
	       (let ((vals (inline-args params args
			      #f limit stack pmethods ingen prgm conf loc))
		     (ndecl (J2SLetOpt/vtype clazz '(get) (gensym 'this)
			       (J2SCast/static #t clazz obj))))
		  (cache-check cache loc (protoinfo-owner callee) obj field kont
		     (LetBlock floc (cons ndecl
				       (filter (lambda (b) (isa? b J2SDecl)) vals))
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (inline!
			      (j2s-alpha body
				 (cons thisp params) (cons ndecl vals))
			      #f limit
			      (cons val stack) pmethods ingen prgm conf)))))))))

   (define (inline-object-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (with-access::J2SRef obj (decl)
	       (let ((vals (inline-args params args
			      #f limit stack pmethods ingen prgm conf loc)))
		  (cache-check cache loc (protoinfo-owner callee) obj field kont
		     (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (inline!
			      (j2s-alpha body
				 (cons thisp params) (cons decl vals))
			      #f limit
			      (cons val stack) pmethods ingen prgm conf)))))))))

   (define (inline-expr-method obj::J2SExpr field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (let ((vals (inline-args params args
			   #f limit stack pmethods ingen prgm conf loc))
		  (decl (J2SLetOpt '(ref) (gensym 'this) obj)))
	       (cache-check cache loc (protoinfo-owner callee) obj field kont
		  (LetBlock floc (cons decl (filter (lambda (b) (isa? b J2SDecl)) vals))
		     (J2SMetaInl (cons val stack)
			(config-get conf :optim 0)
			(inline!
			   (j2s-alpha body
			      (cons thisp params) (cons decl vals))
			   #f limit
			   (cons val stack) pmethods ingen prgm conf))))))))

   (define (inline-method obj field callee args cache loc kont)
      (cond
	 ((not (isa? obj J2SRef))
	  (inline-expr-method obj field callee args cache loc kont))
	 ((isa? (protoinfo-owner callee) J2SRecord)
	  (inline-record-method obj field callee args cache loc kont))
	 (else
	  (inline-object-method obj field callee args cache loc kont))))
   
   (define (inline-object-method-call fun self args)
      (with-access::J2SAccess fun (field cspecs)
	 (let loop ((callees callees)
		    (caches '()))
	    (if (null? callees)
		(let* ((c (get-cache prgm))
		       (f (duplicate::J2SAccess fun
			     (obj self)))
		       (r (J2SLetOpt '(call) (gensym 'r)
			     (J2SMethodCall/cache* f (list self) args
				'(vtable-inline pmap-inline) c))))
		   (J2SLetRecBlock #f (list r)
;* 		      (J2SIfIsRecord (j2s-alpha self '() '())          */
;* 			 (J2SNop)                                      */
			 (let loop ((cs caches))
			    (if (null? cs)
				(J2SNop)
				(let ((v (get-svar (cdar cs))))
				   (J2SIf
				      (J2SCacheCheck 'method-and-owner
					 c (j2s-alpha self '() '()) (J2SHopRef v))
				      (J2SSeq*
					 (map (lambda (c)
						 (J2SStmtExpr
						    (if (eq? c (car cs))
							(J2SCacheUpdate proto-method
							   (car c) (j2s-alpha self '() '()))
							(J2SUndefined))))
					    caches))
				      (loop (cdr cs))))))
;* 			    )                                          */
		      (J2SReturn #t (J2SRef r))))
		(let ((cache (get-cache prgm)))
		   (inline-method self field (car callees) args cache loc
		      (lambda ()
			 (loop (cdr callees)
			    (cons (cons cache (car callees)) caches)))))))))

   (define (gen-check-object obj field args)
      (J2SIf (J2SHopCall (J2SHopRef/rtype 'js-object? 'bool) obj)
	 (inline-object-method-call fun obj args)
	 (J2SMeta 'inline 0 0
	    (J2SReturn #t
	       (J2SCall* (J2SAccess (j2s-alpha obj '() '()) field) args)))))
   
   (with-access::J2SAccess fun (obj field loc)
      ;; see J2S-EXPR-TYPE-TEST@__JS2SCHEME_AST for the
      ;; shape of the test that suits the tyflow analysis
      (let* ((vals (inline-method-args args))
	     (tmps (filter (lambda (b) (isa? b J2SDecl)) vals))
	     (args (map (lambda (v)
			   (if (isa? v J2SDecl)
			       (with-access::J2SDecl v (loc)
				  (J2SRef v))
			       v))
		      vals)))
	 (cond
	    ((not (type-object? (j2s-type obj)))
	     (if (simple-argument? obj)
		 (if (pair? tmps)
		     (LetBlock loc tmps
			(gen-check-object obj field args))
		     (gen-check-object obj field args))
		 (let* ((id (gensym 'this))
			(d (J2SLetOpt '(get) id obj)))
		    (LetBlock loc (cons d tmps)
		       (gen-check-object (J2SRef d) field args)))))
	    ((not (isa? obj J2SRef))
	     (let* ((id (gensym 'this))
		    (d (J2SLetOpt '(get) id obj)))
		(LetBlock loc (cons d tmps)
		   (inline-object-method-call fun (J2SRef d) args))))
	    ((pair? tmps)
	     (LetBlock loc tmps
		(inline-object-method-call fun obj args)))
	    (else
	     (inline-object-method-call fun obj args))))))

;*---------------------------------------------------------------------*/
;*    inline-args ...                                                  */
;*---------------------------------------------------------------------*/
(define (inline-args params args targets limit stack pmethods ingen prgm conf loc)
   (let ((lena (length args))
	 (lenp (length params)))
      (map (lambda (p a)
	      (cond
		 ((and (ronly-variable? p) (simple-argument? a))
		  a)
		 (else
		  (with-access::J2SDecl p ((_usage usage) id writable)
		     (with-access::J2SNode a (loc)
			(let ((d (J2SLetOpt _usage (gensym id)
				    (inline! a
				       targets limit stack pmethods ingen prgm conf))))
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
	       (when (> allcnt 0)
		  (display " cnt: ")
		  (display (format "~(, )"
			      (map (lambda (c)
				      (format "~a ~a%" c
					 (inexact->exact
					    (round (* 100 (/ c allcnt))))))
				 targets-cnt)))))))))

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
		  (not (memq (caddr loc) blacklistloc))
		  (or (null? whitelistloc) (memq (caddr loc) whitelistloc)))
	     (begin
		(set! %info (callinfo parent owner))
		(cons this next))
	     next))))

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
	     (error "update-parent!" "Cannot find node" (j2s->sexp old))
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
		  (display "\n     ")
		  (display* "loading log file "
		     (string-append "\"" logfile "\"...")))))
	 (let loop ((i (-fx (vector-length srcs) 1)))
	    (when (>=fx i 0)
	       (let ((filename (get 'filename (vector-ref srcs i))))
		  (if (string=? file filename)
		      (let ((verb (make-cell 0))
			    (cvec (get 'calls (vector-ref srcs i))))
			 (when (vector? cvec)
			    (vector-map!
			       (lambda (c)
				  (let ((cnt (get 'cnt c)))
				     (list (get 'point c)
					(if (vector? cnt)
					    (sum-method-count cnt)
					    cnt)
					(if (vector? cnt)
					    (vector-map!
					       (lambda (c)
						  (cons (get 'cnt c)
						     (get 'point c)))
					       cnt)
					    #f))))
			       cvec)
			    (sort (lambda (x y) (>= (cadr x) (cadr y))) cvec)))
		      (loop (-fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    load-profile-log ...                                             */
;*---------------------------------------------------------------------*/
(define (load-profile-log logfile)
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
				     (error "fprofile" "Wrong log format"
					logfile))
				  (cell-set! o
				     (cons (cons 'calls val)
					(cell-ref o))))
				 ((string=? p "format")
				  (set! fprofile (equal? val "fprofile")))
				 (else
				  (cell-set! o
				     (cons (cons (string->symbol p) val)
					(cell-ref o))))))
	       :object-return (lambda (o)
				 (reverse! (cell-ref o)))
	       :parse-error (lambda (msg fname loc)
			       (error/location "fprofile" "Wrong JSON file" msg
				  fname loc)))))))

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
;*    private-field? ...                                               */
;*---------------------------------------------------------------------*/
(define (private-field? field)
   (when (isa? field J2SString)
      (with-access::J2SString field (private)
	 private)))
