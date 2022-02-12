;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline-profile.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Feb 12 08:50:30 2022                          */
;*    Last change :  Sat Feb 12 09:20:48 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Profile based inlining                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_inline-profile

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
	   __js2scheme_inline-common)

   (export (j2s-inline-profile prgm::J2SProgram conf logfile)))

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
	  #f)
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
		       (j2s-inline-cleanup! prgm conf))))))
	 (else
	  #f))))

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
				'(vtable-inline pmap-inline poly) c)))
		       (endloc (node-endloc node)))
		   (J2SMetaInl '() 0
		      (if (isa? (j2s-type obj) J2SRecord)
			  r
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
			     (J2SReturn #t (J2SRef r))))))
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


  
