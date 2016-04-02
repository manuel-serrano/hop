;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/js2scheme/letopt.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 06:35:14 2015                          */
;*    Last change :  Tue Mar 29 08:45:01 2016 (serrano)                */
;*    Copyright   :  2015-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let optimisation                                                 */
;*    -------------------------------------------------------------    */
;*    This implements the Let optimisation. When possible, it replaces */
;*    LetInit with LetOpt nodes, which are much more efficient because */
;*    they are potentially implemented as registers while LetInit are  */
;*    always implemented as boxed variables.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_letopt

   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_utils
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_lexer
	   __js2scheme_use)

   (static (class DeclInfo
	      (optdecl (default #unspecified))
	      (used (default #unspecified))
	      (init (default #unspecified))))
   
   (export j2s-letopt-stage))

;*---------------------------------------------------------------------*/
;*    j2s-letopt-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-letopt-stage
   (instantiate::J2SStageProc
      (name "letopt")
      (comment "Allocate let/const variables to registers")
      (proc j2s-letopt)
      (optional #t)))

;*---------------------------------------------------------------------*/
;*    j2s-letopt ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-letopt this args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls)
	 ;; statement optimization
	 (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) headers)
	 (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) decls)
	 (for-each (lambda (o) (j2s-update-ref! (j2s-letopt! o))) nodes)
	 ;; toplevel lets optimization
	 (let ((lets '())
	       (vars '()))
	    (for-each (lambda (x)
			 (cond
			    ((not (isa? x J2SDecl))
			     (error "j2s-letopt" "internal error" (j2s->list x)))
			    ((j2s-let? x)
			     (set! lets (cons x lets)))
			    (else
			     (set! vars (cons x vars)))))
	       decls)
	    (when (pair? lets)
	       ;; this modify nodes in place
	       (set! nodes
		  (map! j2s-update-ref!
		     (j2s-toplevel-letopt! nodes (reverse! lets) vars)))))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-update-ref! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-update-ref! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-update-ref! ::J2SRef ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-update-ref! this::J2SRef)
   ;; patch the optimized ref nodes
   (with-access::J2SRef this (decl)
      (with-access::J2SDecl decl (%info)
	 (when (isa? %info DeclInfo)
	    (with-access::DeclInfo %info (optdecl)
	       (when (isa? optdecl J2SDecl)
		  (set! decl optdecl))))))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-letopt! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letopt! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    reset-use! ...                                                   */
;*---------------------------------------------------------------------*/
(define (reset-use! decls::pair-nil)
   (for-each (lambda (decl)
		(with-access::J2SDecl decl (usecnt)
		   (set! usecnt 0)))
      decls))

;*---------------------------------------------------------------------*/
;*    get-inits ...                                                    */
;*    -------------------------------------------------------------    */
;*    As GET-LET-INITS but returns #f if no init found (easier with    */
;*    COND forms).						       */
;*---------------------------------------------------------------------*/
(define (get-inits::obj node::J2SNode)
   (let ((inits (get-let-inits node '())))
      (when (pair? inits)
	 inits)))

;*---------------------------------------------------------------------*/
;*    init-decl ...                                                    */
;*---------------------------------------------------------------------*/
(define (init-decl::J2SDecl this::J2SInit)
   (with-access::J2SInit this (lhs)
      (with-access::J2SRef lhs (decl)
	 decl)))

;*---------------------------------------------------------------------*/
;*    decl-init ...                                                    */
;*---------------------------------------------------------------------*/
(define (decl-init this::J2SDecl)
   (with-access::J2SDecl this (%info)
      (with-access::DeclInfo %info (init)
	 init)))

;*---------------------------------------------------------------------*/
;*    j2s-letopt! ::J2SLetBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-letopt! this::J2SLetBlock)
   (with-access::J2SLetBlock this (decls nodes)
      ;; start iterating over all the LetBlock statements to find
      ;; the first decl
      (with-trace 'j2s-letopt "j2s-letopt!"
	 (trace-item "" (j2s->list this))
	 (let loop ((n nodes)
		    (decls decls))

	    (cond
	       ((null? n)
		;; should never be reached
		(trace-item "<<<.1 " (j2s->list this))
		this)
	       ((not (get-let-inits (car n) decls))
		(trace-item "---.2 " (j2s->list (car n)))
 		;; optimize recursively
		(set-car! n (j2s-letopt! (car n)))
		;; keep optimizing the current let block
		(let ((used (node-used* (car n) decls)))
		   ;; mark all the used decls as non optimizable
		   (for-each (lambda (d::J2SDecl)
				(with-access::J2SDecl d (%info)
				   (set! %info
				      (instantiate::DeclInfo
					 (optdecl #f)))))
		      used)
		   (loop (cdr n)
		      (filter (lambda (d)
				 (with-access::J2SDecl d (%info)
				    (not (isa? %info DeclInfo))))
			 decls))))
	       ((null? decls)
		(trace-item "---.3 " (j2s->list (car n)))
		;; nothing more to be potentially optimzed
		(map! j2s-letopt! n)
		(trace-item "<<<.3 " (j2s->list (car n)))
		this)
	       (else
		(trace-item "---.4 " (j2s->list (car n)))
		;; got an initialization block, which can be
		;; optimized
		(if (eq? n nodes)
		    ;; the let init started the let-block
		    (let ((res (tail-let! this this)))
		       (trace-item "<<<.4a " (j2s->list res))
		       res)
		    ;; re-organize the let-block to place this inits
		    ;; at the head of a fresh let
		    (let ((res (tail-let! this (head-let! this n))))
		       (trace-item "<<<.4b " (j2s->list res))
		       res))))))))

;*---------------------------------------------------------------------*/
;*    head-let! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Modify the letblock so its first statement is a declaration.     */
;*---------------------------------------------------------------------*/
(define (head-let! this::J2SLetBlock head::pair-nil)
   (with-trace 'j2s-letopt "head-let!"
      (trace-item "" (j2s->list this))
      (with-access::J2SLetBlock this (loc endloc decls nodes)
	 (let loop ((n nodes)
		    (prev '()))
	    (if (eq? n head)
		;; collect the non-optimized decls
		(let ((nodecls (filter (lambda (d)
					  (with-access::J2SDecl d (%info)
					     (when (isa? %info DeclInfo)
						(with-access::DeclInfo %info (optdecl)
						   (not optdecl)))))
				  decls)))
		   (set! nodes n)
		   (if (null? nodecls)
		       (instantiate::J2SBlock
			  (loc loc)
			  (endloc endloc)
			  (nodes (reverse! (cons this prev))))
		       (instantiate::J2SLetBlock
			  (loc loc)
			  (endloc endloc)
			  (decls nodecls)
			  (nodes (reverse! (cons this prev))))))
		(loop (cdr n) (cons (car n) prev)))))))

;*---------------------------------------------------------------------*/
;*    tail-let! ...                                                    */
;*---------------------------------------------------------------------*/
(define (tail-let! this::J2SLetBlock resnode::J2SStmt)
   ;; the main optimization loop
   (with-trace 'j2s-letopt "tail-let!"
      (trace-item "this=" (j2s->list this))
      (with-access::J2SLetBlock this (nodes decls loc)
	 (multiple-value-bind (rests inits)
	    (letblock-nodes-split nodes decls)
	    ;; order the declarations according to the init
	    (let ((odecls (map (lambda (init)
				  (init-decl init))
			     inits)))
	       (for-each (lambda (d)
			    (unless (memq d odecls)
			       (set! odecls (cons d odecls))))
		  decls)
	       (set! decls odecls))
	    ;; iterate over all the inits
	    (let loop ((inits inits)
		       (noopts '()))
	       (cond
		  ((null? inits)
		   ;; we are done
		   (let ((pdecls '()))
		      (set! decls
			 (filter-map (lambda (d)
					(with-access::J2SDecl d (%info scope)
					   (set! scope 'letblock)
					   (if (isa? %info DeclInfo)
					       (with-access::DeclInfo %info (optdecl)
						  (if optdecl
						      optdecl
						      (begin
							 (set! pdecls
							    (cons d pdecls))
							 d)))
					       d)))
			    decls))
		      ;; move all the non-optimized, processed binding upfront
		      (set! decls (append pdecls decls)))
		   ;; check is all decls have been processed
		   (let ((rdecls (rest-decls decls)))
		      (if (and (pair? rests) (pair? rdecls))
			  (begin
			     ;; remove the rdecls from the initial let
			     (set! decls
				(filter (lambda (d)
					   (not (unprocessed-decl? d)))
				   decls))
			     ;; patch the nodes
			     (set! nodes
				(append (map! j2s-letopt! noopts)
				   (list (j2s-letopt!
					    (duplicate::J2SLetBlock this
					       (decls rdecls)
					       (nodes rests)))))))
			  (set! nodes
			     (append! (map! j2s-letopt! noopts)
				(map! j2s-letopt! rests)))))
		   resnode)
		  ((already-bound? (car inits))
		   (loop '() inits))
		  ((fun-init? (car inits))
		   ;; a function, we optimize the binding
		   (let ((decl (init-decl (car inits))))
		      (decl-update-info! decl
			 (lambda ()
			    (new-let-opt (car inits)
			       (j2s-letopt! (car inits)))))
		      (loop (cdr inits) noopts)))
		  (else
		   (let ((decl (init-decl (car inits))))
		      (with-access::J2SDecl decl (%info)
			 (with-access::DeclInfo %info (optdecl)
			    (if (eq? optdecl #f)
				;; not optmized, we stop optimizing everything
				(loop '() inits)
				;; optimize that binding
				(let ((used (used* (car inits) decls))
				      (optdecl (new-let-opt (car inits)
						  (j2s-letopt! (car inits)))))
				   (for-each (lambda (d::J2SDecl)
						;; invalidates all the next
						;; decls
						(decl-update-info! d #f))
				      used)
				   (decl-update-info! decl
				      (lambda ()
					 (new-let-opt (car inits)
					    (j2s-letopt! (car inits)))))
				   (loop (cdr inits) noopts)))))))))))))

;*---------------------------------------------------------------------*/
;*    rest-decls ...                                                   */
;*    -------------------------------------------------------------    */
;*    Compute the unprocess declarations that will be placed in        */
;*    a new nest letblock.                                             */
;*---------------------------------------------------------------------*/
(define (rest-decls decls::pair)
   ;; invalidates all the bindings used by optimized functions
   (for-each (lambda (d::J2SDecl)
		(unless (unprocessed-decl? d)
		   (let ((init (decl-init d)))
		      (when (fun-init? init)
			 (for-each (lambda (d::J2SDecl)
				      (decl-update-info! d #f))
			    (used* init decls))))))
      decls)
   ;; and return the list of unprocessed (never seen) declarations
   (filter unprocessed-decl? decls))
   
;*---------------------------------------------------------------------*/
;*    unprocessed-decl? ...                                            */
;*---------------------------------------------------------------------*/
(define (unprocessed-decl? d)
   (with-access::J2SDecl d (%info)
      (if (isa? %info DeclInfo)
	  (with-access::DeclInfo %info (optdecl)
	     (eq? optdecl #unspecified))
	  #t)))

;*---------------------------------------------------------------------*/
;*    already-bound? ...                                               */
;*---------------------------------------------------------------------*/
(define (already-bound? expr::J2SInit)
   (let ((decl (init-decl expr)))
      (with-access::J2SDecl decl (%info)
	 (when (isa? %info DeclInfo)
	    (with-access::DeclInfo %info (optdecl)
	       (not (eq? optdecl #unspecified)))))))

;*---------------------------------------------------------------------*/
;*    decl-update-info! ...                                            */
;*---------------------------------------------------------------------*/
(define (decl-update-info! decl::J2SDecl proc::obj)
   (with-access::J2SDecl decl (%info)
      (if (isa? %info DeclInfo)
	  (with-access::DeclInfo %info (optdecl)
	     (when (eq? optdecl #unspecified)
		(if proc
		    (set! optdecl (proc))
		    (set! optdecl #f))))
	  (set! %info
	     (instantiate::DeclInfo
		(optdecl (when proc (proc))))))))

;*---------------------------------------------------------------------*/
;*    decl-update-init! ...                                            */
;*---------------------------------------------------------------------*/
(define (decl-update-init! decl::J2SDecl i)
   (with-access::J2SDecl decl (%info)
      (if (isa? %info DeclInfo)
	  (with-access::DeclInfo %info (init)
	     (set! init i))
	  (set! %info
	     (instantiate::DeclInfo
		(init i))))))

;*---------------------------------------------------------------------*/
;*    used* ...                                                        */
;*---------------------------------------------------------------------*/
(define (used* init::J2SInit decls)
   (let ((used (node-used* init decls)))
      (let loop ((decls used)
		 (allused used))
	 (if (null? decls)
	     allused
	     (let ((decl (car decls)))
		(with-access::J2SDecl decl (%info)
		   (if (not (isa? %info DeclInfo))
		       (list decl)
		       (with-access::DeclInfo %info (init used)
			  (cond
			     ((or (pair? used) (null? used))
			      (loop (cdr decls) (append used allused)))
			     ((fun-init? init)
			      (set! used (fun-used* decl decls))
			      (loop (cdr decls) (append used allused)))
			     (else
			      (loop (cdr decls) allused)))))))))))

;*---------------------------------------------------------------------*/
;*    fun-used* ...                                                    */
;*---------------------------------------------------------------------*/
(define (fun-used* decl::J2SDecl decls)
   (let loop ((decl decl)
	      (store #t))
      (with-access::J2SDecl decl (%info)
	 (with-access::DeclInfo %info (used init)
	    (if (or (pair? used) (null? used))
		used
		(with-access::J2SInit init (rhs)
		   (with-access::J2SFun rhs (body)
		      (let* ((fused (node-used* body decls))
			     (res fused))
			 (when store (set! used '()))
			 (for-each (lambda (d)
				      (when (memq d decls)
					 (when (fun-init? (decl-init d))
					    (set! res
					       (append (loop d #f) res)))))
			    fused)
			 (when store (set! used res))
			 res))))))))

;*---------------------------------------------------------------------*/
;*    new-let-opt ...                                                  */
;*---------------------------------------------------------------------*/
(define (new-let-opt node::J2SNode expr::J2SInit)
   ;; create a new declaration for the statement
   (with-access::J2SInit expr (rhs)
      (let ((decl (init-decl expr)))
	 (if (isa? decl J2SDeclInit)
	     (with-access::J2SDeclInit decl (binder val)
		(set! binder 'let-opt)
		(set! val rhs)
		decl)
	     (with-access::J2SDecl decl (loc id %info)
		(let ((new (instantiate::J2SDeclInit
			      (id id)
			      (loc loc)
			      (key -1)
			      (val rhs)))
		      (fields (class-all-fields J2SDecl)))
		   (let loop ((i (-fx (vector-length fields) 1)))
		      (when (>=fx i 0)
			 (let* ((f (vector-ref fields i))
				(get (class-field-accessor f))
				(set (class-field-mutator f)))
			    (when set
			       (set new (get decl))
			       (loop (-fx i 1))))))
		   (with-access::J2SDecl new (binder scope key)
		      (set! key (ast-decl-key))
		      (set! scope 'local)
		      (set! binder 'let-opt))
		   new))))))

;*---------------------------------------------------------------------*/
;*    fun-init? ...                                                    */
;*---------------------------------------------------------------------*/
(define (fun-init? init)
   (when (isa? init J2SInit)
      (with-access::J2SInit init (rhs)
	 (isa? rhs J2SFun))))

;*---------------------------------------------------------------------*/
;*    letblock-nodes-split ...                                         */
;*    -------------------------------------------------------------    */
;*    Split the NODES of a LET-BLOCK in two parts: INITS x RESTS       */
;*      INITS = the consecutive inits of NODES                         */
;*      RETS = the following NODES                                     */
;*---------------------------------------------------------------------*/
(define (letblock-nodes-split nodes::pair-nil decls)
   (let loop ((nodes nodes)
	      (inits '()))
      (cond
	 ((null? nodes)
	  (values '() inits))
	 ((get-let-inits (car nodes) decls)
	  =>
	  (lambda (is) (loop (cdr nodes) (append! inits is))))
	 (else
	  (values nodes inits)))))

;*---------------------------------------------------------------------*/
;*    get-let-inits ...                                                */
;*    -------------------------------------------------------------    */
;*    Extract the list of let-declarations of a statement.             */
;*---------------------------------------------------------------------*/
(define (get-let-inits node::J2SStmt decls)
   (when (isa? node J2SSeq)
      (with-access::J2SSeq node (nodes)
	 (let loop ((nodes nodes)
		    (inits '()))
	    (if (null? nodes)
		(when (pair? inits)
		   (reverse! inits))
		(let ((n::J2SStmt (car nodes)))
		   (when (isa? n J2SStmtExpr)
		      (with-access::J2SStmtExpr n (expr)
			 (when (isa? expr J2SInit)
			    (let ((decl (init-decl expr)))
			       (when (memq decl decls)
				  (decl-update-init! decl expr)
				  (loop (cdr nodes) (cons expr inits)))))))))))))

;*---------------------------------------------------------------------*/
;*    j2s-toplevel-letopt! ...                                         */
;*    -------------------------------------------------------------    */
;*    Optimize toplevel let declarations.                              */
;*---------------------------------------------------------------------*/
(define (j2s-toplevel-letopt! nodes::pair-nil decls::pair-nil vars::pair-nil)
   
   (define (remove-disabled!::pair-nil decls::pair-nil disabled::pair-nil)
      (for-each (lambda (d)
		   (set! decls (remq! d decls)))
	 disabled)
      decls)

   (let loop ((n nodes)
	      (decls decls)
	      (deps '())
	      (res '()))
      (cond
	 ((null? n)
	  (reverse! res))
	 ((null? decls)
	  (append (reverse! res) n))
	 ((get-inits (car n))
	  =>
	  (lambda (inits)
	     ;; a letinit node
	     (let liip ((inits inits)
			(decls decls)
			(deps deps)
			(res res))
		(if (null? inits)
		    (loop (cdr n) decls deps res)
		    (with-access::J2SInit (car inits) (rhs loc)
		       (let ((used (get-used-decls rhs (append decls vars)))
			     (init (car inits)))
			  (cond
			     ((null? used)
			      ;; optimize this binding
			      (let ((decl (init-decl init)))
				 (with-access::J2SInit init (rsh)
				    (with-access::J2SDeclInit decl (binder val)
				       (set! val rhs)
				       (set! binder 'let-opt)))
				 (let ((ndecls (remq decl decls))
				       (stmtinit (instantiate::J2SStmtExpr
						    (loc loc)
						    (expr init))))
				    (liip (cdr inits) ndecls
				       deps
				       (cons stmtinit res)))))
			     ((isa? rhs J2SFun)
			      ;; optimize this binding but keep tracks
			      ;; of its dependencies
			      (let ((decl (init-decl init)))
				 (with-access::J2SInit init (rhs)
				    (with-access::J2SDeclInit decl (binder val)
				       (set! val rhs)
				       (set! binder 'let-opt)))
				 (let ((ndecls (remq decl decls)))
				    (liip (cdr inits) ndecls
				       (cons (cons init used) deps)
				       res))))
			     (else
			      ;; optimize this binding but mark that
			      ;; the variables it uses are disabled
			      (let ((decl (init-decl init))
				    (used (get-used-decls rhs (append decls vars)))
				    (disabled (get-used-deps used deps)))
				 (liip (cdr inits)
				    (remove-disabled! decls disabled)
				    deps
				    (cons (car n) res)))))))))))
	 ((null? (cdr n))
	  ;; this is the last stmt which happens not to be a binding
	  (reverse! (cons (car n) res)))
	 (else
	  ;; a regular statement
	  (with-access::J2SNode (car n) (loc)
	     (let ((used (get-used-decls (car n) (append decls vars))))
		(if (null? used)
		    ;; harmless statement, ignore it
		    (loop (cdr n) decls deps (cons (car n) res))
		    ;; disable optimization for recursively used decls
		    (let ((disabled (get-used-deps used deps)))
		       (loop (cdr n)
			  (remove-disabled! decls disabled)
			  deps
			  (cons (car n) res))))))))))

;*---------------------------------------------------------------------*/
;*    get-used-decls ...                                               */
;*    -------------------------------------------------------------    */
;*    Amongst DECLS, returns those that appear in NODE.                */
;*---------------------------------------------------------------------*/
(define (get-used-decls node::J2SNode decls)
   (delete-duplicates! (node-used* node decls)))

;*---------------------------------------------------------------------*/
;*    node-used* ...                                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SNode decls)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2SDecl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SDecl decls)
   (if (member node decls) (list node) '()))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2Ref ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SRef decls)
   (with-access::J2SRef node (decl)
      (if (member decl decls) (list decl) '())))

;*---------------------------------------------------------------------*/
;*    node-used* ::J2SInit ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-used* node::J2SInit decls)
   (with-access::J2SInit node (lhs rhs)
      (if (isa? lhs J2SRef)
	  (node-used* rhs decls)
	  (append (node-used* lhs decls) (node-used* rhs decls)))))

;*---------------------------------------------------------------------*/
;*    get-used-deps ...                                                */
;*    -------------------------------------------------------------    */
;*    transitive closure of the depends-on property                    */
;*---------------------------------------------------------------------*/
(define (get-used-deps usedecls deps)
   (let loop ((udecls usedecls)
	      (res '()))
      (cond
	 ((null? udecls)
	  res)
	 ((memq (car udecls) res)
	  (loop (cdr udecls) res))
	 (else
	  (let ((udeps (assq (car udecls) deps)))
	     (if (pair? udeps)
		 (loop (append (cdr udeps) (cdr udecls))
		    (cons (car udecls) res))
		 (loop (cdr udecls) (cons (car udecls) res))))))))
