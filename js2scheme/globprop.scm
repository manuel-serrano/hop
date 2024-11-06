;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/globprop.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 26 08:28:06 2017                          */
;*    Last change :  Wed Nov  6 19:29:31 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Global properties optimization (constant propagation).           */
;*                                                                     */
;*    This optimization propages global object constant properties.    */
;*    That is, it replaces a pattern:                                  */
;*                                                                     */
;*      G = INIT                                                       */
;*      ...                                                            */
;*      G.PROP1 = EXPR                                                 */
;*      ...                                                            */
;*      ... G.PROP1 ...                                                */
;*                                                                     */
;*    with:                                                            */
;*                                                                     */
;*      G = INIT                                                       */
;*      ...                                                            */
;*      const TMP = EXPR                                               */
;*      G.PROP1 = TMP                                                  */
;*      const TMPown = G owns PROP1                                    */
;*      ...                                                            */
;*      ... ( TMPOwn ) ? TMP : G.PROP1 ...                             */
;*                                                                     */
;*    if it can be proved that PROP1 is not in the G prototype chain   */
;*    then, the better transformation is used.                         */
;*                                                                     */
;*      G = INIT                                                       */
;*      ...                                                            */
;*      const TMP = EXPR                                               */
;*      G.PROP1 = TMP                                                  */
;*      ...                                                            */
;*      ... TMP ...                                                    */
;*                                                                     */
;*    For that transformation to apply, all the following properties   */
;*    must hold:                                                       */
;*                                                                     */
;*      1. the assignment is toplevel or EXPR is a constant            */
;*      2. G is a read-only variable, initialized, not used as value   */
;*      3. no setter and getter are used                               */
;*      4. never used in a delete op                                   */
;*      5. INIT must be a literal, a function, or a builtin object     */
;*      6. there should be only one single PROP1 in G, in particular   */
;*         there is no computed prop assigned to G                     */
;*      7. there is no direct EVAL                                     */
;*      8. PROP1 is never assigned                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_globprop

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (export j2s-globprop-stage))

;*---------------------------------------------------------------------*/
;*    j2s-globprop-stage ...                                           */
;*---------------------------------------------------------------------*/
(define j2s-globprop-stage
   (instantiate::J2SStageProc
      (name "globprop")
      (comment "Global property constant propagation")
      (proc j2s-globprop!)
      (optional :optim-globprop)))

;*---------------------------------------------------------------------*/
;*    propinfo ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct propinfo init props needcheckp)

;*---------------------------------------------------------------------*/
;*    j2s-globprop! ::J2SProgram ...                                   */
;*---------------------------------------------------------------------*/
(define (j2s-globprop! this conf)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes decls direct-eval)
	 (unless direct-eval
	    (let ((gcnsts (collect-globconst* this conf)))
	       (when (pair? gcnsts)
		  ;; propagate the constants
		  (when (>=fx (config-get conf :verbose 0) 3)
		     (display " collect-this" (current-error-port)))
		  (collect-globprops this conf)
		  (when (>=fx (config-get conf :verbose 0) 3)
		     (display " collect-toplevel" (current-error-port)))
		  (collect-globprops-toplevel! this conf)
		  (when (>=fx (config-get conf :verbose 0) 3)
		     (display " rewrite-access" (current-error-port)))
		  (rewrite-accesses! this)
		  (rewrite-decl-inits! gcnsts)
		  (let ((ndecls (append-map globprop-const gcnsts)))
		     (set! decls (append decls ndecls))
		     (when (>=fx (config-get conf :verbose 0) 3)
			(globprop-verb gcnsts))))))))
   this)

;*---------------------------------------------------------------------*/
;*    globprop-const ...                                               */
;*---------------------------------------------------------------------*/
(define (globprop-const d)
   (with-access::J2SDecl d (id %info)
      (if (and (propinfo? %info) (pair? (propinfo-props %info)))
	  (append-map (lambda (i)
			 (if (and (pair? (cdr i)) (isa? (cadr i) J2SDecl))
			     (cdr i)
			     '()))
	     (propinfo-props %info))
	  '())))

;*---------------------------------------------------------------------*/
;*    globprop-verb ...                                                */
;*---------------------------------------------------------------------*/
(define (globprop-verb gcnsts)
   (fprintf (current-error-port) " (~(, ))"
      (append-map (lambda (g)
		     (with-access::J2SDecl g (%info id)
			(if (pair? (propinfo-props %info))
			    (filter-map (lambda (i)
					   (when (and (pair? (cdr i))
						      (isa? (cadr i) J2SDecl))
					      (format "~a.~a" id (car i))))
			       (propinfo-props %info))
			    '())))
	 gcnsts)))

;*---------------------------------------------------------------------*/
;*    constant-object? ...                                             */
;*---------------------------------------------------------------------*/
(define (constant-object? expr::J2SExpr)
   (cond
      ((isa? expr J2SObjInit)
       #t)
      ((isa? expr J2SLiteralCnst)
       #t)
      ((isa? expr J2SFun)
       #t)
      ((isa? expr J2SNew)
       (with-access::J2SNew expr (clazz)
	  (is-builtin-ref? clazz 'Object)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    collect-globconst* ::J2SNode ...                                 */
;*    -------------------------------------------------------------    */
;*    Collect all the global variables that are initialized but        */
;*    never assigned.                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globconst* this::J2SNode ctx)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-globconst* ::J2SDecl ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globconst* this::J2SDecl ctx)
   '())

;*---------------------------------------------------------------------*/
;*    collect-globconst* ::J2SInit ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globconst* this::J2SInit ctx)
   (with-access::J2SInit this (lhs rhs)
      ;; no need to scan rhs as we are only looking for variable decls/inits
      (if (isa? lhs J2SRef)
	  (with-access::J2SRef lhs (decl)
	     (with-access::J2SDecl decl (%info)
		(if (and (not (decl-usage-has? decl '(assig uninit delete)))
			 (constant-object? rhs))
		    (begin
		       (set! %info (propinfo rhs '() #t))
		       (list decl))
		    '())))
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-globconst* ::J2SDeclInit ...                             */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globconst* this::J2SDeclInit ctx)
   (with-access::J2SDeclInit this (val %info)
      (if (and (not (decl-usage-has? this '(assig delete))) (constant-object? val))
	  (begin
	     (set! %info (propinfo val '() #f))
	     (list this))
	  '())))

;*---------------------------------------------------------------------*/
;*    collect-globconst* ::J2SFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globconst* this::J2SFun ctx)
   '())

;*---------------------------------------------------------------------*/
;*    collect-globprops ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globprops this::J2SNode ctx)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-globprops ::J2SAssig ...                                 */
;*    -------------------------------------------------------------    */
;*    For all constant declarations, collect the list of assigned      */
;*    attributes. Attributes assigned only once are marked with #t.    */
;*    Others are marked with #f.                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globprops this::J2SAssig ctx)
   
   (define (collect-property %info field)
      (when (and (propinfo? %info)
		 (not (eq? (propinfo-props %info) '*))
		 (isa? field J2SString))
	 (with-access::J2SString field (val)
	    (let ((c (assoc val (propinfo-props %info))))
	       (cond
		  ((pair? c)
		   (set-cdr! c #f))
		  ((or (isa? this J2SPostfix) (isa? this J2SPrefix) (isa? this J2SAssigOp))
		   (propinfo-props-set! %info
		      (cons (cons val #f) (propinfo-props %info))))
		  (else
		   (propinfo-props-set! %info
		      (cons (cons val #t) (propinfo-props %info)))))))))
   
   (with-access::J2SAssig this (lhs rhs loc)
      (cond
	 ((isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (obj field)
	     (if (isa? obj J2SRef)
		 (with-access::J2SRef obj (decl)
		    (collect-globprops rhs ctx)
		    (with-access::J2SDecl decl (%info)
		       (or (collect-property %info field)
			   (collect-globprops lhs ctx))))
		 (call-default-walker))))
	 ((and (isa? lhs J2SRef) (isa? rhs J2SObjInit))
	  (with-access::J2SObjInit rhs (inits loc)
	     (if (<fx (length inits) (config-get ctx :max-objinit-optim-size))
		 (with-access::J2SRef lhs (decl)
		    (with-access::J2SDecl decl (%info)
		       (if (propinfo? %info)
			   (for-each (lambda (init)
					(when (isa? init J2SDataPropertyInit)
					   (with-access::J2SDataPropertyInit init (name)
					      (collect-property %info name))))
			      inits)
			   (call-default-walker))))
		 (collect-globprops lhs ctx))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    collect-globprops-toplevel! ...                                  */
;*    -------------------------------------------------------------    */
;*    Collect the initialization value of the attributes that are      */
;*    marked as assigned once.                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globprops-toplevel! this::J2SNode ctx)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    collect-globprops-toplevel! ::J2SAssig ...                       */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globprops-toplevel! this::J2SAssig ctx)
   
   (define (collect-init! loc lhs rhs obj field %info)
      (with-access::J2SString field (val)
	 (let ((c (assoc val (propinfo-props %info))))
	    (cond
	       ((not (and (pair? c) (cdr c)))
		this)
	       ((isa? rhs J2SLiteralCnst)
		(let ((ndecl (J2SLetOptRoGlobal '(ref init)
				(gensym val) rhs)))
		   (set! rhs (J2SRef ndecl))
		   (set-cdr! c (list ndecl))))
	       ((propinfo-needcheckp %info)
		(let ((ndecl (J2SDecl/scope 'tls 'let-opt '(ref init) (gensym val)))
		      (ndeclo (J2SLetOptVtype 'bool '(ref init assig)
				 (gensym val) (J2SBool #f))))
		   (with-access::J2SDeclInit ndeclo (scope)
		      (set! scope 'tls))
		   (set-cdr! c (list ndecl ndeclo))
		   (J2SSequence
		      (J2SInit (J2SRef ndecl) rhs)
		      (J2SAssig lhs (J2SRef ndecl))
		      (J2SAssig (J2SRef ndeclo)
			 (J2SHopCall
			    (J2SHopRef/rtype 'js-has-own-property 'bool
			       '__hopscript_property)
			    obj
			    field
			    (J2SPragma '%this))))))
	       ((or (isa? rhs J2SNumber) (isa? rhs J2SBool) (isa? rhs J2SString))
		(let ((ndecl (J2SLetOptRo '(ref init)
				(gensym val)
				rhs)))
		   (with-access::J2SDecl ndecl (scope id)
		      (set! scope 'tls))
		   (set-cdr! c (list ndecl))
		   (J2SSequence
		      (J2SAssig lhs (J2SRef ndecl)))))
	       (else
		(let ((ndecl (J2SDecl/scope 'tls 'let '(ref init) (gensym val))))
		   (set-cdr! c (list ndecl))
		   (J2SSequence
		      (J2SInit (J2SRef ndecl) rhs)
		      (J2SAssig lhs (J2SRef ndecl)))))))))

   (with-access::J2SAssig this (lhs rhs)
      (if (not (isa? lhs J2SAccess))
	  (call-default-walker)
	  (with-access::J2SAccess lhs (obj field loc)
	     (if (not (isa? obj J2SRef))
		 (call-default-walker)
		 (with-access::J2SRef obj (decl)
		    (set! rhs (collect-globprops-toplevel! rhs ctx))
		    (with-access::J2SDecl decl (%info)
		       (if (and (propinfo? %info)
				(pair? (propinfo-props %info))
				(isa? field J2SString))
			   (collect-init! loc lhs rhs obj field %info)
			   (begin
			      (set! field (collect-globprops-toplevel! field ctx))
			      this)))))))))

;*---------------------------------------------------------------------*/
;*    collect-globprops-toplevel! ::J2SInit ...                        */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globprops-toplevel! this::J2SInit ctx)
   
   (define (collect-init! init::J2SDataPropertyInit %info)
      (with-access::J2SDataPropertyInit init (loc val name)
	 (when (isa? name J2SString)
	    (with-access::J2SString name ((str val))
	       (let ((c (assoc str (propinfo-props %info))))
		  (cond
		     ((not (and (pair? c) (cdr c)))
		      #f)
		     ((isa? val J2SLiteralCnst)
		      (let* ((ndecl (J2SLetOptRoGlobal '(ref init) (gensym str)
				       val))
			     (init (J2SInit (J2SRef ndecl) val)))
			 (set-cdr! c (list ndecl))
			 (set! val (J2SRef ndecl))
			 init))
		     (else
		      (let* ((ndecl (J2SDeclGlobal 'let '(ref init) (gensym str)))
			     (init (J2SInit (J2SRef ndecl) val)))
			 (set-cdr! c (list ndecl))
			 (set! val (J2SRef ndecl))
			 init))))))))
   
   (define (col init %info)
      (when (isa? init J2SDataPropertyInit)
	 (with-access::J2SDataPropertyInit init (name val)
	    (collect-init! init %info))))
   
   (with-access::J2SInit this (lhs rhs)
      (with-access::J2SRef lhs (decl)
	 (set! rhs (collect-globprops-toplevel! rhs ctx))
	 (with-access::J2SDecl decl (%info)
	    (if (and (propinfo? %info)
		     (pair? (propinfo-props %info))
		     (isa? rhs J2SObjInit))
		(with-access::J2SObjInit rhs (inits loc)
		   (let ((assigs (filter-map (lambda (i) (col i %info))
				    inits)))
		      (if (pair? assigs)
			  (J2SSequence* (append assigs (list this)))
			  this)))
		(begin
		   (set! lhs (collect-globprops-toplevel! lhs ctx))
		   this))))))

;*---------------------------------------------------------------------*/
;*    collect-globpprops-toplevel! ::J2SFun ...                        */
;*---------------------------------------------------------------------*/
(define-walk-method (collect-globprops-toplevel! this::J2SFun ctx)
   this)

;*---------------------------------------------------------------------*/
;*    rewrite-decl-inits! ...                                          */
;*---------------------------------------------------------------------*/
(define (rewrite-decl-inits! decls)
   (for-each rewrite-decl-init! decls))

;*---------------------------------------------------------------------*/
;*    rewrite-decl-init! ::J2SNode ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (rewrite-decl-init! this::J2SNode)
   this)

;*---------------------------------------------------------------------*/
;*    rewrite-decl-init! ::J2SDeclInit ...                             */
;*    -------------------------------------------------------------    */
;*    The original properpties are still stored in the object          */
;*    but as the optimization removes references to the global         */
;*    variable pointing to the object, the GC will collect them.       */
;*    To avoid that, this code forces a eternal reference to the       */
;*    initial object.                                                  */
;*---------------------------------------------------------------------*/
(define-method (rewrite-decl-init! this::J2SDeclInit)
   (with-access::J2SDeclInit this (val loc)
      (set! val (J2SHopCall (J2SHopRef 'js-tls-gc-mark!) val))
      this))

;*---------------------------------------------------------------------*/
;*    rewrite-decl-init! ::J2SDeclFun ...                              */
;*---------------------------------------------------------------------*/
(define-method (rewrite-decl-init! this::J2SDeclFun)
   this)
   
;*---------------------------------------------------------------------*/
;*    rewrite-accesses! ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-accesses! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    rewrite-accesses! ::J2SAccess ...                                */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-accesses! this::J2SAccess)
   (with-access::J2SAccess this (obj field)
      (if (isa? obj J2SRef)
	  (with-access::J2SRef obj (decl loc)
	     (with-access::J2SDecl decl (%info)
		(if (propinfo? %info)
		    (if (isa? field J2SString)
			(with-access::J2SString field (val)
			   (if (eq? (propinfo-props %info) '*)
			       this
			       (let ((c (assoc val (propinfo-props %info))))
				  (cond
				     ((or (not (pair? c)) (not (pair? (cdr c))))
				      this)
				     ((pair? (cddr c))
				      (J2SCond (J2SRef (caddr c))
					 (J2SRef (cadr c))
					 this))
				     (else
				      (J2SRef (cadr c)))))))
			(begin
			   (set! field (rewrite-accesses! field))
			   this))
		    (call-default-walker))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    rewrite-accesses! ::J2SAssig ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-accesses! this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (obj field)
	     (if (isa? obj J2SRef)
		 (begin
		    ;; don't traverse the access per se to avoid
		    ;; rewriting the property initialization
		    (set! field (rewrite-accesses! field))
		    (set! rhs (rewrite-accesses! rhs))
		    this)
		 (call-default-walker)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    rewrite-accesses! ::J2SCall ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (rewrite-accesses! this::J2SCall)
   (with-access::J2SCall this (args fun thisargs)
      (if (isa? fun J2SAccess)
	  (with-access::J2SAccess fun (obj field)
	     (if (isa? obj J2SRef)
		 (let ((nfun (rewrite-accesses! fun)))
		    (let ((node (call-default-walker)))
		       (unless (eq? nfun fun)
			  (with-access::J2SCall node (thisargs)
			     (set! thisargs (list obj))))
		       node))
		 (call-default-walker)))
	  (call-default-walker))))
