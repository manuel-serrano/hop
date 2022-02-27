;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/inline-depth.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 18 04:15:19 2017                          */
;*    Last change :  Sun Feb 27 07:01:11 2022 (serrano)                */
;*    Copyright   :  2017-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Function/Method inlining optimization                            */
;*    -------------------------------------------------------------    */
;*    This uses the depth strategy that consists in granting a         */
;*    certain expansion quota per function call and expanding          */
;*    that call recursively until the quota is expired.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_inline-depth

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

   (export (j2s-inline-depth ::J2SProgram conf)))

;*---------------------------------------------------------------------*/
;*    j2s-inline-depth ...                                             */
;*---------------------------------------------------------------------*/
(define (j2s-inline-depth this::J2SProgram conf)
   (with-access::J2SProgram this (decls nodes)
      ;; count and mark all the calls
      (j2s-count-calls! this conf)
      (let ((pms (ptable
		    (append
		       (append-map collect-proto-methods* nodes)
		       (if (config-get conf :optim-inline-class-method)
			   (append-map collect-proto-methods* decls)
			   '())))))
	 (inline! this #f 24.0 '() pms #f this conf)
	 (ptable-verb pms))
      (j2s-inline-cleanup! this conf)))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SNode ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SNode
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SMeta ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMeta
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SMeta this (optim debug meta stmt)
      (if (eq? meta 'inline)
	  (if (or (=fx optim 0) (>fx debug 0))
	      this
	      (inline! stmt targets (fixnum->flonum optim)
		 stack pmethods ingen prgm conf))
	  (call-default-walker))))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SMetaInl ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SMetaInl
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SMetaInl this (inlstack stmt loc optim)
      (when (>fx optim 0)
	 (set! stmt
	    (inline! stmt
	       targets limit (append inlstack stack) pmethods ingen prgm conf)))
      this))
   
;*---------------------------------------------------------------------*/
;*    inline! ::J2SFun ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFun
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SFun this (optimize body generator %info)
      (when optimize
	 (set! body
	    (inline! body
	       targets limit (cons this stack) pmethods
	       generator prgm conf)))
      this))

;*---------------------------------------------------------------------*/
;*    inline! ::J2SWhile ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SWhile
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SWhile this (test body)
      (set! test (inline! test targets limit stack pmethods ingen prgm conf))
      (let ((nlimit (if (flonum? limit) (*fl limit 1.1) limit)))
	 ;; push the limit by 10% inside loops
	 (set! body (inline! body targets nlimit stack pmethods ingen prgm conf))
	 this)))
		  
;*---------------------------------------------------------------------*/
;*    inline! ::J2SFor ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SFor
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SFor this (init test incr body)
      (set! init (inline! init targets limit stack pmethods ingen prgm conf))
      (set! test (inline! test targets limit stack pmethods ingen prgm conf))
      (set! incr (inline! incr targets limit stack pmethods ingen prgm conf))
      (let ((nlimit (if (flonum? limit) (*fl limit 1.1) limit)))
	 ;; push the limit by 10% inside loops
	 (set! body (inline! body targets nlimit stack pmethods ingen prgm conf))
	 this)))
		  
;*---------------------------------------------------------------------*/
;*    inline! ::J2SForIn ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SForIn
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)
   (with-access::J2SForIn this (lhs obj body)
      (set! lhs (inline! lhs targets limit stack pmethods ingen prgm conf))
      (set! obj (inline! obj targets limit stack pmethods ingen prgm conf))
      (let ((nlimit (if (flonum? limit) (*fl limit 1.1) limit)))
	 ;; push the limit by 10% inside loops
	 (set! body (inline! body targets nlimit stack pmethods ingen prgm conf))
	 this)))
		  
;*---------------------------------------------------------------------*/
;*    inline! ::J2SCall ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (inline! this::J2SCall
		       targets limit stack::pair-nil
		       pmethods ingen prgm conf)

   (define (quota limit args)
      ;; Compute the maximal size this call can expand to.
      ;;   - if LIMIT is a floating point number when the call has never
      ;;     been inlined and it is an expansion factor;
      ;;   - if LIMIT is a fixnum, the call has already been inlined
      ;;     the it is the remaining available size
      (if (fixnum? limit)
	  limit
	  (flonum->fixnum (* limit (maxfx 2 (length args))))))

   (define (inlinable? val::J2SFun args id::symbol)
      ;; is function is inlinable, regardless of the context of the call
      (and (not (isa? val J2SSvc))
	   (>=fx (function-arity val) (length args))
	   (function-fxarg? val)
	   (not (function-generator? val))
	   (not (function-newtarget? val))
	   (not (function-delete-argument? val))
	   (not (function-self-recursive? val))
	   (eq? (function-mode val) (function-mode (car stack)))
	   (inline-check-id id)))

   (define (method-type-compatible? fun::J2SAccess met::J2SFun)
      ;; check that the inline candidate MET, has a type compabile
      ;; with actual caller
      (with-access::J2SAccess fun (obj)
	 (with-access::J2SFun met (thisp)
	    (let ((thisty (j2s-type thisp))
		  (objty (j2s-type obj)))
	       (or (memq objty '(any unknown))
		   (memq thisty '(any unknown))
		   (type-maybe-subtype? objty thisty))))))

   (define (find-inline-ref-candidate this::J2SCall fun::J2SRef args)
      (with-access::J2SRef fun (decl)
	 (when (isa? decl J2SDeclFun)
	    (with-access::J2SDeclFun decl (id)
	       (let ((val (j2sdeclinit-val-fun decl)))
		  (when (inlinable? val args id)
		     val))))))
   
   (define (find-inline-access-candidates this::J2SCall fun::J2SAccess args)
      (with-access::J2SAccess fun (obj field loc)
	 (if (and (isa? field J2SString)
		    (config-get conf :optim-inline-method #f)
		    (not (isa? obj J2SSuper)))
	     (with-access::J2SString field (val)
		(let* ((id (string->symbol val))
		       (mets (filter (lambda (m::struct)
					(let ((v (protoinfo-method m)))
					   (and (inlinable? v args id)
						(method-type-compatible? fun v))))
				(or (hashtable-get pmethods val) '()))))
		   (sort (lambda (f1 f2)
			    (<=fx (function-size (protoinfo-method f1))
			       (function-size (protoinfo-method f2))))
		      mets)))
	     '())))

   (define (reduce-for-quota mets args quota)
      ;; reduce the set of possibly inlined mets so that
      ;; it does not overpass the authorized quota
      (let* ((size (apply +
		      (map (lambda (m)
			      (method-size (protoinfo-method m)))
			 mets)))
	     (used (maxfx 1 (-fx size (length args)))))
	 (let loop ((mets mets)
		    (used used))
	    (cond
	       ((null? mets)
		'())
	       ((<=fx used quota)
		mets)
	       (else
		(let ((fs (function-size (protoinfo-method (car mets)))))
		   (loop (cdr mets) (-fx size (+fx fs inline-method-check-size)))))))))

   (define (inline-access-call this::J2SCall fun::J2SAccess args loc)
      (with-access::J2SAccess fun (obj field)
	 (unless (eq? (j2s-type obj) 'proxy)
	    (let* ((quota (quota limit args))
		   (mets (find-inline-access-candidates this fun args))
		   (mets (filter-rutype
			    (if (pair? targets)
				(filter (lambda (t) (memq t mets)) targets)
				mets)
			    prgm))
		   (mets (reduce-for-quota mets args quota)))
;* 	       (when (and (isa? field J2SString)                       */
;* 			  (with-access::J2SString field (val)          */
;* 			     (string=? val "linear_combination")))     */
;* 		  (tprint "CALL " loc " " (length mets) " quota=" quota */
;* 		     " limit=" limit " size=" (node-size this)         */
;* 		     " isize=" (map (lambda (m)                        */
;* 				       (method-size (protoinfo-method m))) */
;* 				  mets)))                              */
	       (when (pair? mets)
		  (let* ((funs (map protoinfo-method mets))
			 (size (apply + (map method-size funs)))
			 (used (maxfx 1 (-fx size (length args)))))
		  (inline-verb loc fun
		     (map (lambda (x) '-) mets) size quota 0 conf)
		  (let* ((funs (map protoinfo-method mets))
			 (new-limit (-fx quota used))
			 (new-stack (append funs stack)))
		     (inline!
			(inline-stmt->expr loc
			   (inline-method-call fun mets args
			      loc (node-endloc this)
			      '() limit stack pmethods ingen prgm conf)
			   (function-rutype (protoinfo-method (car mets))))
			#t new-limit new-stack pmethods ingen prgm conf))))))))
   
   (define (inline-ref-call this::J2SCall ref::J2SRef thisargs args loc)
      (cond
	 ((find-inline-ref-candidate this ref args)
	  =>
	  (lambda (fun)
	     (let* ((quota (quota limit args))
		    (size (function-size fun))
		    (used (maxfx 1 (-fx size (length args)))))
		(when (and (<=fx used quota) (not (memq fun stack)))
		   ;; the function is small enought
		   (inline-verb loc ref '(-) size quota 0 conf)
		   ;; compute the limit, ensuring that it always decreases
		   (let ((new-limit (-fx quota used))
			 (new-stack (cons fun stack)))
		      (inline! 
			 (inline-stmt->expr loc
			    (inline-function-call fun thisargs args loc)
			    (function-rutype fun))
			 #t new-limit new-stack pmethods ingen prgm conf))))))
	 (else
	  #f)))

   (with-access::J2SCall this (fun thisargs args type loc cache protocol)
      (cond
	 ;; not inlined calls
	 ((memq (caddr loc) inline-blacklistloc)
	  (call-default-walker))
	 ((and (pair? inline-whitelistloc) (not (memq (caddr loc) inline-whitelistloc)))
	  (call-default-walker))
	 ((null? stack)
	  (call-default-walker))
	 (cache
	  (call-default-walker))
	 ((eq? protocol 'spread)
	  (call-default-walker))
	 ((and ingen (any yield? args))
	  (call-default-walker))
	 ;; inlined calls
	 ((isa? fun J2SRef)
	  (or (inline-ref-call this fun thisargs args loc)
	      (call-default-walker)))
	 ((isa? fun J2SAccess)
	  (or (inline-access-call this fun args loc)
	      (call-default-walker)))
;* 	 ((pair? targets)                                              */
;* 	  (or (inline-expr-call this fun thisargs args loc)            */
;* 	      (call-default-walker)))                                  */
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    inline-function-call ...                                         */
;*---------------------------------------------------------------------*/
(define (inline-function-call val::J2SFun thisargs args::pair-nil loc)
   (with-access::J2SFun val (%info thisp params (floc loc))
      (let* ((body (funinfo-initial-body %info))
	     (vals (inline-args (cons thisp params)
		      (if (pair? thisargs)
			  (append thisargs args)
			  (cons (J2SUndefined) args))
		      loc))
	     (nbody (j2s-alpha body (cons thisp params) vals)))
	 (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
	    nbody))))

;* {*---------------------------------------------------------------------*} */
;* {*    inline-unknown-call ...                                          *} */
;* {*---------------------------------------------------------------------*} */
;* (define (inline-unknown-call ref::J2SRef thisargs args::pair-nil loc */
;* 	   targets limit stack::pair-nil pmethods ingen prgm conf) */
;*    (let loop ((targets targets))                                    */
;*       (if (null? targets)                                           */
;* 	  (J2SStmtExpr (J2SCall* ref args))                            */
;* 	  (let* ((target (car targets))                                */
;* 		 (fun (targetinfo-fun target)))                        */
;* 	     (if (< (function-size fun) limit)                         */
;* 		 (begin                                                */
;* 		    (inline-verb loc fun '(-) (function-size fun) limit 0 conf) */
;* 		    (J2SIf (J2SHopCall (J2SHopRef/rtype 'eq? 'bool)    */
;* 			      (J2SRef (with-access::J2SRef ref (decl) decl)) */
;* 			      (J2SRef (targetinfo-decl target)))       */
;* 		       (inline-function-call fun thisargs args loc     */
;* 			  #f 0 stack pmethods ingen prgm conf)         */
;* 		       (loop (cdr targets))))                          */
;* 		 (loop '()))))))                                       */

;*---------------------------------------------------------------------*/
;*    inline-method-call ...                                           */
;*---------------------------------------------------------------------*/
(define (inline-method-call fun::J2SAccess callees::pair args::pair-nil loc endloc
	   targets limit stack::pair-nil pmethods ingen prgm conf)

   (define (get-cache prgm::J2SProgram)
      (with-access::J2SProgram prgm (pcache-size)
	 (let ((n pcache-size))
	    (set! pcache-size (+fx pcache-size 1))
	    n)))

   (define (proto-method obj)
      (if (isa? (j2s-type obj) J2SRecord)
	  (if (pair? (cdr callees)) 'record-method-mono 'record-method-poly)
	  (if (pair? (cdr callees)) 'proto-method-mono 'proto-method-poly)))

   (define (cache-check c loc owner obj field kont inline::J2SStmt)
      (if (private-field? field)
	  inline
	  (J2SIf (J2SCacheCheck (proto-method obj) c owner obj field)
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
			(J2SLetOpt '(ref assig) id a)))))
	 args))

   (define (inline-record-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee))
	    (clazz (protoinfo-owner callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (with-access::J2SRef obj (decl)
	       (let ((vals (inline-args params args loc))
		     (ndecl (J2SLetOpt/vtype clazz '(get) (gensym 'this)
			       (J2SCast/static #t clazz obj))))
		  (cache-check cache loc (protoinfo-owner callee) obj field kont
		     (LetBlock floc (cons ndecl
				       (filter (lambda (b) (isa? b J2SDecl)) vals))
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (j2s-alpha body
			      (cons thisp params) (cons ndecl vals))))))))))

   (define (inline-object-method obj::J2SRef field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (with-access::J2SRef obj (decl)
	       (let ((vals (inline-args params args loc)))
		  (cache-check cache loc (protoinfo-owner callee) obj field kont
		     (LetBlock floc (filter (lambda (b) (isa? b J2SDecl)) vals)
			(J2SMetaInl (cons val stack)
			   (config-get conf :optim 0)
			   (j2s-alpha body
			      (cons thisp params) (cons decl vals))))))))))

   (define (inline-expr-method obj::J2SExpr field callee args cache loc kont)
      (let ((val (protoinfo-method callee)))
	 (with-access::J2SFun val (body thisp params (floc loc))
	    (let ((vals (inline-args params args loc))
		  (decl (J2SLetOpt '(ref) (gensym 'this) obj)))
	       (cache-check cache loc (protoinfo-owner callee) obj field kont
		  (LetBlock floc (cons decl (filter (lambda (b) (isa? b J2SDecl)) vals))
		     (J2SMetaInl (cons val stack)
			(config-get conf :optim 0)
			(j2s-alpha body
			   (cons thisp params) (cons decl vals)))))))))

   (define (inline-method obj field callee args cache loc kont)
      (cond
	 ((not (isa? obj J2SRef))
	  (inline-expr-method obj field callee args cache loc kont))
	 ((isa? (protoinfo-owner callee) J2SRecord)
	  (inline-record-method obj field callee args cache loc kont))
	 (else
	  (inline-object-method obj field callee args cache loc kont))))

   (define (inline-object-method-call fun self args)
      (with-access::J2SAccess fun (obj field cspecs)
	 (let loop ((callees callees)
		    (caches '()))
	    (if (null? callees)
		(let ((f (duplicate::J2SAccess fun
			    (obj self))))
		   (if (isa? (j2s-type self) J2SRecord)
		       (J2SMetaInl stack 0
			  (J2SReturn #t
			     (J2SMethodCall* f (list self) args)))
		       (let* ((c (get-cache prgm))
			      (r (J2SLetOpt '(call) (gensym 'r)
				    (J2SMethodCall/cache* f (list self) args
				       '(vtable-inline pmap-inline poly) c))))
			  (J2SLetRecBlock #f (list r)
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
							    (J2SCacheUpdate (proto-method obj)
							       (car c) (j2s-alpha self '() '()))
							    (J2SUndefined))))
						caches))
					  (loop (cdr cs))))))
			     (J2SReturn #t (J2SRef r))))))
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
		    (d (J2SLetOpt/vtype (j2s-type obj) '(get) id obj)))
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
(define (inline-args params args loc)
   (let ((lena (length args))
	 (lenp (length params)))
      (map (lambda (p a)
	      (cond
		 ((and (ronly-variable? p) (simple-argument? a))
		  a)
		 (else
		  (with-access::J2SDecl p ((_usage usage) id writable)
		     (with-access::J2SNode a (loc)
			(let ((d (J2SLetOpt _usage (gensym id) a)))
			   (with-access::J2SDecl d ((w writable))
			      (set! w writable))
			   d))))))
	 params
	 (if (<fx lena lenp)
	     (append args
		;; complement with missing args
		(map! (lambda (i) (J2SUndefined)) (iota (-fx lenp lena))))
	     args))))

