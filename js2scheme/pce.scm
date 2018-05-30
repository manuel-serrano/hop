;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/pce.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 15 09:53:30 2018                          */
;*    Last change :  Wed May 30 07:18:44 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Property Cache Elimination optimization                          */
;*    -------------------------------------------------------------    */
;*    This optimization eliminate redundant cache checks.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_pce

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)

   (static (class J2SBlockPCE::J2SBlock
	      (ainfos::pair read-only))
	   (class AInfo
	      access::J2SAccess
	      field::bstring))
	      
   (export j2s-pce-stage))

;*---------------------------------------------------------------------*/
;*    j2s-pce-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-pce-stage
   (instantiate::J2SStageProc
      (name "pce")
      (comment "Property Cache Elimination optimization")
      (proc j2s-pce!)
      (optional :optim-pce)))

;*---------------------------------------------------------------------*/
;*    j2s-pce! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-pce! this args)
   (if (isa? this J2SProgram)
       (let ((nthis (insert-pce! this)))
	  (when (>=fx (bigloo-debug) 1)
	     (let* ((tmp (config-get args :tmp "/tmp"))
		    (f (make-file-path tmp
			  (string-replace "pce-" (file-separator) #\_))))
		(call-with-output-file f
		   (lambda (p)
		      (fprint p ";; -*-bee-*-")
		      (pp (j2s->list nthis) p)))))
	  (with-access::J2SProgram nthis (pcache-size)
	     (let ((counter (make-counter pcache-size)))
		(let ((res (expand-pce! nthis counter #t)))
		   (set! pcache-size (get counter))
		   res))))
       this))

;*---------------------------------------------------------------------*/
;*    pce-duplicate-threshold ...                                      */
;*---------------------------------------------------------------------*/
(define pce-duplicate-threshold 3)

;*---------------------------------------------------------------------*/
;*    j2s-info->list ::AInfo ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-info->list this::AInfo)
   (with-access::AInfo this (access field)
      (with-access::J2SAccess access (obj)
	 (with-access::J2SRef obj (decl)
	    (with-access::J2SDecl decl (id key)
	       (format "~a.~a:~a" id key field))))))

;*---------------------------------------------------------------------*/
;*    j2s->list ::J2SBlockPCE ...                                      */
;*---------------------------------------------------------------------*/
(define-method (j2s->list this::J2SBlockPCE)
   (with-access::J2SBlockPCE this (ainfos)
      `(J2SBlockPCE :ainfos ,@(map j2s-info->list ainfos)
	  ,@(cdr (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    make-counter ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-counter val)
   (make-cell val))

;*---------------------------------------------------------------------*/
;*    inc! ...                                                         */
;*---------------------------------------------------------------------*/
(define (inc! count)
   (let ((c (cell-ref count)))
      (cell-set! count (+fx c 1))
      c))

;*---------------------------------------------------------------------*/
;*    get ...                                                          */
;*---------------------------------------------------------------------*/
(define (get count)
   (cell-ref count))

;*---------------------------------------------------------------------*/
;*    stop ...                                                         */
;*---------------------------------------------------------------------*/
(define stop '(stop))

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SNode)
   stop)

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SLiteral ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SLiteral)
   '())

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SArray ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SArray)
   (with-access::J2SArray this (exprs)
      (let ((ainfos (map get-accesses* exprs)))
	 (if (any (lambda (ai) (eq? ai stop)) ainfos)
	     stop
	     (append-map ainfos)))))
   
;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SAccess ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SAccess)
   (with-access::J2SAccess this (obj field %info)
      (let ((afd (get-accesses* field)))
	 (cond
	    ((eq? afd stop)
	     stop)
	    ((and (isa? obj J2SRef) (isa? field J2SString))
	     (with-access::J2SRef obj (decl)
		(with-access::J2SString field (val)
		   (let ((ai (instantiate::AInfo
				(access this)
				(field val))))
		      (cons ai afd)))))
	    (else
	     stop)))))

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SRef ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SRef)
   '())

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SDecl ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SDecl)
   '())

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SDeclInit ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SDeclInit)
   (with-access::J2SDeclInit this (val)
      (get-accesses* val)))

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SAssig ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SAssig)
   (with-access::J2SAssig this (lhs rhs)
      (let ((alhs (get-accesses* lhs))
	    (arhs (get-accesses* rhs)))
	 (if (or (eq? alhs stop) (eq? arhs stop))
	     stop
	     (append alhs arhs)))))

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SBinary ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SBinary)
   (with-access::J2SBinary this (lhs rhs)
      (let ((alhs (get-accesses* lhs))
	    (arhs (get-accesses* rhs)))
	 (if (or (eq? alhs stop) (eq? arhs stop))
	     stop
	     (append alhs arhs)))))

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SUnary ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SUnary)
   (with-access::J2SUnary this (expr)
      (get-accesses* expr)))

;*---------------------------------------------------------------------*/
;*    get-accesses* ::J2SStmtExpr ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (get-accesses* this::J2SStmtExpr)
   (with-access::J2SStmtExpr this (expr)
      (get-accesses* expr)))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SProgram)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SFun)
   (with-access::J2SFun this (optimize body)
      (when optimize
	 (set! body (insert-pce! body)))
      this))

;*---------------------------------------------------------------------*/
;*    insert-pce-nodes! ...                                            */
;*---------------------------------------------------------------------*/
(define (insert-pce-nodes!::pair-nil nodes::pair-nil)
   
   (define (skip-stmt lst::pair-nil)
      (let loop ((lst lst)
		 (acc '()))
	 (if (null? lst)
	     (values (reverse! acc) '())
	     (let ((x (get-accesses* (car lst))))
		(if (or (eq? x stop) (null? x))
		    (loop (cdr lst) (cons (car lst) acc))
		    (values (reverse! acc) lst))))))
   
   (define (collect-stmt lst::pair-nil)
      (let loop ((lst lst)
		 (acc '())
		 (xs '()))
	 (if (null? lst)
	     (values (reverse! acc) xs '())
	     (let ((x (get-accesses* (car lst))))
		(if (or (eq? x stop) (null? x))
		    (values (reverse! acc) xs lst)
		    (loop (cdr lst) (cons (car lst) acc) (append x xs)))))))
   
   (define (next-block nodes)
      (insert-pce!
	 (instantiate::J2SBlock
	    (loc (node-loc (car nodes)))
	    (endloc (node-endloc (car (last-pair nodes))))
	    (nodes nodes))))
   
   (define (pce-block nodes xs)
      (if (>=fx (length xs) pce-duplicate-threshold)
	  (instantiate::J2SBlockPCE
	     (endloc (node-endloc (car (last-pair nodes))))
	     (loc (node-loc (car nodes)))
	     (ainfos xs)
	     (nodes nodes))
	  (instantiate::J2SBlock
	     (endloc (node-endloc (car (last-pair nodes))))
	     (loc (node-loc (car nodes)))
	     (nodes (map! insert-pce! nodes)))))
   
   (multiple-value-bind (skip next)
      (skip-stmt nodes)
      (if (null? next)
	  (map! insert-pce! nodes)
	  (multiple-value-bind (collect xs next)
	     (collect-stmt next)
	     (cond
		((and (null? skip) (null? next))
		 (list (pce-block collect xs)))
		((null? skip)
		 (list (pce-block collect xs) (next-block next)))
		((null? next)
		 (append skip (list (pce-block collect xs))))
		(else
		 (append skip
		    (list (pce-block collect xs) (next-block next)))))))))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SBlock ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SBlock)
   (with-access::J2SBlock this (nodes)
      (let ((newnodes (insert-pce-nodes! nodes)))
	 (if (and (pair? newnodes)
		  (null? (cdr newnodes))
		  (isa? (car newnodes) J2SBlock))
	     (car newnodes)
	     (begin
		(set! nodes newnodes)
		this)))))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SLetBlock ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SLetBlock)

   (define (skip-decl lst::pair-nil)
      (let loop ((lst lst)
		 (acc '()))
	 (if (null? lst)
	     (values (reverse! acc) '())
	     (let ((x (get-accesses* (car lst))))
		(if (or (eq? x stop) (null? x))
		    (loop (cdr lst) (cons (car lst) acc))
		    (values (reverse! acc) lst))))))

   (define (collect-decl lst::pair-nil)
      (let loop ((lst lst)
		 (acc '())
		 (xs '()))
	 (if (null? lst)
	     (values (reverse! acc) xs '())
	     (let ((x (get-accesses* (car lst))))
		(if (or (eq? x stop) (null? x))
		    (values (reverse! acc) xs lst)
		    (loop (cdr lst) (cons (car lst) acc) (append x xs)))))))

   (define (collect-stmt lst::pair-nil xs)

      (define (xmember? x xs)
	 (with-access::AInfo x (access (xfield field))
	    (with-access::J2SAccess access (obj)
	       (with-access::J2SRef obj ((xdecl decl))
		  (find (lambda (ai)
			   (with-access::AInfo ai (access field)
			      (with-access::J2SAccess access (obj)
				 (with-access::J2SRef obj (decl)
				    (and (eq? decl xdecl)
					 (string=? xfield field))))))
		     xs)))))
			   
      (let loop ((lst lst)
		 (acc '()))
	 (if (null? lst)
	     (values (reverse! acc) '())
	     (let ((ax (get-accesses* (car lst))))
		(if (or (eq? ax stop) (null? ax)
			(not (every (lambda (x) (xmember? x xs)) ax)))
		    (values (reverse! acc) lst)
		    (loop (cdr lst) (cons (car lst) acc)))))))

   (define (pce-letblock collect xs nodes)
      (let ((block (instantiate::J2SLetBlock
		      (rec #f)
		      (loc (node-loc (car collect)))
		      (endloc (node-endloc (car (last-pair nodes))))
		      (decls collect)
		      (nodes nodes))))
	 (if (>=fx (length xs) pce-duplicate-threshold)
	     (instantiate::J2SBlockPCE
		(endloc (node-endloc (car (last-pair nodes))))
		(loc (node-loc (car nodes)))
		(ainfos xs)
		(nodes (list block)))
	     block)))

   (define (pce-letblock-nonext collect xs nodes)
      (let ((endloc (node-endloc (car (last-pair nodes)))))
	 (if (>=fx (length xs) pce-duplicate-threshold)
	     (multiple-value-bind (ncollect next)
		(collect-stmt nodes xs)
		(cond
		   ((null? next)
		    (let ((block (instantiate::J2SLetBlock
				    (rec #f)
				    (loc (node-loc (car collect)))
				    (endloc endloc)
				    (decls collect)
				    (nodes ncollect))))
		       (instantiate::J2SBlockPCE
			  (endloc endloc)
			  (loc (node-loc (car nodes)))
			  (ainfos xs)
			  (nodes (list block)))))
		   (else
		    (let* ((block (instantiate::J2SBlock
				     (loc (node-loc (car next)))
				     (endloc endloc)
				     (nodes next)))
			   (letblock (instantiate::J2SLetBlock
					(rec #f)
					(loc (node-loc (car collect)))
					(endloc endloc)
					(decls collect)
					(nodes (append ncollect
						  (list (insert-pce! block)))))))
		       (instantiate::J2SBlockPCE
			  (endloc endloc)
			  (loc (node-loc (car nodes)))
			  (ainfos xs)
			  (nodes (list letblock)))))))
	     (let ((block (instantiate::J2SBlock
			     (loc (node-loc (car nodes)))
			     (endloc endloc)
			     (nodes nodes))))
		(instantiate::J2SLetBlock
		   (rec #f)
		   (loc (node-loc (car collect)))
		   (endloc endloc)
		   (decls collect)
		   (nodes (list (insert-pce! block))))))))

   (define (next-letblock next nodes)
      (insert-pce!
	 (instantiate::J2SLetBlock
	    (rec #f)
	    (loc (node-loc (car next)))
	    (endloc (node-endloc (car (last-pair nodes))))
	    (decls next)
	    (nodes nodes))))
   
   (define (insert-norec-pce! this::J2SLetBlock)
      (with-access::J2SLetBlock this (decls nodes endloc)
	 (multiple-value-bind (skip next)
	    (skip-decl decls)
	    (if (null? next)
		(insert-rec-pce! this)
		(multiple-value-bind (collect xs next)
		   (collect-decl next)
		   (cond
		      ((and (null? skip) (null? next))
		       (pce-letblock-nonext collect xs nodes))
		      ((null? next)
		       (duplicate::J2SLetBlock this
			  (decls skip)
			  (nodes (list (pce-letblock-nonext collect xs nodes)))))
		      (else
		       (let ((nodes (list (next-letblock next nodes))))
			  (duplicate::J2SLetBlock this
			     (decls skip)
			     (nodes (list (pce-letblock collect xs nodes))))))))))))
      
   (define (insert-rec-pce! this::J2SLetBlock)
      (with-access::J2SLetBlock this (decls nodes)
	 (set! decls (map! insert-pce! decls))
	 (set! nodes (insert-pce-nodes! nodes))
	 this))

   (with-access::J2SLetBlock this (rec)
      (if rec
	  (insert-rec-pce! this)
	  (insert-norec-pce! this))))
      
;*---------------------------------------------------------------------*/
;*    expand-pce! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (expand-pce! this::J2SNode counter expandp::bool)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    expand-pce! ::J2SBlockPCE ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (expand-pce! this::J2SBlockPCE counter expandp::bool)
   (if expandp
       (with-access::J2SBlockPCE this (ainfos loc endloc)
	  (let ((ncaches (get-caches ainfos counter))
		(dupblock (j2s-alpha (duplicate::J2SBlock this) '() '())))
	     (J2SIf (expand-pce-pretest ncaches loc)
		(expand-pce!
		   (update-cache! (duplicate::J2SBlock this) ncaches ainfos)
		   counter #t)
		(J2SBlock
		   (expand-pce! dupblock counter #f)
		   (J2SIf (expand-pce-posttest ncaches loc)
		      (J2SStmtExpr (J2SUndefined))
		      ;;(enable-pce-cache ncaches loc)
		      (disable-pce-cache ncaches loc))))))
       (duplicate::J2SBlock this)))

;*---------------------------------------------------------------------*/
;*    expand-pce-pretest ...                                           */
;*---------------------------------------------------------------------*/
(define (expand-pce-pretest ncaches loc)
   
   (define (pretest entry)
      (with-access::J2SAccess (car entry) (obj)
	 (with-access::J2SRef obj (decl type)
	    (let ((tmp (gensym 'tmp)))
	       (J2SPragma/bindings 'bool
		  (list tmp) (list (J2SRef decl))
		  (if (eq? type 'object)
		      `(with-access::JsObject ,tmp (cmap)
			  (eq? cmap (js-pcache-imap (js-pcache-ref %pcache ,(cdr entry)))))
		      `(and (js-object? ,tmp)
			    (with-access::JsObject ,tmp (cmap)
			       (eq? cmap (js-pcache-imap (js-pcache-ref %pcache ,(cdr entry))))))))))))
	 
   (let loop ((ncaches ncaches))
      (if (null? (cdr ncaches))
	  (pretest (car ncaches))
	  (J2SCond/type 'bool (pretest (car ncaches))
	     (loop (cdr ncaches))
	     (J2SBool #f)))))

;*---------------------------------------------------------------------*/
;*    expand-pce-posttest ...                                          */
;*---------------------------------------------------------------------*/
(define (expand-pce-posttest ncaches loc)

   (define (test/w-object tmp entry cache)
      `(with-access::JsObject ,tmp (cmap)
	  (when (eq? cmap (js-pcache-imap (js-pcache-ref %pcache ,cache)))
	     (with-access::JsPropertyCache (js-pcache-ref %pcache ,(cdr entry)) (imap)
		(cond
		   ((not cmap) #f)
		   ((eq? imap #t) (set! imap cmap) #t)
		   ((eq? imap cmap) #t)
		   (else #f))))))
   
   (define (pretext entry)
      (with-access::J2SAccess (car entry) (obj cache)
	 (with-access::J2SRef obj (decl type)
	    (let ((tmp (gensym 'tmp)))
	       (J2SPragma/bindings 'bool
		  (list tmp) (list (J2SRef decl))
		  (if (eq? type 'object)
		      (test/w-object tmp entry cache)
		      `(and (js-object? ,tmp)
			    ,(test/w-object tmp entry cache))))))))
	 
   (let loop ((ncaches ncaches))
      (if (null? (cdr ncaches))
	  (pretext (car ncaches))
	  (J2SCond/type 'bool (pretext (car ncaches))
	     (loop (cdr ncaches))
	     (J2SBool #f)))))
   
;*---------------------------------------------------------------------*/
;*    enable-pce-cache ...                                             */
;*---------------------------------------------------------------------*/
(define (enable-pce-cache ncaches loc)
   (J2SSeq*
      (map (lambda (entry)
	      (with-access::J2SAccess (car entry) (obj)
		 (with-access::J2SRef obj (decl loc)
		    (let ((tmp (gensym 'tmp)))
		       (J2SStmtExpr
			  (J2SPragma/bindings 'undefined
			     (list tmp) (list (J2SRef decl))
			     `(with-access::JsPropertyCache (js-pcache-ref %pcache ,(cdr entry))
				    (imap)
				 (with-access::JsObject ,tmp (cmap)
				    (set! imap cmap)))))))))
	 ncaches)))

;*---------------------------------------------------------------------*/
;*    disable-pce-cache ...                                            */
;*---------------------------------------------------------------------*/
(define (disable-pce-cache ncaches loc)
   (J2SSeq*
      (map (lambda (entry)
	      (J2SPragma
		 `(with-access::JsPropertyCache (js-pcache-ref %pcache ,(cdr entry)) (imap)
		    (set! imap #t))))
	 ncaches)))

;*---------------------------------------------------------------------*/
;*    get-caches ...                                                   */
;*---------------------------------------------------------------------*/
(define (get-caches %info counter)
   (let loop ((info %info)
	      (caches '()))
      (if (null? info)
	  caches
	  (with-access::AInfo (car info) (access)
	     (with-access::J2SAccess access (obj)
		(with-access::J2SRef obj (decl)
		   (if (find (lambda (c)
				(with-access::J2SAccess (car c) (obj)
				   (with-access::J2SRef obj ((d decl))
				      (eq? d decl))))
			  caches)
		       (loop (cdr info) caches)
		       (loop (cdr info)
			  (cons (cons access (inc! counter)) caches)))))))))

;*---------------------------------------------------------------------*/
;*    update-cache! ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (update-cache! this::J2SNode ncaches %info)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    update-cache! ::J2SAccess ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (update-cache! this::J2SAccess ncaches %info)
   (if (find (lambda (ai)
		(with-access::AInfo ai (access)
		   (eq? access this)))
	  %info)
       (with-access::J2SAccess this (cspecs obj)
	  (with-access::J2SRef obj (type)
	     (set! type 'object))
	  (set! cspecs '(imap-incache))
	  (call-default-walker))
       (call-default-walker)))
