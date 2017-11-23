;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/propcce.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr  2 19:46:13 2017                          */
;*    Last change :  Thu Nov 23 07:48:00 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Property common caching elimination optimization                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_propcce

   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)
   
   (export j2s-propcce-stage))

;*---------------------------------------------------------------------*/
;*    j2s-propcce-stage ...                                            */
;*---------------------------------------------------------------------*/
(define j2s-propcce-stage
   (instantiate::J2SStageProc
      (name "propcce")
      (comment "Common caching elimination")
      (proc j2s-propcce)
      (optional :optim-cce)))

;*---------------------------------------------------------------------*/
;*    j2s-propcce ...                                                  */
;*---------------------------------------------------------------------*/
(define (j2s-propcce this::obj args)
   (when (isa? this J2SProgram)
      (with-access::J2SProgram this (nodes headers decls)
	 ;; compute the cache scoping
	 (for-each (lambda (n) (j2s-propcache n (make-cursor 0) #f '()))
	    nodes)
	 (for-each (lambda (n) (j2s-propcache n (make-cursor 0) #f '()))
	    decls)
	 ;; precache
	 (for-each (lambda (n) (j2s-precache! n)) nodes)
	 (for-each (lambda (n) (j2s-precache! n)) decls)))
   this)

;*---------------------------------------------------------------------*/
;*    cceinfo ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct cceinfo count)
(define-struct cceanchor nodes)

;*---------------------------------------------------------------------*/
;*    ccecursor ...                                                    */
;*---------------------------------------------------------------------*/
(define-struct ccecursor decl access field count stamp anchor hookp)

;*---------------------------------------------------------------------*/
;*    dump-cursor ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-cursor c)
   (ccecursor (j2s->list (ccecursor-decl c)) (j2s->list (ccecursor-access c))
      (ccecursor-field c) (ccecursor-count c) (ccecursor-stamp c)
      (typeof (ccecursor-anchor c))
      (ccecursor-hookp c)))
		 
;*---------------------------------------------------------------------*/
;*    make-cursor ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-cursor stamp)
   (ccecursor '* #f "" 0 stamp #f #f))

;*---------------------------------------------------------------------*/
;*    duplicate-cursor ...                                             */
;*---------------------------------------------------------------------*/
(define (duplicate-cursor c)
   (ccecursor
      (ccecursor-decl c)
      (ccecursor-access c)
      (ccecursor-field c)
      (ccecursor-count c)
      (ccecursor-stamp c)
      (ccecursor-anchor c)
      #f))

;*---------------------------------------------------------------------*/
;*    cursor-flush! ...                                                */
;*---------------------------------------------------------------------*/
(define (cursor-flush! c::struct)
   (when (and (>fx (ccecursor-count c) 0)
	      (ccecursor-hookp c))
      (with-access::J2SAccess (ccecursor-access c) (%info)
	 (set! %info (cceinfo (ccecursor-count c)))
	 (with-access::J2SNode (ccecursor-anchor c) (%info)
	    (if (cceanchor? %info)
		(cceanchor-nodes-set! %info
		   (cons (ccecursor-access c) (cceanchor-nodes %info)))
		(set! %info (cceanchor (list (ccecursor-access c)))))))))
   
;*---------------------------------------------------------------------*/
;*    cursor-invalidate! ...                                           */
;*---------------------------------------------------------------------*/
(define (cursor-invalidate! cursor)
   (cursor-flush! cursor)
   (ccecursor-decl-set! cursor '*)
   (ccecursor-access-set! cursor #f)
   (ccecursor-field-set! cursor "")
   (ccecursor-count-set! cursor 0))

;*---------------------------------------------------------------------*/
;*    cursor-reset! ...                                                */
;*---------------------------------------------------------------------*/
(define (cursor-reset! cursor decl::J2SDecl access::J2SAccess field::bstring a)
   (cursor-flush! cursor)
   (ccecursor-decl-set! cursor decl)
   (ccecursor-access-set! cursor access)
   (ccecursor-field-set! cursor field)
   (ccecursor-count-set! cursor 0)
   (ccecursor-stamp-set! cursor (+fx 1 (ccecursor-stamp cursor)))
   (ccecursor-anchor-set! cursor a)
   (ccecursor-hookp-set! cursor #t))

;*---------------------------------------------------------------------*/
;*    cursor-inc! ...                                                  */
;*---------------------------------------------------------------------*/
(define (cursor-inc! cursor)
   (ccecursor-count-set! cursor (+fx 1 (ccecursor-count cursor))))

;*---------------------------------------------------------------------*/
;*    cursor-stamp ...                                                 */
;*---------------------------------------------------------------------*/
(define (cursor-stamp cursor)
   (ccecursor-stamp cursor))

;*---------------------------------------------------------------------*/
;*    iscursor? ...                                                    */
;*---------------------------------------------------------------------*/
(define (iscursor? cursor decl field)
   (and (eq? (ccecursor-decl cursor) decl)
	(string=? (ccecursor-field cursor) field)))

;*---------------------------------------------------------------------*/
;*    cursor-equal? ...                                                */
;*---------------------------------------------------------------------*/
(define (cursor-equal? left right)
   (equal? left right))

;*---------------------------------------------------------------------*/
;*    merge ...                                                        */
;*---------------------------------------------------------------------*/
(define (merge props::pair-nil)
   (let loop ((props props)
	      (res '()))
      (cond
	 ((null? props)
	  res)
	 ((eq? (car props) 'invalidate)
	  '(invalidate))
	 ((null? (car props))
	  (loop (cdr props) res))
	 (else
	  (let* ((var (car (car props)))
		 (fields (cdr (car props)))
		 (c (assq var res)))
	     (if (pair? c)
		 (begin
		    (unless (eq? (cdr c) 'invalidate)
		       (for-each (lambda (f)
				    (unless (eq? (cdr c) 'invalidate)
				       (cond
					  ((eq? f 'invalidate)
					   (set-cdr! c 'invalidate))
					  ((not (member f (cdr c)))
					   (set-cdr! c (cons f (cdr c)))))))
			  fields))
		    (loop (cdr props) res))
		 (loop (cdr props) (cons (car props) res))))))))

;*---------------------------------------------------------------------*/
;*    j2s-propcache* ...                                               */
;*---------------------------------------------------------------------*/
(define (j2s-propcache* nodes::pair-nil cursor anchor blacklist)
   (let loop ((nodes nodes)
	      (anchor anchor))
      (when (pair? nodes)
	 (j2s-propcache (car nodes) cursor anchor blacklist)
	 (loop (cdr nodes) (car nodes)))))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SNode cursor anchor blacklist)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SExpr ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SExpr cursor anchor blacklist)
   (cursor-invalidate! cursor))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SRef ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SRef cursor anchor blacklist)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SLiteral ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SLiteral cursor anchor blacklist)
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SLiteral ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SArray cursor anchor blacklist)
   (with-access::J2SArray this (exprs)
      (for-each (lambda (e) *j2s-propcache e cursor anchor blacklist) exprs)))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SFun ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SFun cursor anchor blacklist)
   (with-access::J2SFun this (body)
      (j2s-propcache body (make-cursor 0) #f '())))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SAccess ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SAccess cursor anchor blacklist)
   (with-access::J2SAccess this (obj field cache %info)
      (if (and (isa? obj J2SRef) cache)
	  (with-access::J2SRef obj (decl)
	     (if (isa? field J2SString)
		 (with-access::J2SString field (val)
		    (cond
		       ((iscursor? cursor decl val)
			(set! %info (cceinfo -1))
			(cursor-inc! cursor))
		       ((memq decl blacklist)
			(cursor-invalidate! cursor))
		       (else
			(cursor-reset! cursor decl this val anchor))))
		 (cursor-invalidate! cursor)))
	  (cursor-invalidate! cursor))))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SAssig ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SAssig cursor anchor blacklist)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SParen ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SParen cursor anchor blacklist)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SSequence ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SSequence cursor anchor blacklist)
   (with-access::J2SSequence this (exprs)
      (for-each (lambda (e) (j2s-propcache e cursor anchor blacklist)) exprs)))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SCond ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SCond cursor anchor blacklist)
   (with-access::J2SCond this (test then else)
      (j2s-propcache test cursor anchor blacklist)
      (let ((cursort (duplicate-cursor cursor))
	    (cursore (duplicate-cursor cursor)))
	 (j2s-propcache then cursort anchor blacklist)
	 (j2s-propcache else cursore anchor blacklist)
	 (unless (and (cursor-equal? cursor cursort)
		      (cursor-equal? cursor cursore))
	    (cursor-invalidate! cursor)
	    (cursor-invalidate! cursort)
	    (cursor-invalidate! cursore)))))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SBinary ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SBinary cursor anchor blacklist)
   (with-access::J2SBinary this (lhs rhs)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SStmt ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SStmt cursor anchor blacklist)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SSeq ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SSeq cursor anchor blacklist)
   (with-access::J2SSeq this (nodes)
      (let ((stamp (cursor-stamp cursor)))
	 (j2s-propcache* nodes cursor this blacklist)
	 (when (>fx (cursor-stamp cursor) stamp)
	    (cursor-invalidate! cursor)))))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SLetBlock ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SLetBlock cursor anchor blacklist)
   (with-access::J2SLetBlock this (decls nodes loc)
      (let ((stamp (cursor-stamp cursor))
	    (blist (append decls blacklist)))
	 (for-each (lambda (d) (j2s-propcache d cursor anchor blist)) decls)
	 (let ((nanchor (J2SSeq* nodes)))
	    (j2s-propcache nanchor cursor nanchor blacklist)
	    (set! nodes (list nanchor)))
	 (when (>fx (cursor-stamp cursor) stamp)
	    (cursor-invalidate! cursor)))))

;*---------------------------------------------------------------------*/
;*    j2s-propcache ::J2SIf ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-propcache this::J2SIf cursor anchor blacklist)
   (with-access::J2SIf this (%info test then else)
      (j2s-propcache test cursor anchor blacklist)
      (let ((cursort (duplicate-cursor cursor))
	    (cursore (duplicate-cursor cursor)))
	 (j2s-propcache then cursort anchor blacklist)
	 (j2s-propcache else cursore anchor blacklist)
	 (unless (and (cursor-equal? cursor cursort)
		      (cursor-equal? cursor cursore))
	    (let ((decl (ccecursor-decl cursor))
		  (declt (ccecursor-decl cursort))
		  (decle (ccecursor-decl cursore)))
	       (cursor-invalidate! cursor)
	       (when (and (not (eq? decl declt)) (isa? declt J2SDecl))
		  (cursor-invalidate! cursort))
	       (when (and (not (eq? decl decle)) (isa? decle J2SDecl))
		  (cursor-invalidate! cursore)))))))
      
;*---------------------------------------------------------------------*/
;*    j2s-precache! ::J2SNode ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-precache! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-precache! ::J2SStmt ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-precache! this::J2SStmt)
   (with-access::J2SStmt this (%info loc)
      (if (and (cceanchor? %info) (pair? (cceanchor-nodes %info)))
	  (let ((accesses (cceanchor-nodes %info)))
	     (tprint "precaching..." (length accesses))
	     (let ((pc (J2SPrecache accesses
			  (j2s-cachelevel1! (j2s-alpha this '() '()))
			  (J2SMeta 0 0 (j2s-uncache! this)))))
		(if (isa? this J2SBlock)
		    (J2SBlock/w-endloc pc)
		    pc)))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    j2s-cachelevel1! ::J2SNode ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cachelevel1! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-cachelevel1! ::J2SAccess ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-cachelevel1! this::J2SAccess)
   (with-access::J2SAccess this (%info clevel obj)
      (when (cceinfo? %info)
	 (tprint "ACCESS.. " (j2s->list this))
	 (with-access::J2SRef obj (type)
	    (set! type 'object))
	 (set! %info #f)
	 (set! clevel 1)))
   this)

;*---------------------------------------------------------------------*/
;*    j2s-uncache! ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-uncache! this::J2SNode)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    j2s-uncache! ::J2SAccess ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (j2s-uncache! this::J2SAccess)
   (with-access::J2SAccess this (%info clevel obj)
      (set! clevel 2))
   this)
