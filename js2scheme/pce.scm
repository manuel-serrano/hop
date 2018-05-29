;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/pce.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 15 09:53:30 2018                          */
;*    Last change :  Tue May 29 07:14:25 2018 (serrano)                */
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

   (static (class J2SBlockPCE::J2SBlock)
	   (class AInfo
	      access::J2SAccess
	      field::bstring
	      (depth::long (default -1))))
	      
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
       (begin
	  (mark-accesses! this (make-counter 0))
	  (let ((nthis (insert-pce! this 0)))
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
		      res)))))
       this))

;*---------------------------------------------------------------------*/
;*    pce-duplicate-threshold ...                                      */
;*---------------------------------------------------------------------*/
(define pce-duplicate-threshold 3)

;*---------------------------------------------------------------------*/
;*    j2s-info->list ::AInfo ...                                       */
;*---------------------------------------------------------------------*/
(define-method (j2s-info->list this::AInfo)
   (with-access::AInfo this (access field depth)
      (with-access::J2SAccess access (obj)
	 (with-access::J2SRef obj (decl)
	    (with-access::J2SDecl decl (id key)
	       (format "~a.~a:~a[~a]" id key field depth))))))

;*---------------------------------------------------------------------*/
;*    ainfo-depth ...                                                  */
;*---------------------------------------------------------------------*/
(define (ainfo-depth ai::AInfo)
   (with-access::AInfo ai (depth)
      depth))
   
;*---------------------------------------------------------------------*/
;*    ainfo-filter ...                                                 */
;*---------------------------------------------------------------------*/
(define (ainfo-filter l::pair-nil d::long)
   (filter (lambda (ai) (=fx (ainfo-depth ai) d)) l))

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
;*    uptostop ...                                                     */
;*    -------------------------------------------------------------    */
;*    Collect all the first elements up to the stop mark.              */
;*---------------------------------------------------------------------*/
(define (uptostop::pair-nil lst::pair-nil)
   (let loop ((l lst))
      (cond
	 ((null? l) '())
	 ((eq? (car l) 'stop) (list 'stop))
	 (else (cons (car l) (loop (cdr l)))))))
   
;*---------------------------------------------------------------------*/
;*    mark-accesses! ::obj ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (mark-accesses!::pair-nil this::obj depth::cell)
   (if (pair? this)
       (append-map (lambda (n) (mark-accesses! n depth)) this)
       '()))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SNode depth)
   (with-access::J2SNode this (%info)
      (let ((fields (class-all-fields (object-class this)))
	    (d (get depth)))
	 (let loop ((i (-fx (vector-length fields) 1))
		    (info '()))
	    (if (=fx i -1)
		(begin
		   (set! %info (ainfo-filter info d))
		   %info)
		(let* ((f (vector-ref fields i))
		       (ast (class-field-info f)))
		   (if (and (pair? ast) (member "ast" ast))
		       (let ((a (mark-accesses!
				   ((class-field-accessor f) this)
				   depth)))
			  (loop (-fx i 1) (append info a)))
		       (loop (-fx i 1) info))))))))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SFun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SFun depth)
   (with-access::J2SFun this (%info body optimize)
      (when optimize
	 (mark-accesses! body (make-counter 0)))
      (set! %info '())
      '()))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SAccess depth)
   (with-access::J2SAccess this (obj field %info)
      (let* ((d (get depth))
	     (aobj (mark-accesses! obj depth))
	     (afd (mark-accesses! field depth)))
	 (if (not (=fx d (get depth)))
	     '()
	     (if (and (isa? obj J2SRef) (isa? field J2SString))
		 (with-access::J2SRef obj (decl)
		    (with-access::J2SString field (val)
		       (let ((ai (instantiate::AInfo
				    (depth d)
				    (access this)
				    (field val))))
			  (set! %info (list ai)))
		       %info))
		 '())))))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SAssigOp ...                                  */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SAssigOp depth)
   (with-access::J2SAssigOp this (lhs rhs)
      (append (mark-accesses! lhs depth) (call-next-method))))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SCall depth)
   (with-access::J2SCall this (%info fun args thisarg)
      (mark-accesses! fun depth)
      (mark-accesses! args depth)
      (mark-accesses! thisarg depth)
      (inc! depth)
      (set! %info '())
      '()))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SLetBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SLetBlock depth)
   (with-access::J2SLetBlock this (loc endloc decls nodes %info)
      (let* ((d (get depth))
	     (asd (mark-accesses! decls depth))
	     (asn (mark-accesses! nodes depth)))
	 (set! %info (ainfo-filter (append asd asn) d))
	 %info)))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SFor ...                                      */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SFor depth)
   (with-access::J2SFor this (body %info)
      (let ((d (get depth)))
	 (call-next-method)
	 (unless (=fx d (get depth))
	    (set! %info '()))
	 %info)))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SNode depth::long)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SProgram ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SProgram depth)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SFun ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SFun depth)
   (with-access::J2SFun this (optimize)
      (if optimize
	  (call-default-walker)
	  this)))

;*---------------------------------------------------------------------*/
;*    insert-pce-block! ...                                            */
;*---------------------------------------------------------------------*/
(define (insert-pce-block! this::J2SBlock depth)
   
   (define (skip-stmt lst::pair-nil depth)
      (let loop ((lst lst)
		 (acc '()))
	 (cond
	    ((null? lst)
	     (values (reverse! acc) '()))
	    ((or (null? (node-%info (car lst)))
		 (every (lambda (ai) (<fx (ainfo-depth ai) depth))
		    (node-%info (car lst))))
	     (loop (cdr lst) (cons (car lst) acc)))
	    (else
	     (values (reverse! acc) lst)))))
   
   (define (collect-stmt lst depth)
      (let loop ((lst lst)
		 (acc '()))
	 (cond
	    ((null? lst)
	     (values (reverse! acc) '()))
	    ((or (null? (node-%info (car lst)))
		 (every (lambda (ai) (<fx (ainfo-depth ai) depth))
		    (node-%info (car lst))))
	     (multiple-value-bind (skip next)
		(skip-stmt lst depth)
		(cond
		   ((null? next)
		    (values (reverse! acc) skip))
		   ((every (lambda (ai) (=fx (ainfo-depth ai) depth))
		       (node-%info (car next)))
		    (loop (cdr lst) (cons (car lst) acc)))
		   (else
		    (values (reverse! acc) lst)))))
	    ((every (lambda (ai) (=fx (ainfo-depth ai) depth))
		(node-%info (car lst)))
	     (loop (cdr lst) (cons (car lst) acc)))
	    (else
	     (values (reverse! acc) lst)))))
   
   (define (next-block nodes depth)
      (insert-pce-block!
	 (instantiate::J2SBlock
	    (loc (node-loc (car nodes)))
	    (endloc (node-endloc (car (last-pair nodes))))
	    (%info (ainfo-filter (append-map node-%info nodes) (+fx depth 1)))
	    (nodes nodes))
	 (+fx depth 1)))
   
   (define (pce-block nodes depth)
      (let ((info (ainfo-filter (append-map node-%info nodes) depth)))
	 (if (>=fx (length info) pce-duplicate-threshold)
	     (instantiate::J2SBlockPCE
		(endloc (node-endloc (car (last-pair nodes))))
		(loc (node-loc (car nodes)))
		(%info info)
		(nodes nodes))
	     (instantiate::J2SBlock
		(endloc (node-endloc (car (last-pair nodes))))
		(loc (node-loc (car nodes)))
		(nodes nodes)))))
   
   (with-access::J2SBlock this (nodes)
      (multiple-value-bind (skip next)
	 (skip-stmt nodes depth)
	 (cond
	    ((null? next)
	     this)
	    ((every (lambda (ai) (=fx (ainfo-depth ai) depth))
		(node-%info (car next)))
	     (multiple-value-bind (collect next)
		(collect-stmt next depth)
		(cond
		   ((and (null? skip) (null? next))
		    (pce-block collect depth))
		   ((null? skip)
		    (instantiate::J2SBlock
		       (loc (node-loc (car collect)))
		       (endloc (node-endloc (car (last-pair next))))
		       (nodes (list (pce-block collect depth)
				 (next-block next depth)))))
		   ((null? next)
		    (instantiate::J2SBlock
		       (loc (node-loc (car skip)))
		       (endloc (node-endloc (car (last-pair collect))))
		       (nodes (append skip (list (pce-block collect depth))))))
		   (else
		    (instantiate::J2SBlock
		       (loc (node-loc (car skip)))
		       (endloc (node-endloc (car (last-pair next))))
		       (nodes (append skip
				 (list (pce-block collect depth)
				    (next-block skip depth)))))))))
	    ((null? skip)
	     (next-block next depth))
	    (else
	     (instantiate::J2SBlock
		(loc (node-loc (car skip)))
		(endloc (node-loc (car (last-pair next))))
		(nodes (append skip (list (next-block next depth))))))))))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SBlock ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (insert-pce! this::J2SBlock depth)
   (insert-pce-block! this depth))

;*---------------------------------------------------------------------*/
;*    insert-pce! ::J2SLetBlock ...                                    */
;*---------------------------------------------------------------------*/
;* (define-walk-method (insert-pce! this::J2SLetBlock)                 */
;*                                                                     */
;*    (define (node-%info n)                                           */
;*       (with-access::J2SNode n (%info) %info))                       */
;*                                                                     */
;*    (define (node-loc n)                                             */
;*       (with-access::J2SNode n (loc) loc))                           */
;*                                                                     */
;*    (define (node-endloc n)                                          */
;*       (if (isa? n J2SBlock)                                         */
;* 	  (with-access::J2SBlock n (endloc) endloc)                    */
;* 	  (with-access::J2SNode n (loc) loc)))                         */
;*                                                                     */
;*    (define (make-block-pce nodes)                                   */
;*       (let ((%info (append-map node-%info nodes))                   */
;* 	    (endloc (node-endloc (car nodes)))                         */
;* 	    (nodes (reverse! nodes)))                                  */
;* 	 (if (pair? %info)                                             */
;* 	     (instantiate::J2SBlockPCE                                 */
;* 		(loc (node-loc (car nodes)))                           */
;* 		(endloc endloc)                                        */
;* 		(%info %info)                                          */
;* 		(nodes nodes))                                         */
;* 	     (instantiate::J2SBlock                                    */
;* 		(loc (node-loc (car nodes)))                           */
;* 		(endloc endloc)                                        */
;* 		(nodes nodes)))))                                      */
;*                                                                     */
;*                                                                     */
;*                                                                     */
;*    (define (insert-rec-block! this asn)                             */
;*       (with-access::J2SLetBlock this (decls)                        */
;* 	 (if (any (lambda (d) (memq 'stop (node-%info d))) decls)      */
;* 	     ;; give up optimizing                                     */
;* 	     (insert-pce-body! this asn)                               */
;* 	     (make-block-pce                                           */
;* 		(duplicate::J2SLetBlock this                           */
;* 		   (decls dsplit))))))                                 */
;*                                                                     */
;*    (define (insert-no-rec-block! this asn)                          */
;*       (with-access::J2SLetBlock this (decls)                        */
;* 	 (let loop ((decls decls)                                      */
;* 		    (dshare '())                                       */
;* 		    (dsplit '()))                                      */
;* 	    (cond                                                      */
;* 	       ((null? decls)                                          */
;* 		(if (null? dshare)                                     */
;* 		    (make-block-pce dsplit (list this))                */
;* 		    (duplicate::J2SLetBlock this                       */
;* 		       (decls dshare)                                  */
;* 		       (nodes (list (make-block-pce                    */
;* 				       (duplicate::J2SLetBlock this    */
;* 					  (decls dsplit))))))))        */
;* 	       ((or (null? (node-%info (car decls)))                   */
;* 		    (memq 'stop (node-%info (car decls))))             */
;* 		(loop (cdr decls) (cons (car decls) dshare) dsplit))   */
;* 	       (else                                                   */
;* 		(loop (cdr decls) dshare (cons (car decls) dsplit))))))) */
;*                                                                     */
;*    (with-access::J2SLetBlock this (decls nodes %info rec)           */
;*       (if (<fx (length %info) pce-duplicate-threshold)              */
;* 	  (begin                                                       */
;* 	     (for-each insert-pce! decls)                              */
;* 	     (insert-pce-block! this))                                 */
;* 	  (begin                                                       */
;* 	     (for-each (lambda (d) (insert-pce! d %info)) decls)       */
;* 	     (insert-pce-block! this %info)))))                        */

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
       (with-access::J2SBlockPCE this (%info loc endloc)
	  (let ((ncaches (get-caches %info counter))
		(dupblock (j2s-alpha (duplicate::J2SBlock this) '() '())))
	     (J2SIf (expand-pce-pretest ncaches loc)
		(expand-pce!
		   (update-cache! (duplicate::J2SBlock this) ncaches %info)
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

   
