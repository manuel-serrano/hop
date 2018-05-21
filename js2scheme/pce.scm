;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/pce.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 15 09:53:30 2018                          */
;*    Last change :  Mon May 21 08:27:09 2018 (serrano)                */
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
	   (class AInfo access::J2SAccess field::bstring))
	      
   (export j2s-pce-stage))

;*---------------------------------------------------------------------*/
;*    j2s-pce-stage ...                                                */
;*---------------------------------------------------------------------*/
(define j2s-pce-stage
   (instantiate::J2SStageProc
      (name "pce")
      (comment "Property Cache Elimination optimization")
      (proc (lambda (n args) (j2s-pce! n args)))
      (optional :optim-pce)))

;*---------------------------------------------------------------------*/
;*    j2s-pce! ...                                                     */
;*---------------------------------------------------------------------*/
(define (j2s-pce! this args)
   (if (isa? this J2SProgram)
       (let ((nodes (mark-accesses! this)))
	  (when (>=fx (bigloo-debug) 1)
	     (let* ((tmp (config-get args :tmp "/tmp"))
		    (f (make-file-path tmp
			 (string-replace "pce-" (file-separator) #\_))))
		(call-with-output-file f
		   (lambda (p)
		      (fprint p ";; -*-bee-*-")
		      (pp (j2s->list nodes) p)))))
	  (with-access::J2SProgram this (pcache-size)
	     (let ((counter (make-counter pcache-size)))
		(let ((res (expand-pce! this counter #t)))
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
(define-generic (mark-accesses!::pair-nil this::obj)
   (if (pair? this)
       (uptostop (append-map mark-accesses! this))
       '()))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SNode ...                                     */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SNode)
   (with-access::J2SNode this (%info)
      (let ((fields (class-all-fields (object-class this))))
	 (let loop ((i (-fx (vector-length fields) 1))
		    (info '()))
	    (if (=fx i -1)
		(begin
		   (set! %info (uptostop info))
		   %info)
		(let* ((f (vector-ref fields i))
		       (ast (class-field-info f)))
		   (if (and (pair? ast) (member "ast" ast))
		       (let ((a (mark-accesses! ((class-field-accessor f) this))))
			  (loop (-fx i 1) (append info a)))
		       (loop (-fx i 1) info))))))))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SAccess)
   (with-access::J2SAccess this (obj field %info)
      (let ((aobj (mark-accesses! obj))
	    (afd (mark-accesses! field)))
	 (if (or (memq 'stop aobj) (memq 'stop afd))
	     '(stop)
	     (if (and (isa? obj J2SRef) (isa? field J2SString))
		 (with-access::J2SRef obj (decl)
		    (with-access::J2SString field (val)
		       (let ((ai (instantiate::AInfo
				    (access this)
				    (field val))))
			  (set! %info (list ai)))
		       %info))
		 '())))))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SAssigOp ...                                  */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SAssigOp)
   (with-access::J2SAssigOp this (lhs rhs)
      (append (mark-accesses! lhs) (call-next-method))))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SCall)
   (with-access::J2SCall this (%info fun args thisarg)
      (mark-accesses! fun)
      (mark-accesses! args)
      (mark-accesses! thisarg)
      (set! %info '(stop))
      %info))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SLetBlock ...                                 */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SLetBlock)

   (define (node-%info n)
      (with-access::J2SNode n (%info) %info))
   
   (with-access::J2SLetBlock this (loc endloc decls nodes %info)
      (let ((asd (mark-accesses! decls))
	    (asn (mark-accesses! nodes)))
	 (set! %info 
	    (uptostop
	       (filter (lambda (ai)
			  (or (eq? ai 'stop)
			      (when (isa? ai AInfo)
				 (with-access::AInfo ai (access)
				    (with-access::J2SAccess access (obj)
				       (with-access::J2SRef obj (decl)
					  (not (memq decl decls))))))))
		  (append asd asn))))
	 (when (>=fx (-fx (length asn) 0) pce-duplicate-threshold)
	    (let loop ((n nodes)
		       (acc '())
		       (before '()))
	       (cond
		  ((null? n)
		   (if (pair? before)
		       (set! nodes
			  (reverse!
			     (cons
				(instantiate::J2SBlockPCE
				   (loc loc)
				   (endloc endloc)
				   (%info (append-map node-%info before))
				   (nodes (reverse! before)))
				acc)))
		       (reverse! acc)))
		  ((with-access::J2SNode (car n) (%info) (memq 'stop %info))
		   (if (pair? before)
		       (loop (cdr n)
			  (cons*
			     (car n)
			     (instantiate::J2SBlockPCE
				(loc loc)
				(endloc (with-access::J2SNode (car n) (loc) loc))
				(%info (append-map node-%info before))
				(nodes (reverse! before)))
			     acc)
			  '())
		       (loop (cdr n) (cons (car n) acc) '())))
		  (else
		   (loop (cdr n) acc (cons (car n) before))))))
	 %info)))

;*---------------------------------------------------------------------*/
;*    mark-accesses! ::J2SFor ...                                      */
;*---------------------------------------------------------------------*/
(define-method (mark-accesses! this::J2SFor)
   (with-access::J2SFor this (body %info)
      (call-next-method)
      (when (memq 'stop %info)
	 (set! %info '(stop)))
      %info))

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

   
