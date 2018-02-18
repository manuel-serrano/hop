;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/js2scheme/clevel.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr  2 19:46:13 2017                          */
;*    Last change :  Sun Feb 18 06:19:52 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Annotate property accesses with cache level information          */
;*    This analysis scans the AST to find property assignments and     */
;*    property declarations. In then separate properties in several    */
;*    kinds:                                                           */
;*       - Those that are known to be always defined as property       */
;*         values.                                                     */
;*       - Those that are known to be defined as accessor              */
;*         properties.                                                 */
;*       - Those that are known to be defined as prototype values.     */
;*       - Those that are likeley to be polymorphic.                   */
;*    It then set the accessor cache levels accordingly.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_clevel

   (library web)
   
   (include "ast.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)
   
   (export j2s-clevel-stage))

;*---------------------------------------------------------------------*/
;*    j2s-clevel-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-clevel-stage
   (instantiate::J2SStageProc
      (name "clevel")
      (comment "Cache level annotation optimization")
      (proc j2s-clevel)
      (optional :optim-clevel)))

;*---------------------------------------------------------------------*/
;*    j2s-clevel ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-clevel this::obj args)
   (when (isa? this J2SProgram)
      (let ((log (config-get args :profile-log #f)))
	 (if (string? log)
	     (cache-profile-log this log args)
	     (with-access::J2SProgram this (nodes headers decls)
		(let ((ptable (create-hashtable)))
		   (propcollect* decls ptable)
		   (propcollect* nodes ptable)
		   (propclevel* decls ptable)
		   (propclevel* nodes ptable))))))
   this)

;*---------------------------------------------------------------------*/
;*    cache-profile-log ...                                            */
;*---------------------------------------------------------------------*/
(define (cache-profile-log this::J2SProgram logfile conf)

   (define (get key lst)
      (let ((c (assq key lst)))
	 (when (pair? c) (cdr c))))

   (define (cache-verb . args)
      (when (>=fx (config-get conf :verbose 0) 3)
	 (with-output-to-port (current-error-port)
	    (lambda ()
	       (display "\n      ")
	       (for-each display args)))))

   (cache-verb "loading log file " (string-append "\"" logfile "\""))
   
   (let* ((log (load-profile-log logfile))
	  (srcs (get 'sources log))
	  (file (config-get conf :filename)))
      (when (vector? srcs)
	 (let loop ((i (-fx (vector-length srcs) 1)))
	    (when (>=fx i 0)
	       (let ((filename (get 'filename (vector-ref srcs i))))
		  (if (string=? file filename)
		      (let ((verb (make-cell 0))
			    (logtable (get 'caches (vector-ref srcs i))))
			 (profile-clevel this logtable 'get verb conf)
			 (cache-verb "cspecs " (cell-ref verb)))
		      (loop (-fx i 1))))))))
   this)

;*---------------------------------------------------------------------*/
;*    pcache ...                                                       */
;*---------------------------------------------------------------------*/
(define-struct pcache point usage imap cmap pmap amap vtable)

;*---------------------------------------------------------------------*/
;*    load-profile-log ...                                             */
;*---------------------------------------------------------------------*/
(define (load-profile-log logfile)
   
   (define (alist->pcache l)
      (let ((p (pcache -1 '- 0 0 0 0 0)))
	 (for-each (lambda (l)
		      (case (car l)
			 ((point)
			  (pcache-point-set! p (cdr l)))
			 ((usage)
			  (pcache-usage-set! p (string->symbol (cdr l))))
			 ((imap)
			  (pcache-imap-set! p (cdr l)))
			 ((cmap)
			  (pcache-cmap-set! p (cdr l)))
			 ((pmap)
			  (pcache-pmap-set! p (cdr l)))
			 ((amap)
			  (pcache-amap-set! p (cdr l)))
			 ((vtable)
			  (pcache-vtable-set! p (cdr l)))))
	    l)
	 p))
   
   (define (val->caches vals::vector)
      (sort (lambda (x y)
	       (<=fx (pcache-point x) (pcache-point y)))
	 (vector-map! alist->pcache vals)))

   (call-with-input-file logfile
      (lambda (ip)
	 (json-parse ip
	    :array-alloc (lambda () (make-cell '()))
	    :array-set (lambda (a i val)
			  (cell-set! a (cons val (cell-ref a))))
	    :array-return (lambda (a i)
			     (list->vector (reverse! (cell-ref a))))
	    :object-alloc (lambda ()
			     (make-cell '()))
	    :object-set (lambda (o p val)
			   (if (string=? p "caches")
			       (cell-set! o
				  (cons (cons 'caches (val->caches val))
				     (cell-ref o)))
			       (cell-set! o
				  (cons (cons (string->symbol p) val)
				     (cell-ref o)))))
	    :object-return (lambda (o)
			      (reverse! (cell-ref o)))
	    :parse-error (lambda (msg fname loc)
			    (error "json->ast" "Wrong JSON file" msg))))))

;*---------------------------------------------------------------------*/
;*    propinfo ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct propinfo get set value accessor prototype polymorphic)

;*---------------------------------------------------------------------*/
;*    profile-clevel ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-clevel this::J2SNode logtable ctx verb conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    profile-clevel ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-clevel this::J2SAccess logtable ctx verb conf)
   (with-access::J2SAccess this (obj field cspecs loc)
      (let ((entry (logtable-find logtable (loc->point loc) ctx)))
	 (cond
	    (entry
	     (let ((policy (pcache->cspecs entry)))
		(when policy
		   (when (>=fx (config-get conf :verbose 0) 4)
		      (display* "\n        " (loc->string loc)
			 " (" (pcache-usage entry) ") -> " policy))
		   (cell-set! verb (+fx (cell-ref verb) 1))
		   (set! cspecs policy))))
	    ((eq? ctx 'put)
	     (set! cspecs '(pmap cmap+)))
	    (else
	     (set! cspecs '(imap+))))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    profile-clevel ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-clevel this::J2SAssig logtable ctx verb conf)
   (with-access::J2SAssig this (lhs rhs)
      (profile-clevel rhs logtable ctx verb conf)
      (profile-clevel lhs logtable 'put verb conf)))
   
;*---------------------------------------------------------------------*/
;*    profile-clevel ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (profile-clevel this::J2SCall logtable ctx verb conf)
   (with-access::J2SCall this (cspecs fun)
      (when (isa? fun J2SAccess)
	 (with-access::J2SAccess fun (loc)
	    (let ((entry (logtable-find logtable (loc->point loc) 'call)))
	       (when entry
		  (let ((policy (pcache->cspecs entry)))
		     (when policy
			(when (>=fx (config-get conf :verbose 0) 4)
			   (display* "\n        " (loc->string loc)
			      " (" (pcache-usage entry) ") -> " policy))
			(cell-set! verb (+fx (cell-ref verb) 1))
			(set! cspecs policy))))))))
   (call-default-walker))
   
;*---------------------------------------------------------------------*/
;*    loc->point ...                                                   */
;*---------------------------------------------------------------------*/
(define (loc->point loc)
   (match-case loc
      ((at ?fname ?point) point)
      (else -1)))

;*---------------------------------------------------------------------*/
;*    loc->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (loc->string loc)
   (match-case loc
      ((at ?fname ?point) (format "~a:~a" fname point))
      (else "")))

;*---------------------------------------------------------------------*/
;*    logtable-find ...                                                */
;*---------------------------------------------------------------------*/
(define (logtable-find table::vector point #!optional usage)
   (let ((len (vector-length table)))
      (when (>fx len 0)
	 (let loop ((start 0)
		    (end (-fx len 1))
		    (pivot (/fx len 2)))
	    (let* ((pi (vector-ref table pivot))
		   (po (pcache-point pi)))
	       (cond
		  ((=fx po point)
		   (if (or (not usage) (eq? (pcache-usage pi) usage))
		       pi
		       (let loop ((i (+fx pivot 1)))
			  (if (<fx i end)
			      (let ((pi (vector-ref table i)))
				 (when (=fx (pcache-point pi) point)
				    (if (eq? (pcache-usage pi) usage)
					pi
					(loop (+fx i 1)))))))))
		  ((or (=fx start end) (=fx start pivot) (=fx end pivot))
		   #f)
		  ((>fx po point)
		   (loop start pivot
		      (+fx start (/fx (-fx pivot start) 2))))
		  (else
		   (loop (+fx pivot 1) end
		      (+fx pivot (+fx 1 (/fx (-fx end (+fx pivot 1)) 2)))))))))))

;*---------------------------------------------------------------------*/
;*    pcache->cspecs ...                                               */
;*---------------------------------------------------------------------*/
(define (pcache->cspecs pc)
   (cond
      ((and (> (pcache-imap pc) 0)
	    (= (pcache-cmap pc) 0)
	    (= (pcache-pmap pc) 0)
	    (= (pcache-amap pc) 0)
	    (= (pcache-vtable pc) 0))
       '(imap))
      ((and (> (pcache-imap pc) 0)
	    (> (pcache-cmap pc) 0)
	    (= (pcache-pmap pc) 0)
	    (= (pcache-amap pc) 0)
	    (= (pcache-vtable pc) 0))
       (if (> (pcache-imap pc) (pcache-cmap pc))
	   '(imap cmap)
	   '(cmap imap)))
      ((and (> (pcache-cmap pc) 0)
	    (= (pcache-pmap pc) 0)
	    (= (pcache-amap pc) 0)
	    (= (pcache-vtable pc) 0))
       '(cmap))
      ((and (> (pcache-pmap pc) 0)
	    (= (pcache-cmap pc) 0)
	    (= (pcache-amap pc) 0)
	    (= (pcache-vtable pc) 0))
       '(pmap))
      ((and (> (pcache-vtable pc) 0)
	    (= (pcache-cmap pc) 0)
	    (= (pcache-pmap pc) 0)
	    (= (pcache-amap pc) 0))
       '(vtable))
      ((and (> (pcache-amap pc) 0)
	    (= (pcache-cmap pc) 0)
	    (= (pcache-pmap pc) 0)
	    (= (pcache-vtable pc) 0))
       '(amap))
      ((and (> (pcache-cmap pc) 0)
	    (> (pcache-vtable pc) 0)
	    (= (pcache-pmap pc) 0))
       (if (> (pcache-vtable pc) (pcache-cmap pc))
	   '(vtable cmap)
	   '(cmap vtable)))
      ((and (= (pcache-cmap pc) 0)
	    (> (pcache-pmap pc) 0)
	    (> (pcache-vtable pc) 0))
       (if (> (pcache-vtable pc) (pcache-pmap pc))
	   '(vtable pmap)
	   '(pmap vtable)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    propcollect* ...                                                 */
;*---------------------------------------------------------------------*/
(define (propcollect* nodes ptable)
   (for-each (lambda (n) (propcollect n ptable)) nodes))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SNode ptable)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SObjInit ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SObjInit ptable)
   (with-access::J2SObjInit this (inits)
      (for-each (lambda (i)
		   (with-access::J2SPropertyInit i (name)
		      (let ((pi (hashtable-get ptable name)))
			 (if (propinfo? i)
			     (propinfo-set-set! i (+fx 1 (propinfo-set i)))
			     (begin
				(set! pi (propinfo 0 1 0 0 0 0 ))
				(hashtable-put! ptable name pi)))
			 (cond
			    ((isa? i J2SDataPropertyInit)
			     (with-access::J2SDataPropertyInit i (val)
				(propcollect val ptable)))
			    ((isa? i J2SAccessorPropertyInit)
			     (with-access::J2SAccessorPropertyInit i (get set)
				(propcollect get ptable)
				(propcollect set ptable)
				(propinfo-accessor-set! pi
				   (+fx (propinfo-accessor pi) 1))))))))
	 inits)))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SAccess ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SAccess ptable)
   (call-default-walker)
   (with-access::J2SAccess this (obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((i (hashtable-get ptable val)))
	       (if (propinfo? i)
		   (propinfo-get-set! i (+fx 1 (propinfo-get i)))
		   (hashtable-put! ptable val (propinfo 1 0 0 0 0 0))))))))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SAssig ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SAssig ptable)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (obj field)
	     (propcollect obj ptable)
	     (propcollect field ptable)
	     (when (isa? field J2SString)
		(with-access::J2SString field (val)
		   (let ((i (hashtable-get ptable val)))
		      (if (propinfo? i)
			  (propinfo-set-set! i (+fx 1 (propinfo-set i)))
			  (hashtable-put! ptable val (propinfo 0 1 0 0 0 0)))))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    propcollect ::J2SCall ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (propcollect this::J2SCall ptable)
   
   (define (defineProperty this)
      ;; a call to Object.defineProperty( obj, prop, val )
      (with-access::J2SCall this (fun args)
	 (when (and (pair? args) (pair? (cdr args))
		    (isa? (cadr args) J2SString))
	    (when (isa? fun J2SAccess)
	       (with-access::J2SAccess fun (obj field)
		  (when (and (isa? obj J2SGlobalRef) (isa? field J2SString))
		     (with-access::J2SString field (val)
			(when (string=? val "defineProperty")
			   (with-access::J2SGlobalRef obj (decl)
			      (with-access::J2SDecl decl (id)
				 (when (eq? id 'Object)
				    (with-access::J2SString (cadr args) (val)
				       val))))))))))))
      
   (call-default-walker)
   (let ((prop (defineProperty this)))
      (when (string? prop)
	 (let ((i (hashtable-get ptable prop)))
	    (if (propinfo? i)
		(propinfo-accessor-set! i (+fx 1 (propinfo-accessor i)))
		(hashtable-put! ptable prop (propinfo 0 1 0 1 0 0)))))))
	 
;*---------------------------------------------------------------------*/
;*    propclevel* ...                                                  */
;*---------------------------------------------------------------------*/
(define (propclevel* nodes ptable)
   (for-each (lambda (n) (propclevel n ptable)) nodes))

;*---------------------------------------------------------------------*/
;*    propclevel ::J2SNode ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (propclevel this::J2SNode ptable)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    propclevel ::J2SAccess ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (propclevel this::J2SAccess ptable)
   (call-default-walker)
   (with-access::J2SAccess this (cspecs obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((i (hashtable-get ptable val)))
	       ;; if there is only one setter, which is not an accessor
	       ;; use simple cache here
	       (tprint "val: " val " " i)
	       (when (propinfo? i)
		  (if (and (=fx (propinfo-set i) 1)
			   (=fx (propinfo-accessor i) 0))
		      (tprint "YES val: " val " " i)
		      (set! cspecs '(cmap pmap+ vtable))))))))
   this)
