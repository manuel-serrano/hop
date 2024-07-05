;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/cspecs.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr  2 19:46:13 2017                          */
;*    Last change :  Fri Jul  5 12:28:33 2024 (serrano)                */
;*    Copyright   :  2017-24 Manuel Serrano                            */
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
;*       - Those that are likely to be polymorphic.                    */
;*    It then set the accessor cache levels accordingly.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __js2scheme_cspecs

   (include "ast.sch"
	    "usage.sch")
   
   (import __js2scheme_ast
	   __js2scheme_dump
	   __js2scheme_compile
	   __js2scheme_stage
	   __js2scheme_utils
	   __js2scheme_alpha)
   
   (export j2s-cspecs-stage))

;*---------------------------------------------------------------------*/
;*    j2s-cspecs-stage ...                                             */
;*---------------------------------------------------------------------*/
(define j2s-cspecs-stage
   (instantiate::J2SStageProc
      (name "cspecs")
      (comment "Cache level annotation")
      (proc j2s-cspecs)))

;*---------------------------------------------------------------------*/
;*    cspecs ...                                                       */
;*---------------------------------------------------------------------*/
(define-struct cspecs access assig assigop assignew call assigthis)

;*---------------------------------------------------------------------*/
;*    j2s-cspecs ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-cspecs this::obj conf)
   (when (isa? this J2SProgram)
      (let ((csdef (if (config-get conf :optim-size)
		       ;; small code
		       (cspecs
			  (config-get conf :cspecs-get '(imap cmap))
			  (config-get conf :cspecs-put '(imap nmap))
			  (config-get conf :cspecs-assigop '(imap cmap))
			  (config-get conf :cspecs-assignew '(emap))
			  (config-get conf :cspecs-call '(pmap))
			  (config-get conf :cspecs-put '(imap nmap)))
		       ;; fast code
		       (cspecs
			  ;; access
			  (config-get conf :cspecs-get '(imap cmap))
			  ;; assig
			  (config-get conf :cspecs-put '(imap emap cmap nmap))
			  ;; assiggop
			  (config-get conf :cspecs-assigop '(imap cmap))
			  ;; assignew
			  (config-get conf :cspecs-assignew '(emap imap cmap))
			  ;; call
			  (config-get conf :cspecs-call '(pmap nmap cmap vtable poly))
			  ;; assigthis
			  (config-get conf :cspecs-put '(imap emap cmap nmap amap)))
		       #;(cspecs
			  ;; access
			  (config-get conf :cspecs-get '(imap emap cmap vtable))
			  ;; assig
			  (config-get conf :cspecs-put '(imap emap cmap nmap amap vtable))
			  ;; assiggop
			  (config-get conf :cspecs-assigop '(imap cmap))
			  ;; assignew
			  (config-get conf :cspecs-assignew '(emap imap cmap))
			  ;; call
			  (config-get conf :cspecs-call '(pmap nmap cmap vtable poly))
			  ;; assigthis
			  (config-get conf :cspecs-put '(imap emap cmap nmap amap vtable)))
		       )))
	 (cspecs-default! this csdef)
	 (when (or (config-get conf :optim-cspecs) (config-get conf :cspecs))
	    (let loop ((log (config-get conf :profile-log #f)))
	       (cond
		  (log
		   (if (cache-profile-log this log conf)
		       (with-access::J2SProgram this (nodes headers decls)
			  (let ((ptable (create-hashtable)))
			     (prop-collect* decls ptable)
			     (prop-collect* nodes ptable)
			     (prop-cspecs* decls ptable)
			     (prop-cspecs* nodes ptable)))
		       (loop #f)))
		  ((config-get conf :cspecs #f)
		   =>
		   (lambda (cspecs)
		      (let ((cs (cond
				   ((pair? cspecs)  cspecs)
				   ((symbol? cspecs) (list cspecs))
				   (else (error "j2s-cspecs" "Illegal cspecs" cspecs)))))
			 (cspecs-update this cs conf)))))))))
   this)

;*---------------------------------------------------------------------*/
;*    cache-verb ...                                                   */
;*---------------------------------------------------------------------*/
(define (cache-verb conf . args)
   (when (>=fx (config-get conf :verbose 0) 3)
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (display "\n      ")
	    (for-each display args)))))

;*---------------------------------------------------------------------*/
;*    cache-profile-log ...                                            */
;*---------------------------------------------------------------------*/
(define (cache-profile-log this::J2SProgram logfile conf)
   (with-access::J2SProgram this (profiling)
      (let ((verb (make-cell 0))
	    (caches (assq 'caches profiling)))
	 (when caches
	    (let ((logtable (val->logtable (cdr caches))))
	       (cpsecs-profile this logtable 'get verb conf)
	       (cache-verb conf "cspecs " (cell-ref verb)))))))

;*---------------------------------------------------------------------*/
;*    cspecs-update ...                                                */
;*---------------------------------------------------------------------*/
(define (cspecs-update this::J2SProgram cspecs conf)
   (cache-verb conf "update cspecs " cspecs)
   (cspecs-update! this cspecs))
   
;*---------------------------------------------------------------------*/
;*    pcache ...                                                       */
;*---------------------------------------------------------------------*/
(define-struct pcache point usage imap cmap emap pmap amap xmap vtable)

;*---------------------------------------------------------------------*/
;*    val->logtable ...                                                */
;*---------------------------------------------------------------------*/
(define (val->logtable vals::vector)
   
   (define (alist->pcache l)
      (let ((p (pcache -1 '- 0 0 0 0 0 0 0)))
	 (for-each (lambda (l)
		      (case (car l)
			 ((point)
			  (pcache-point-set! p (cdr l)))
			 ((usage)
			  (pcache-usage-set! p (string->symbol (cdr l))))
			 ((imap)
			  (pcache-imap-set! p (cdr l)))
			 ((emap)
			  (pcache-emap-set! p (cdr l)))
			 ((cmap)
			  (pcache-cmap-set! p (cdr l)))
			 ((pmap)
			  (pcache-pmap-set! p (cdr l)))
			 ((amap)
			  (pcache-amap-set! p (cdr l)))
			 ((xmap)
			  (pcache-xmap-set! p (cdr l)))
			 ((vtable)
			  (pcache-vtable-set! p (cdr l)))))
	    l)
	 p))
   
   (sort (lambda (x y)
	    (<=fx (pcache-point x) (pcache-point y)))
      (vector-map! alist->pcache vals)))

;*---------------------------------------------------------------------*/
;*    propinfo ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct propinfo get set value accessor prototype polymorphic)

;*---------------------------------------------------------------------*/
;*    cpsecs-profile ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (cpsecs-profile this::J2SNode logtable ctx verb conf)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cpsecs-profile ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (cpsecs-profile this::J2SAccess logtable ctx verb conf)
   (with-access::J2SAccess this (obj field cspecs loc)
      (let ((entry (logtable-find logtable (loc->point loc) ctx)))
	 (cond
	    (entry
	     (let ((policy (or (pcache->cspecs entry ctx) '())))
		(when (>=fx (config-get conf :verbose 0) 4)
		   (with-output-to-port (current-error-port)
		      (lambda ()
			 (display* "\n        " (loc->string loc)
			    " (" (pcache-usage entry) ") -> " policy))))
		(cell-set! verb (+fx (cell-ref verb) 1))
		(set! cspecs policy)))
	    ((eq? ctx 'put)
	     (set! cspecs '(pmap cmap+)))
	    (else
	     (set! cspecs '())))))
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cpsecs-profile ::J2SAssig ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (cpsecs-profile this::J2SAssig logtable ctx verb conf)
   (with-access::J2SAssig this (lhs rhs loc)
      (cpsecs-profile rhs logtable ctx verb conf)
      (cpsecs-profile lhs logtable 'put verb conf)))
   
;*---------------------------------------------------------------------*/
;*    cpsecs-profile ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (cpsecs-profile this::J2SCall logtable ctx verb conf)
   (with-access::J2SCall this (fun)
      (when (isa? fun J2SAccess)
	 (with-access::J2SAccess fun (loc)
	    (let ((entryc (logtable-find logtable (loc->point loc) 'call))
		  (entrya (logtable-find logtable (loc->point loc) 'get)))
	       (when entryc
		  (let ((policy (pcache->cspecs entryc ctx)))
		     (when policy
			(when (>=fx (config-get conf :verbose 0) 4)
			   (with-output-to-port (current-error-port)
			      (lambda ()
				 (display* "\n        " (loc->string loc)
				    " (" (pcache-usage entryc) ") -> "
				    policy))))
			(cell-set! verb (+fx (cell-ref verb) 1))
			(with-access::J2SCall this (cspecs)
			   (set! cspecs policy)))))
	       (when entrya
		  (let ((policy (or (pcache->cspecs entrya ctx) '(cmap+))))
		     (when (>=fx (config-get conf :verbose 0) 4)
			(with-output-to-port (current-error-port)
			   (lambda ()
			      (display* "\n        " (loc->string loc)
				 " (" (pcache-usage entrya) ") -> " policy))))
		     (cell-set! verb (+fx (cell-ref verb) 1))
		     (with-access::J2SAccess fun (cspecs)
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
		   pi)
		  ((=fx start end)
		   #f)
		  ((>fx po point)
		   (unless (=fx start pivot)
		      (loop start pivot
			 (+fx start (/fx (-fx pivot start) 2)))))
		  (else
		   (unless (=fx end pivot)
		      (loop (+fx pivot 1) end
			 (+fx pivot (+fx 1 (/fx (-fx end (+fx pivot 1)) 2))))))))))))

;*---------------------------------------------------------------------*/
;*    pcache->cspecs ...                                               */
;*    -------------------------------------------------------------    */
;*    Transforms a profiling cache statistics into an actual           */
;*    compiler cspecs directive.                                       */
;*---------------------------------------------------------------------*/
(define (pcache->cspecs pc ctx)

   (define threshold 10000)

   (define threshold-min 1000)

   (define (generic-cspecs)
      (cond
	 ((and (> (pcache-imap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-emap pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-amap pc) threshold)
	       (< (pcache-vtable pc) threshold))
	  '(imap))
	 ((and (> (pcache-imap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (> (pcache-emap pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-amap pc) threshold)
	       (< (pcache-vtable pc) threshold))
	  (if (> (pcache-imap pc) (pcache-emap pc))
	      '(imap emap)
	      '(emap imap)))
	 ((and (> (pcache-imap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-emap pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-amap pc) threshold)
	       (< (pcache-vtable pc) threshold))
	  (if (> (pcache-imap pc) (pcache-cmap pc))
	      '(imap cmap)
	      '(cmap imap)))
	 ((and (> (pcache-cmap pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-emap pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-amap pc) threshold)
	       (< (pcache-vtable pc) threshold))
	  '(cmap))
	 ((and (> (pcache-pmap pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-emap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-amap pc) threshold)
	       (< (pcache-vtable pc) threshold))
	  '(pmap))
	 ((and (> (pcache-vtable pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-emap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-amap pc) threshold))
	  '(vtable))
	 ((and (> (pcache-amap pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-emap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-vtable pc) threshold))
	  '(amap))
	 ((and (> (pcache-emap pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-amap pc) threshold)
	       (< (pcache-vtable pc) threshold))
	  '(emap))
	 ((and (> (pcache-imap pc) threshold)
	       (> (pcache-emap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-vtable pc) threshold)
	       (< (pcache-pmap pc) threshold))
	  (if (> (pcache-imap pc) (pcache-emap pc))
	      '(imap emap)
	      '(emap imap)))
	 ((and (> (pcache-emap pc) threshold)
	       (> (pcache-cmap pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-vtable pc) threshold)
	       (< (pcache-pmap pc) threshold))
	  (if (> (pcache-cmap pc) (pcache-emap pc))
	      '(cmap emap)
	      '(emap cmap)))
	 ((and (> (pcache-imap pc) threshold)
	       (> (pcache-vtable pc) threshold)
	       (< (pcache-emap pc) threshold)
	       (< (pcache-pmap pc) threshold))
	  '(imap vtable))
	 ((and (> (pcache-cmap pc) threshold)
	       (> (pcache-vtable pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-emap pc) threshold))
	  '(cmap vtable))
	 ((and (> (pcache-emap pc) threshold)
	       (> (pcache-vtable pc) threshold)
	       (< (pcache-pmap pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-cmap pc) threshold))
	  '(emap vtable))
	 ((and (> (pcache-pmap pc) threshold)
	       (> (pcache-vtable pc) threshold)
	       (< (pcache-imap pc) threshold)
	       (< (pcache-cmap pc) threshold)
	       (< (pcache-emap pc) threshold))
	  '(pmap vtable))
	 (else
	  #f)))

   (define (get-cspecs)
      (let ((cs '()))
	 (when (or (>fx (pcache-imap pc) threshold-min)
		   (>fx (pcache-cmap pc) threshold-min))
	    ;; imap: inline direct properties
	    ;; the ctor pgo optimization might turn cmap into imap
	    (set! cs (cons 'imap cs)))
	 (when (>fx (pcache-cmap pc) threshold-min)
	    ;; imap: noinline direct properties
	    (set! cs (cons 'cmap cs)))
	 (when (>fx (pcache-pmap pc) threshold-min)
	    ;; pmap: prototype properties
	    (set! cs (cons 'pmap cs)))
	 (when (>fx (pcache-amap pc) threshold)
	    ;; amap: accessor properties
	    (set! cs (cons 'amap cs)))
	 (when (>fx (pcache-xmap pc) threshold)
	    ;; xmap: cache miss
	    (set! cs (cons 'xmap cs)))
	 (when (>fx (pcache-vtable pc) threshold)
	    ;; polymorphic caches
	    (set! cs (cons 'vmap cs)))))

   (define (put-cspecs)
      (let ((cs (generic-cspecs)))
	 (when cs
	    (if (not (memq 'imap cs))
		(cons 'imap cs)
		cs))))

   (case ctx
      ((get) (get-cspecs))
      ((put) (put-cspecs))
      (else (generic-cspecs))))

;*---------------------------------------------------------------------*/
;*    prop-collect* ...                                                */
;*---------------------------------------------------------------------*/
(define (prop-collect* nodes ptable)
   (for-each (lambda (n) (prop-collect n ptable)) nodes))

;*---------------------------------------------------------------------*/
;*    prop-collect ::J2SNode ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (prop-collect this::J2SNode ptable)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    prop-collect ::J2SObjInit ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (prop-collect this::J2SObjInit ptable)
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
				(prop-collect val ptable)))
			    ((isa? i J2SAccessorPropertyInit)
			     (with-access::J2SAccessorPropertyInit i (get set)
				(prop-collect get ptable)
				(prop-collect set ptable)
				(propinfo-accessor-set! pi
				   (+fx (propinfo-accessor pi) 1))))))))
	 inits)))

;*---------------------------------------------------------------------*/
;*    prop-collect ::J2SAccess ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (prop-collect this::J2SAccess ptable)
   (call-default-walker)
   (with-access::J2SAccess this (obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((i (hashtable-get ptable val)))
	       (if (propinfo? i)
		   (propinfo-get-set! i (+fx 1 (propinfo-get i)))
		   (hashtable-put! ptable val (propinfo 1 0 0 0 0 0))))))))

;*---------------------------------------------------------------------*/
;*    prop-collect ::J2SAssig ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (prop-collect this::J2SAssig ptable)
   (with-access::J2SAssig this (lhs rhs)
      (if (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (obj field)
	     (prop-collect obj ptable)
	     (prop-collect field ptable)
	     (when (isa? field J2SString)
		(with-access::J2SString field (val)
		   (let ((i (hashtable-get ptable val)))
		      (if (propinfo? i)
			  (propinfo-set-set! i (+fx 1 (propinfo-set i)))
			  (hashtable-put! ptable val (propinfo 0 1 0 0 0 0)))))))
	  (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    prop-collect ::J2SCall ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (prop-collect this::J2SCall ptable)
   
   (define (defineProperty this)
      ;; a call to Object.defineProperty( obj, prop, val )
      (with-access::J2SCall this (fun args)
	 (when (and (pair? args) (pair? (cdr args))
		    (isa? (cadr args) J2SString))
	    (when (isa? fun J2SAccess)
	       (with-access::J2SAccess fun (obj field)
		  (when (isa? field J2SString)
		     (with-access::J2SString field (val)
			(when (string=? val "defineProperty")
			   (when (is-builtin-ref? obj 'Object)
			      (with-access::J2SString (cadr args) (val)
				 val))))))))))
      
   (call-default-walker)
   (let ((prop (defineProperty this)))
      (when (string? prop)
	 (let ((i (hashtable-get ptable prop)))
	    (if (propinfo? i)
		(propinfo-accessor-set! i (+fx 1 (propinfo-accessor i)))
		(hashtable-put! ptable prop (propinfo 0 1 0 1 0 0)))))))
	 
;*---------------------------------------------------------------------*/
;*    prop-cspecs* ...                                                 */
;*---------------------------------------------------------------------*/
(define (prop-cspecs* nodes ptable)
   (for-each (lambda (n) (prop-cspecs n ptable)) nodes))

;*---------------------------------------------------------------------*/
;*    prop-cspecs ::J2SNode ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (prop-cspecs this::J2SNode ptable)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    prop-cspecs ::J2SAccess ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (prop-cspecs this::J2SAccess ptable)
   (call-default-walker)
   (with-access::J2SAccess this (cspecs obj field)
      (when (isa? field J2SString)
	 (with-access::J2SString field (val)
	    (let ((i (hashtable-get ptable val)))
	       ;; if there is only one setter, which is not an accessor
	       ;; use simple cache here
	       (when (propinfo? i)
		  (when (and (=fx (propinfo-set i) 1)
			     (=fx (propinfo-accessor i) 0)
			     (>fx (length cspecs) 4))
		      (set! cspecs '(imap cmap pmap+ vtable))))))))
   this)

;*---------------------------------------------------------------------*/
;*    cspecs-update! ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-update! this::J2SNode cs::struct)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cspecs-update! ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-update! this::J2SAccess cs::struct)
   (with-access::J2SAccess this (cspecs)
      (set! cspecs (cspecs-access cs))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cspecs-update! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-update! this::J2SCall cs)
   (with-access::J2SCall this (cspecs loc)
      (set! cspecs (cspecs-call cs))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cspecs-update! ::J2SDeclFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-update! this::J2SDeclFun cs)
   (if (decl-usage-has? this '(new))
       (with-access::J2SDeclFun this (val loc)
	  (cspecs-update! val
	     (cspecs (remq 'pmap (remq 'imap (cspecs-access cs)))
		(cspecs-assig cs)
		(cspecs-assigop cs)
		(cspecs-assignew cs)
		(cspecs-call cs)
		(cspecs-assignew cs))))
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SNode csdef::struct)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ::J2SDeclFun ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SDeclFun csdef)
   (if (decl-usage-has? this '(new))
       (with-access::J2SDeclFun this (val c id)
	  (cspecs-default! val
	     (cspecs (cspecs-access csdef)
		(cspecs-assig csdef)
		(cspecs-assigop csdef)
		(cspecs-assignew csdef)
		(cspecs-call csdef)
		(cspecs-assignew csdef)))
	  this)
       (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ::J2SAccess ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SAccess csdef)
   (with-access::J2SAccess this (cspecs)
      (set! cspecs (cspecs-access csdef))
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ::J2SAssig ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SAssig csdef)
   (with-access::J2SAssig this (lhs rhs)
      (cspecs-default! rhs csdef)
      (if (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (cspecs obj field)
	     (set! cspecs (if (isa? lhs J2SThis)
			      (cspecs-assig csdef)
			      (cspecs-assigthis csdef)))
	     (cspecs-default! obj csdef)
	     (cspecs-default! field csdef)
	     this)
	  (cspecs-default! lhs csdef))
      this))

;*---------------------------------------------------------------------*/
;*    cspecs-assigop-default! ...                                      */
;*---------------------------------------------------------------------*/
(define (cspecs-assigop-default! this csdef)
   (let ((oldaccess (cspecs-access csdef))
	 (oldassig (cspecs-assig csdef)))
      (with-access::J2SAssig this (lhs rhs)
	 (set! rhs (cspecs-default! rhs csdef))
	 (cspecs-assig-set! csdef (cspecs-assigop csdef))
	 (cspecs-access-set! csdef (cspecs-assigop csdef))
	 (set! lhs (cspecs-default! lhs csdef))
	 (cspecs-access-set! csdef oldaccess)
	 (cspecs-assig-set! csdef oldassig)
	 this)))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ::J2SPrefix ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SPrefix csdef)
   (cspecs-assigop-default! this csdef))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ::J2SPostfix ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SPostfix csdef)
   (cspecs-assigop-default! this csdef))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ::J2SAssigOp ...                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SAssigOp csdef)
   (cspecs-assigop-default! this csdef))

;*---------------------------------------------------------------------*/
;*    cspecs-default! ::J2SCall ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-default! this::J2SCall csdef)

   (define (is-special-ref? expr clazz)
      (cond
	 ((isa? expr J2SUnresolvedRef)
	  (with-access::J2SUnresolvedRef expr (id)
	     (eq? id clazz)))
	 ((isa? expr J2SGlobalRef)
	  (with-access::J2SGlobalRef expr (decl)
	     (with-access::J2SDecl decl (id)
		(and (eq? id clazz) (not (decl-usage-has? decl '(assig)))))))
	 ((isa? expr J2SRef)
	  (with-access::J2SRef expr (decl)
	     (with-access::J2SDecl decl (id scope)
		(and (eq? id clazz)
		     (memq scope '(%scope tls))
		     (not (decl-usage-has? decl '(assig assig set delete eval)))))))
	 (else
	  #f)))

  (define (cspecs-method fun)
      ;; find specific method cache policy to save generated code space
      (if (isa? fun J2SAccess)
	  (with-access::J2SAccess fun (obj field)
	     (cond
		((is-special-ref? obj 'console)
		 (values '(pmap) '()))
		(else
		 (values #f #f))))
	  (values #f #f)))
   
   (with-access::J2SCall this (cspecs fun args loc)
      
      (multiple-value-bind (ccall caccess)
	 (cspecs-method fun)
	 (if ccall
	     (begin
		(set! args (map! (lambda (a) (cspecs-default! a csdef)) args))
		(set! cspecs ccall)
		(with-access::J2SAccess fun (cspecs)
		   (set! cspecs caccess)
		   this))
	     (begin
		(set! cspecs (cspecs-call csdef))
		(call-default-walker))))))
