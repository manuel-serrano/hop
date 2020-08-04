;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/js2scheme/cspecs.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr  2 19:46:13 2017                          */
;*    Last change :  Sun Apr 19 08:23:15 2020 (serrano)                */
;*    Copyright   :  2017-20 Manuel Serrano                            */
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

   (library web)
   
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
(define-struct cspecs access assig assigop call)

;*---------------------------------------------------------------------*/
;*    j2s-cspecs ...                                                   */
;*---------------------------------------------------------------------*/
(define (j2s-cspecs this::obj conf)
   (when (isa? this J2SProgram)
      (let ((csdef (if (config-get conf :optim-size)
		       ;; small code
		       (cspecs
			  '(imap)
			  '(imap nmap)
			  '(imap cmap)
			  '(pmap))
		       ;; fast code
		       (cspecs
			  '(imap emap cmap pmap amap vtable)
			  '(imap emap cmap nmap amap vtable)
			  '(imap cmap)
			  '(pmap cmap vtable poly)))))
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
			 (cspecs-update this cs conf))))
		  (else
		   (cspecs-default! this csdef)))))))
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

   (define (get key lst)
      (let ((c (assq key lst)))
	 (when (pair? c) (cdr c))))

   (cache-verb conf "loading log file " (string-append "\"" logfile "\""))
   
   (let* ((log (load-profile-log logfile))
	  (srcs (get 'caches log))
	  (file (config-get conf :filename)))
      (when (vector? srcs)
	 (let loop ((i (-fx (vector-length srcs) 1))
		    (r #f))
	    (if (>=fx i 0)
		(let ((filename (get 'filename (vector-ref srcs i))))
		   (if (string=? file filename)
		       (let ((verb (make-cell 0))
			     (caches (get 'caches (vector-ref srcs i))))
			  (if caches
			      (let ((logtable (val->logtable caches)))
				 (cpsecs-profile this logtable 'get verb conf)
				 (cache-verb conf "cspecs " (cell-ref verb))
				 (loop (-fx i 1) #t))
			      (loop (-fx i 1) r)))
		       (loop (-fx i 1) #f)))
		r)))))

;*---------------------------------------------------------------------*/
;*    cspecs-update ...                                                */
;*---------------------------------------------------------------------*/
(define (cspecs-update this::J2SProgram cspecs conf)
   (cache-verb conf "update cspecs " cspecs)
   (cspecs-update! this cspecs))
   
;*---------------------------------------------------------------------*/
;*    pcache ...                                                       */
;*---------------------------------------------------------------------*/
(define-struct pcache point usage imap cmap emap pmap amap vtable)

;*---------------------------------------------------------------------*/
;*    val->logtable ...                                                */
;*---------------------------------------------------------------------*/
(define (val->logtable vals::vector)
   
   (define (alist->pcache l)
      (let ((p (pcache -1 '- 0 0 0 0 0 0)))
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
			 ((vtable)
			  (pcache-vtable-set! p (cdr l)))))
	    l)
	 p))
   
   (sort (lambda (x y)
	    (<=fx (pcache-point x) (pcache-point y)))
      (vector-map! alist->pcache vals)))

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
				 ((string=? p "caches")
				  (unless fprofile
				     (error "fprofile" "Wrong log format"
					logfile))
				  (cell-set! o
				     (cons (cons 'caches val)
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
	     (let ((policy (or (pcache->cspecs entry) '())))
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
		  (let ((policy (pcache->cspecs entryc)))
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
		  (let ((policy (or (pcache->cspecs entrya) '(cmap+))))
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

(define (logtable-find-TBR-18nov1029 table::vector point #!optional usage)

   (define (find-left pivot)
      (let loop ((i pivot))
	 (if (>=fx i 0)
	     (let ((pi (vector-ref table i)))
		(if (=fx (pcache-point pi) point)
		    (loop (-fx i 1))
		    (+fx i 1)))
	     0)))

   (define (find-right pivot end)
      (let loop ((i pivot))
	 (when (<=fx i end)
	    (let ((pi (vector-ref table i)))
	       (when (=fx (pcache-point pi) point)
		  (if (eq? (pcache-usage pi) usage)
		      pi
		      (loop (+fx i 1))))))))
   
   (let ((len (vector-length table)))
      (when (>fx len 0)
	 (let loop ((start 0)
		    (end (-fx len 1))
		    (pivot (/fx len 2)))
	    (let* ((pi (vector-ref table pivot))
		   (po (pcache-point pi)))
	       (cond
		  ((=fx po point)
		   (find-right (find-left pivot) end))
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

   (define threshold 100)
   
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
(define-walk-method (cspecs-update! this::J2SNode cs)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    cspecs-update! ::J2SAccess ...                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-update! this::J2SAccess cs)
   (with-access::J2SAccess this (cspecs)
      (set! cspecs cs)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cspecs-update! ::J2SCall ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-update! this::J2SCall cs)
   (with-access::J2SCall this (cspecs)
      (set! cspecs cs)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    cspecs-update! ::J2SDeclFun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (cspecs-update! this::J2SDeclFun cs)
   (if (decl-usage-has? this '(new))
       (with-access::J2SDeclFun this (val)
	  (cspecs-update! val (remq 'imap (remq 'pmap cs))))
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
       (with-access::J2SDeclFun this (val c)
	  (cspecs-update! val '(emap amap))
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
      (when (isa? lhs J2SAccess)
	  (with-access::J2SAccess lhs (cspecs)
	     (set! cspecs (cspecs-assig csdef)))
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
   (with-access::J2SCall this (cspecs)
      (set! cspecs (cspecs-call csdef)))
   (call-default-walker))
