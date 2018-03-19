;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/profile.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  6 17:28:45 2018                          */
;*    Last change :  Mon Mar 19 07:42:36 2018 (serrano)                */
;*    Copyright   :  2018 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HopScript profiler.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopscript_profile
   
   (library hop)
   
   (cond-expand
      (profile
       (extern (export js-profile-allocs "bgl_js_profile_allocs"))))
   (cond-expand
      (profile
       (export js-profile-allocs::obj)))

   (import __hopscript_types
	   __hopscript_property)

   (export (js-profile-init conf)

	   (js-profile-log-cache ::JsPropertyCache
	      #!key imap cmap pmap amap vtable)
	   (js-profile-log-index ::long)
	   
	   (js-profile-log-get ::symbol)
	   (js-profile-log-put ::symbol)
	   (js-profile-log-call ::symbol)

	   
	   (log-cache-miss!)

	   (log-pmap-invalidation!)
	   (log-vtable! idx)
	   (profile-function ::obj ::symbol)
	   (profile-cache-index ::long)
	   (profile-cache-extension ::long)
	   (profile-vector-extension ::long ::long)
	   (profile-functions)
	   (profile-allocs)))

;*---------------------------------------------------------------------*/
;*    *profile* ...                                                    */
;*---------------------------------------------------------------------*/
(define *profile* #f)
(define *profile-cache* #f)
(define *profile-caches* '())
(define *profile-gets* 0)
(define *profile-gets-props* #f)
(define *profile-puts* 0)
(define *profile-puts-props* #f)
(define *profile-calls* 0)
(define *profile-calls-props* #f)

(define *profile-port* (current-error-port))

(define *profile-cache-hit*
   '(getCache getCachePrototype getCacheAccessor getCacheVtable
     putCache putCachePrototype putCacheAccessor utCacheVtable putCacheExtend
     callCacheVtable callCache))

(define *profile-cache-miss*
   '(getCacheMiss
     putCacheMiss
     callCacheMissUncachable))

;*---------------------------------------------------------------------*/
;*    js-profile-init ...                                              */
;*---------------------------------------------------------------------*/
(define (js-profile-init conf)
   (unless *profile*
      (set! *profile* #t)
      (let ((trc (or (getenv "HOPTRACE") "")))
	 (when (string-contains trc "hopscript")
	    (let ((m (pregexp-match "logfile=([^ ]+)" trc)))
	       (when m
		  (set! *profile-port* (open-output-file (cadr m)))))
	    (profile-cache-start! trc)
	    (when (string-contains trc "hopscript:cache")
	       (log-cache-miss!))
	    (when (string-contains trc "hopscript:function")
	       (log-function!))
	    (when (string-contains trc "hopscript:uncache")
	       (set! *profile-gets-props* '())
	       (set! *profile-puts-props* '())
	       (set! *profile-calls-props* '()))
	    (register-exit-function!
	       (lambda (n)
		  (profile-report-start trc)
		  (profile-report-cache trc)
		  (when (string-contains trc "hopscript:function")
		     (profile-functions))
		  (when (string-contains trc "hopscript:alloc")
		     (profile-allocs))
		  (profile-report-end trc conf)
		  (unless (eq? *profile-port* (current-error-port))
		     (close-output-port *profile-port*))))))))

;*---------------------------------------------------------------------*/
;*    profile-cache-start! ...                                         */
;*---------------------------------------------------------------------*/
(define (profile-cache-start! trc)
   (when (string-contains trc "hopscript:access")
      (set! *profile-cache* #t)
      (set! *profile-caches*
	 (append-map (lambda (k) (cons k '()))
	    *profile-cache-hit* *profile-cache-miss*))))

;*---------------------------------------------------------------------*/
;*    js-profile-log-cache ...                                         */
;*---------------------------------------------------------------------*/
(define (js-profile-log-cache cache::JsPropertyCache
	   #!key imap cmap pmap amap vtable)
   (with-access::JsPropertyCache cache (cntimap cntcmap cntpmap cntamap cntvtable)
      (cond
	 (imap (set! cntimap (+ 1 cntimap)))
	 (cmap (set! cntcmap (+ 1 cntcmap)))
	 (pmap (set! cntpmap (+ 1 cntpmap)))
	 (amap (set! cntamap (+ 1 cntamap)))
	 (vtable (set! cntvtable (+ 1 cntvtable))))))

;*---------------------------------------------------------------------*/
;*    js-profile-log-index ...                                         */
;*---------------------------------------------------------------------*/
(define (js-profile-log-index idx)
   (let* ((len (vector-length js-profile-accesses))
	  (i (if (>= idx len) (- len 1) idx)))
      (vector-set! js-profile-accesses i
	 (+llong (fixnum->llong 1) (vector-ref js-profile-accesses i)))))

;*---------------------------------------------------------------------*/
;*    js-profile-log-get ...                                           */
;*---------------------------------------------------------------------*/
(define (js-profile-log-get prop)
   (set! *profile-gets* (+ 1 *profile-gets*))
   (when *profile-gets-props*
      (let ((c (assq prop *profile-gets-props*)))
	 (if (pair? c)
	     (set-cdr! c (+ 1 (cdr c)))
	     (set! *profile-gets-props* (cons (cons prop 1) *profile-gets-props*))))))
   
;*---------------------------------------------------------------------*/
;*    js-profile-log-put ...                                           */
;*---------------------------------------------------------------------*/
(define (js-profile-log-put prop)
   (set! *profile-puts* (+ 1 *profile-puts*))
   (when *profile-puts-props*
      (let ((c (assq prop *profile-puts-props*)))
	 (if (pair? c)
	     (set-cdr! c (+ 1 (cdr c)))
	     (set! *profile-puts-props* (cons (cons prop 1) *profile-puts-props*))))))
   
;*---------------------------------------------------------------------*/
;*    js-profile-log-call ...                                           */
;*---------------------------------------------------------------------*/
(define (js-profile-log-call prop)
   (set! *profile-calls* (+ 1 *profile-calls*))
   (when *profile-calls-props*
      (let ((c (assq prop *profile-calls-props*)))
	 (if (pair? c)
	     (set-cdr! c (+ 1 (cdr c)))
	     (set! *profile-calls-props* (cons (cons prop 1) *profile-calls-props*))))))
   
;*---------------------------------------------------------------------*/
;*    *misses* ...                                                     */
;*---------------------------------------------------------------------*/
(define *misses* '())
(define *log-misses* #f)
(define *log-miss-threshold* 100)
(define *log-pmap-invalidate* #f)
(define *log-vtables* #f)

(define *pmap-invalidations* 0)
(define *vtables* 0)
(define *vtables-mem* 0)

;*---------------------------------------------------------------------*/
;*    log-cache-miss! ...                                              */
;*---------------------------------------------------------------------*/
(define (log-cache-miss!)
   (set! *log-misses* #t)
   (set! *log-pmap-invalidate* #t)
   (set! *log-vtables* #t))

;*---------------------------------------------------------------------*/
;*    percent ...                                                      */
;*---------------------------------------------------------------------*/
(define (percent x y)
   (inexact->exact (floor (/ (* 100 x) y))))

;*---------------------------------------------------------------------*/
;*    padding ...                                                      */
;*---------------------------------------------------------------------*/
(define (padding o sz #!optional (align 'left))

   (define (/int a b)
      (let ((r (/ a b)))
	 (if (flonum? r)
	     (inexact->exact (round r))
	     r)))
   
   (define (format-number o)
      (let ((s (number->string o)))
	 (cond
	    ((and (<= (string-length s) sz) (<= o 10000))
	     s)
	    ((< o 1000)
	     (number->string o))
	    ((< o 1000000)
	     (string-append (number->string (/int o 1000)) ".10^3"))
	    ((< o 1000000000)
	     (string-append (number->string (/int o 1000000)) ".10^6"))
	    (else
	     s))))

   (define (format-uint32 o)
      (let ((s (number->string o)))
	 (cond
	    ((and (<= (string-length s) sz) (<u32 o #u32:10000))
	     s)
	    ((<u32 o #u32:1000)
	     (number->string o))
	    ((<u32 o #u32:1000000)
	     (string-append (number->string (/u32 o #u32:1000)) ".10^3"))
	    ((<u32 o #u32:1000000000)
	     (string-append (number->string (/u32 o #u32:1000000)) ".10^6"))
	    (else
	     s))))
   
   (let* ((s (cond
		((string? o) o)
		((uint32? o) (format-uint32 o))
		((number? o) (format-number o))
		((symbol? o) (symbol->string o))
		(else (call-with-output-string (lambda () (display o))))))
	  (l (string-length s)))
      (if (> l sz)
          (substring s 0 sz)
          (case align
             ((left)
              (string-append s (make-string (- sz l) #\space)))
             ((right)
              (string-append (make-string (- sz l) #\space) s))
             (else
              (let ((res (make-string sz #\space)))
                 (blit-string! s 0 res (/fx (-fx sz l) 2) l)
                 res))))))

;*---------------------------------------------------------------------*/
;*    js-symbol->string! ...                                           */
;*---------------------------------------------------------------------*/
(define (js-symbol->string! s)
   (cond
      ((symbol? s)
       (symbol->string! s))
      ((isa? s JsSymbolLiteral)
       (with-access::JsSymbolLiteral s (val)
	  val))
      (else
       (typeof s))))

;*---------------------------------------------------------------------*/
;*    *functions* ...                                                  */
;*---------------------------------------------------------------------*/
(define *functions* '())
(define *log-functions* #f)
(define *function-threshold* 10)

;*---------------------------------------------------------------------*/
;*    log-function! ...                                                */
;*---------------------------------------------------------------------*/
(define (log-function! )
   (set! *log-functions* #t))

;*---------------------------------------------------------------------*/
;*    log-pmap-invalidation! ...                                       */
;*---------------------------------------------------------------------*/
(define (log-pmap-invalidation!)
   (set! *pmap-invalidations* (+ 1 *pmap-invalidations*)))

;*---------------------------------------------------------------------*/
;*    log-vtable! ...                                                  */
;*---------------------------------------------------------------------*/
(define (log-vtable! idx)
   (when *log-vtables*
      (set! *vtables* (+ 1 *vtables*))
      (set! *vtables-mem* (+ *vtables-mem* (+ idx 1)))))

;*---------------------------------------------------------------------*/
;*    attr->index ...                                                  */
;*---------------------------------------------------------------------*/
(define (attr->index attr)
   (case attr
      ((hint) 0)
      ((nohint) 1)
      ((dispatch) 2)
      ((type) 3)
      ((notype) 4)
      (else (error "profile-function" "illegal attr" attr))))

;*---------------------------------------------------------------------*/
;*    profile-function ...                                             */
;*---------------------------------------------------------------------*/
(define (profile-function name attr)
   (when *log-functions*
      (let ((o (assq name *functions*))
	    (i (attr->index attr)))
	 (if (not o)
	     (let ((vec (make-vector (+fx 1 (attr->index 'notype)) 0)))
		(vector-set! vec i 1)
		(set! *functions* (cons (cons name (cons -1 vec)) *functions*)))
	     (vector-set! (cddr o) i (+fx 1 (vector-ref (cddr o) i)))))))

;*---------------------------------------------------------------------*/
;*    profile-functions ...                                            */
;*---------------------------------------------------------------------*/
(define (profile-functions)
   (with-output-to-port *profile-port*
      (lambda ()
	 (let ((m (pregexp-match "hopscript:function([0-9]+)"
		     (getenv "HOPTRACE"))))
	    (when m
	       (set! *function-threshold* (string->integer (cadr m)))))
	 (for-each (lambda (e)
		      (set-car! (cdr e) (apply + (vector->list (cddr e)))))
	    *functions*)
	 (print  "\nFUNCTIONS:\n" "==========\n")
	 (print "total number of functions: "
	    (length *functions*))
	 (print "  total function hinted calls  : "
	    (let ((i (attr->index 'hint)))
	       (apply + (map (lambda (e) (vector-ref (cddr e) i)) *functions*))))
	 (print "  total function unhinted calls: "
	    (let ((i (attr->index 'nohint)))
	       (apply + (map (lambda (e) (vector-ref (cddr e) i)) *functions*))))
	 (print "  total function dipatch calls : "
	    (let ((i (attr->index 'dispatch)))
	       (apply + (map (lambda (e) (vector-ref (cddr e) i)) *functions*))))
	 (print "  total function typed calls   : " 
	    (let ((i (attr->index 'type)))
	       (apply + (map (lambda (e) (vector-ref (cddr e) i)) *functions*))))
	 (print "  total function untyped calls : "
	    (let ((i (attr->index 'notype)))
	       (apply + (map (lambda (e) (vector-ref (cddr e) i)) *functions*))))
	 (newline)
	 (print ";; function: hint/nohint/dispatch/type/notype")
	 (for-each (lambda (e)
		      (when (>= (cadr e) *function-threshold*)
			 (print (car e) ": "
			    (format "~(/)" (vector->list (cddr e))))))
	    (sort (lambda (e1 e2)
		     (cond
			((> (cadr e1) (cadr e2)) #t)
			((< (cadr e1) (cadr e2)) #f)
			(else (string<=? (car e1) (car e2)))))
	       *functions*))
	 (newline))))

;*---------------------------------------------------------------------*/
;*    profiling                                                        */
;*---------------------------------------------------------------------*/
(define js-profile-allocs (make-vector 32 #l0))
(define js-profile-accesses (make-vector 32 #l0))
(define js-profile-extensions (make-vector 32 #l0))
(define js-profile-vectors (make-vector 32 #l0))

;*---------------------------------------------------------------------*/
;*    profile-allocs ...                                               */
;*---------------------------------------------------------------------*/
(define (profile-allocs)
   
   (define (show-json-percentages vec)
      (let ((len (vector-length vec)))
	 (display " [")
	 (let loop ((i (-fx len 1))
		    (sum #l0))
	    (if (=fx i -1)
		(let luup ((i 0)
			   (cum #l0)
			   (sep "\n"))
		   (when (and (<fx i len) (<llong cum sum))
		      (display sep)
		      (let* ((n0 (vector-ref vec i))
			     (n (if (fixnum? n0) (fixnum->llong n0) n0))
			     (c (+llong cum n))
			     (p (/llong (*llong n (fixnum->llong 100)) sum))
			     (pc (/llong (*llong c (fixnum->llong 100)) sum)))
			 (printf "   {\"idx\": ~d, \"occ\": ~d, \"per\": ~2,0d, \"cumul\": ~d}" i n p pc)
			 (luup (+fx i 1) c ",\n"))))
		(let ((n (vector-ref vec i)))
		   (loop (-fx i 1)
		      (+llong sum (if (fixnum? n) (fixnum->llong n) n))))))
	 (display "]")))

   (define (show-text-percentages vec)
      (let ((len (vector-length vec)))
	 (let loop ((i (-fx len 1))
		    (sum #l0))
	    (if (=fx i -1)
		(let luup ((i 0)
			   (cum #l0))
		   (when (and (<fx i len) (<llong cum sum))
		      (let* ((n0 (vector-ref vec i))
			     (n (if (fixnum? n0) (fixnum->llong n0) n0))
			     (c (+llong cum n))
			     (p (/llong (*llong n (fixnum->llong 100)) sum))
			     (pc (/llong (*llong c (fixnum->llong 100)) sum)))
			 (printf "  ~a: ~10d (~2,0d%) -> ~d%\n"
			    (if (=fx i (-fx len 1))
				"rest"
				(format "~4d" i))
			    n p pc)
			 (luup (+fx i 1) c))))
		(let ((n (vector-ref vec i)))
		   (loop (-fx i 1)
		      (+llong sum (if (fixnum? n) (fixnum->llong n) n))))))))

   (define (show-json-alloc)
      (cond-expand
	 (profile
	  (print "\"allocs\": {")
	  (display " \"objectAllocs\":")
	  (show-json-percentages js-profile-allocs)
	  (display ",\n \"accesses\":")
	  (show-json-percentages js-profile-accesses)
	  (display ",\n \"extensions\":")
	  (show-json-percentages js-profile-extensions)
	  (display ",\n \"vectorExtensions\":")
	  (show-json-percentages js-profile-vectors)
	  (print "\n},"))
	 (else #f)))
				  
   (define (show-text-alloc)
      (cond-expand
	 (profile
	  (print  "\nOBJECT ALLOCS:\n" "==============\n")
	  (show-text-percentages js-profile-allocs)
	  (print  "\nACCESSES:\n" "=========\n")
	  (show-text-percentages js-profile-accesses)
	  (print  "\nEXTENSIONS:\n" "===========\n")
	  (show-text-percentages js-profile-extensions)
	  (print  "\nVECTOR EXTENSIONS:\n" "==================\n")
	  (show-text-percentages js-profile-vectors))
	 (else #f)))
   
   (with-output-to-port *profile-port*
      (lambda ()
	 (let ((m (pregexp-match "hopscript:alloc([0-9]+)"
		     (getenv "HOPTRACE"))))
	    (cond-expand
	       (profile
		(if (string-contains (getenv "HOPTRACE") "format:json")
		    (show-json-alloc)
		    (show-text-alloc)))
	       (else
		(print "Allocation profiling disabled (re-configure Hop with \"--profile\"")))))))

;*---------------------------------------------------------------------*/
;*    profile-cache-index ...                                          */
;*---------------------------------------------------------------------*/
(define (profile-cache-index idx)
   (cond-expand
      (profile
       (let* ((len (vector-length js-profile-accesses))
	      (i (if (>= idx len) (- len 1) idx)))
	  (vector-set! js-profile-accesses i
	     (+llong (fixnum->llong 1) (vector-ref js-profile-accesses i)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    profile-cache-extension ...                                      */
;*---------------------------------------------------------------------*/
(define (profile-cache-extension idx)
   (cond-expand
      (profile
       (let* ((len (vector-length js-profile-extensions))
	      (i (if (>= idx len) (- len 1) idx)))
	  (vector-set! js-profile-extensions i
	     (+llong (fixnum->llong 1) (vector-ref js-profile-extensions i)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    profile-vector-extension ...                                     */
;*---------------------------------------------------------------------*/
(define (profile-vector-extension nlen olen)
   (cond-expand
      (profile
       (let* ((len (vector-length js-profile-vectors))
	      (i (if (>= olen len) (- len 1) olen)))
	  (vector-set! js-profile-vectors i
	     (+llong (fixnum->llong 1) (vector-ref js-profile-vectors i)))))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    profile-report-start ...                                         */
;*---------------------------------------------------------------------*/
(define (profile-report-start trc)
   (when (or (string-contains trc "format:json")
	     (string-contains trc "format:fprofile"))
      (display "{\n" *profile-port*)))

;*---------------------------------------------------------------------*/
;*    profile-report-end ...                                           */
;*---------------------------------------------------------------------*/
(define (profile-report-end trc conf)
   (when (or (string-contains trc "format:json")
	     (string-contains trc "format:fprofile"))
      (with-output-to-port *profile-port*
	 (lambda ()
	    (display "\"config\": ")
	    (profile-config conf)
	    (display ",\n" )
	    (display "\"run\": {\n")
	    (printf "  \"commandline\": \"~( )\",\n" (command-line))
	    (printf "  \"date\": ~s\n  }\n}\n" (date))))))

;*---------------------------------------------------------------------*/
;*    profile-config ...                                               */
;*---------------------------------------------------------------------*/
(define (profile-config conf)
   (display "{\n")
   (when (pair? conf)
      (let loop ((conf conf))
	 (let ((v (cadr conf)))
	    (printf "  \"~a\": ~a" (keyword->string (car conf))
	       (cond
		  ((boolean? v) (if v "true" "false"))
		  ((string? v) (string-append "\"" v "\""))
		  (else v)))
	    (when (pair? (cddr conf))
	       (display ",\n")
	       (loop (cddr conf))))))
   (display "\n}"))

;*---------------------------------------------------------------------*/
;*    profile-report-cache ...                                         */
;*---------------------------------------------------------------------*/
(define (profile-report-cache trc)
   
   (define (hit? e) (memq e *profile-cache-hit*))
   (define (hit0? e) (memq e '(getCache putCache callCache)))
   (define (miss? e) (memq e *profile-cache-miss*))
   
   (define (vfor-each proc::procedure vec::vector)
      (let loop ((i (-fx (vector-length vec) 1)))
	 (when (>=fx i 0)
	    (proc (vector-ref vec i))
	    (loop (-fx i 1)))))

   (define (vfilter pred::procedure vec::vector)
      (let ((res '()))
	 (vfor-each (lambda (pc)
		       (when (pred pc)
			  (set! res (cons pc res))))
	    vec)
	 (reverse! res)))

   (define (vmap proc::procedure vec::vector)
      (vector->list (vector-map proc vec)))

   (define (vany proc::procedure vec::vector)
      (let loop ((i (-fx (vector-length vec) 1)))
	 (when (>=fx i 0)
	    (if (proc (vector-ref vec i))
		(vector-ref vec i)
		(loop (-fx i 1))))))

   (define (filecache-name fc)
      (car fc))

   (define (filecache-caches fc)
      (cdr fc))
   
   (define filecaches
      (let ((m (pregexp-match "srcfile=([^ ]+)" trc)))
	 (if m
	     (let ((filename (cadr m)))
		(filter (lambda (fc) (string=? (filecache-name fc) filename))
		   ($js-get-pcaches)))
	     ($js-get-pcaches))))

   (define threshold
      (let ((m (pregexp-match "hopscript:cache=([0-9]+)" trc)))
	 (if m (string->integer (cadr m)) 100)))

   (define (pcache-hits pc)
      (with-access::JsPropertyCache pc (cntimap cntcmap cntpmap cntamap cntvtable)
	 (+ cntimap 
	    (+ cntcmap
	       (+ cntpmap
		  (+ cntamap cntvtable))))))

   (define (pcache-polymorphic pc)
      (with-access::JsPropertyCache pc (name usage cntimap cntcmap cntpmap cntamap cntvtable)
	 (when (> (+ (if (> cntimap 0) 1 0)
		     (if (> cntcmap 0) 1 0)
		     (if (> cntpmap 0) 1 0)
		     (if (> cntamap 0) 1 0)
		     (if (> cntvtable 0) 1 0))
		  1)
	    (cons name (+ cntimap cntcmap cntpmap cntamap cntvtable)))))

   (define (filecache-sum-field filecaches fieldname)
      (let ((field (find-class-field JsPropertyCache fieldname)))
	 (if field
	     (let ((proc (class-field-accessor field)))
		(apply +
		   (append-map (lambda (fc) (vmap proc (filecache-caches fc)))
		      filecaches)))
	     (error "filecache-sum-field" "cannot find field" fieldname))))
   
   (define (filecaches-hits filecaches)
      (apply +
	 (append-map (lambda (fc) (vmap pcache-hits (filecache-caches fc)))
	    filecaches)))

   (define (filecaches-misses filecaches)
      (filecache-sum-field filecaches 'cntmiss))

   (define (filecaches-cmaps filecaches)
      (filecache-sum-field filecaches 'cntcmap))

   (define (filecaches-imaps filecaches)
      (filecache-sum-field filecaches 'cntimap))

   (define (filecaches-pmaps filecaches)
      (filecache-sum-field filecaches 'cntpmap))

   (define (filecaches-amaps filecaches)
      (filecache-sum-field filecaches 'cntamap))

   (define (filecaches-vtables filecaches)
      (filecache-sum-field filecaches 'cntvtable))

   (define (filecaches-polymorphics filecaches)
      (append-map (lambda (fc)
		     (filter (lambda (x) x)
			(vmap pcache-polymorphic (filecache-caches fc))))
	 filecaches))

   (define (filecaches-usage-filter filecaches u)
      (map (lambda (fc)
	      (cons (filecache-name fc)
		 (list->vector
		    (vfilter (lambda (pc)
				(with-access::JsPropertyCache pc (usage cntmiss)
				   (eq? u usage)))
		       (filecache-caches fc)))))
	 filecaches))

   (define (total-uncaches)
      (+ *profile-gets* *profile-puts*))

   (define total
      (+ (filecaches-hits filecaches)
	 (filecaches-misses filecaches)
	 (total-uncaches)))

   (define poly
      (filecaches-polymorphics filecaches))

   (define (show-json-property-cache-entries filecaches fieldname)
      (let* ((field (find-class-field JsPropertyCache fieldname))
	     (proc (class-field-accessor field)))
	 (let ((table '()))
	    (for-each (lambda (fc)
			 (vfor-each (lambda (pc)
				       (with-access::JsPropertyCache pc (name)
					  (when (> (proc pc) 0)
					     (let ((old (assq name table)))
						(if (not old)
						    (set! table (cons (cons name (proc pc)) table))
						    (set-cdr! old (+ (cdr old) (proc pc))))))))
			    (filecache-caches fc)))
	       filecaches)
	    (for-each (lambda (e)
			 (when (> (cdr e) *log-miss-threshold*)
			    (print "        { \"" (car e) "\": " (cdr e) " }, ")))
	       (sort (lambda (x y)
			(cond
			   ((> (cdr x) (cdr y)) #t)
			   ((< (cdr x) (cdr y)) #f)
			   (else (string<=? (js-symbol->string! (car x))
				    (js-symbol->string! (car y))))))
		  table)))))

   (define (show-json-cache map)
      (let ((k (symbol-append 'cnt map)))
	 (print "  \"" map "\": {")
	 (print "    \"get\": {")
	 (print "      \"total\": " (filecache-sum-field (filecaches-usage-filter filecaches 'get) k) ",")
	 (print "      \"entries\": [")
	 (show-json-property-cache-entries (filecaches-usage-filter filecaches 'get) k)
	 (print "        -1 ]")
	 (print "    },")
	 (print "    \"put\": {")
	 (print "      \"total\": " (filecache-sum-field (filecaches-usage-filter filecaches 'put) k) ",")
	 (print "      \"entries\": [")
	 (show-json-property-cache-entries (filecaches-usage-filter filecaches 'put) k)
	 (print "        -1 ]")
	 (print "    },")
	 (print "    \"call\": {")
	 (print "      \"total\": " (filecache-sum-field (filecaches-usage-filter filecaches 'call) k) ",")
	 (print "      \"entries\": [")
	 (show-json-property-cache-entries (filecaches-usage-filter filecaches 'call) k)
	 (print "        -1 ]")
	 (print "    }")
	 (print "  },")))

   (define (collapse vec)
      (let ((len (vector-length vec)))
	 (when (>fx len 1)
	    (let loop ((i 1)
		       (old (vector-ref vec 0))
		       (res '()))
	       (if (=fx i len)
		   (list->vector (cons old res))
		   (let ((x (vector-ref vec i)))
		      (with-access::JsPropertyCache x ((xpoint point)
						       (xusage usage)
						       (xcntmiss cntmiss)
						       (xcntimap cntimap)
						       (xcntcmap cntcmap)
						       (xcntpmap cntpmap)
						       (xcntamap cntamap)
						       (xcntvtable cntvtable))
			 (with-access::JsPropertyCache old (point usage cntmiss cntimap cntcmap cntpmap cntamap cntvtable)
			    (if (and (= point xpoint) (eq? xusage usage))
				(begin
				   (set! cntmiss (+ cntmiss xcntmiss))
				   (set! cntimap (+ cntimap xcntimap))
				   (set! cntcmap (+ cntcmap xcntcmap))
				   (set! cntpmap (+ cntpmap xcntpmap))
				   (set! cntamap (+ cntamap xcntamap))
				   (set! cntvtable (+ cntvtable xcntamap))
				   (loop (+fx i 1) old res))
				(loop (+fx i 1) x (cons old res)))))))))))

   (cond
      ((string-contains trc "format:fprofile")
       (when (pair? filecaches)
	  (with-output-to-port *profile-port*
	     (lambda ()
		(print "\"format\": \"fprofile\",")
		(print "\"sources\": [")
		(for-each (lambda (fc)
			     (when (and (vector? (filecache-caches fc))
					(vany (lambda (pc)
						 (with-access::JsPropertyCache pc (cntmiss)
						    (or (> (pcache-hits pc) 0) (> cntmiss *log-miss-threshold*))))
					   (filecache-caches fc)))
				(print "  { \"filename\": \"" (filecache-name fc) "\",")
				(print "    \"caches\": [")
				(vfor-each (lambda (pc)
					      (with-access::JsPropertyCache pc (point usage cntmiss cntimap cntcmap cntpmap cntamap cntvtable)
						 (when (or (> (pcache-hits pc) 0) (> cntmiss *log-miss-threshold*))
						    (display* "      { \"point\": " point)
						    (display* ", \"usage\": \"" usage "\"")
						    (when (> cntmiss 1) (display* ", \"miss\": " cntmiss))
						    (when (> cntimap 0) (display* ", \"imap\": " cntimap))
						    (when (> cntcmap 0) (display* ", \"cmap\": " cntcmap))
						    (when (> cntpmap 0) (display* ", \"pmap\": " cntpmap))
						    (when (> cntamap 0) (display* ", \"amap\": " cntamap))
						    (when (> cntvtable 0) (display* ", \"vtable\": " cntvtable))
						    (print " }, "))))
				   (collapse
				      (sort (lambda (x y)
					       (with-access::JsPropertyCache x ((xpoint point))
						  (with-access::JsPropertyCache y ((ypoint point))
						     (<= xpoint ypoint))))
					 (filecache-caches fc))))
				(print "      { \"point\": -1 } ]")
				(print "  },")))
		   filecaches)
		(print "  { \"filename\": \"\", \"caches\": [] }")
		(print "],")))))
      ((string-contains trc "format:json")
       (with-output-to-port *profile-port*
	  (lambda ()
	     (print "\"format\": \"json\",")
	     (print "\"caches\": {")
	     (print "  \"accesses\": " total ",")
	     (print "  \"hits\": " (filecaches-hits filecaches) ",")
	     (print "  \"misses\": " (filecaches-misses filecaches) ",")
	     (print "  \"polymorphics\": " (apply + (map cdr poly)) ",")
	     (print "  \"uncaches\": {")
	     (print "    \"total\": " (total-uncaches) ",")
	     (print "    \"get\": " *profile-gets* ",")
	     (print "    \"put\": " *profile-puts* ",")
	     (print "    \"call\": " *profile-calls*)
	     (print "  },")
	     (show-json-cache 'imap)
	     (show-json-cache 'cmap)
	     (show-json-cache 'pmap)
	     (show-json-cache 'amap)
	     (show-json-cache 'vtable)
	     (print "  \"hclasses\": " (gencmapid) ",")
	     (print "  \"invalidations\": " *pmap-invalidations* ",")
	     (print "  \"vtables\": { \"number\": " *vtables* ", \"mem\": " *vtables-mem* "}")
	     (print "},"))))
      (else
       (fprint *profile-port* "\nCACHES:\n" "=======")
       (fprintf *profile-port* "~(, )\n\n" (map car filecaches))
       (for-each (lambda (what)
		    (let ((c 0))
		       (fprint *profile-port* (car what) ": "
			  (cadr what))
		       (for-each (lambda (e)
				    (when (or (>= (cdr e) *log-miss-threshold*)
					      (< c 10))
				       (set! c (+ c 1))
				       (fprint *profile-port* "   "
					  (car e) ": " (cdr e))))
			  (sort (lambda (e1 e2)
				   (cond
				      ((> (cdr e1) (cdr e2)) #t)
				      ((< (cdr e1) (cdr e2)) #f)
				      (else
				       (string<=? (js-symbol->string! (car e1))
					  (js-symbol->string! (car e2))))))
			     (cddr what)))
		       (newline *profile-port*)))
	  *profile-caches*)
       (when (> total 0)
	  (fprint *profile-port*
	     "total accesses           : "
	     (padding total 12 'right))
	  (fprint *profile-port*
	     "total cache hits         : "
	     (padding (filecaches-hits filecaches) 12 'right)
	     " (" (percent (filecaches-hits filecaches) total) "%)")
	  (fprint *profile-port*
	     "total cache imap hits    : "
	     (padding (filecaches-imaps filecaches) 12 'right)
	     " (" (percent (filecaches-imaps filecaches) total) "%)")
	  (fprint *profile-port*
	     "total cache cmap hits    : "
	     (padding (filecaches-cmaps filecaches) 12 'right)
	     " (" (percent (filecaches-cmaps filecaches) total) "%)")
	  (fprint *profile-port*
	     "total cache pmap hits    : "
	     (padding (filecaches-pmaps filecaches) 12 'right)
	     " (" (percent (filecaches-pmaps filecaches) total) "%)")
	  (fprint *profile-port*
	     "total cache amap hits    : "
	     (padding (filecaches-amaps filecaches) 12 'right)
	     " (" (percent (filecaches-amaps filecaches) total) "%)")
	  (fprint *profile-port*
	     "total cache vtable hits  : "
	     (padding (filecaches-vtables filecaches) 12 'right)
	     " (" (percent (filecaches-vtables filecaches) total) "%)")
	  (fprint *profile-port*
	     "total cache misses       : "
	     (padding (filecaches-misses filecaches) 12 'right)
	     " (" (percent (filecaches-misses filecaches) total) "%)")
	  (fprint *profile-port*
	     "total uncaches           : "
	     (padding (total-uncaches) 12 'right)
	     " (" (percent (total-uncaches) total) "%)")
	  
	  (let ((l (sort (lambda (n1 n2) (<= (cdr n1) (cdr n2))) poly))
		(poly (apply + (map cdr poly))))
	     (fprint *profile-port*
		"total cache polymorphic  : "
		(padding poly 12 'right)
		" (" (percent poly total) "%) "
		(map car (take l (min (length l) 5)))))
	  (fprint *profile-port*
	     "hidden classes num       : "
	     (padding (gencmapid) 12 'right))
	  (fprint *profile-port*
	     "pmap invalidations       : "
	     (padding *pmap-invalidations* 12 'right))
	  (fprint *profile-port*
	     "vtables                  : "
	     (padding *vtables* 12 'right)
	     " (" *vtables-mem* "b)")

	  (if (and (pair? filecaches) (pair? (cdr filecaches)))
	      (for-each (lambda (fc)
			   (when (and (vector? (filecache-caches fc))
				      (vany (lambda (pc)
					       (with-access::JsPropertyCache pc (cntmiss)
						  (or (> (pcache-hits pc) 0)
						      (> cntmiss *log-miss-threshold*))))
					 (filecache-caches fc)))
			      (profile-pcache fc)))
		 filecaches)
	      (profile-pcache (car filecaches))))
       
       (when (pair? *profile-gets-props*)
	  (let ((gets (sort (lambda (x y) (>= (cdr x) (cdr y)))
			 *profile-gets-props*))
		(puts (sort (lambda (x y) (>= (cdr x) (cdr y)))
			 *profile-puts-props*))
		(calls (sort (lambda (x y) (>= (cdr x) (cdr y)))
			  *profile-calls-props*)))
	     (newline *profile-port*) 
	     (fprint *profile-port*
		"UNCACHED GETS:\n==============")
	     (profile-uncached gets *profile-gets*)
	     (newline *profile-port*)
	     (fprint *profile-port*
		"UNCACHED PUTS:\n==============")
	     (profile-uncached puts *profile-puts*)
	     (newline *profile-port*)
	     (fprint *profile-port*
		"UNCACHED CALLS:\n===============")
	     (profile-uncached calls *profile-calls*)
	     (newline *profile-port*))))))

;*---------------------------------------------------------------------*/
;*    profile-uncached ...                                             */
;*---------------------------------------------------------------------*/
(define (profile-uncached entries total)
   (let loop ((es entries)
	      (sum 0))
      (when (and (pair? es) (> (/ (cdar es) total) 0.01))
	 (let ((e (car es)))
	    (fprint *profile-port* (padding (car e) 10 'right) ": "
	       (padding (cdr e) 10 'right)
	       (padding
		  (string-append " (" (number->string (percent (cdr e) total)) "%)")
		  7 'right)
	       (padding
		  (string-append " [" (number->string (percent (+ (cdr e) sum) total)) "%]")
		  7 'right))
	    (loop (cdr es) (+ sum (cdr e)))))))
		   
;*---------------------------------------------------------------------*/
;*    profile-pcache ...                                               */
;*---------------------------------------------------------------------*/
(define (profile-pcache pcache)
   (newline *profile-port*)
   (fprint *profile-port*
      (car pcache) ":")
   (fprint *profile-port*
      (make-string (string-length (car pcache)) #\=) "=")

   (let* ((pcache (sort (lambda (x y)
			   (with-access::JsPropertyCache x
				 ((p1 point))
			      (with-access::JsPropertyCache y
				    ((p2 point))
				 (< p1 p2))))
		     (cdr pcache)))
	  (maxpoint (let ((pc (vector-ref pcache (-fx (vector-length pcache) 1))))
		       (with-access::JsPropertyCache pc (point)
			  (number->string point))))
	  (ppading (max (string-length maxpoint) 5))
	  (cwidth 8))
      (fprint *profile-port* (padding "point" ppading 'center)
	 " "
	 (padding "property" cwidth 'center)
	 " "
	 (padding "use" 4 'center)
	 " | "
	 (padding "miss" cwidth 'right)
	 " " 
	 (padding "imap" cwidth 'right)
	 " "
	 (padding "cmap" cwidth 'right)
	 " "
	 (padding "pmap" cwidth 'right)
	 " " 
	 (padding "amap" cwidth 'right)
	 " " 
	 (padding "vtable" cwidth 'right))
      (fprint *profile-port* (make-string (+ ppading 1 cwidth 1 4) #\-)
	 "-+-"
	 (make-string (* 6 (+ cwidth 1)) #\-))
      (for-each (lambda (pc)
		   (with-access::JsPropertyCache pc (point name usage
						       cntmiss
						       cntimap
						       cntcmap
						       cntpmap
						       cntamap
						       cntvtable)
		      (when (> (+ cntmiss cntimap cntcmap cntpmap cntamap cntvtable)
			       *log-miss-threshold*)
			 (fprint *profile-port*
			    (padding (number->string point) ppading 'right)
			    " " 
			    (padding name cwidth 'right)
			    " " 
			    (padding usage 4)
			    " | "
			    (padding cntmiss cwidth 'right)
			    " " 
			    (padding cntimap cwidth 'right)
			    " " 
			    (padding cntcmap cwidth 'right)
			    " " 
			    (padding cntpmap cwidth 'right)
			    " " 
			    (padding cntamap cwidth 'right)
			    " " 
			    (padding cntvtable cwidth 'right)))))
	 (vector->list pcache))))
