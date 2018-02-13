;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/profile.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  6 17:28:45 2018                          */
;*    Last change :  Tue Feb 13 17:02:50 2018 (serrano)                */
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
	      #!key cmap pmap amap vtable)
	   (js-profile-log-index ::long)
	   
	   (log-cache-miss!)
	   (add-cache-log! ::symbol ::obj)
	   (log-pmap-invalidation!)
	   (log-vtable! idx)
	   (profile-cache-misses)
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
	    (profile-cache-start! trc)
	    (when (string-contains trc "hopscript:cache")
	       (log-cache-miss!))
	    (when (string-contains trc "hopscript:function")
	       (log-function!))
	    (register-exit-function!
	       (lambda (n)
		  (profile-report-start trc)
		  (profile-report-cache trc)
		  (when (string-contains trc "hopscript:cache")
		     (profile-cache-misses))
		  (when (string-contains trc "hopscript:function")
		     (profile-functions))
		  (when (string-contains trc "hopscript:alloc")
		     (profile-allocs))
		  (profile-report-end trc conf)))))))

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
	   #!key cmap pmap amap vtable)
   (with-access::JsPropertyCache cache (cntcmap cntpmap cntamap cntvtable)
      (cond
	 (cmap (set! cntcmap (+fx 1 cntcmap)))
	 (pmap (set! cntpmap (+fx 1 cntpmap)))
	 (amap (set! cntamap (+fx 1 cntamap)))
	 (vtable (set! cntvtable (+fx 1 cntvtable))))))

;*---------------------------------------------------------------------*/
;*    js-profile-log-index ...                                         */
;*---------------------------------------------------------------------*/
(define (js-profile-log-index idx)
   (let* ((len (vector-length js-profile-accesses))
	  (i (if (>=fx idx len) (-fx len 1) idx)))
      (vector-set! js-profile-accesses i
	 (+llong (fixnum->llong 1) (vector-ref js-profile-accesses i)))))
      
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
;*    add-cache-log! ...                                               */
;*---------------------------------------------------------------------*/
(define (add-cache-log! what name)
   (when *log-misses*
      (let ((ow (assq what *misses*)))
	 (if (not ow)
	     (set! *misses*
		(cons (cons what (cons 1 (list (cons name 1)))) *misses*))
	     (let ((on (assq name (cddr ow))))
		(set-car! (cdr ow) (+fx 1 (cadr ow)))
		(if (not on)
		    (set-cdr! (cdr ow) (cons (cons name 1) (cddr ow)))
		    (set-cdr! on (+fx 1 (cdr on)))))))))

;*---------------------------------------------------------------------*/
;*    padding ...                                                      */
;*---------------------------------------------------------------------*/
(define (padding o sz #!optional (align 'left))
   
   (define (format-number o)
      (let ((s (number->string o)))
	 (cond
	    ((and (<=fx (string-length s) sz) (<=fx o 10000))
	     s)
	    ((<fx o 1000)
	     (number->string o))
	    ((<fx o 1000000)
	     (string-append (number->string (/fx o 1000)) ".10^3"))
	    ((<fx o 1000000000)
	     (string-append (number->string (/fx o 1000000)) ".10^6"))
	    (else
	     s))))

   (define (format-uint32 o)
      (let ((s (number->string o)))
	 (cond
	    ((and (<=fx (string-length s) sz) (<u32 o #u32:10000))
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
      (if (>fx l sz)
          (substring s 0 sz)
          (case align
             ((left)
              (string-append s (make-string (-fx sz l) #\space)))
             ((right)
              (string-append (make-string (-fx sz l) #\space) s))
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
   (set! *pmap-invalidations* (+fx 1 *pmap-invalidations*)))

;*---------------------------------------------------------------------*/
;*    log-vtable! ...                                                  */
;*---------------------------------------------------------------------*/
(define (log-vtable! idx)
   (when *log-vtables*
      (set! *vtables* (+fx 1 *vtables*))
      (set! *vtables-mem* (+fx *vtables-mem* (+fx idx 1)))))

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
   (with-output-to-port (current-error-port)
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
		      (when (>=fx (cadr e) *function-threshold*)
			 (print (car e) ": "
			    (format "~(/)" (vector->list (cddr e))))))
	    (sort (lambda (e1 e2)
		     (cond
			((>fx (cadr e1) (cadr e2)) #t)
			((<fx (cadr e1) (cadr e2)) #f)
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
   
   (with-output-to-port (current-error-port)
      (lambda ()
	 (let ((m (pregexp-match "hopscript:alloc([0-9]+)"
		     (getenv "HOPTRACE"))))
	    (cond-expand
	       (profile
		(if (string-contains (getenv "HOPTRACE") "format:json")
		    (show-json-alloc)
		    (show-text-alloc)))
	       (else
		(print "re-configure hop in profiling mode")))))))

;*---------------------------------------------------------------------*/
;*    profile-cache-index ...                                          */
;*---------------------------------------------------------------------*/
(define (profile-cache-index idx)
   (cond-expand
      (profile
       (let* ((len (vector-length js-profile-accesses))
	      (i (if (>=fx idx len) (-fx len 1) idx)))
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
	      (i (if (>=fx idx len) (-fx len 1) idx)))
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
	      (i (if (>=fx olen len) (-fx len 1) olen)))
	  (vector-set! js-profile-vectors i
	     (+llong (fixnum->llong 1) (vector-ref js-profile-vectors i)))))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    profile-report-start ...                                         */
;*---------------------------------------------------------------------*/
(define (profile-report-start trc)
   (when (string-contains trc "format:json")
      (display "{\n" (current-error-port))))

;*---------------------------------------------------------------------*/
;*    profile-report-end ...                                           */
;*---------------------------------------------------------------------*/
(define (profile-report-end trc conf)
   (when (string-contains trc "format:json")
      (with-output-to-port (current-error-port)
	 (lambda ()
	    (display "\"config\": ")
	    (profile-config conf)
	    (display ",\n" )
	    (printf "\"commandline\": \"~( )\",\n" (command-line))
	    (printf "\"date\": ~s\n}\n" (date))))))

;*---------------------------------------------------------------------*/
;*    profile-config ...                                               */
;*---------------------------------------------------------------------*/
(define (profile-config conf)
   (display "{\n")
   (when (pair? conf)
      (let loop ((conf conf))
	 (let ((v (cadr conf)))
	    (printf "   \"~a\": ~a" (keyword->string (car conf))
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
   
   (define pcaches
      (let ((m (pregexp-match "hopscript:file=([^ ]+)" trc)))
	 (if m
	     (let ((filename (cadr m)))
		(filter (lambda (pc) (string=? (car pc) filename))
		   ($js-get-pcaches)))
	     ($js-get-pcaches))))
   
   (define threshold
      (let ((m (pregexp-match "hopscript:cache=([0-9]+)" trc)))
	 (if m (string->integer (cadr m)) 100)))
   
   (define (pcache-for-each proc pcaches)
      (for-each (lambda (p)
		   (let ((vec (cdr p)))
		      (let loop ((i (-fx (vector-length vec) 1)))
			 (when (>=fx i 0)
			    (proc (vector-ref vec i))
			    (loop (-fx i 1))))))
	 pcaches))
   
   (define (pcache-hits pc)
      (with-access::JsPropertyCache pc (cntcmap cntpmap cntamap cntvtable)
	 (+fx cntcmap
	    (+fx cntpmap
	       (+fx cntamap cntvtable)))))

   (define (pcache-polymorphic pc)
      (with-access::JsPropertyCache pc (name usage cntcmap cntpmap cntamap cntvtable)
	 (when (> (+ (if (>fx cntcmap 0) 1 0)
		     (if (>fx cntpmap 0) 1 0)
		     (if (>fx cntamap 0) 1 0)
		     (if (>fx cntvtable 0) 1 0))
		  1)
	    (cons name (+ cntcmap cntpmap cntamap cntvtable)))))
   
   (define (total-cache-hits)
      (let ((res 0))
	 (pcache-for-each (lambda (pc)
			     (set! res (+fx res (pcache-hits pc))))
	    pcaches)
	 res))

   (define (total-cache-polymorphics)
      (let ((res '()))
	 (pcache-for-each (lambda (pc)
			     (let ((n (pcache-polymorphic pc)))
				(when  n
				   (set! res (cons n res)))))
	    pcaches)
	 res))
   
   (define (total-cache-cmaps)
      (let ((res 0))
	 (pcache-for-each (lambda (pc)
			     (with-access::JsPropertyCache pc (cntcmap)
				(set! res (+fx res cntcmap))))
	    pcaches)
	 res))
   
   (define (total-cache-pmaps)
      (let ((res 0))
	 (pcache-for-each (lambda (pc)
			     (with-access::JsPropertyCache pc (cntpmap)
				(set! res (+fx res cntpmap))))
	    pcaches)
	 res))

   (define (total-cache-vtables)
      (let ((res 0))
	 (pcache-for-each (lambda (pc)
			     (with-access::JsPropertyCache pc (cntvtable)
				(set! res (+fx res cntvtable))))
	    pcaches)
	 res))
   
   (define (total-cache-misses)
      (let ((res 0))
	 (pcache-for-each (lambda (pc)
			     (with-access::JsPropertyCache pc (cntmiss)
				(set! res (+fx res cntmiss))))
	    pcaches)
	 res))
   
   (define (total-cache-hits0)
      (apply + (map cadr (filter hit0? *profile-caches*))))
   
   (cond
      ((string-contains (getenv "HOPTRACE") "format:json")
       (with-output-to-port (current-error-port)
	  (lambda ()
	     (display "\"caches\": ")
	     (display "  \"caches\": {\n")
	     (for-each (lambda (what)
			  (print "    \"" (car what) "\": {")
			  (print "      \"total\": " (cadr what) ",")
			  (print "      \"entries\": [")
			  (for-each (lambda (e)
				       (when (>=fx (cdr e) *log-miss-threshold*)
					  (print "       { \"" (car e) "\": "
					     (cdr e) "},")))
			     (sort (lambda (e1 e2)
				      (cond
					 ((>fx (cdr e1) (cdr e2)) #t)
					 ((<fx (cdr e1) (cdr e2)) #f)
					 (else
					  (string<=? (js-symbol->string! (car e1))
					     (js-symbol->string! (car e2))))))
				(cddr what)))
			  (print "     -1 ] },"))
		*profile-caches*)
	     (print "  },")
	     (print "  \"totalCacheHits\": " (total-cache-hits) ", ")
	     (print "  \"totalCacheHits0\": " (total-cache-hits0) ", ")
	     (print "  \"totalCacheMisses\": " (total-cache-misses) ", ")
	     (print "  \"hiddenClassNumber\": " (gencmapid) ",")
	     (print "  \"pmapInvalidations\": " *pmap-invalidations* ",")
	     (print "  \"vtables\": { \"number\": " *vtables*
		", \"mem\": " *vtables-mem* "}")
	     (print "}, \n"))))
      (else
       (fprint (current-error-port) "\nCACHES:\n" "=======")
       (fprintf (current-error-port) "~(, )\n\n" (map car pcaches))
       (for-each (lambda (what)
		    (let ((c 0))
		       (fprint (current-error-port) (car what) ": "
			  (cadr what))
		       (for-each (lambda (e)
				    (when (or (>=fx (cdr e) *log-miss-threshold*)
					      (<fx c 10))
				       (set! c (+fx c 1))
				       (fprint (current-error-port) "   "
					  (car e) ": " (cdr e))))
			  (sort (lambda (e1 e2)
				   (cond
				      ((>fx (cdr e1) (cdr e2)) #t)
				      ((<fx (cdr e1) (cdr e2)) #f)
				      (else
				       (string<=? (js-symbol->string! (car e1))
					  (js-symbol->string! (car e2))))))
			     (cddr what)))
		       (newline (current-error-port))))
	  *profile-caches*)
       (let ((total (+ (total-cache-hits) (total-cache-misses)))
	     (poly (total-cache-polymorphics)))
	  (when (>fx total 0)
	     (fprint (current-error-port)
		"total accesses           : "
		(padding total 12 'right))
	     (fprint (current-error-port)
		"total cache hits         : "
		(padding (total-cache-hits) 12 'right)
		" (" (/fx (*fx 100 (total-cache-hits)) total) "%)")
	     (fprint (current-error-port)
		"total cache cmap hits    : "
		(padding (total-cache-cmaps) 12 'right)
		" (" (/fx (*fx 100 (total-cache-cmaps)) total) "%)")
	     (fprint (current-error-port)
		"total cache pmap hits    : "
		(padding (total-cache-pmaps) 12 'right)
		" (" (/fx (*fx 100 (total-cache-pmaps)) total) "%)")
	     (fprint (current-error-port)
		"total cache vtable hits  : "
		(padding (total-cache-vtables) 12 'right)
		" (" (/fx (*fx 100 (total-cache-vtables)) total) "%)")
	     (fprint (current-error-port)
		"total cache misses       : "
		(padding (total-cache-misses) 12 'right)
		" (" (/fx (*fx 100 (total-cache-misses)) total) "%)")
	     (let ((l (sort (lambda (n1 n2) (<=fx (cdr n1) (cdr n2))) poly))
		   (poly (apply + (map cdr poly))))
		(fprint (current-error-port)
		   "total cache polymorphic  : "
		   (padding poly 12 'right)
		   " (" (/fx (*fx 100 poly) total) "%) "
		   (map car (take l (min (length l) 5)))))
	     (fprint (current-error-port)
		"hidden classes num       : "
		(padding (gencmapid) 12 'right))
	     (fprint (current-error-port)
		"pmap invalidations       : "
		(padding *pmap-invalidations* 12 'right))
	     (fprint (current-error-port)
		"vtables                  : "
		(padding *vtables* 12 'right)
		" (" *vtables-mem* "b)")
	     (when (and (pair? pcaches) (null? (cdr pcaches)))
		(profile-pcache (car pcaches))))))))

;*---------------------------------------------------------------------*/
;*    profile-pcache ...                                               */
;*---------------------------------------------------------------------*/
(define (profile-pcache pcache)
   (newline (current-error-port))
   (fprint (current-error-port)
      (car pcache) ":")
   (fprint (current-error-port)
      (make-string (string-length (car pcache)) #\=) "=")

   (let* ((pcache (sort (lambda (x y)
			   (with-access::JsPropertyCache x
				 ((p1 point))
			      (with-access::JsPropertyCache y
				    ((p2 point))
				 (<fx p1 p2))))
		     (cdr pcache)))
	  (maxpoint (let ((pc (vector-ref pcache (-fx (vector-length pcache) 1))))
		       (with-access::JsPropertyCache pc (point)
			  (number->string point))))
	  (ppading (max (string-length maxpoint) 5)))
      (fprint (current-error-port) (padding "point" ppading 'center)
	 " "
	 (padding "property" 8 'center)
	 " "
	 (padding "use" 4 'center)
	 " | "
	 (padding "cntmiss" 8 'right)
	 " " 
	 (padding "cntcmap" 8 'right)
	 " " 
	 (padding "cntpmap" 8 'right)
	 " " 
	 (padding "cntamap" 8 'right)
	 " " 
	 (padding "cntvtable" 8 'right))
      (fprint (current-error-port) (make-string (+ ppading 1 8 1 4) #\-)
	 "-+-"
	 (make-string (+ 8 1 8 1 8 1 8 1 8) #\-))
      (for-each (lambda (pc)
		   (with-access::JsPropertyCache pc (point name usage
						       cntmiss
						       cntcmap
						       cntpmap
						       cntamap
						       cntvtable)
		      (when (> (+ cntmiss cntcmap cntpmap cntamap cntvtable)
			       *log-miss-threshold*)
			 (fprint (current-error-port)
			    (padding (number->string point) ppading 'right)
			    " " 
			    (padding name 8 'right)
			    " " 
			    (padding usage 4)
			    " | "
			    (padding cntmiss 8 'right)
			    " " 
			    (padding cntcmap 8 'right)
			    " " 
			    (padding cntpmap 8 'right)
			    " " 
			    (padding cntamap 8 'right)
			    " " 
			    (padding cntvtable 8 'right)))))
	 (vector->list pcache))))

;*---------------------------------------------------------------------*/
;*    profile-cache-misses ...                                         */
;*---------------------------------------------------------------------*/
(define (profile-cache-misses)
   
   (define (true-miss m) (memq (car m) '(get put call)))
   
   (let ((m (pregexp-match "hopscript:cache=([0-9]+)" (getenv "HOPTRACE"))))
      (when m
	 (set! *log-miss-threshold* (string->integer (cadr m)))))
   (cond
      ((string-contains (getenv "HOPTRACE") "format:json")
       (with-output-to-port (current-error-port)
	  (lambda ()
	     (display "\"caches\": ")
	     (display "  \"caches\": {\n")
	     (for-each (lambda (what)
			  (print "    \"" (car what) "\": {")
			  (print "      \"total\": " (cadr what) ",")
			  (print "      \"entries\": [")
			  (for-each (lambda (e)
				       (when (>=fx (cdr e) *log-miss-threshold*)
					  (print "       { \"" (car e) "\": "
					     (cdr e) "},")))
			     (sort (lambda (e1 e2)
				      (cond
					 ((>fx (cdr e1) (cdr e2)) #t)
					 ((<fx (cdr e1) (cdr e2)) #f)
					 (else
					  (string<=? (js-symbol->string! (car e1))
					     (js-symbol->string! (car e2))))))
				(cddr what)))
			  (print "     -1 ] },"))
		*misses*)
	     (print "  },")
	     (print "  \"totalCacheMisses\": "
		(apply + (map cadr (filter true-miss *misses*))) ", ")
	     (print "  \"totalCacheLevel0Misses\": "
		(apply + (map cadr *misses*)) ", ")
	     (print "  \"hiddenClassNumber\": "
		(gencmapid) ",")
	     (print "  \"pmapInvalidations\": "
		*pmap-invalidations* ",")
	     (print "  \"vtables\": { \"number\": " *vtables*
		", \"mem\": " *vtables-mem* "}")
	     (print "}, \n"))))
      (else
       (fprint (current-error-port) "\nCACHES:\n" "=======\n")
       (for-each (lambda (what)
		    (let ((c 0))
		       (fprint (current-error-port) (car what) ": "
			  (cadr what))
		       (for-each (lambda (e)
				    (when (or (>=fx (cdr e) *log-miss-threshold*)
					      (<fx c 10))
				       (set! c (+fx c 1))
				       (fprint (current-error-port) "   "
					  (car e) ": " (cdr e))))
			  (sort (lambda (e1 e2)
				   (cond
				      ((>fx (cdr e1) (cdr e2)) #t)
				      ((<fx (cdr e1) (cdr e2)) #f)
				      (else
				       (string<=? (js-symbol->string! (car e1))
					  (js-symbol->string! (car e2))))))
			     (cddr what)))
		       (newline (current-error-port))))
	  *misses*)
       (fprint (current-error-port)
	  "total cache misses       : "
	  (apply + (map cadr (filter true-miss *misses*))))
       (fprint (current-error-port)
	  "total cache level0 misses: "
	  (apply + (map cadr *misses*)))
       (fprint (current-error-port)
	  "hidden classes num       : "
	  (gencmapid))
       (fprint (current-error-port)
	  "pmap invalidations       : "
	  *pmap-invalidations*)
       (fprint (current-error-port)
	  "vtables                  : "
	  *vtables* " (" *vtables-mem* "b)"))))

