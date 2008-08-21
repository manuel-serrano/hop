;*=====================================================================*/
;*    serrano/prgm/project/hop/1.9.x/runtime/cache.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr  1 06:54:00 2006                          */
;*    Last change :  Thu Aug 21 17:43:07 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LRU file caching.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_cache

   (import __hop_misc
	   __hop_param)
   
   (export (class cache-entry
	      (%prev (default #f))
	      (%next (default #f))
	      (signature::elong read-only)
	      (value read-only)
	      (upath::bstring read-only))

	   (abstract-class cache
	      (%table (default #f))
	      (%head (default #f))
	      (%tail (default #f))
	      (%mutex (default (make-mutex)))
	      (register::bool (default #t))
	      (max-entries::long (default 128))
	      (current-entries::long (default 0))
	      (uid::long (default 0))
	      (validity::procedure (default cache-entry-valid?)))
	   
	   (class cache-disk::cache
	      (%cache-disk-new)
	      (path read-only)
	      (map (default #f))
	      (out::procedure read-only)
	      (max-file-size::elong (default #e100000)))

	   (class cache-memory::cache
	      (%cache-memory-new)
	      (max-file-size::elong (default #e4096)))

	   (registered-caches::pair-nil)
	   (unregister-cache! ::cache)
	   
	   (cache-entry-valid?::bool ::obj ::bstring)

	   (%cache-disk-new ::cache-disk)
	   (%cache-memory-new ::cache-memory)
	   (generic cache-clear ::cache)
	   (generic cache->list ::cache)
	   (generic cache-get::obj ::cache ::bstring)
	   (generic cache-put! ::cache ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    *all-caches* ...                                                 */
;*---------------------------------------------------------------------*/
(define *all-caches* '())

;*---------------------------------------------------------------------*/
;*    registered-caches ...                                            */
;*---------------------------------------------------------------------*/
(define (registered-caches)
   *all-caches*)

;*---------------------------------------------------------------------*/
;*    unregister-cache! ...                                            */
;*---------------------------------------------------------------------*/
(define (unregister-cache! c)
   (set! *all-caches* (remq! c *all-caches*)))

;*---------------------------------------------------------------------*/
;*    %cache-disk-new ...                                              */
;*---------------------------------------------------------------------*/
(define (%cache-disk-new c::cache-disk)
   (with-access::cache-disk c (path map %table max-entries register)
      ;; cleanup the cache directory, unless we restore it
      (unless (hop-restore-disk-cache) (delete-path path))
      (unless (or (directory? path) (make-directories path))
	 (error 'instantiate::cache "Can't create directory" path))
      (set! %table (make-hashtable (*fx 4 max-entries)))
      (when register (set! *all-caches* (cons c *all-caches*)))
      ;; the name of the cache map path
      (unless (string? map) (set! map (make-file-name path "cache.map")))
      (when (and (hop-restore-disk-cache) (file-exists? map))
	 ;; restore the cache
	 (cache-restore! c))
      c))

;*---------------------------------------------------------------------*/
;*    %cache-memory-new ...                                            */
;*---------------------------------------------------------------------*/
(define (%cache-memory-new c::cache-memory)
   (with-access::cache-memory c (%table max-entries register)
      (set! %table (make-hashtable (*fx 4 max-entries)))
      (when register (set! *all-caches* (cons c *all-caches*)))))

;*---------------------------------------------------------------------*/
;*    cache-entry-valid? ...                                           */
;*---------------------------------------------------------------------*/
(define (cache-entry-valid? ce path)
   (and (cache-entry? ce)
	(signature-equal? (cache-entry-signature ce) (cache-signature path))))

;*---------------------------------------------------------------------*/
;*    signature-equal? ...                                             */
;*---------------------------------------------------------------------*/
(define (signature-equal? a b)
   (=elong a b))

;*---------------------------------------------------------------------*/
;*    cache-signature ...                                              */
;*---------------------------------------------------------------------*/
(define (cache-signature path)
   (file-modification-time path))

;*---------------------------------------------------------------------*/
;*    for-each-cache ...                                               */
;*---------------------------------------------------------------------*/
(define (for-each-cache proc::procedure c::cache)
   (with-access::cache c (%head)
      (let loop ((h %head))
	 (when (cache-entry? h)
	    (proc h)
	    (loop (cache-entry-%next h))))))

;*---------------------------------------------------------------------*/
;*    cache-clear ::cache ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (cache-clear c::cache)
   (with-access::cache c (%table %head %tail current-entries uid max-entries)
      (set! %table (make-hashtable (*fx 4 max-entries)))
      (set! %head #f)
      (set! %tail #f)
      (set! current-entries 0)
      (set! uid 0)))

;*---------------------------------------------------------------------*/
;*    cache-clear ::cache-disk ...                                     */
;*    -------------------------------------------------------------    */
;*    Removes all the entry from the cache                             */
;*---------------------------------------------------------------------*/
(define-method (cache-clear c::cache-disk)
   (for-each-cache (lambda (ce)
		      (with-access::cache-entry ce (value)
			 (when (file-exists? value)
			    (delete-file value))))
		   c)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    cache->list ::cache ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (cache->list c::cache)
   (with-access::cache c (%tail)
      (let loop ((tail %tail)
		 (res '()))
	 (if (not tail)
	     res
	     (loop (cache-entry-%prev tail) (cons tail res))))))

;*---------------------------------------------------------------------*/
;*    cache-get ::cache ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (cache-get c::cache path::bstring)
   (with-access::cache c (%table %head %tail %mutex validity)
      (mutex-lock! %mutex)
      (let ((ce (hashtable-get %table path)))
	 (cond
	    ((validity ce path)
	     (with-access::cache-entry ce (value %prev %next)
		(unless (eq? %head ce)
		   (when %prev
		      (if %next
			  (begin
			     (cache-entry-%prev-set! %next %prev)
			     (cache-entry-%next-set! %prev %next))
			  (begin
			     (cache-entry-%next-set! %prev #f)
			     (set! %tail %prev)))
		      (set! %prev #f)
		      (set! %next %head)
		      (cache-entry-%prev-set! %head ce)
		      (set! %head ce)))
		(mutex-unlock! %mutex)
		value))
	    ((cache-entry? ce)
	     (hashtable-remove! %table path)
	     (with-access::cache-entry ce (value %prev %next)
		(if %prev
		    (cache-entry-%next-set! %prev %next)
		    (set! %head %next))
		(if %next
		    (cache-entry-%prev-set! %next %prev)
		    (set! %tail #f))
		(mutex-unlock! %mutex)
		#f))
	    (else
	     (mutex-unlock! %mutex)
	     #f)))))

;*---------------------------------------------------------------------*/
;*    cache-get ::cache-disk ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cache-get c::cache-disk path::bstring)
   (with-access::cache-disk c (%table %head %tail validity %mutex)
      (mutex-lock! %mutex)
      (let ((ce (hashtable-get %table path)))
	 (cond
	    ((and (validity ce path) (file-exists? (cache-entry-value ce)))
	     (with-access::cache-entry ce (value %prev %next)
		(when %prev
		   (if %next
		       (begin
			  (cache-entry-%prev-set! %next %prev)
			  (cache-entry-%next-set! %prev %next))
		       (begin
			  (cache-entry-%next-set! %prev #f)
			  (set! %tail %prev)))
		   (set! %prev #f)
		   (set! %next %head)
		   (cache-entry-%prev-set! %head ce)
		   (set! %head ce))
		(mutex-unlock! %mutex)
		value))
	    ((cache-entry? ce)
	     (hashtable-remove! %table path)
	     (with-access::cache-entry ce (value %prev %next)
		(if (file-exists? value) (delete-file value))
		(if %prev
		    (cache-entry-%next-set! %prev %next)
		    (set! %head %next))
		(if %next
		    (cache-entry-%prev-set! %next %prev)
		    (set! %tail #f))
		(mutex-unlock! %mutex)
		#f))
	    (else
	     (mutex-unlock! %mutex)
	     #f)))))

;*---------------------------------------------------------------------*/
;*    cache-restore! ::cache ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (cache-restore! c::cache))

;*---------------------------------------------------------------------*/
;*    cache-restore! ::cache-disk ...                                  */
;*---------------------------------------------------------------------*/
(define-method (cache-restore! c::cache-disk)
   (with-access::cache-disk c (validity map path uid)
      (let ((omap (with-input-from-file map read))
	    (t (make-hashtable)))
	 (delete-file map)
	 (set! uid (car omap))
	 (for-each (lambda (e)
		      (match-case e
			 ((?upath ?sig ?value)
			  (when (and (file-exists? upath) (file-exists? value))
			     (let ((ce (instantiate::cache-entry
					  (value value)
					  (signature sig)
					  (upath upath))))
				(when (validity ce upath)
				   (cache-add-entry! c ce)
				   (hashtable-put! t value #t)))))))
		   (cadr omap))
	 (hashtable-put! t (make-file-name path "cache.map") #t)
	 (for-each (lambda (f)
		      (unless (hashtable-get t f)
			 (delete-file f)))
		   (directory->path-list path)))))
   
;*---------------------------------------------------------------------*/
;*    cache-put! ::cache ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (cache-put! c::cache upath::bstring value::obj))

;*---------------------------------------------------------------------*/
;*    cache-put! ::cache-disk ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cache-put! c::cache-disk upath::bstring value)
   (with-access::cache-disk c (%table %head %tail %mutex
				      max-entries current-entries uid
				      path max-file-size out)
      (when (<elong (file-size upath) max-file-size)
	 (mutex-lock! %mutex)
	 (let* ((name (format "~a-~a" uid (basename upath)))
		(cpath (make-file-name path name))
		(ce (instantiate::cache-entry
		       (value cpath)
		       (upath upath)
		       (signature (cache-signature upath)))))
	    ;; make sure that the cache path exists
	    (cond
	       ((not (file-exists? path))
		(make-directories path))
	       ((not (directory? path))
		(delete-file path)
		(make-directories path)))
	    ;; put the file in the cache
	    (let ((p (open-output-file cpath)))
	       (when (output-port? p)
		  (unwind-protect
		     (out value p)
		     (close-output-port p))))
	    ;; and the entry in the cache
	    (set! uid (+fx 1 uid))
	    (cache-add-entry! c ce)
	    (mutex-unlock! %mutex)
	    cpath))))

;*---------------------------------------------------------------------*/
;*    cache-put! ::cache-memory ...                                    */
;*---------------------------------------------------------------------*/
(define-method (cache-put! c::cache-memory upath::bstring value)
   (with-access::cache-memory c (%table %head %tail %mutex
					max-entries
					max-file-size)
      (when (<elong (file-size upath) max-file-size)
	 (let ((ce (instantiate::cache-entry
		      (value value)
		      (upath upath)
		      (signature (cache-signature upath)))))
	    (mutex-lock! %mutex)
	    (cache-add-entry! c ce)
	    (mutex-unlock! %mutex)
	    value))))

;*---------------------------------------------------------------------*/
;*    cache-add-entry! ::cache ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (cache-add-entry! c::cache ce::cache-entry))

;*---------------------------------------------------------------------*/
;*    cache-add-entry! ::cache-disk ...                                */
;*---------------------------------------------------------------------*/
(define-method (cache-add-entry! c::cache-disk ce::cache-entry)
   (with-access::cache-disk c (%table %head %tail max-entries current-entries
				      path max-file-size out map uid)
      (with-access::cache-entry ce (upath value)
	 ;; add the entry in the hash table
	 (hashtable-put! %table upath ce)
	 (if (<fx current-entries max-entries)
	     (set! current-entries (+fx 1 current-entries))
	     (begin
		(hashtable-remove! %table (cache-entry-upath %tail))
		(set! %tail (cache-entry-%prev %tail))
		(cache-entry-%next-set! %tail #f)))
	 (if %head
	     (begin
		(cache-entry-%next-set! ce %head)
		(cache-entry-%prev-set! %head ce)
		(set! %head ce))
	     (begin
		(set! %head ce)
		(set! %tail ce)))
	 ;; update the cache map (for future sessions)
	 (with-output-to-file map
	    (lambda ()
	       (display "(")
	       (write uid)
	       (display " ")
	       (write (hashtable-map
		       %table
		       (lambda (k ce)
			  (with-access::cache-entry ce (upath signature value)
			     (list upath signature value)))))
	       (display ")"))))))

;*---------------------------------------------------------------------*/
;*    cache-add-entry! ::cache-memory ...                              */
;*---------------------------------------------------------------------*/
(define-method (cache-add-entry! c::cache-memory ce::cache-entry)
   (with-access::cache-memory c (%table %head %tail max-entries current-entries
					max-file-size)
      (hashtable-put! %table (cache-entry-upath ce) ce)
      (if (<fx current-entries max-entries)
	  (set! current-entries (+fx 1 current-entries))
	  (begin
	     (hashtable-remove! %table (cache-entry-upath %tail))
	     (set! %tail (cache-entry-%prev %tail))
	     (cache-entry-%next-set! %tail #f)))
      (if %head
	  (begin
	     (cache-entry-%next-set! ce %head)
	     (cache-entry-%prev-set! %head ce)
	     (set! %head ce))
	  (begin
	     (set! %head ce)
	     (set! %tail ce)))))
   

   
