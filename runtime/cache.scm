;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/runtime/cache.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr  1 06:54:00 2006                          */
;*    Last change :  Wed Oct 24 14:39:07 2018 (serrano)                */
;*    Copyright   :  2006-18 Manuel Serrano                            */
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
	      (%forced (default #f))
	      (%mutex (default (make-mutex "cache")))
	      (register::bool (default #t))
	      (max-entries::long (default 128))
	      (current-entries::long (default 0))
	      (uid::long (default 0))
	      (validity::procedure (default cache-entry-valid?)))
	   
	   (class cache-disk::cache
	      (%cache-disk-new)
	      (path read-only)
	      (clear read-only (default #t))
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
	   (generic cache-put!::obj ::cache ::bstring ::obj)))

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
   (with-access::cache-disk c (path clear map %table max-entries register)
      ;; create the new directory if it does not exist
      (cond
	 ((directory? path)
	  ;; cleanup the cache directory when configured
	  (when clear (for-each delete-path (directory->path-list path))))
	 ((not (make-directories path))
	  (error "instantiate::cache" "Can't create directory" path)))
      (set! %table (make-hashtable (*fx 4 max-entries)))
      (when register (set! *all-caches* (cons c *all-caches*)))
      ;; the name of the cache map path
      (unless (string? map) (set! map (make-file-name path "cache.map")))
      ;; restore the cache
      (when (file-exists? map) (cache-restore! c))
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
   (and (isa? ce cache-entry)
	(with-access::cache-entry ce (signature)
	   (signature-equal? signature (cache-signature path)))))

;*---------------------------------------------------------------------*/
;*    signature-equal? ...                                             */
;*---------------------------------------------------------------------*/
(define (signature-equal? a b)
   (=elong a b))

;*---------------------------------------------------------------------*/
;*    cache-signature ...                                              */
;*---------------------------------------------------------------------*/
(define (cache-signature path)
   (bit-xorelong (file-modification-time path) (fixnum->elong (hop-session))))

;*---------------------------------------------------------------------*/
;*    for-each-cache ...                                               */
;*---------------------------------------------------------------------*/
(define (for-each-cache proc::procedure c::cache)
   (with-access::cache c (%head)
      (let loop ((h %head))
	 (when (isa? h cache-entry)
	    (proc h)
	    (with-access::cache-entry h (%next)
	       (loop %next))))))

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
	     (with-access::cache-entry tail (%prev)
		(loop %prev (cons tail res)))))))

;*---------------------------------------------------------------------*/
;*    cache-get ::cache ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (cache-get c::cache path::bstring)
   (with-access::cache c (%table %head %tail %mutex validity current-entries)
      (synchronize %mutex
	 (let ((ce (hashtable-get %table path)))
	    (cond
	       ((validity ce path)
		(with-access::cache-entry ce (%prev %next)
		   (unless (eq? %head ce)
		      (when %prev
			 (if %next
			     (begin
				(with-access::cache-entry %next ((prev %prev))
				   (set! prev %prev))
				(with-access::cache-entry %prev ((next %next))
				   (set! next %next)))
			     (begin
				(with-access::cache-entry %prev (%next)
				   (set! %next #f))
				(set! %tail %prev)))
			 (set! %prev #f)
			 (set! %next %head)
			 (with-access::cache-entry %head (%prev)
			    (set! %prev ce))
			 (set! %head ce)))
		   ce))
	       ((isa? ce cache-entry)
		(hashtable-remove! %table path)
		(set! current-entries (-fx current-entries 1))
		(with-access::cache-entry ce (%prev %next)
		   (if %prev
		       (with-access::cache-entry %prev ((next %next))
			  (set! next %next))
		       (set! %head %next))
		   (if %next
		       (with-access::cache-entry %next ((prev %prev))
			  (set! prev %prev))
		       (set! %tail %prev))
		   #f))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    cache-get ::cache-disk ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cache-get c::cache-disk path::bstring)
   (with-access::cache-disk c (%table %head %tail validity %mutex current-entries)
      (synchronize %mutex
	 (let ((ce (hashtable-get %table path)))
	    (cond
	       ((and ce
		     (validity ce path)
		     (with-access::cache-entry ce (value) (file-exists? value)))
		(with-access::cache-entry ce (%prev %next)
		   (when %prev
		      (if %next
			  (begin
			     (with-access::cache-entry %next ((prev %prev))
				(set! prev %prev))
			     (with-access::cache-entry %prev ((next %next))
				(set! next %next)))
			  (begin
			     (with-access::cache-entry %prev (%next)
				(set! %next #f))
			     (set! %tail %prev)))
		      (set! %prev #f)
		      (set! %next %head)
		      (with-access::cache-entry %head (%prev)
			 (set! %prev ce))
		      (set! %head ce))
		   ce))
	       ((isa? ce cache-entry)
		(hashtable-remove! %table path)
		(set! current-entries (-fx current-entries 1))
		(with-access::cache-entry ce (value %prev %next)
		   (if (file-exists? value) (delete-file value))
		   (if %prev
		       (with-access::cache-entry %prev ((next %next))
			  (set! next %next))
		       (set! %head %next))
		   (if %next
		       (with-access::cache-entry %next ((prev %prev))
			  (set! prev %prev))
		       (set! %tail %prev))
		   #f))
	       (else
		#f))))))

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
	 (when (pair? omap)
	    (set! uid (car omap))
	    (for-each (lambda (e)
			 (match-case e
			    ((?upath ?sig ?value)
			     (when (and (file-exists? upath)
					(file-exists? value))
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
		      (directory->path-list path))))))
   
;*---------------------------------------------------------------------*/
;*    cache-put! ::cache ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (cache-put!::cache-entry c::cache upath::bstring value::obj))

;*---------------------------------------------------------------------*/
;*    cache-put! ::cache-disk ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cache-put!::cache-entry c::cache-disk upath::bstring value)
   (with-access::cache-disk c (%table %forced %head %tail %mutex
				      max-entries current-entries uid
				      path max-file-size out)
      (when (and (or (hop-cache-enable) %forced)
		 (or (<=elong max-file-size #e0)
		     (<elong (file-size upath) max-file-size)))
	 (synchronize %mutex
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
	       ce)))))

;*---------------------------------------------------------------------*/
;*    cache-put! ::cache-memory ...                                    */
;*---------------------------------------------------------------------*/
(define-method (cache-put! c::cache-memory upath::bstring value)
   (with-access::cache-memory c (%table %forced %head %tail %mutex
					max-entries
					max-file-size)
      (when (and (or (hop-cache-enable) %forced)
		 (or (<=elong max-file-size #e0)
		     (<elong (file-size upath) max-file-size)))
	 (let ((ce (instantiate::cache-entry
		      (value value)
		      (upath upath)
		      (signature (cache-signature upath)))))
	    (synchronize %mutex
	       (cache-add-entry! c ce))
	    ce))))

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
		(with-access::cache-entry %tail (upath)
		   (hashtable-remove! %table upath))
		(with-access::cache-entry %tail (%prev)
		   (set! %tail %prev))
		(with-access::cache-entry %tail (%next)
		   (set! %next #f))))
	 (if %head
	     (begin
		(with-access::cache-entry ce (%next)
		   (set! %next %head))
		(with-access::cache-entry %head (%prev)
		   (set! %prev ce))
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
      (with-access::cache-entry ce (upath)
	 (hashtable-put! %table upath ce)
	 (if (<fx current-entries max-entries)
	     (set! current-entries (+fx 1 current-entries))
	     (begin
		(with-access::cache-entry %tail (upath)
		   (hashtable-remove! %table upath))
		(with-access::cache-entry %tail (%prev)
		   (set! %tail %prev))
		(with-access::cache-entry %tail (%next)
		   (set! %next #f))))
	 (if %head
	     (begin
		(with-access::cache-entry ce (%next)
		   (set! %next %head))
		(with-access::cache-entry %head (%prev)
		   (set! %prev ce))
		(set! %head ce))
	     (begin
		(set! %head ce)
		(set! %tail ce))))))
   

   
