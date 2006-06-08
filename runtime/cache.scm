;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/cache.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr  1 06:54:00 2006                          */
;*    Last change :  Thu Jun  8 14:47:50 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LRU file caching.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_cache
   
   (export (class cache-entry
	      (%prev (default #f))
	      (%next (default #f))
	      (mtime::elong read-only)
	      (path::bstring read-only)
	      (upath::bstring read-only))

	   (abstract-class %cache
	      (%table (default #f))
	      (%head (default #f))
	      (%tail (default #f))
	      (register::bool (default #t))
	      (max-entries::long (default 128))
	      (current-entries::long (default 0))
	      (path::bstring read-only)
	      (validity::procedure (default cache-entry-valid?))
	      (out::procedure read-only))
	   
	   (class cache::%cache
	      (%cache-new)
	      (max-file-size::elong (default #e100000)))

	   (registered-caches::pair-nil)
	   (unregister-cache! ::cache)
	   
	   (cache-entry-valid?::bool ::obj ::bstring)

	   (%cache-new ::%cache)
	   (cache-clear ::cache)
	   (cache->list ::cache)
	   (cache-get::obj ::obj ::bstring)
	   (cache-put! ::cache ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    rm-rf ...                                                        */
;*---------------------------------------------------------------------*/
(define (rm-rf path)
   (when (file-exists? path)
      (if (directory? path)
	  (let ((files (directory->list path)))
	     (for-each (lambda (f) (rm-rf (make-file-name path f))) files)
	     (delete-directory path))
	  (delete-file path))))

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
;*    %cache-new ...                                                   */
;*---------------------------------------------------------------------*/
(define (%cache-new c::%cache)
   (with-access::%cache c (path %table max-entries register)
      (rm-rf path)
      (unless (make-directories path)
	 (error 'instantiate::cache "Can't create directory" path))
      (set! %table (make-hashtable (*fx 4 max-entries)))
      (when register
	 (set! *all-caches* (cons c *all-caches*)))))

;*---------------------------------------------------------------------*/
;*    cache-entry-valid? ...                                           */
;*---------------------------------------------------------------------*/
(define (cache-entry-valid? ce path)
   (and (cache-entry? ce)
	(file-exists? (cache-entry-path ce))
	(=elong (cache-entry-mtime ce) (file-modification-time path))))

;*---------------------------------------------------------------------*/
;*    for-each-cache ...                                               */
;*---------------------------------------------------------------------*/
(define (for-each-cache proc::procedure c::%cache)
   (with-access::%cache c (%head)
      (let loop ((h %head))
	 (when (cache-entry? h)
	    (proc h)
	    (loop (cache-entry-%next h))))))

;*---------------------------------------------------------------------*/
;*    cache-clear ::cache-file ...                                     */
;*    -------------------------------------------------------------    */
;*    Removes all the entry from the cache                             */
;*---------------------------------------------------------------------*/
(define (cache-clear c::cache)
   (with-access::%cache c (%table %head %tail current-entries max-entries)
      (set! %table (make-hashtable (*fx 4 max-entries)))
      (set! %head #f)
      (set! %tail #f)
      (set! current-entries 0))
   (for-each-cache (lambda (ce)
		      (with-access::cache-entry ce (path)
			 (when (file-exists? path)
			    (delete-file path))))
		   c))

;*---------------------------------------------------------------------*/
;*    cache->list ...                                                  */
;*---------------------------------------------------------------------*/
(define (cache->list c::cache)
   (with-access::%cache c (%tail)
      (let loop ((tail %tail)
		 (res '()))
	 (if (not tail)
	     res
	     (loop (cache-entry-%prev tail) (cons tail res))))))

;*---------------------------------------------------------------------*/
;*    cache-get ...                                                    */
;*---------------------------------------------------------------------*/
(define (cache-get c path::bstring)
   (if (not (%cache? c))
       (bigloo-type-error 'cache-get '%cache c)
       (with-access::%cache c (%table %head %tail validity)
	  (let ((ce (hashtable-get %table path)))
	     (cond
		((validity ce path)
		 (with-access::cache-entry ce (path %prev %next)
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
		    path))
		((cache-entry? ce)
		 (hashtable-remove! %table path)
		 (with-access::cache-entry ce (path %prev %next)
		    (if %prev
			(cache-entry-%next-set! %prev %next)
			(set! %head %next))
		    (if %next
			(cache-entry-%prev-set! %next %prev)
			(set! %tail #f))
		    #f))
		(else
		 #f))))))

;*---------------------------------------------------------------------*/
;*    cache-put! ...                                                   */
;*---------------------------------------------------------------------*/
(define (cache-put! c upath::bstring value)
   (with-access::cache c (%table %head %tail max-entries current-entries path
				 max-file-size out)
      (when (<elong (file-size path) max-file-size)
	 (let* ((name (format "~a-~a" current-entries (basename upath)))
		(cpath (make-file-name path name))
		(ce (instantiate::cache-entry
		       (path cpath)
		       (upath upath)
		       (mtime (file-modification-time upath)))))
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
	    cpath))))
