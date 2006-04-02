;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/cache.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Apr  1 06:54:00 2006                          */
;*    Last change :  Sat Apr  1 13:07:41 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LRU file caching.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_cache
   
   (static (class cache-entry
	      (prev (default #f))
	      (next (default #f))
	      (mtime::elong read-only)
	      (path::bstring read-only)
	      (upath::bstring read-only)))
   
   (export (abstract-class %cache
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

	   (get-register-caches::pair-nil)
	   (unregister-cache! ::cache)
	   
	   (cache-entry-valid?::bool ::obj ::bstring)

	   (%cache-new ::%cache)
	   (cache-clear ::cache)
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
;*    mkdir-p ...                                                      */
;*---------------------------------------------------------------------*/
(define (mkdir-p path)
   (or (make-directory path)
       (begin
	  (mkdir-p (dirname path))
	  (make-directory path))))
	       
;*---------------------------------------------------------------------*/
;*    *all-caches* ...                                                 */
;*---------------------------------------------------------------------*/
(define *all-caches* '())

;*---------------------------------------------------------------------*/
;*    get-register-caches ...                                          */
;*---------------------------------------------------------------------*/
(define (get-register-caches)
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
      (unless (mkdir-p path)
	 (error 'instantiate::cache "Can't create directory" path))
      (set! %table (make-hashtable (*fx 4 max-entries)))
      (when register
	 (set! *all-caches* (cons c *all-caches*)))))

;*---------------------------------------------------------------------*/
;*    cache-entry-valid? ...                                           */
;*---------------------------------------------------------------------*/
(define (cache-entry-valid? ce path)
   (and (cache-entry? ce)
	(=elong (cache-entry-mtime ce) (file-modification-time path))))

;*---------------------------------------------------------------------*/
;*    for-each-cache ...                                               */
;*---------------------------------------------------------------------*/
(define (for-each-cache proc::procedure c::%cache)
   (with-access::%cache c (%head)
      (let loop ((h %head))
	 (when (cache-entry? h)
	    (proc h)
	    (loop (cache-entry-next h))))))

;*---------------------------------------------------------------------*/
;*    cache-clear ::cache-file ...                                     */
;*    -------------------------------------------------------------    */
;*    Removes all the entry from the cache                             */
;*---------------------------------------------------------------------*/
(define (cache-clear c::cache)
   (with-access::%cache c (%table %head %tail current-entries)
      (set! %head #f)
      (set! %tail #f)
      (set! current-entries 0))
   (for-each-cache (lambda (ce)
		      (with-access::cache-entry ce (path)
			 (when (file-exists? path)
			    (delete-file path))))
		   c))

;*---------------------------------------------------------------------*/
;*    cache-get ...                                                    */
;*---------------------------------------------------------------------*/
(define (cache-get c path::bstring)
   (if (not (%cache? c))
       (bigloo-type-error 'cache-get '%cache c)
       (with-access::%cache c (%table %head %tail validity)
	  (let ((ce (hashtable-get %table path)))
	     (when (validity ce path)
		(with-access::cache-entry ce (path prev next)
		   (when prev
		      (cache-entry-next-set! prev next)
		      (if next
			  (cache-entry-prev-set! next prev)
			  (set! %tail prev))
		      (set! next %head)
		      (cache-entry-prev-set! %head ce)
		      (set! %head ce))
		   path))))))

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
		   (set! %tail (cache-entry-prev %tail))))
	    (if %tail
		(begin
		   (cache-entry-next-set! %tail ce)
		   (cache-entry-prev-set! ce %tail)
		   (set! %tail ce))
		(begin
		   (set! %head ce)
		   (set! %tail ce)))
	    cpath))))
