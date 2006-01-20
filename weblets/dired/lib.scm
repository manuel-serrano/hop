;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/lib.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 17:36:19 2005                          */
;*    Last change :  Fri Oct  7 06:10:00 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The dired hop library.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    dired-install-directory ...                                      */
;*---------------------------------------------------------------------*/
(define dired-install-directory
   (let ((dir (make-file-name (hop-weblets-directory) "dired")))
      (lambda ()
	 dir)))

;*---------------------------------------------------------------------*/
;*    *dired-icon-directory* ...                                       */
;*---------------------------------------------------------------------*/
(define *dired-icon-directory*
   (make-file-path (dired-install-directory) "icons" "apps"))

;*---------------------------------------------------------------------*/
;*    dired-app-icon ...                                               */
;*---------------------------------------------------------------------*/
(define (dired-app-icon icon)
   (make-file-name *dired-icon-directory* icon))

;*---------------------------------------------------------------------*/
;*    dired-mozilla-client? ...                                        */
;*---------------------------------------------------------------------*/
(define (dired-mozilla-client? req)
   (with-access::http-request req (header)
      (let ((c (assq user-agent: header)))
	 (and (pair? c) (substring-at? (cdr c) "Mozilla" 0)))))

;*---------------------------------------------------------------------*/
;*    dired-directory->list ...                                        */
;*---------------------------------------------------------------------*/
(define (dired-directory->list dir)
   (let ((fs (directory->list dir)))
      (if (null? *dired-icon-directory*)
	  fs
	  (filter! (lambda (f)
		      (not (member f *dired-hidden-directories*)))
		   fs))))

;*---------------------------------------------------------------------*/
;*    dired-canonicalize-dirname ...                                   */
;*---------------------------------------------------------------------*/
(define (dired-canonicalize-dirname path)
   (let ((len (string-length path)))
      (cond
	 ((=fx len 0)
	  path)
	 ((char=? (string-ref path (-fx len 1)) (file-separator))
	  (if (=fx len 1)
	      ""
	      (substring path 0 (-fx len 1))))
	 (else
	  path))))

;*---------------------------------------------------------------------*/
;*    dired-get-files ...                                              */
;*---------------------------------------------------------------------*/
(define (dired-get-files path hide sort-by order)
   (define (compare cmp)
      (if (eq? order 'increase)
	  (lambda (f1 f2)
	     (cmp (make-file-name path f1) (make-file-name path f2)))
	  (lambda (f1 f2)
	     (not (cmp (make-file-name path f1) (make-file-name path f2))))))
   (let ((files (dired-directory->list path)))
      (sort (if hide
		(filter (lambda (s)
			   (and (>fx (string-length s) 1)
				(not (char=? (string-ref s 0) #\.))))
			files)
		files)
	    (case sort-by
	       ((name)
		(if (eq? order 'increase) string<? string>?))
	       ((date)
		(compare (lambda (f1 f2)
			    (>second
			     (file-modification-time f1)
			     (file-modification-time f2)))))
	       ((size)
		(compare (lambda (f1 f2)
			    (> (file-size f1) (file-size f2)))))
	       ((user)
		(compare (lambda (f1 f2)
			    (>fx (file-uid f1) (file-uid f2)))))
	       (else
		(if (eq? order 'increase) string<? string>?))))))

;*---------------------------------------------------------------------*/
;*    linux-mode ...                                                   */
;*---------------------------------------------------------------------*/
(define (linux-mode mode)
   (define linux-mode-table
      '((S_IFSOCK #o0140000 #\s 0)
	(S_IFCHR #o0020000 #\c 0)
	(S_IFLNK #o0120000 #\l 0)
	(S_IFBLK #o0060000 #\b 0)
	(S_IFDIR #o0040000 #\d 0)
	(S_IFIFO #o0010000 #\p 0)
	(S_ISUID #o0004000 #\s 1)
	(S_ISGID #o0002000 #\S 1)
	(S_IRUSR #o00400 #\r 1)
	(S_IWUSR #o00200 #\w 2)
	(S_IXUSR #o00100 #\x 3)
	(S_IRGRP #o00040 #\r 4)
	(S_IWGRP #o00020 #\w 5)
	(S_IXGRP #o00010 #\x 6)
	(S_IROTH #o00004 #\r 7)
	(S_IWOTH #o00002 #\w 8)
	(S_IXOTH #o00001 #\x 9)))
   (let ((r (make-string 10 #\-)))
      ;; file type
      (let loop ((table linux-mode-table))
	 (if (null? table)
	     r
	     (let ((m (cadr (car table))))
		(if (=fx (bit-and mode m) m)
		    (string-set! r (cadddr (car table)) (caddr (car table))))
		(loop (cdr table)))))))

;*---------------------------------------------------------------------*/
;*    dired-plist-assq ...                                             */
;*---------------------------------------------------------------------*/
(define (dired-plist-assq k l)
   (and (pair? l)
	(let ((r (memq k l)))
	   (and (pair? r) (pair? (cdr r)) (cadr r)))))

;*---------------------------------------------------------------------*/
;*    dired-cookie->exp ...                                            */
;*---------------------------------------------------------------------*/
(define (dired-cookie->exp req name)
   (let ((cookie (http-cookie-get req name #f #f)))
      (if (string? cookie)
	  (let ((p (open-input-string cookie)))
	     (let ((l (read p)))
		(close-input-port p)
		l))
	  #unspecified)))

;*---------------------------------------------------------------------*/
;*    dired-cookie-val ...                                             */
;*---------------------------------------------------------------------*/
(define (dired-cookie-val req name key def)
   (let ((cookie (http-cookie-get req name #f #f)))
      (if (string? cookie)
	  (let ((p (open-input-string cookie)))
	     (let* ((e (read p))
		    (l (memq key e))
		    (v (and (pair? l) (pair? (cdr l)) (cadr l))))
		(close-input-port p)
		v))
	  def)))
   
;*---------------------------------------------------------------------*/
;*    dired-sysadmin ...                                               */
;*---------------------------------------------------------------------*/
(define (dired-sysadmin req action path)
   (with-access::http-request req (user)
      (if (user-authorized-service? user 'admin)
	  (action req '())
	  (user-access-denied req))))

