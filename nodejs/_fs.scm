;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_fs.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 17 06:10:40 2014                          */
;*    Last change :  Wed Oct  1 18:25:52 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    File system bindings                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__fs

   (library hopscript)

   (import  __nodejs_uv)

   (export O_RDONLY
	   O_WRONLY
	   O_RDWR
	   O_EXCL
	   O_TRUNC
	   O_NOCTTY
	   O_NOFOLLOW
	   O_SYNC
	   O_CREAT
	   O_APPEND
	   O_DIRECTORY

	   S_IFMT
	   S_IFDIR
	   S_IFREG
	   
	   (process-fs ::WorkerHopThread ::JsGlobalObject)))

;*---------------------------------------------------------------------*/
;*    Constants                                                        */
;*---------------------------------------------------------------------*/
(define O_RDONLY
   (cond-expand (bigloo-c (pragma::long "O_RDONLY")) (else 0)))
(define O_WRONLY
   (cond-expand (bigloo-c (pragma::long "O_WRONLY")) (else 1)))
(define O_RDWR
   (cond-expand (bigloo-c (pragma::long "O_RDWR")) (else 2)))
(define O_CREAT
   (cond-expand (bigloo-c (pragma::long "O_CREAT")) (else #o64)))
(define O_EXCL
   (cond-expand (bigloo-c (pragma::long "O_EXCL")) (else #o200)))
(define O_NOCTTY
   (cond-expand (bigloo-c (pragma::long "O_NOCTTY")) (else #o400)))
(define O_TRUNC
   (cond-expand (bigloo-c (pragma::long "O_TRUNC")) (else #o1000)))
(define O_APPEND
   (cond-expand (bigloo-c (pragma::long "O_APPEND")) (else #o2000)))
(define O_DIRECTORY
   (cond-expand (bigloo-c (pragma::long "O_DIRECTORY")) (else #o200000)))
(define O_SYNC
   (cond-expand (bigloo-c (pragma::long "O_SYNC")) (else #o4010000)))
(define O_NOFOLLOW
   (cond-expand (bigloo-c (pragma::long "O_NOFOLLOW")) (else #o400000)))

(define S_IFMT
   (cond-expand (bigloo-c (pragma::long "S_IFMT")) (else #o170000)))
(define S_IFREG
   (cond-expand (bigloo-c (pragma::long "S_IFREG")) (else #o100000)))

(define S_IFDIR
   (cond-expand (bigloo-c (pragma::long "S_IFDIR")) (else #o40000)))

;*---------------------------------------------------------------------*/
;*    process-fs-stats ...                                             */
;*---------------------------------------------------------------------*/
(define process-fs-stats #f)
(define process-fs-fstats #f)

;*---------------------------------------------------------------------*/
;*    get-process-fs-stats ...                                         */
;*---------------------------------------------------------------------*/
(define (get-process-fs-stats %this)
   (with-access::JsGlobalObject %this (js-object)
      (unless process-fs-stats
	 (set! process-fs-stats (js-new %this js-object)))
      process-fs-stats))

;*---------------------------------------------------------------------*/
;*    get-process-fs-fstats ...                                        */
;*---------------------------------------------------------------------*/
(define (get-process-fs-fstats %this)
   (with-access::JsGlobalObject %this (js-object)
      (unless process-fs-fstats
	 (set! process-fs-fstats (js-new %this js-object)))
      process-fs-fstats))

;*---------------------------------------------------------------------*/
;*    process-fs ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/fs.html                                    */
;*---------------------------------------------------------------------*/
(define (process-fs %worker %this)

   (define (rename this old new cb)
      (nodejs-rename-file %worker %this old new cb))
   
   (define (ftruncate this fd offset cb)
      (nodejs-ftruncate %worker %this fd offset cb))
   
   (define (truncate this path offset cb)
      (nodejs-truncate %worker %this path offset cb))
   
   (define (fchown this fd uid gid cb)
      (nodejs-fchown %worker %this fd uid gid cb))
   
   (define (chown this path uid gid cb)
      (nodejs-chown %worker %this path uid gid cb))
   
   (define (lchown this path uid gid cb)
      (nodejs-lchown %worker %this path uid gid cb))
   
   (define (fchmod this fd mod cb)
      (nodejs-fchmod %worker %this fd mod cb))
   
   (define (chmod this path mod cb)
      (nodejs-chmod %worker %this path mod cb))
   
   (define (lchmod this path mod cb)
      (nodejs-lchmod %worker %this path mod cb))
   
   (define (readdir this path cb)
      (let ((l (directory->list path)))
	 (if (and (null? l) (not (directory? path)))
	     (let ((exn (with-access::JsGlobalObject %this (js-error)
			   (let ((obj (js-new %this js-error
					 (format "readdir: cannot read dir ~a"
					    path))))
			      (js-put! obj 'errno 20 #f %this)
			      (js-put! obj 'code "ENOTDIR" #f %this)
			      obj))))
		(if (isa? cb JsFunction)
		    (js-worker-push-thunk! %worker
		       (lambda ()
			  (js-call2 %this cb this exn (js-undefined))))
		    (js-raise exn)))
	     (let ((r (js-vector->jsarray (list->vector l)  %this)))
		(if (isa? cb JsFunction)
		    (js-worker-push-thunk! %worker
		       (lambda ()
			  (js-call2 %this cb this #f r)))
		    r)))))

   (define (fstat this fd callback)
      (nodejs-fstat %worker %this fd callback (get-process-fs-stats %this)))

   (define (stat this path callback)
      (nodejs-stat %worker %this path callback (get-process-fs-stats %this)))

   (define (lstat this path callback)
      (nodejs-lstat %worker %this path callback (get-process-fs-stats %this)))

   (define (link this src dst callback)
      (nodejs-link %worker %this src dst callback))

   (define (symlink this src dst type callback)
      (if (eq? callback (js-undefined))
	  (nodejs-symlink %worker %this src dst type)
	  (nodejs-symlink %worker %this src dst callback)))

   (define (readlink this path callback)
      (nodejs-readlink %worker %this path callback))

   (define (unlink this path callback)
      (nodejs-unlink %worker %this path callback))

   (define (rmdir this path callback)
      (nodejs-rmdir %worker %this path callback))

   (define (mkdir this path mode callback)
      (if (eq? callback (js-undefined))
	  (if (isa? mode JsFunction)
	      (nodejs-mkdir %worker %this path #o777 mode)
	      (nodejs-mkdir %worker %this path mode #f))
	  (nodejs-mkdir %worker %this path mode callback)))

   (define (close this fd callback)
      (nodejs-fs-close %worker %this fd callback))

   (define (open this path flags mode callback)
      (nodejs-open %worker %this path flags mode callback))

   (define (utimes this path atime mtime callback)
      (nodejs-utimes %worker %this path atime mtime callback))
   
   (define (futimes this fd atime mtime callback)
      (nodejs-futimes %worker %this fd atime mtime callback))
   
   (define (fsync this fd callback)
      (nodejs-fsync %worker %this fd callback))
   
   (define (write this fd buffer offset length position callback)
      (nodejs-write %worker %this fd buffer offset length
	 (if (eq? position (js-undefined))
	     -1
	     (int32->fixnum (js-toint32 position %this)))
	 callback))
;*       (tprint "write fd=" fd " offset=" offset " length=" length    */
;* 	 " position=" position)                                        */
;*       (unless (= position 0)                                        */
;* 	 (set-output-port-position! fd position))                      */
;*       (display-substring buffer offset (+ offset length) fd))       */
   
   (define (read this fd buffer offset length position callback)
      (nodejs-read %worker %this fd buffer
	 (int32->fixnum (js-toint32 offset %this))
	 (int32->fixnum (js-toint32 length %this))
	 (if (eq? position (js-undefined))
	     -1
	     (int32->fixnum (js-toint32 position %this)))
	 callback))

   (js-alist->jsobject
      `((rename . ,(js-make-function %this rename 2 "rename"))
	(ftruncate . ,(js-make-function %this ftruncate 2 "ftruncate"))
	(truncate . ,(js-make-function %this truncate 2 "truncate"))
	(chown . ,(js-make-function %this chown 3 "chown"))
	(fchown . ,(js-make-function %this fchown 3 "fchown"))
	(lchown . ,(js-make-function %this lchown 3 "lchown"))
	(chmod . ,(js-make-function %this chmod 2 "chmod"))
	(fchmod . ,(js-make-function %this fchmod 2 "fchmod"))
	(lchmod . ,(js-make-function %this lchmod 2 "lchmod"))
	(fstat . ,(js-make-function %this fstat 2 "fstat"))
	(stat . ,(js-make-function %this stat 2 "stat"))
	(lstat . ,(js-make-function %this lstat 2 "lstat"))
	(link . ,(js-make-function %this link 3 "link"))
	(symlink . ,(js-make-function %this symlink 4 "symlink"))
	(readlink . ,(js-make-function %this readlink 2 "readlink"))
	(unlink . ,(js-make-function %this unlink 2 "unlink"))
	(rmdir . ,(js-make-function %this rmdir 2 "rmdir"))
	(mkdir . ,(js-make-function %this mkdir 3 "mkdir"))
	(readdir . ,(js-make-function %this readdir 1 "readdir"))
	(Stats . ,(js-alist->jsobject `((prototype . ,(get-process-fs-stats %this))) %this))
	(close . ,(js-make-function %this close 2 "close"))
	(utimes . ,(js-make-function %this utimes 4 "utimes"))
	(futimes . ,(js-make-function %this futimes 4 "futimes"))
	(fsync . ,(js-make-function %this fsync 1 "fsync"))
	(write . ,(js-make-function %this write 5 "write"))
	
	(open . ,(js-make-function %this open 4 "open"))
	(read . ,(js-make-function %this read 6 "read"))
	
	)
      %this))
