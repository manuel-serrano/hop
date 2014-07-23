;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_fs.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 17 06:10:40 2014                          */
;*    Last change :  Wed Jul 23 16:24:51 2014 (serrano)                */
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
	   
	   (process-fs ::JsGlobalObject)))

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
(define (process-fs %this)

   (define (rename this old new cb)
      (nodejs-rename-file %this old new cb))
   
   (define (ftruncate this fd offset cb)
      (nodejs-ftruncate %this fd offset cb))
   
   (define (truncate this path offset cb)
      (nodejs-truncate %this path offset cb))
   
   (define (fchown this fd uid gid cb)
      (nodejs-fchown %this fd uid gid cb))
   
   (define (chown this path uid gid cb)
      (nodejs-fchown %this path uid gid cb))
   
   (define (readdir this path)
      (js-vector->jsarray (list->vector (directory->path-list path))  %this))

   (define (lstat this path callback)
      (let ((obj (js-alist->jsobject
		    `((mode . ,(if (directory? path) S_IFDIR S_IFREG)))
		    %this)))
	 (with-access::JsObject obj (__proto__)
	    (set! __proto__ (get-process-fs-stats %this))
	    (if (isa? callback JsFunction)
		(js-call2 %this callback (js-undefined) #f obj)
		obj))))

   (define (stat this path callback)
      (cond
	 ((file-exists? path)
	  (let ((obj (js-alist->jsobject
			`((size . ,(elong->fixnum (file-size path))))
			%this)))
	     (with-access::JsObject obj (__proto__)
		(set! __proto__ (get-process-fs-stats %this))
		(if (isa? callback JsFunction)
		    (js-call2 %this callback (js-undefined) #f obj)
		    obj))))
	 ((isa? callback JsFunction)
	  (js-call2 %this callback (js-undefined) "File does not exists" #f))
	 (else
	  #f)))

   (define (fstat this fd callback)
      (nodejs-fstat %this fd callback))

   (define (close this fd callback)
      (nodejs-close %this fd callback))

   (define (open this path flags mode callback)
      (nodejs-open %this path flags mode callback))

   (define (read this fd buffer offset length position callback)
      (nodejs-read %this fd buffer
	 (int32->fixnum (js-toint32 offset %this))
	 (int32->fixnum (js-toint32 length %this))
	 (int32->fixnum (js-toint32 position %this))
	 callback))

   (define (write this fd buffer offset length position)
      (unless (= position 0)
	 (set-output-port-position! fd position))
      (display-substring buffer offset (+ offset length) fd))

   (define (fsync this fd)
      (cond
	 ((output-port? fd) (flush-output-port fd))))
   
   (js-alist->jsobject
      `((rename . ,(js-make-function %this rename 2 "rename"))
	(ftruncate . ,(js-make-function %this ftruncate 2 "ftruncate"))
	(truncate . ,(js-make-function %this truncate 2 "truncate"))
	(fchown . ,(js-make-function %this fchown 3 "fchown"))
	(chown . ,(js-make-function %this chown 3 "chown"))
	(readdir . ,(js-make-function %this readdir 1 "readdir"))
	(Stats . ,(js-alist->jsobject `((prototype . ,(get-process-fs-stats %this))) %this))
	(lstat . ,(js-make-function %this lstat 2 "lstat"))
	(stat . ,(js-make-function %this stat 2 "stat"))
	(fstat . ,(js-make-function %this fstat 2 "fstat"))
	(close . ,(js-make-function %this close 2 "close"))
	(open . ,(js-make-function %this open 4 "open"))
	(read . ,(js-make-function %this read 6 "read"))
	(write . ,(js-make-function %this write 5 "write"))
	(fsync . ,(js-make-function %this fsync 0 "fsync")))
      %this))
