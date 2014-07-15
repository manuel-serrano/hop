;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/nodejs/_fs.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 17 06:10:40 2014                          */
;*    Last change :  Thu Jul 10 15:25:15 2014 (serrano)                */
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

(define ENOENT
   (cond-expand (bigloo-c (pragma::long "ENOENT")) (else 2)))

;*---------------------------------------------------------------------*/
;*    process-fs-stats ...                                             */
;*---------------------------------------------------------------------*/
(define process-fs-stats #f)

;*---------------------------------------------------------------------*/
;*    get-process-fs-stats ...                                         */
;*---------------------------------------------------------------------*/
(define (get-process-fs-stats %this)
   (with-access::JsGlobalObject %this (js-object)
      (unless process-fs-stats (set! process-fs-stats (js-new %this js-object)))
      process-fs-stats))

;*---------------------------------------------------------------------*/
;*    process-fs ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/fs.html                                    */
;*---------------------------------------------------------------------*/
(define (process-fs %this)

   (define (rename this old new cb)
      (if (isa? cb JsFunction)
	  ;; asynchronous call
	  (nodejs-rename-file old new
	     (lambda (res path)
		(if (=fx res 0)
		    (js-call1 %this cb this (js-undefined))
		    (with-access::JsGlobalObject %this (js-error)
		       (let ((obj (js-new %this js-error
				     (format "Cannot rename file ~s into ~a"
					old new)
				     new)))
			  (js-put! obj 'path old #f %this)
			  (unless (file-exists? old)
			     (js-put! obj 'errno ENOENT #f %this)
			     (js-put! obj 'code "ENOENT" #f %this))
			  (js-call1 %this cb this obj))))))
	  ;; synchronous call
	  (if (rename-file old new)
	      (js-call1 %this cb (js-undefined) (js-undefined))
	      (with-access::JsGlobalObject %this (js-error)
		 (let ((obj (js-new %this js-error
			       (format "Cannot rename file ~s into ~a" old new)
			       new)))
		    (js-put! obj 'path old #f %this)
		    (unless (file-exists? old)
		       (js-put! obj 'errno ENOENT #f %this)
		       (js-put! obj 'code "ENOENT" #f %this))
		    (js-call1 %this cb this obj))))))
   
   (define (ftruncate this path len cb)
      (let ((r (output-port-truncate path len)))
	 (when (isa? cb JsFunction)
	    (js-call1 %this cb %this (js-toboolean r)))))
   
   (define (readdir this path)
      (js-vector->jsarray (list->vector (directory->path-list path))  %this))

   (define (lstat this path)
      (let ((obj (js-alist->jsobject
		    `((mode . ,(if (directory? path) S_IFDIR S_IFREG)))
		    %this)))
	 (with-access::JsObject obj (__proto__)
	    (set! __proto__ (get-process-fs-stats %this))
	    obj)))

   (define (stat this path)
      (when (file-exists? path)
	 (let ((obj (js-alist->jsobject
		       `((size . ,(elong->fixnum (file-size path))))
		       %this)))
	    (with-access::JsObject obj (__proto__)
	       (set! __proto__ (get-process-fs-stats %this))
	       obj))))

   (define (close this fd)
      (cond
	 ((output-port? fd) (close-output-port fd))
	 ((input-port? fd) (close-input-port fd))))

   (define (open this path flags mode)
      (cond
	 ((not (integer? flags))
	  (error "open" "wrong flag" flags))
	 ((=fx flags O_RDONLY)
	  (open-input-file path))
	 ((=fx flags O_WRONLY)
	  (open-output-file path))
	 ((=fx flags O_APPEND)
	  (append-output-file path))
	 (else
	  (error "open" "flags not implemented" flags))))

   (define (read this fd buffer offset length position)
      (unless (= position 0)
	 (set-input-port-position! fd position))
      (let ((fast-buffer (js-get buffer '%fast-buffer %this)))
	 (read-fill-string! fast-buffer offset length fd)))

   (define (write this fd buffer offset length position)
      (unless (= position 0)
	 (set-output-port-position! fd position))
      (display-substring buffer offset (+ offset length) fd))

   (define (fsync this fd)
      (cond
	 ((output-port? fd) (flush-output-port fd))))
   
   (js-alist->jsobject
      `((rename . ,(js-make-function %this rename 2 "rename"))
	(truncate . ,(js-make-function %this truncate 2 "truncate"))
	(readdir . ,(js-make-function %this readdir 1 "readdir"))
	(Stats . ,(js-alist->jsobject `((prototype . ,(get-process-fs-stats %this))) %this))
	(lstat . ,(js-make-function %this lstat 1 "lstat"))
	(stat . ,(js-make-function %this stat 1 "stat"))
	(close . ,(js-make-function %this close 1 "close"))
	(open . ,(js-make-function %this open 3 "open"))
	(read . ,(js-make-function %this read 5 "read"))
	(write . ,(js-make-function %this write 5 "write"))
	(fsync . ,(js-make-function %this fsync 0 "fsync")))
      %this))
