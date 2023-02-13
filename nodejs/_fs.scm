;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/nodejs/_fs.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 17 06:10:40 2014                          */
;*    Last change :  Mon Feb 13 08:47:09 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    File system bindings                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __nodejs__fs

   (library hopscript)

   (include "nodejs_async.sch" "nodejs_types.sch")
   (include "../hopscript/stringthread.sch")
   
   (import  __nodejs_uv
	    __nodejs_process)

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
	   S_IFBLK
	   S_IFCHR
	   S_IFLNK
	   S_IFIFO
	   S_IFSOCK
	   
	   (process-fs ::WorkerHopThread ::JsGlobalObject ::JsObject)))

;*---------------------------------------------------------------------*/
;*    &begin!                                                          */
;*---------------------------------------------------------------------*/
(define __js_strings (&begin!))

;*---------------------------------------------------------------------*/
;*    Constants                                                        */
;*---------------------------------------------------------------------*/
(define O_RDONLY
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_RDONLY")) (else 0)))
(define O_WRONLY
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_WRONLY")) (else 1)))
(define O_RDWR
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_RDWR")) (else 2)))
(define O_CREAT
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_CREAT")) (else #o64)))
(define O_EXCL
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_EXCL")) (else #o200)))
(define O_NOCTTY
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_NOCTTY")) (else #o400)))
(define O_TRUNC
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_TRUNC")) (else #o1000)))
(define O_APPEND
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_APPEND")) (else #o2000)))
(define O_DIRECTORY
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_DIRECTORY")) (else #o200000)))
(define O_SYNC
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_SYNC")) (else #o4010000)))
(define O_NOFOLLOW
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "O_NOFOLLOW")) (else #o400000)))

(define S_IFMT
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFMT")) (else #o170000)))
(define S_IFREG
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFREG")) (else #o100000)))
(define S_IFDIR
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFDIR")) (else #o40000)))
(define S_IFBLK
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFBLK")) (else #o60000)))
(define S_IFCHR
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFCHR")) (else #o20000)))
(define S_IFLNK
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFLNK")) (else #o120000)))
(define S_IFIFO
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFIFO")) (else #o10000)))
(define S_IFSOCK
   (cond-expand
      ((and enable-libuv bigloo-c) (pragma::long "S_IFSOCK")) (else #o140000)))

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
;*    8bits-encode-utf8 ...                                            */
;*---------------------------------------------------------------------*/
(define (8bits-encode-utf8 str::bstring start end)
   (let ((s (substring str start end)))
      (if (utf8-string? str #t)
	  s
	  (iso-latin->utf8! s))))

;*---------------------------------------------------------------------*/
;*    string->ucs2-string ...                                          */
;*    -------------------------------------------------------------    */
;*    Convert a LE 8bits strings into an equivalent UCS2 string.       */
;*---------------------------------------------------------------------*/
(define (string->ucs2-string string::bstring start end)
   (let* ((len (*fx (/fx (-fx end start) 2) 2))
	  (res (make-ucs2-string (/fx len 2))))
      (let loop ((i 0))
	 (if (=fx i len)
	     res
	     (let* ((j (+fx start i))
		    (c0 (char->integer (string-ref string j)))
		    (c1 (char->integer (string-ref string (+fx j 1))))
		    (ucs2 (+fx (bit-lsh c1 8) c0)))
		(ucs2-string-set! res (/fx i 2) (integer->ucs2 ucs2))
		(loop (+fx i 2)))))))
   
;*---------------------------------------------------------------------*/
;*    process-fs ...                                                   */
;*    -------------------------------------------------------------    */
;*    http://nodejs.org/api/fs.html                                    */
;*---------------------------------------------------------------------*/
(define (process-fs %worker %this process)
   
   (define (rename this old new cb)
      (nodejs-rename-file %worker %this process old new cb))
   
   (define (ftruncate this fd offset cb)
      (nodejs-ftruncate %worker %this process fd offset cb))
   
   (define (truncate this path offset cb)
      (nodejs-truncate %worker %this process path offset cb))
   
   (define (fchown this fd uid gid cb)
      (nodejs-fchown %worker %this process fd uid gid cb))
   
   (define (chown this path uid gid cb)
      (nodejs-chown %worker %this process path uid gid cb))
   
   (define (lchown this path uid gid cb)
      (nodejs-lchown %worker %this process path uid gid cb))
   
   (define (fchmod this fd mod cb)
      (nodejs-fchmod %worker %this process fd mod cb))
   
   (define (chmod this path mod cb)
      (nodejs-chmod %worker %this process path mod cb))
   
   (define (lchmod this path mod cb)
      (nodejs-lchmod %worker %this process path mod cb))
   
   (define (readdir this jspath opt cb)
      (let* ((path (js-tostring jspath %this))
	     (v (directory->vector path)))
	 (when (js-jsobject? opt)
	    (let ((enc (js-get-jsobject opt opt (& "encoding") %this)))
	       (cond
		  ((eq? enc (& "utf8"))
		   (vector-map! (lambda (data)
				   (multiple-value-bind (str enc)
				      (utf8-normalize-utf16 data #t 0 (string-length data))
				      str))
		      v))
		  ((eq? enc (& "binary"))
		   (vector-map! (lambda (data)
				   (8bits-encode-utf8 data 0 (string-length data)))
		      v))
		  ((eq? enc (& "ascii"))
		   v)
		  ((eq? enc (& "hex"))
		   (vector-map! (lambda (data)
				   (string-hex-intern data))
		      v))
		  ((eq? enc (& "ucs2"))
		   (vector-map! (lambda (data)
				   (ucs2-string->utf8-string
				      (string->ucs2-string data 0 (string-length data))))
		      v))
		  ((eq? enc (& "latin1"))
		   (vector-map! (lambda (data)
				   (iso-latin->utf8 data))
		      v))
		  ((eq? enc (& "base64"))
		   (vector-map! (lambda (data)
				   (let ((ip (open-input-string! data 0 (string-length data)))
					 (op (open-output-string)))
				      (base64-encode-port ip op 0)
				      (close-output-port op)))
		      v))
		  (else
		   (js-raise-type-error %this
		      (format "Bad encoding \"~a\"" enc)
		      enc)))))
	 (if (and (=fx (vector-length v) 0) (not (directory? path)))
	     (let ((exn (with-access::JsGlobalObject %this (js-error)
			   (let ((obj (js-new %this js-error
					 (js-string->jsstring
					    (format "readdir: cannot read dir ~a"
					       path)))))
			      (js-put! obj (& "errno") 20 #f %this)
			      (js-put! obj (& "code") (js-string->jsstring "ENOTDIR") #f %this)
			      obj))))
		(if (js-procedure? cb)
		    (js-worker-push-thunk! %worker "readdir"
		       (lambda ()
			  (js-call2-jsprocedure %this cb this exn (js-undefined))))
		    (js-raise exn)))
	     (let ((r (js-vector->jsarray (vector-map! js-string->jsstring v) %this)))
		(if (js-procedure? cb)
		    (js-worker-push-thunk! %worker "readdir"
		       (lambda ()
			  (js-call2-jsprocedure %this cb this #f r)))
		    r)))))
   
   (define (fstat this fd callback)
      (nodejs-fstat %worker %this process
	 fd callback (get-process-fs-stats %this)))
   
   (define (stat this path callback)
      (nodejs-stat %worker %this process
	 path callback (get-process-fs-stats %this)))
   
   (define (lstat this path callback)
      (nodejs-lstat %worker %this process
	 path callback (get-process-fs-stats %this)))
   
   (define (link this src dst callback)
      (nodejs-link %worker %this process src dst callback))
   
   (define (symlink this src dst type callback)
      (if (eq? callback (js-undefined))
	  (nodejs-symlink %worker %this process src dst type)
	  (nodejs-symlink %worker %this process src dst callback)))
   
   (define (readlink this path callback)
      (nodejs-readlink %worker %this process path callback))
   
   (define (unlink this path callback)
      (nodejs-unlink %worker %this process path callback))
   
   (define (rmdir this path recursive mode callback)
      (nodejs-rmdir %worker %this process path recursive mode callback))
   
   (define (fdatasync this path callback)
      (nodejs-fdatasync %worker %this process path callback))
   
   (define (mkdir this path mode recursive callback)
      (if (eq? callback (js-undefined))
	  (if (js-procedure? mode)
	      (nodejs-mkdir %worker %this process path #o777 recursive mode)
	      (nodejs-mkdir %worker %this process path mode recursive #f))
	  (nodejs-mkdir %worker %this process path mode recursive callback)))
   
   (define (close this fd callback)
      (if (fixnum? fd)
	  (nodejs-fs-close %worker %this process fd callback)
	  (let ((fd (js-tointeger fd %this)))
	     (nodejs-fs-close %worker %this process fd callback))))

   (define (copyfile this src dest mode callback ctx)
      (nodejs-fs-copyfile %worker %this process src dest mode callback))
   
   (define (open this path flags mode callback)
      (unless (integer? flags)
	 (js-raise-type-error %this
	    "Illegal flags (~a)" (typeof flags)))
      (nodejs-open %worker %this process path flags mode callback))
   
   (define (utimes this path atime mtime callback)
      (nodejs-utimes %worker %this process path atime mtime callback))
   
   (define (futimes this fd atime mtime callback)
      (nodejs-futimes %worker %this process fd atime mtime callback))
   
   (define (fsync this fd callback)
      (nodejs-fsync %worker %this process fd callback))
   
   (define (write this fd buffer offset length position callback)
      (nodejs-write %worker %this process fd buffer offset length
	 position
	 callback))
   
   (define (writeString this fd buffer offset length position callback)
      (nodejs-write-string %worker %this process fd buffer offset length
	 position
	 callback))
   
   (define (read this fd buffer offset length position callback)
      (nodejs-read %worker %this process fd buffer
	 (int32->fixnum (js-toint32 offset %this))
	 (int32->fixnum (js-toint32 length %this))
	 position
	 callback))

   (define (create-fs-watcher-proto)
      (with-access::JsGlobalObject %this (js-object)
	 (with-access::JsFunction js-object (prototype constrmap)
	    (let ((obj (js-make-jsobject 2 constrmap prototype)))
	       
	       (js-put! obj (& "start")
		  (js-make-function %this
		     (lambda (this::JsHandle path options interval)
			(with-access::JsHandle this (handle)
			   (nodejs-fs-poll-start %this (get-process-fs-stats %this)
			      handle
			      (js-tostring path %this)
			      (lambda (_ status prev curr)
				 (let ((onchange (js-get this (& "onchange") %this)))
				    (unless (=fx status 0)
				       (js-put! process (& "_errno")
					  (nodejs-err-name status)
					  #f %this))
				    (!js-callback3 'fs-watcher %worker %this
				       onchange this curr prev status)))
			      interval))
			(unless (js-totest options)
			   (with-access::JsHandle this (handle)
			      (nodejs-unref handle %worker))))
		     (js-function-arity 3 0)
		     (js-function-info :name "start" :len 3))
		  #f %this)
	       
	       (js-put! obj (& "stop")
		  (js-make-function %this
		     (lambda (this)
			(let ((onstop (js-get this (& "onstop") %this)))
			   (when (js-procedure? onstop)
			      (!js-callback0 'fs-watcher %worker %this
				 onstop this)))
			(with-access::JsHandle this (handle)
			   (nodejs-fs-poll-stop handle)))
		     (js-function-arity 0 0)
		     (js-function-info :name "stop" :len 0))
		  #f %this)
	       
	       obj))))
   
   (define (get-fs-watcher-proto process)
      (with-access::JsProcess process (fs-watcher-proto)
	 (unless fs-watcher-proto
	    (set! fs-watcher-proto (create-fs-watcher-proto)))
	 fs-watcher-proto))
   
   (define (fs-watcher this)
      (instantiateJsHandle
	 (handle (nodejs-make-fs-poll %worker))
	 (cmap (js-make-jsconstructmap))
	 (__proto__ (get-fs-watcher-proto process))))

   (set! __js_strings (&init!))

   (let ((proto (js-alist->jsobject
		   `((rename . ,(js-make-function %this rename
				   (js-function-arity rename)
				   (js-function-info :name "rename" :len 2)))
		     (ftruncate . ,(js-make-function %this ftruncate
				      (js-function-arity ftruncate)
				      (js-function-info :name "ftruncate" :len 2)))
		     (truncate . ,(js-make-function %this truncate
				     (js-function-arity truncate)
				     (js-function-info :name "truncate" :len 2)))
		     (chown . ,(js-make-function %this chown
				  (js-function-arity chown)
				  (js-function-info :name "chown" :len 3)))
		     (fchown . ,(js-make-function %this fchown
				   (js-function-arity fchown)
				   (js-function-info :name "fchown" :len 3)))
		     (lchown . ,(js-make-function %this lchown
				   (js-function-arity lchown)
				   (js-function-info :name "lchown" :len 3)))
		     (chmod . ,(js-make-function %this chmod
				  (js-function-arity chmod)
				  (js-function-info :name "chmod" :len 2)))
		     (fchmod . ,(js-make-function %this fchmod
				   (js-function-arity fchmod)
				   (js-function-info :name "fchmod" :len 2)))
		     (lchmod . ,(js-make-function %this lchmod
				   (js-function-arity lchmod)
				   (js-function-info :name "lchmod" :len 2)))
		     (fstat . ,(js-make-function %this fstat
				  (js-function-arity fstat)
				  (js-function-info :name "fstat" :len 2)))
		     (stat . ,(js-make-function %this stat
				 (js-function-arity stat)
				 (js-function-info :name "stat" :len 2)))
		     (lstat . ,(js-make-function %this lstat
				  (js-function-arity lstat)
				  (js-function-info :name "lstat" :len 2)))
		     (link . ,(js-make-function %this link
				 (js-function-arity link)
				 (js-function-info :name "link" :len 3)))
		     (symlink . ,(js-make-function %this symlink
				    (js-function-arity symlink)
				    (js-function-info :name "symlink" :len 4)))
		     (readlink . ,(js-make-function %this readlink
				     (js-function-arity readlink)
				     (js-function-info :name "readlink" :len 2)))
		     (unlink . ,(js-make-function %this unlink
				   (js-function-arity unlink)
				   (js-function-info :name "unlink" :len 2)))
		     (rmdir . ,(js-make-function %this rmdir
				  (js-function-arity rmdir)
				  (js-function-info :name "rmdir" :len 3)))
		     (fdatasync . ,(js-make-function %this fdatasync
				      (js-function-arity fdatasync)
				      (js-function-info :name "fdatasync" :len 2)))
		     (mkdir . ,(js-make-function %this mkdir
				  (js-function-arity mkdir)
				  (js-function-info :name "mkdir" :len 3)))
		     (readdir . ,(js-make-function %this readdir
				    (js-function-arity readdir)
				    (js-function-info :name "readdir" :len 1)))
		     (Stats . ,(js-make-function %this (lambda (this) this)
				  (js-function-arity 0 0)
				  (js-function-info :name "Stats" :len 0)
				  :alloc (lambda (%this o) #unspecified)
				  :prototype (get-process-fs-stats %this)))
		     (close . ,(js-make-function %this close
				  (js-function-arity close)
				  (js-function-info :name "close" :len 2)))
		     (copyFile . ,(js-make-function %this copyfile
				     (js-function-arity copyfile)
				     (js-function-info :name "copyFile" :len 5)))
		     (utimes . ,(js-make-function %this utimes
				   (js-function-arity utimes)
				   (js-function-info :name "utimes" :len 4)))
		     (futimes . ,(js-make-function %this futimes
				    (js-function-arity futimes)
				    (js-function-info :name "futimes" :len 4)))
		     (fsync . ,(js-make-function %this fsync
				  (js-function-arity fsync)
				  (js-function-info :name "fsync" :len 1)))
		     (write . ,(js-make-function %this write
				  (js-function-arity write)
				  (js-function-info :name "write" :len 5)))
		     (writeString . ,(js-make-function %this writeString
					(js-function-arity writeString)
					(js-function-info :name "writeString" :len 5)))
		     (open . ,(js-make-function %this open
				 (js-function-arity open)
				 (js-function-info :name "open" :len 4)))
		     (read . ,(js-make-function %this read
				 (js-function-arity read)
				 (js-function-info :name "read" :len 6)))
		     (StatWatcher . ,(js-make-function %this fs-watcher
					(js-function-arity fs-watcher)
					(js-function-info :name "StatWatcher" :len 0)
					:alloc (lambda (%this o) #unspecified))))
		   %this)))
      ;; Nodejs is compiled in -Os mode and in this optimization mode
      ;; function are only cached if found in the prototype object
      ;; then to get faster code, binding methods are stored in
      ;; its prototype.
      (js-make-jsobject 0 (js-make-jsconstructmap) proto)))

;*---------------------------------------------------------------------*/
;*    &end!                                                            */
;*---------------------------------------------------------------------*/
(&end!)

