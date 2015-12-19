;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/misc.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 15 11:28:31 2004                          */
;*    Last change :  Sat Dec 12 13:33:30 2015 (serrano)                */
;*    Copyright   :  2004-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP misc                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_misc

   (include "param.sch")

   (cond-expand
      (enable-ssl (library ssl)))

   (import  __hop_configure
	    __hop_param
	    __hop_types)

   (extern  (macro fork::int () "fork"))

   (export  (verb-mutex::mutex)
	    (hop-verb ::int . args)
	    (hop-color ::obj ::obj ::obj)
	    (shortest-prefix ::bstring)
	    (longest-suffix ::bstring)
	    (is-suffix?::bool ::bstring ::bstring)
	    (is-suffix-ci?::bool ::bstring ::bstring)
	    (suffix-assoc ::bstring ::pair-nil)
	    (suffix-assoc-ci ::bstring ::pair-nil)
	    (suffix-member ::bstring ::pair-nil)
	    (suffix-member-ci ::bstring ::pair-nil)
	    (string-member? ::bstring ::bstring)
	    (string-member-ci? ::bstring ::bstring)
	    (string-escape::bstring ::bstring ::char)
	    (delete-path ::bstring)
	    (make-cache-name::bstring #!optional name)
	    (make-url-name::bstring ::bstring ::bstring)
	    (make-hop-url-name::bstring ::bstring)
	    (make-client-socket/timeout ::bstring ::int ::int ::obj ::bool)
	    (ipv4->elong::elong ::bstring)
	    (inline micro-seconds::int ::int)
	    (inline input-timeout-set! ::input-port ::int)
	    (inline output-timeout-set! ::output-port ::int)
	    (inline socket-timeout-set! ::socket ::int ::int)
	    (socket-buffers-detach! ::socket)
	    (call-in-background ::procedure)))

;*---------------------------------------------------------------------*/
;*    *verb-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *verb-mutex* (make-mutex "verb"))

;*---------------------------------------------------------------------*/
;*    verb-mutex ...                                                   */
;*---------------------------------------------------------------------*/
(define (verb-mutex)
   *verb-mutex*)

;*---------------------------------------------------------------------*/
;*    hop-verb ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-verb level . args)
   (when (>=fx (hop-verbose) level)
      (synchronize *verb-mutex*
	 (for-each (lambda (a) (display a (current-error-port))) args)
	 (flush-output-port (current-error-port)))))

;*---------------------------------------------------------------------*/
;*    hop-color ...                                                    */
;*---------------------------------------------------------------------*/
(define (hop-color col req msg)
   (let ((c (cond
	       ((integer? col)
		(+fx (modulo col 16) 1))
	       ((isa? col http-request)
		(with-access::http-request col (id)
		   (+fx (modulo id 16) 1)))
	       (else
		1))))
      (trace-color c
	 (if (isa? req http-request)
	     (with-access::http-request req (id) id)
	     req)
	 msg)))

;*---------------------------------------------------------------------*/
;*    shortest-prefix ...                                              */
;*---------------------------------------------------------------------*/
(define (shortest-prefix string)
   (let* ((len (string-length string))
          (len-1 (-fx len 1)))
      (let loop ((read len-1)
		 (last #f))
         (cond
            ((<fx read 0)
	     (if (not last)
		 ""
		 (substring string 0 last)))
            ((char=? (string-ref string read) (file-separator))
	     (if (not last)
		 ""
		 (substring string 0 last)))
            ((char=? (string-ref string read) #\.)
	     (loop (-fx read 1) read))
            (else
             (loop (-fx read 1) last))))))

;*---------------------------------------------------------------------*/
;*    longest-suffix ...                                               */
;*---------------------------------------------------------------------*/
(define (longest-suffix string)
   (let* ((len (string-length string))
          (len-1 (-fx len 1)))
      (let loop ((read len-1)
		 (last #f))
         (cond
            ((<fx read 0)
	     (if (not last)
		 ""
		 (substring string (+fx last 1) len)))
            ((char=? (string-ref string read) (file-separator))
	     (if (not last)
		 ""
		 (substring string (+fx last 1) len)))
            ((char=? (string-ref string read) #\.)
	     (loop (-fx read 1) read))
            (else
             (loop (-fx read 1) last))))))

;*---------------------------------------------------------------------*/
;*    is-suffix? ...                                                   */
;*---------------------------------------------------------------------*/
(define (is-suffix? name suf)
   (let* ((nlen (-fx (string-length name) 1))
	  (slen (string-length suf))
	  (pos (-fx nlen slen)))
      (and (>fx pos 0)
	   (char=? (string-ref name pos) #\.)
	   (substring-at? name suf (+fx pos 1)))))

;*---------------------------------------------------------------------*/
;*    is-suffix-ci? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-suffix-ci? name suf)
   (let* ((nlen (-fx (string-length name) 1))
	  (slen (string-length suf))
	  (pos (-fx nlen slen)))
      (and (>fx pos 0)
	   (char=? (string-ref name pos) #\.)
	   (substring-ci-at? name suf (+fx pos 1)))))

;*---------------------------------------------------------------------*/
;*    suffix-assoc ...                                                 */
;*---------------------------------------------------------------------*/
(define (suffix-assoc name lst)
   (let loop ((lst lst))
      (cond
	 ((null? lst)
	  #f)
	 ((is-suffix? name (caar lst))
	  (car lst))
	 (else
	  (loop (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    suffix-assoc-ci ...                                              */
;*---------------------------------------------------------------------*/
(define (suffix-assoc-ci name lst)
   (let loop ((lst lst))
      (cond
	 ((null? lst)
	  #f)
	 ((is-suffix-ci? name (caar lst))
	  (car lst))
	 (else
	  (loop (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    suffix-member ...                                                */
;*---------------------------------------------------------------------*/
(define (suffix-member name lst)
   (let loop ((lst lst))
      (cond
	 ((null? lst)
	  #f)
	 ((is-suffix? name (car lst))
	  (car lst))
	 (else
	  (loop (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    suffix-member-ci ...                                             */
;*---------------------------------------------------------------------*/
(define (suffix-member-ci name lst)
   (let loop ((lst lst))
      (cond
	 ((null? lst)
	  #f)
	 ((is-suffix-ci? name (car lst))
	  (car lst))
	 (else
	  (loop (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    string-member? ...                                               */
;*---------------------------------------------------------------------*/
(define (string-member? s1 s2)
   (let* ((l2 (string-length s2))
	  (l1 (string-length s1))
	  (stop (-fx l1 l2)))
      (let loop ((i 0))
	 (cond
	    ((>fx i stop)
	     #f)
	    ((substring-at? s1 s2 i)
	     #t)
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-member-ci? ...                                            */
;*---------------------------------------------------------------------*/
(define (string-member-ci? s1 s2)
   (let* ((l2 (string-length s2))
	  (l1 (string-length s1))
	  (stop (-fx l1 l2)))
      (let loop ((i 0))
	 (cond
	    ((>fx i stop)
	     #f)
	    ((substring-ci-at? s1 s2 i)
	     #t)
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    string-escape ...                                                */
;*---------------------------------------------------------------------*/
(define (string-escape a char)
   (let* ((l (string-length a))
	  (nl (let loop ((i 0)
			 (nl 0))
		 (cond
		    ((=fx i l)
		     nl)
		    ((char=? (string-ref a i) char)
		     (loop (+fx i 1) (+fx nl 2)))
		    (else
		     (loop (+fx i 1) (+fx nl 1)))))))
      (if (=fx nl l)
	  a
	  (let ((res (make-string nl)))
	     (let loop ((i 0)
			(j 0))
		(cond
		   ((=fx i l)
		    res)
		   ((char=? (string-ref a i) char)
		    (string-set! res j #\\)
		    (string-set! res (+fx j 1) char)
		    (loop (+fx i 1) (+fx j 2)))
		   (else
		    (string-set! res j (string-ref a i))
		    (loop (+fx i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    delete-path ...                                                  */
;*---------------------------------------------------------------------*/
(define (delete-path path)
   (cond
      ((not (file-exists? path))
       #t)
      ((directory? path)
       (when (every delete-path (directory->path-list path))
	  (delete-directory path)
	  #t))
      (else
       (delete-file path))))

;*---------------------------------------------------------------------*/
;*    make-cache-name ...                                              */
;*---------------------------------------------------------------------*/
(define (make-cache-name #!optional name)
   (let ((base (make-file-name (hop-cache-directory)
		  (integer->string (hop-port)))))
      (if name
	  (make-file-name base name)
	  base)))

;*---------------------------------------------------------------------*/
;*    make-url-name ...                                                */
;*---------------------------------------------------------------------*/
(define (make-url-name directory file)
   (let* ((ldir (string-length directory)))
      (if (and (=fx ldir 1) (char=? (string-ref directory 0) #\.))
	  file
	  (let* ((lfile (string-length file))
		 (len (+fx ldir (+fx lfile 1)))
		 (str (make-string len #\/)))
	     (blit-string-ur! directory 0 str 0 ldir)
	     (blit-string-ur! file 0 str (+fx 1 ldir) lfile)
	     str))))

;*---------------------------------------------------------------------*/
;*    make-hop-url-name ...                                            */
;*---------------------------------------------------------------------*/
(define (make-hop-url-name abspath)
   (make-url-name (hop-service-base) abspath))

;*---------------------------------------------------------------------*/
;*    make-client-socket/timeout ...                                   */
;*---------------------------------------------------------------------*/
(define (make-client-socket/timeout host port timeout::int msg::obj ssl::bool)
   (let ((tmt (if (>fx timeout 0)
		  (micro-seconds timeout)
		  (micro-seconds (hop-connection-timeout)))))
      (let loop ((ttl (hop-connection-ttl)))
	 (let ((res (with-handler
		       (lambda (e)
			  (if (and (>fx ttl 0) (isa? e &io-timeout-error))
			      (begin
				 (hop-verb 1
				    (hop-color msg msg " REMOTE")
				    ": " host ":" port
				    (if (isa? msg http-request)
					(with-access::http-request msg (path)
					   (format " (~a)" path))
					"")
				    (trace-color 1 " CONNECTION FAILED")
				    " ttl=" ttl "\n")
				 (-fx ttl 1))
			      (raise e)))
		       (if ssl
			   (cond-expand
			      (enable-ssl
				 (make-ssl-client-socket host port
				    :protocol (hop-https-protocol)
				    :timeout tmt))
			      (else
			       (error "make-client-socket/timeout"
				  "SSL not supported"
				  "make-ssl-client-socket")))
			   (make-client-socket host port :timeout tmt)))))
	    (if (number? res)
		(loop res)
		res)))))

;*---------------------------------------------------------------------*/
;*    iter ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (iter loop v iinit rest)
   `(let ,loop ((i ,iinit)
		(,v 0))
	 (let ((c (string-ref ipv4 i)))
	    (if (char=? c #\.)
		,rest
		(,loop (+fx i 1) (+fx (*fx ,v 10) (char->byte c)))))))

;*---------------------------------------------------------------------*/
;*    ipv4->elong ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes a well formed IPv4 address.                */
;*---------------------------------------------------------------------*/
(define (ipv4->elong::elong ipv4::bstring)
   (define (char->byte c)
      (-fx (char->integer c) (char->integer #\0)))
   (define (word v0 v1 v2 v3)
      (bit-orelong
       (bit-lshelong (fixnum->elong v0) 24)
       (bit-orelong (bit-lshelong (fixnum->elong v1) 16)
		    (bit-orelong (bit-lshelong (fixnum->elong v2) 8)
				 (fixnum->elong v3)))))
   (let ((len (string-length ipv4)))
      (iter loop0 v0 0
	    (iter loop1 v1 (+fx i 1)
		  (iter loop2 v2 (+fx i 1)
			(let loop3 ((i (+fx i 1))
				    (v3 0))
			   (if (=fx i len)
			       (word v0 v1 v2 v3)
			       (loop3 (+fx i 1)
				      (+fx (*fx v3 10)
					   (char->byte
					    (string-ref ipv4 i)))))))))))

;*---------------------------------------------------------------------*/
;*    micro-seconds ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (micro-seconds ms)
   (*fx 1000 ms))

;*---------------------------------------------------------------------*/
;*    input-timeout-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (input-timeout-set! port t)
   (let ((ms (if (>fx t 0) (micro-seconds t) t)))
      (input-port-timeout-set! port ms)))

;*---------------------------------------------------------------------*/
;*    output-timeout-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (output-timeout-set! port t)
   (let ((ms (if (>fx t 0) (micro-seconds t) t)))
      (output-port-timeout-set! port ms)))

;*---------------------------------------------------------------------*/
;*    socket-timeout-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (socket-timeout-set! socket ti to)
   (input-timeout-set! (socket-input socket) ti)
   '(output-timeout-set! (socket-output socket) to))

;*---------------------------------------------------------------------*/
;*    socket-buffers-detach! ...                                       */
;*    -------------------------------------------------------------    */
;*    This function duplicates and rebinds the buffer associated       */
;*    with a socket. It is used by persisent and asynchronous          */
;*    responses.                                                       */
;*---------------------------------------------------------------------*/
(define (socket-buffers-detach! socket::socket)
   (let ((in (socket-input socket))
	 (out (socket-output socket)))
      (input-port-buffer-set! in (string-copy (input-port-buffer in)))
      (output-port-buffer-set! out (string-copy (output-port-buffer out)))))
   
;*---------------------------------------------------------------------*/
;*    call-in-background ...                                           */
;*    -------------------------------------------------------------    */
;*    In a multi-threaded environment there is no need to spawn        */
;*    a new process, we simply execute in the current thread.          */
;*    -------------------------------------------------------------    */
;*    In a single-threaded environment it is required to spawn         */
;*    a new process for executing in background.                       */
;*---------------------------------------------------------------------*/
(define (call-in-background thunk)
   (cond-expand
      (enable-threads (thunk))
      (bigloo-c (when (=fx (fork) 0) (thunk) (exit 0)))
      (else (thunk))))
