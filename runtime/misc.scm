;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/misc.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 15 11:28:31 2004                          */
;*    Last change :  Mon Oct 23 15:24:05 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP misc                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_misc

   (library ssl)
   
   (import  __hop_param
	    __hop_types
	    __hop_read)
   
   (export (hop-verb ::int . args)
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
	   (is-local?::bool ::bstring)
	   (string-escape::bstring ::bstring ::char)
	   (escape-string::bstring ::bstring)
	   (delete-path ::bstring)
	   (make-url-name::bstring ::bstring ::bstring)
	   (make-client-socket/timeout ::bstring ::int ::int ::obj ::bool)
	   (inline micro-seconds::int ::int)
	   (inline input-timeout-set! ::input-port ::int)
	   (inline output-timeout-set! ::output-port ::int)))

;*---------------------------------------------------------------------*/
;*    *verb-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *verb-mutex* (make-mutex 'verb))

;*---------------------------------------------------------------------*/
;*    hop-verb ...                                                     */
;*---------------------------------------------------------------------*/
(define (hop-verb level . args)
   (when (>=fx (hop-verbose) level)
      (with-lock *verb-mutex*
	 (lambda ()
	    (for-each display args)
	    (flush-output-port (current-output-port))))))

;*---------------------------------------------------------------------*/
;*    hop-color ...                                                    */
;*---------------------------------------------------------------------*/
(define (hop-color col req msg)
   (let ((c (cond
	       ((integer? col)
		(+fx (modulo col 16) 1))
	       ((http-request? req)
		(+fx (modulo (http-request-id col) 16) 1))
	       (else
		1))))
      (trace-color c
		   (if (http-request? req)
		       (http-request-id req)
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
;*    is-local? ...                                                    */
;*---------------------------------------------------------------------*/
(define (is-local? dest)
   (or (string=? dest (hop-server-hostname))
       (member dest (hop-server-aliases))))

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
;*    escape-string ...                                                */
;*---------------------------------------------------------------------*/
(define (escape-string string)
   (escape-scheme-string string))

;*---------------------------------------------------------------------*/
;*    delete-path ...                                                  */
;*---------------------------------------------------------------------*/
(define (delete-path path)
   (cond
      ((not (file-exists? path))
       #t)
      ((directory? path)
       (let ((lst (directory->list path)))
	  (for-each (lambda (f) (delete-path (make-file-name path f))) lst)
	  (delete-directory path)))
      (else
       (delete-file path))))

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
;*    output-port-timeout-set! ...                                     */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo2.8a (define (output-port-timeout-set! p t) #f))
   (else #unspecified))

(cond-expand
   (bigloo2.8a (define (input-port-timeout-set! p t) #f))
   (else #unspecified))

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
			  (if (>fx ttl 0)
			      (begin
				 (hop-verb 1
					   (hop-color msg msg " REMOTE")
					   ": " host ":" port
					   (if (http-request? msg)
					       (format " (~a)"
						       (http-request-path msg))
					       "")
					   (trace-color 1 " CONNECTION FAILED")
					   " ttl=" ttl "\n")
				 (-fx ttl 1))
			      (raise e)))
		       (if ssl
			   (make-ssl-client-socket host port :timeout tmt)
			   (make-client-socket host port :timeout tmt)))))
	    (if (number? res)
		(loop res)
		res)))))

;*---------------------------------------------------------------------*/
;*    micro-seconds ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (micro-seconds ms)
   (*fx 1000 ms))

;*---------------------------------------------------------------------*/
;*    input-port-timeout-set! ...                                      */
;*---------------------------------------------------------------------*/
(cond-expand
   (bigloo2.8a (define (input-port-timeout-set! p t) #f))
   (else #unspecified))

;*---------------------------------------------------------------------*/
;*    input-timeout-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (input-timeout-set! port t)
   (let ((ms (micro-seconds t)))
      (input-port-timeout-set! port ms)))

;*---------------------------------------------------------------------*/
;*    output-timeout-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (output-timeout-set! port t)
   (let ((ms (micro-seconds t)))
      (output-port-timeout-set! port ms)))

