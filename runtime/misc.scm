;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/misc.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 15 11:28:31 2004                          */
;*    Last change :  Fri Mar 17 10:36:07 2006 (serrano)                */
;*    Copyright   :  2004-06 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP misc                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_misc

   (import  __hop_param
	    __hop_types
	    __hop_read)
   
   (export (hop-verb ::int . args)
	   (hop-color ::obj ::obj ::obj)
	   (load-once ::bstring)
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
	   (autoload-prefix::procedure ::bstring)))

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
;*    *table* ...                                                      */
;*---------------------------------------------------------------------*/
(define *table* #f)

;*---------------------------------------------------------------------*/
;*    *load-once-mutex* ...                                            */
;*---------------------------------------------------------------------*/
(define *load-once-mutex* (make-mutex "load-once"))

;*---------------------------------------------------------------------*/
;*    load-once ...                                                    */
;*---------------------------------------------------------------------*/
(define (load-once file)
   (mutex-lock! *load-once-mutex*)
   (unless (hashtable? *table*) (set! *table* (make-hashtable)))
   (let* ((f (file-name-unix-canonicalize file))
	  (g (hashtable-get *table* f)))
      (if g
	  (mutex-unlock! *load-once-mutex*)
	  (begin
	     (hashtable-put! *table* f #t)
	     (mutex-unlock! *load-once-mutex*)
	     (with-handler
		(lambda (e)
		   ;; unload the file on error
		   (mutex-lock! *load-once-mutex*)
		   (hashtable-remove! *table* f)
		   (mutex-unlock! *load-once-mutex*)
		   (raise e))
		(hop-load f))))))

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
       (member dest (hop-server-aliases))
       (string=? (host dest) (hop-server-ip))))

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
;*    autoload-prefix ...                                              */
;*    -------------------------------------------------------------    */
;*    Builds a predicate that matches if the request path is a         */
;*    prefix of STRING.                                                */
;*---------------------------------------------------------------------*/
(define (autoload-prefix string)
   (let* ((p string)
	  (p/ (string-append string "/")))
      (lambda (req)
	 (with-access::http-request req (path)
	    (let ((i (string-index path #\?))
		  (l (string-length path))
		  (lp (string-length p)))
	       (if (=fx i -1)
		   (and (substring-at? path p 0)
			(or (=fx l lp) (eq? (string-ref path lp) #\/)))
		   (substring-at? path p 0 i)))))))
