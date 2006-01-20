;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/misc.scm                        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 15 11:28:31 2004                          */
;*    Last change :  Thu Jan 19 09:32:23 2006 (serrano)                */
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
	   (list-split::pair-nil ::pair-nil ::int . obj)
	   (list-split!::pair-nil ::pair-nil ::int . obj)
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
	   (keyword->symbol::symbol ::keyword)
	   (symbol->keyword::keyword ::symbol)
	   (delete-path ::bstring)
	   (hop-calendar::pair ::date))

   (eval   (export-exports)))

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
;*    list-split ...                                                   */
;*---------------------------------------------------------------------*/
(define (list-split l num . fill)
   (let loop ((l l)
	      (i 0)
	      (acc '())
	      (res '()))
      (cond
	 ((null? l)
	  (reverse! (cons (if (or (null? fill) (=fx i num))
			      (reverse! acc)
			      (append! (reverse! acc)
				       (make-list (-fx num i) (car fill))))
			  res)))
	 ((=fx i num)
	  (loop l 0 '() (cons (reverse! acc) res)))
	 (else
	  (loop (cdr l) (+fx i 1) (cons (car l) acc) res)))))

;*---------------------------------------------------------------------*/
;*    list-split! ...                                                  */
;*---------------------------------------------------------------------*/
(define (list-split! l num . fill)
   (let loop ((l l)
	      (i 0)
	      (last #f)
	      (acc l)
	      (rows '()))
      (cond
	 ((null? l)
	  (let ((lrow (if (or (null? fill) (=fx i num))
			  acc
			  (begin
			     (set-cdr! last
				       (make-list (-fx num i) (car fill)))
			     acc))))
	     (reverse! (cons lrow rows))))
	 ((=fx i num)
	  (set-cdr! last '())
	  (loop l 0 l l (cons acc rows)))
	 (else
	  (loop (cdr l) (+fx i 1) l acc rows)))))

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
	     (hop-load f)))))

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
            ((char=? (string-ref string read) runtime-file-separator)
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
            ((char=? (string-ref string read) runtime-file-separator)
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
;*    keyword->symbol ...                                              */
;*---------------------------------------------------------------------*/
(define (keyword->symbol k)
   (let ((s (keyword->string k)))
      (string->symbol (substring s 1 (string-length s)))))

;*---------------------------------------------------------------------*/
;*    symbol->keyword ...                                              */
;*---------------------------------------------------------------------*/
(define (symbol->keyword k)
   (string->keyword (string-append ":" (symbol->string k))))

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
;*    *month-lengths* ...                                              */
;*---------------------------------------------------------------------*/
(define *month-lengths* `#(31 28 31 30 31 30 31 31 30 31 30 31))

;*---------------------------------------------------------------------*/
;*    month-length ...                                                 */
;*---------------------------------------------------------------------*/
(define (month-length d)
   (let ((m (date-month d)))
      (if (=fx m 2)
	  (if (leap-year? (date-year d)) 29 28)
	  (vector-ref *month-lengths* (-fx m 1)))))

;*---------------------------------------------------------------------*/
;*    hop-calendar ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-calendar dt)
   (let* ((mlen (month-length dt))
	  (ds (date-copy dt :day 1))
	  (de (date-copy dt :day mlen))
	  (start (-second (date->seconds ds)
			  (*second (integer->second (-fx (date-wday ds) 1))
				   (day-seconds))))
	  (end (+second (date->seconds de)
			(*second (integer->second (-fx 7 (date-wday de)))
				 (day-seconds)))))
      (let loop ((i start)
		 (res '()))
	 (if (>second i end)
	     (list-split! (reverse! res) 7)
	     (loop (+second i (day-seconds))
		   (cons (seconds->date i) res))))))
