;*=====================================================================*/
;*    serrano/prgm/project/hop/3.0.x/runtime/client-exception.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug 10 05:33:45 2013                          */
;*    Last change :  Fri Oct 16 08:42:01 2015 (serrano)                */
;*    Copyright   :  2013-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Common exception implementation.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop-name-aliases ...                                             */
;*---------------------------------------------------------------------*/
(define hop-name-aliases
   '(("hop_add_event_listener" . "add-event-listener!")
     ("hop_innerHTML_set" . "innerHTML-set!")))

;*---------------------------------------------------------------------*/
;*    scheme2js-mangled? ...                                           */
;*---------------------------------------------------------------------*/
(define (scheme2js-mangled? str)
   (string-prefix? "sc_" str))

;*---------------------------------------------------------------------*/
;*    hop-hidden-frame-regexp ...                                      */
;*---------------------------------------------------------------------*/
(define hop-hidden-frame-regexp
   "^(?:hop_send_request|withHOP|HopFrame.post|HTML[A-z][a-z]*Element[.]on[a-z]*|applyCallback|&pool-scheduler[0-9]+|&pthread-[0-9]+)$")

;*---------------------------------------------------------------------*/
;*    scheme2js-demangle ...                                           */
;*---------------------------------------------------------------------*/
(define (scheme2js-demangle str)
   
   (define (is-has-bang str)
      (cond
	 ((string-prefix? "is-" str)
	  (is-has-bang
	     (string-append (substring str 3 (string-length str)) "?")))
	 ((string-prefix? "has-" str)
	  (is-has-bang (string-append str "?")))
	 ((string-suffix? "-bang" str)
	  (is-has-bang
	     (string-append (substring str 0 (- (string-length str) 5)) "!")))
	 (else
	  str)))

   (when (scheme2js-mangled? str)
      (let loop ((chars (list-tail (string->list str) 3))
		 (rev-res '()))
	 (if (null? chars)
	     (let ((res (list->string (reverse! rev-res))))
		(is-has-bang res))
	     (cond
		((and (char-upper-case? (car chars)))
		 (loop (cdr chars)
		    (cons (char-downcase (car chars)) (cons #\- rev-res))))
		((eq? (car chars) #\_)
		 (loop (cdr chars)
		    (cons #\- rev-res)))
		((eq? (car chars) #\2)
		 (loop (cdr chars)
		    (cons #\> (cons #\- rev-res))))
		(else
		 (loop (cdr chars)
		    (cons (car chars) rev-res))))))))

;*---------------------------------------------------------------------*/
;*    hop-demangle ...                                                 */
;*---------------------------------------------------------------------*/
(define (hop-demangle str)
   (or (bigloo-demangle str)
       (scheme2js-demangle str)
       (let ((a (assoc str hop-name-aliases)))
	  (if (pair? a)
	      (cdr a)
	      str))))

;*---------------------------------------------------------------------*/
;*    *firefox-stack-frame-regexp* ...                                 */
;*---------------------------------------------------------------------*/
(define *firefox-stack-frame-regexp*
   (pregexp "^([^ ]+)@([^ ]+)$"))

(define *firefox-frame-location-regexp*
   (pregexp "(.+):([0-9]+)$"))

;*---------------------------------------------------------------------*/
;*    *opera-stack-frame-regexp* ...                                   */
;*---------------------------------------------------------------------*/
(define *opera-stack-frame-regexp*
   (pregexp "^([^()]+)[(][^)]*[)]@([^ ]+)$"))

;*---------------------------------------------------------------------*/
;*    *chrome-stack-regexp* ...                                        */
;*---------------------------------------------------------------------*/
(define *chrome-stack-regexp*
   (pregexp "^.+\n[ \t]+at "))

(define *chrome-stack-frame-regexp*
   (pregexp "^[ \t]+at ([^ ]+) [(](.+)[)]"))

(define *chrome-frame-location-regexp*
   (pregexp "(.+):([0-9]+):([0-9]+)$"))

;*---------------------------------------------------------------------*/
;*    exception-context? ...                                           */
;*---------------------------------------------------------------------*/
(define (exception-context? f)
   (when (and (pair? f) (pair? (cdr f)))
      (let ((t (assq 'type (cddr f))))
	 (and (pair? t) (eq? (cdr t) 'exception)))))

;*---------------------------------------------------------------------*/
;*    hop-hidden-frame-id? ...                                         */
;*---------------------------------------------------------------------*/
(define (hop-hidden-frame-id? id)
   (pregexp-match hop-hidden-frame-regexp id))
   
;*---------------------------------------------------------------------*/
;*    hop-exception-context->frames ...                                */
;*---------------------------------------------------------------------*/
(define (hop-exception-context->frames f)

   (define host "")
   
   (define (abspath f)
      (if (string-prefix? host f)
	  (substring f (string-length host))
	  f))
   
   (define (firefox-frame f)
      
      (define (firefox-demangle id)
	 (let ((i (string-index id "/")))
	    (if (integer? i)
		(let* ((pref (substring id 0 i))
		       (dm (hop-demangle pref)))
		   (if (string-suffix? "/sc_let" id)
		       (string-append dm " (let)")
		       (string-append dm (hop-demangle (substring id i)))))
		(hop-demangle id))))
      
      (let ((m (pregexp-match *firefox-stack-frame-regexp* f)))
	 (when (pair? m)
	    (let* ((id (cadr m))
		   (loc (caddr m))
		   (dm (firefox-demangle id))
		   (l (pregexp-match *firefox-frame-location-regexp* loc)))
	       (unless (hop-hidden-frame-id? id)
		  (if (pair? l)
		      (let ((file (abspath (cadr l)))
			    (line (string->integer (caddr l))))
			 (multiple-value-bind (srcfile srcline _)
			    (hop-source-map file line 0)
			    (if (string? srcfile)
				(list dm
				   `(line ,srcfile ,srcline)
				   `(js-line ,file ,(-fx line 1))
				   `(type . ,(if (string=? dm id) 'js 'client))
				   '(format . "~~~a"))
				(list dm
				   `(line ,file ,(-fx line 1))
				   `(type . ,(if (string=? dm id) 'js 'client))
				   '(format . "~~~a")))))
		      (list dm)))))))
   
   (define (opera-frame f)
      (let ((m (pregexp-match "^([^()]+)[(][^)]*[)]@([^ ]+)$" f)))
	 (if (not m)
	     (firefox-frame f)
	     (let* ((id (cadr m))
		    (loc (caddr m))
		    (m2 (pregexp-match "<anonymous function: ([^.]+)>" id)))
		(firefox-frame (format "~a@~a" (if m2 (cadr m2) id) loc))))))
   
   (define (chrome-frame f)
      (let ((m (pregexp-match *chrome-stack-frame-regexp* f)))
	 (when (pair? m)
	    (let* ((id (cadr m))
		   (loc (caddr m))
		   (dm (hop-demangle id))
		   (l (pregexp-match *chrome-frame-location-regexp* loc)))
	       (unless (hop-hidden-frame-id? id)
		  (if (pair? l)
		      ;; source map the file position
		      (let ((file (abspath (cadr l)))
			    (line (string->integer (caddr l)))
			    (col (string->integer (cadddr l))))
			 (multiple-value-bind (srcfile srcline srccol)
			    (hop-source-map file line col)
			    (if (string? srcfile)
				(list dm
				   `(line-col ,srcfile ,srcline ,srccol)
				   `(js-line-col ,file ,(-fx line 1), col)
				   `(type . ,(if (string=? dm id) 'js 'client))
				   '(format . "~~~a"))
				(list dm
				   `(line-col ,file ,(-fx line 1), col)
				   `(type . ,(if (string=? dm id) 'js 'client))
				   '(format . "~~~a")))))
		      (list dm)))))))
   
   (let* ((stack (car f))
	  (lines (string-split stack "\n"))
	  (offset (cdr (assq 'offset (cddr f)))))
      (set! host (cdr (assq 'host (cddr f))))
      (cond
	 ((null? lines)
	  '())
	 ((pregexp-match *opera-stack-frame-regexp* (car lines))
	  (filter-map opera-frame (list-tail lines offset)))
	 ((pregexp-match *firefox-stack-frame-regexp* (car lines))
	  (filter-map firefox-frame (list-tail lines offset)))
	 ((pregexp-match *chrome-stack-regexp* stack)
	  (filter-map chrome-frame (list-tail (cdr lines) offset)))
	 (else
	  (map (lambda (l)
		  (list l #f '(format . "~~~a") '(type . js)))
	     (list-tail lines offset))))))

;*---------------------------------------------------------------------*/
;*    hop-debug-exception-stack ...                                    */
;*---------------------------------------------------------------------*/
(define (hop-debug-exception-stack ctxs)
   (let loop ((ctxs ctxs)
	      (frames '()))
      (cond
	 ((null? ctxs)
	  (reverse! frames))
	 ((exception-context? (car ctxs))
	  (loop (cdr ctxs)
	     (append (reverse! (hop-exception-context->frames (car ctxs)))
		frames)))
	 (else
	  (loop (cdr ctxs) (cons (car ctxs) frames))))))

