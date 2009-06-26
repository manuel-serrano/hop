;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module pipe-port
   (library pthread)
   (export (open-pipe-port)))

(define *INITIAL-BUFFER-SIZE* 128)
(define *MAX-BUFFER-SIZE* 1024)

(define (increase-buffer-size buf min-additional-size start end)
   (let* ((current-size (string-length buf))
	  (tmp (*fx 2 (+fx current-size min-additional-size)))
	  (new-size (minfx tmp *MAX-BUFFER-SIZE*))
	  (new-buf (make-string new-size)))
      (if (<=fx start end)
	  (begin
	     (blit-string! buf start new-buf 0 (-fx end start))
	     (values new-buf 0 (-fx end start)))
	  (let ((first-part-len (-fx current-size start)))
	     (blit-string! buf start new-buf 0 first-part-len)
	     (blit-string! buf 0 new-buf first-part-len end)
	     (values new-buf 0 (+fx first-part-len end))))))

;; for now no configuration option.
(define (open-pipe-port)
   (let* ((l (make-mutex))
	  (data-avail (make-condition-variable))
	  (buffer-empty (make-condition-variable))
	  (buf (make-string *INITIAL-BUFFER-SIZE*))
	  (start 0)
	  (end 0)
	  (closed? #f))

      (define (buffer-unread-size)
	 (let ((buf-len (string-length buf)))
	    (if (>=fx end start)
		(-fx end start)
		(+fx end (-fx buf-len start)))))

      (define (add-new-text str from)
	 (let* ((buf-len (string-length buf))
		(used (buffer-unread-size))
		(unused (-fx (-fx buf-len used) 1))
		(add-len (-fx (string-length str) from)))
	    (cond
	       ((<=fx add-len unused)
		;; copy into buffer
		(let* ((up-to-buf-len (-fx buf-len end))
		       (needs-to-be-wrapped? (<fx up-to-buf-len add-len)))
		   (if needs-to-be-wrapped?
		       (let* ((first-part-len up-to-buf-len)
			      (second-part-len (-fx add-len up-to-buf-len))
			      (new-end second-part-len))
			  (blit-string! str from buf end first-part-len)
			  (blit-string! str (+fx from first-part-len)
					buf 0
					second-part-len)
			  (set! end new-end))
		       (let ((new-end (+fx end add-len)))
			  (blit-string! str from buf end add-len)
			  (set! end new-end)))))
	       ((<fx buf-len *MAX-BUFFER-SIZE*)
		(receive (new-buf new-start new-end)
		   (increase-buffer-size buf add-len start end)
		   (set! buf new-buf)
		   (set! start new-start)
		   (set! end new-end)
		   (add-new-text str from)))
	       ((not (zerofx? used))
		(flush)
		(condition-variable-wait! buffer-empty l)
		(add-new-text str from))
	       ;; vvvv buffer-empty. vvvv
	       ((>=fx add-len buf-len)
		;; takes more than one "push".
		;; we must not fill the whole buffer. Otherwise we can't
		;; differentiate between 0 and buf-len chars.
		(blit-string! str from buf 0 (-fx buf-len 1))
		(set! start 0)
		(set! end (-fx buf-len 1))
		(flush)
		(add-new-text str (+fx from (-fx buf-len 1))))
	       (else
		(error "pipe-port"
		       "should never happen."
		       #f)))))

      (define (flush)
	 (unless (zerofx? (buffer-unread-size))
	    (condition-variable-broadcast! data-avail)))

      (define (sink str)
	 (with-lock l
	    (lambda ()
	       (unless closed?
		  (cond
		     ((not str)
		      (set! closed? #t)
		      (flush))
		     (else
		      (add-new-text str 0)
		      (let* ((buf-len (string-length buf))
			     (new-size (buffer-unread-size)))
			 ;; wait before sending the broadcast.
			 (when (>=fx new-size (/fx buf-len 2))
			    (flush)))))))))

      (define (read-buffer)
	 (let ((buf-size (string-length buf)))
	    (cond
	       ((=fx start buf-size)
		;; avoid empty string case
		(set! start 0)
		(read-buffer))
	       ((<fx end start)
		(let ((res (substring buf start (-fx buf-size start))))
		   (set! start 0)
		   res))
	       (else
		(let ((res (substring buf start end)))
		   (set! start 0)
		   (set! end 0)
		   (condition-variable-broadcast! buffer-empty)
		   res)))))

      (define (read-or-wait)
      	 (let ((unread-size (buffer-unread-size)))
	    (cond
	       ((and (zerofx? unread-size)
		     closed?)
		#f)
	       ((zerofx? unread-size)
		(condition-variable-wait! data-avail l)
		(read-or-wait))
	       (else
		(read-buffer)))))

      (define (source)
	 (with-lock l
	    read-or-wait))

      (values (open-output-procedure sink flush)
	      (open-input-procedure source)
	      (lambda ()
		 (set! closed? #t)
		 ;; always send broadcast, so that waiting read can continue
		 (condition-variable-broadcast! data-avail)))))
