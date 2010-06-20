;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/mime.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 07:59:54 2006                          */
;*    Last change :  Sat Jun 19 06:24:02 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HOP mime types management.                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_mime

   (export (mime-type ::bstring ::obj)
	   (mime-type-add! ::bstring ::bstring)
	   (mime-type-add-list! ::pair-nil)
	   (mime-type-parse ::input-port)
	   (load-mime-types ::bstring)))

;*---------------------------------------------------------------------*/
;*    *mime-types-table* ...                                           */
;*---------------------------------------------------------------------*/
(define *mime-types-table*
   (create-hashtable
    :eqtest (lambda (v k)
	       (string-suffix? v k))
    :hash (lambda (s)
	     (let* ((l (string-length s))
		    (r (string-index-right s #\.))
		    (b (if r (+fx r 1) 0)))
		(string-hash s b l)))))

;*---------------------------------------------------------------------*/
;*    mime-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (mime-type path default)
   (or (hashtable-get *mime-types-table* path) default))

;*---------------------------------------------------------------------*/
;*    mime-type-add! ...                                               */
;*---------------------------------------------------------------------*/
(define (mime-type-add! mimetype suffix)
   (hashtable-put! *mime-types-table* suffix mimetype))

;*---------------------------------------------------------------------*/
;*    mime-type-add-list! ...                                          */
;*---------------------------------------------------------------------*/
(define (mime-type-add-list! lst)
   (for-each (lambda (a)
		(for-each (lambda (s)
			     (mime-type-add! (car a) s))
			  (cdr a)))
	     lst))

;*---------------------------------------------------------------------*/
;*    mime-type-parse ...                                              */
;*---------------------------------------------------------------------*/
(define (mime-type-parse ip)
   (let* ((gsuf (regular-grammar ()
		   ((+ (in " \t"))
		    (ignore))
		   ((+ (out " \t\n"))
		    (cons (the-string) (ignore)))
		   (else
		    '())))
	  (g (regular-grammar ()
		((: #\# (* all))
		 (ignore))
		((+ blank)
		 (ignore))
		((+ #\Newline)
		 (ignore))
		((bol (+ (out "\n \t")))
		 (let* ((mime (the-string))
			(suf (read/rp gsuf (the-port))))
		    (if (pair? suf)
			(cons (cons mime suf) (ignore))
			(ignore))))
		(else
		 (let ((c (the-failure)))
		    (if (eof-object? c)
			'()
			(error "mime-type-parse"
			       "Illegal mime type syntax"
			       (the-failure))))))))
      (read/rp g ip)))

;*---------------------------------------------------------------------*/
;*    load-mime-types ...                                              */
;*---------------------------------------------------------------------*/
(define (load-mime-types file)
   (if (file-exists? file)
       (let ((p (open-input-file file)))
	  (if (input-port? p)
	      (unwind-protect
		 (mime-type-add-list! (mime-type-parse p))
		 (close-input-port p))))))


   
