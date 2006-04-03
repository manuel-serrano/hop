;*=====================================================================*/
;*    serrano/prgm/project/hop/hopwiki/syntax.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  3 07:05:06 2006                          */
;*    Last change :  Mon Apr  3 11:45:44 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The HOP wiki syntax tools                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwiki_syntax

   (library hop)
   
   (export  (class wiki-syntax
	       (h1::procedure (default <H1>))
	       (h2::procedure (default <H2>))
	       (h3::procedure (default <H3>))
	       (h4::procedure (default <H4>))
	       (h5::procedure (default <H5>))
	       (p::procedure (default <P>))
	       (ol::procedure (default <OL>))
	       (ul::procedure (default <UL>))
	       (li::procedure (default <LI>)))
	    
	    (wiki-string->hop ::bstring)))

;*---------------------------------------------------------------------*/
;*    wiki-string->hop ...                                             */
;*---------------------------------------------------------------------*/
(define (wiki-string->hop string)
   (<B> string))

;*---------------------------------------------------------------------*/
;*    wiki-read-error ...                                              */
;*---------------------------------------------------------------------*/
(define (wiki-read-error msg obj port)
   (let ((loc (if (epair? obj)
		  (match-case (cer obj)
		     ((at ?fname ?pos) pos)
		     (else 0))
		  0)))
      (raise (instantiate::&io-read-error
		(fname (input-port-name port))
		(location loc)
		(proc 'wiki-parser)
		(msg msg)
		(obj obj)))))

;*---------------------------------------------------------------------*/
;*    *wiki-grammar* ...                                               */
;*---------------------------------------------------------------------*/
(define *wiki-grammar*
   (regular-grammar ((blank (in " \t"))
		     syn row state)

      (define (foo x)
	 (set! state #f)
	 (ignore))
      
      ;; sectionning
      ((: (+ #\=) (* blank) #\Newline)
       (if row
	   (let ((el ((wiki-syntax-h1 syn row))))
	      (set! row #f)
	      (cons el (ignore)))
	   (ignore)))
      ((: (+ #\-) (* blank) #\Newline)
       (if row
	   (let ((el ((wiki-syntax-h2 syn) row)))
	      (set! row #f)
	      (cons el (ignore)))
	   (ignore)))
      ((: (+ #\~) (* blank) #\Newline)
       (if row
	   (let ((el ((wiki-syntax-h3 syn) row)))
	      (set! row #f)
	      (cons el (ignore)))
	   (ignore)))
      ((: (+ #\^) (* blank) #\Newline)
       (if row
	   (let ((el ((wiki-syntax-h4 syn) row)))
	      (set! row #f)
	      (cons el (ignore)))
	   (ignore)))
      ((: (+ #\+) (* blank) #\Newline)
       (if row
	   (let ((el ((wiki-syntax-h5 syn) row)))
	      (set! row #f)
	      (cons el (ignore)))
	   (ignore)))

      ;; lists
      ((: (+ #\space) (in "*-") #\space)
       (let* ((nstate (string->symbol
		       (string-append 'ol (number->string (the-length)))))
	      (l ((wiki-syntax-li syn) (read/rp *wiki-line-grammar* (the-port)))))
	  (cond
	     ((not state)
	      (set! state nstate)
	      (set! constr (wiki-syntax-ol syn))
	      (set! rows (list l)))
	     ((eq? state (wiki-syntax-ol syn))
	      (set! rows (cons l rows))
	      (ignore))
	     (else
	      (let ((prev (apply state rows)))
		 (set! state nstate)
		 (set! constr (wiki-syntax-ol syn))
		 (set! rows (list l))
		 (cons prev (ignore)))))))
	      
      ;; paragraph
      ((: (? #\Return) #\Newline)
       (let ((el ((wiki-syntax-p syn))))
	  (if row
	      (begin
		 (set row #f)
		 (cons* row "\n" el (ignore)))
	      (cons* row "\n" el (ignore)))))
      
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      '()
	      (wiki-read-error "Illegal character" c (the-port)))))))

   

   
