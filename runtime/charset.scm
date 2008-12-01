;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/runtime/charset.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 10 06:46:43 2007                          */
;*    Last change :  Mon Dec  1 10:07:45 2008 (serrano)                */
;*    Copyright   :  2007-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Functions for dealing with charset.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_charset
   (export (charset-convert ::obj ::symbol ::symbol)
	   (charset-convert! ::obj ::symbol ::symbol)
	   (charset-converter::procedure ::symbol ::symbol)
	   (charset-converter!::procedure ::symbol ::symbol)))

;*---------------------------------------------------------------------*/
;*    compatibility kit                                                */
;*---------------------------------------------------------------------*/
(cond-expand
   ((or bigloo3.1a bigloo3.1b)
    (define (utf8->cp1252 x) (utf8->iso-latin x))
    (define (utf8->cp1252! x) (utf8->iso-latin! x))
    (define (cp1252->utf8 x) (iso-latin->utf8 x))
    (define (cp1252->utf8! x) (iso-latin->utf8! x))))

;*---------------------------------------------------------------------*/
;*    charset-alias ...                                                */
;*---------------------------------------------------------------------*/
(define (charset-alias charset)
   (case charset
      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1 ISO-8869-1)
       'ISO-8859-1)
      ((WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
       'CP1252)
      (else
       charset)))

;*---------------------------------------------------------------------*/
;*    charset-convert ...                                              */
;*    -------------------------------------------------------------    */
;*    Convert a string from charset1 to charset2                       */
;*---------------------------------------------------------------------*/
(define (charset-convert str charset1 charset2)
   (let ((cset1 (charset-alias charset1))
	 (cset2 (charset-alias charset2)))
      (if (eq? cset1 cset2)
	  str
	  (case cset1
	     ((ISO-8859-1)
	      (case cset2
		 ((UTF-8)
		  (iso-latin->utf8 str))
		 ((UCS-2)
		  (utf8-string->ucs2-string (iso-latin->utf8 str)))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii str))
		 (else
		  str)))
	     ((CP1252)
	      (case cset2
		 ((UTF-8)
		  (cp1252->utf8 str))
		 ((UCS-2)
		  (utf8-string->ucs2-string (cp1252->utf8 str)))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii str))
		 (else
		  str)))
	     ((UTF-8)
	      (case cset2
		 ((ISO-8859-1)
		  (utf8->iso-latin str))
		 ((CP1252)
		  (utf8->cp1252 str))
		 ((UCS-2)
		  (utf8-string->ucs2-string str))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii! (utf8->iso-latin str)))
		 (else
		  str)))
	     ((UCS-2)
	      (case cset2
		 ((ISO-8859-1)
		  (utf8->iso-latin! (ucs2-string->utf8-string str)))
		 ((CP1252)
		  (utf8->cp1252! (ucs2-string->utf8-string str)))
		 ((UTF-8)
		  (ucs2-string->utf8-string str))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii!
		   (utf8->iso-latin!
		    (ucs2-string->utf8-string str))))
		 (else
		  str)))
	     ((US-ASCII)
	      (charset-convert! (us-ascii->iso-latin str) 'ISO-LATIN-1 cset2))
	     (else
	      str)))))

;*---------------------------------------------------------------------*/
;*    charset-convert! ...                                             */
;*    -------------------------------------------------------------    */
;*    Convert a string from charset1 to charset2                       */
;*---------------------------------------------------------------------*/
(define (charset-convert! str charset1 charset2)
   (let ((cset1 (charset-alias charset1))
	 (cset2 (charset-alias charset2)))
      (if (eq? cset1 cset2)
	  str
	  (case cset1
	     ((ISO-8859-1)
	      (case cset2
		 ((UTF-8)
		  (iso-latin->utf8! str))
		 ((UCS-2)
		  (utf8-string->ucs2-string (iso-latin->utf8! str)))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii! str))
		 (else
		  str)))
	     ((CP1252)
	      (case cset2
		 ((UTF-8)
		  (cp1252->utf8! str))
		 ((UCS-2)
		  (utf8-string->ucs2-string (cp1252->utf8! str)))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii! str))
		 (else
		  str)))
	     ((UTF-8)
	      (case cset2
		 ((ISO-8859-1)
		  (utf8->iso-latin! str))
		 ((CP1252)
		  (utf8->cp1252! str))
		 ((UCS-2)
		  (utf8-string->ucs2-string str))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii! (utf8->iso-latin! str)))
		 (else
		  str)))
	     ((UCS-2)
	      (case cset2
		 ((ISO-8859-1)
		  (utf8->iso-latin! (ucs2-string->utf8-string str)))
		 ((CP1252)
		  (utf8->cp1252! (ucs2-string->utf8-string str)))
		 ((UTF-8)
		  (ucs2-string->utf8-string str))
		 ((US-ASCII)
		  (iso-8859-1->us-ascii!
		   (utf8->iso-latin!
		    (ucs2-string->utf8-string str))))
		 (else
		  str)))
	     ((US-ASCII)
	      (charset-convert! (us-ascii->iso-latin! str) 'ISO-LATIN-1 cset2))
	     (else
	      str)))))

;*---------------------------------------------------------------------*/
;*    charset-converter ...                                            */
;*---------------------------------------------------------------------*/
(define (charset-converter charset1 charset2)
   (let ((cset1 (charset-alias charset1))
	 (cset2 (charset-alias charset2)))
      (if (eq? cset1 cset2)
	  (lambda (x) x)
	  (case cset1
	     ((ISO-8859-1)
	      (case cset2
		 ((UTF-8)
		  iso-latin->utf8)
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string (iso-latin->utf8 str))))
		 ((US-ASCII)
		  iso-8859-1->us-ascii)
		 (else
		  (lambda (x) x))))
	     ((CP1252)
	      (case cset2
		 ((UTF-8)
		  cp1252->utf8)
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string (cp1252->utf8 str))))
		 ((US-ASCII)
		  iso-8859-1->us-ascii)
		 (else
		  (lambda (x) x))))
	     ((UTF-8)
	      (case cset2
		 ((ISO-8859-1)
		  utf8->iso-latin)
		 ((CP1252)
		  utf8->cp1252)
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string str)))
		 ((US-ASCII)
		  (lambda (s)
		     (iso-8859-1->us-ascii! (utf8->iso-latin s))))
		 (else
		  (lambda (x) x))))
	     ((UCS-2)
	      (case cset2
		 ((ISO-8859-1)
		  (lambda (str)
		     (utf8->iso-latin! (ucs2-string->utf8-string str))))
		 ((CP1252)
		  (lambda (str)
		     (utf8->cp1252! (ucs2-string->utf8-string str))))
		 ((UTF-8)
		  ucs2-string->utf8-string)
		 ((US-ASCII)
		  (lambda (str)
		     (iso-8859-1->us-ascii!
		      (utf8->iso-latin! (ucs2-string->utf8-string str)))))
		 (else
		  (lambda (x) x))))
	     ((US-ASCII)
	      (case cset2
		 ((ISO-8859-1 CP1252)
		  (lambda (x) x))
		 ((UTF-8)
		  iso-latin->utf8)
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string (iso-latin->utf8 str))))
		 (else
		  (lambda (x) x))))	  
	     (else
	      (lambda (x) x))))))

;*---------------------------------------------------------------------*/
;*    charset-converter! ...                                           */
;*---------------------------------------------------------------------*/
(define (charset-converter! charset1 charset2)
   (let ((cset1 (charset-alias charset1))
	 (cset2 (charset-alias charset2)))
      (if (eq? cset1 cset2)
	  (lambda (x) x)
	  (case cset1
	     ((ISO-8859-1)
	      (case cset2
		 ((UTF-8)
		  iso-latin->utf8!)
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string (iso-latin->utf8 str))))
		 ((US-ASCII)
		  iso-8859-1->us-ascii!)
		 (else
		  (lambda (x) x))))
	     ((CP1252)
	      (case cset2
		 ((UTF-8)
		  cp1252->utf8!)
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string (cp1252->utf8 str))))
		 ((US-ASCII)
		  iso-8859-1->us-ascii!)
		 (else
		  (lambda (x) x))))
	     ((UTF-8)
	      (case cset2
		 ((ISO-8859-1)
		  utf8->iso-latin!)
		 ((CP1252)
		  utf8->cp1252!)
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string str)))
		 ((US-ASCII)
		  (lambda (s)
		     (iso-8859-1->us-ascii! (utf8->iso-latin! s))))
		 (else
		  (lambda (x) x))))
	     ((UCS-2)
	      (case cset2
		 ((ISO-8859-1)
		  (lambda (str)
		     (utf8->iso-latin! (ucs2-string->utf8-string str))))
		 ((CP1252)
		  (lambda (str)
		     (utf8->cp1252! (ucs2-string->utf8-string str))))
		 ((UTF-8)
		  ucs2-string->utf8-string)
		 ((US-ASCII)
		  (lambda (str)
		     (iso-8859-1->us-ascii!
		      (utf8->iso-latin! (ucs2-string->utf8-string str)))))
		 (else
		  (lambda (x) x))))
	     ((US-ASCII)
	      (case cset2
		 ((ISO-8859-1 CP1252)
		  iso-8859-1->us-ascii!)
		 ((UTF-8)
		  (lambda (str)
		     (iso-latin->utf8! (iso-8859-1->us-ascii! str))))
		 ((UCS-2)
		  (lambda (str)
		     (utf8-string->ucs2-string
		      (iso-latin->utf8!
		       (iso-8859-1->us-ascii! str)))))
		 (else
		  (lambda (x) x))))	  
	     (else
	      (lambda (x) x))))))

;*---------------------------------------------------------------------*/
;*    count-crlf ...                                                   */
;*---------------------------------------------------------------------*/
(define (count-crlf str)
   (let ((len-1 (-fx (string-length str) 1)))
      (let loop ((i 0)
		 (n 0))
	 (if (>=fx i len-1)
	     n
	     (let ((c (string-ref str i)))
		(if (and (char=? c #\Return)
			 (char=? (string-ref str (+fx i 1)) #\Newline))
		    (loop (+fx i 2) (+fx n 1))
		    (loop (+fx i 1) n)))))))

;*---------------------------------------------------------------------*/
;*    us-ascii->iso-latin-inner ...                                    */
;*    -------------------------------------------------------------    */
;*    Replace CRLF with NL                                             */
;*---------------------------------------------------------------------*/
(define (us-ascii->iso-latin-inner str res)
   (let ((len (string-length str)))
      (let loop ((i 0)
		 (j 0))
	 (if (=fx i len)
	     res
	     (let ((c (string-ref str i)))
		(if (and (char=? c #\Return)
			 (char=? (string-ref str (+fx i 1)) #\Newline))
		    (begin
		       (string-set! res j #\Newline)
		       (loop (+fx i 2) (+fx j 1)))
		    (begin
		       (string-set! res j c)
		       (loop (+fx i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    us-ascii->iso-latin ...                                          */
;*    -------------------------------------------------------------    */
;*    Just replace CRLF with NL                                        */
;*---------------------------------------------------------------------*/
(define (us-ascii->iso-latin str)
   (let* ((len (string-length str))
	  (count (count-crlf str))
	  (res (make-string (-fx len count))))
      (if (=fx count 0)
	  (begin
	     (blit-string! str 0 res 0 len)
	     res)
	  (us-ascii->iso-latin-inner str res))))

;*---------------------------------------------------------------------*/
;*    us-ascii->iso-latin! ...                                         */
;*    -------------------------------------------------------------    */
;*    Just replace CRLF with NL                                        */
;*---------------------------------------------------------------------*/
(define (us-ascii->iso-latin! str)
   (let ((len (string-length str))
	 (count (count-crlf str)))
      (if (=fx count 0)
	  str
	  (let ((res (us-ascii->iso-latin-inner str str)))
	     (string-shrink! res (-fx len count))))))

;*---------------------------------------------------------------------*/
;*    count-nl ...                                                     */
;*---------------------------------------------------------------------*/
(define (count-nl str)
   (let ((len (string-length str)))
      (let loop ((i 0)
		 (n 0))
	 (cond
	    ((=fx i len)
	     n)
	    ((char=? (string-ref str i) #\Newline)
	     (loop (+fx i 1) (+fx n 1)))
	    (else
	     (loop (+fx i 1) n))))))

;*---------------------------------------------------------------------*/
;*    iso-8859-1->us-ascii-inner ...                                   */
;*    -------------------------------------------------------------    */
;*    Replace NL with CRLF.                                            */
;*---------------------------------------------------------------------*/
(define (iso-8859-1->us-ascii-inner str res)
   (let ((len (string-length str)))
      (let loop ((i 0)
		 (j 0))
	 (if (=fx i len)
	     res
	     (let ((c (string-ref str i)))
		(if (char=? c #\Return)
		    (begin
		       (string-set! res j #\Return)
		       (string-set! res (+fx j 1) c)
		       (loop (+fx i 1) (+fx j 2)))
		    (begin
		       (string-set! res j c)
		       (loop (+fx i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    iso-8859-1->us-ascii ...                                         */
;*    -------------------------------------------------------------    */
;*    Just replace CRLF with NL                                        */
;*---------------------------------------------------------------------*/
(define (iso-8859-1->us-ascii str)
   (let* ((len (string-length str))
	  (count (count-nl str))
	  (res (make-string (+fx len count))))
      (if (=fx count 0)
	  (begin
	     (blit-string! str 0 res 0 len)
	     res)
	  (iso-8859-1->us-ascii-inner str res))))

;*---------------------------------------------------------------------*/
;*    iso-8859-1->us-ascii! ...                                        */
;*---------------------------------------------------------------------*/
(define (iso-8859-1->us-ascii! str)
   (let* ((len (string-length str))
	  (count (count-nl str)))
      (if (=fx count 0)
	  str
	  (iso-8859-1->us-ascii-inner str (make-string (+fx len count))))))
   
