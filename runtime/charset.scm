;*=====================================================================*/
;*    /users/serrano/prgm/project/hop/1.9.x/runtime/charset.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct 10 06:46:43 2007                          */
;*    Last change :  Thu Sep 11 15:06:11 2008 (root)                   */
;*    Copyright   :  2007-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Functions for dealing with charset.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hop_charset
   (export (charset-convert ::obj ::symbol ::symbol)
	   (charset-converter::procedure ::symbol ::symbol)
	   (charset-converter!::procedure ::symbol ::symbol)))

;*---------------------------------------------------------------------*/
;*    charset-convert ...                                              */
;*    -------------------------------------------------------------    */
;*    Convert a string from charset1 to charset2                       */
;*---------------------------------------------------------------------*/
(define (charset-convert str charset1 charset2)
   (if (eq? charset1 charset2)
       str
       (case charset1
	  ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
		       WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       str)
	      ((UTF-8)
	       (iso-latin->utf8 str))
	      ((UCS-2)
	       (utf8-string->ucs2-string (iso-latin->utf8 str)))
	      (else
	       str)))
	  ((UTF-8)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       (utf8->iso-latin str))
	      ((UCS-2)
	       (utf8-string->ucs2-string str))
	      (else
	       str)))
	  ((UCS-2)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       (utf8->iso-latin! (ucs2-string->utf8-string str)))
	      ((UTF-8)
	       (ucs2-string->utf8-string str))
	      (else
	       str)))
	  (else
	   str))))

;*---------------------------------------------------------------------*/
;*    charset-converter ...                                            */
;*---------------------------------------------------------------------*/
(define (charset-converter charset1 charset2)
   (if (eq? charset1 charset2)
       (lambda (x) x)
       (case charset1
	  ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
		       WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       (lambda (x) x))
	      ((UTF-8)
	       iso-latin->utf8)
	      ((UCS-2)
	       (lambda (str)
		  (utf8-string->ucs2-string (iso-latin->utf8 str))))
	      (else
	       (lambda (x) x))))
	  ((UTF-8)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       utf8->iso-latin)
	      ((UCS-2)
	       (lambda (str)
		  (utf8-string->ucs2-string str)))
	      (else
	       (lambda (x) x))))
	  ((UCS-2)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       (lambda (str)
		  (utf8->iso-latin! (ucs2-string->utf8-string str))))
	      ((UTF-8)
	       ucs2-string->utf8-string)
	      (else
	       (lambda (x) x))))
	  (else
	   (lambda (x) x)))))

;*---------------------------------------------------------------------*/
;*    charset-converter! ...                                           */
;*---------------------------------------------------------------------*/
(define (charset-converter! charset1 charset2)
   (if (eq? charset1 charset2)
       (lambda (x) x)
       (case charset1
	  ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
		       WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       (lambda (x) x))
	      ((UTF-8)
	       iso-latin->utf8!)
	      ((UCS-2)
	       (lambda (str)
		  (utf8-string->ucs2-string (iso-latin->utf8 str))))
	      (else
	       (lambda (x) x))))
	  ((UTF-8)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       utf8->iso-latin!)
	      ((UCS-2)
	       (lambda (str)
		  (utf8-string->ucs2-string str)))
	      (else
	       (lambda (x) x))))
	  ((UCS-2)
	   (case charset2
	      ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1
			   WINDOWS-1250 WINDOWS-1252 WINDOWS-1256 WINDOWS-1258)
	       (lambda (str)
		  (utf8->iso-latin! (ucs2-string->utf8-string str))))
	      ((UTF-8)
	       ucs2-string->utf8-string)
	      (else
	       (lambda (x) x))))
	  (else
	   (lambda (x) x)))))
