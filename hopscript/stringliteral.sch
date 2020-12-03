;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/stringliteral.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Fri Oct 11 07:52:43 2019 (serrano)                */
;*    Copyright   :  2014-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JsStringLiteral Helper macros.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives

   (include "stringthread.sch")
   
   (import __hopscript_stringliteral)
   
   (include "names.sch"))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring                                              */
;*---------------------------------------------------------------------*/
(define-expander js-string->jsstring
   (lambda (x e)

      (define (utf8-codeunit-length str::bstring)
	 (let ((len (string-length str)))
	    (let loop ((r 0)
		       (l 0))
	       (if (>=fx r len)
		   (fixnum->uint32 l)
		   (let* ((c (string-ref str r))
			  (s (utf8-char-size c)))
		      (if (and (=fx s 4)
			       (or (=fx (char->integer c) #xf0)
				   (=fx (char->integer c) #xf4)))
			  (loop (+fx r s) (+fx l 2))
			  (loop (+fx r s) (+fx l 1))))))))
      
      (match-case x
	 ((js-string->jsstring (and ?val (? string?)))
	  (case (string-minimal-charset val)
	     ((ascii)
	      (evepairify `(js-ascii->jsstring ,val) x))
	     ((latin1)
	      (evepairify `(js-latin1->jsstring ,val) x))
	     (else
	      (evepairify
		 `(js-utf8->jsstring/ulen ,val ,(utf8-codeunit-length val)) x))))
	 ((js-string->jsstring ?val)
	  `(js-string->jsstring
	      ,(e (evepairify val x) e)))
	 (else
	  (error "js-string->jsstring" "wrong syntax" x)))))
