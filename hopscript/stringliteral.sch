;*=====================================================================*/
;*    serrano/prgm/project/hop/3.2.x/hopscript/stringliteral.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Tue Oct 24 02:21:35 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JsStringLiteral Helper macros.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (import __hopscript_stringliteral))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring                                              */
;*    -------------------------------------------------------------    */
;*    Static allocation of constant strings.                           */
;*---------------------------------------------------------------------*/
(define-expander js-string->jsstring
   (lambda (x e)
      (match-case x
	 ((js-string->jsstring (and ?val (? string?)))
	  (if (eq? (string-minimal-charset val) 'ascii)
	      (evepairify `(js-ascii->jsstring ,val) x)
	      (evepairify `(js-utf8->jsstring ,val) x)))
	 ((js-string->jsstring ?val)
	  `(js-string->jsstring ,(e (evepairify val x) e)))
	 (else
	  (error "js-string->jsstring" "wrong syntax" x)))))

;*---------------------------------------------------------------------*/
;*    %js-jsstringliteral-begin! ...                                   */
;*---------------------------------------------------------------------*/
(define-expander %js-jsstringliteral-begin!
   (lambda (x e)
      (eval '(define js-strings '()))
      #unspecified))

;*---------------------------------------------------------------------*/
;*    %js-jsstringliteral-end! ...                                     */
;*---------------------------------------------------------------------*/
(define-expander %js-jsstringliteral-end!
   (lambda (x e)
      (when (pair? (eval 'js-strings))
	 (e `(define ,(eval 'js-strings-vector)
		(vector
		   ,@(map (lambda (c)
			     (let ((s (car c)))
				`(instantiate::JsStringLiteral
				    (weight ,(string-length s))
				    (left (string-ascii-sentinel-mark! ,s)))))
			(reverse! (eval 'js-strings)))))
	    e))))


   
