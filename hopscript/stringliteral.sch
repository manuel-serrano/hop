;*=====================================================================*/
;*    serrano/prgm/project/hop/3.1.x/hopscript/stringliteral.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Thu Feb 25 19:38:30 2016 (serrano)                */
;*    Copyright   :  2014-16 Manuel Serrano                            */
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
	 ((js-string->jsstring ?val)
	  (e val e))
;* 	 ((js-string->jsstring (and ?val (? string?)))                 */
;* 	  (let ((index (eval `(js-string-register! ,val))))            */
;* 	     (evepairify `(vector-ref-ur ,js-strings-vector ,index) x))) */
;* 	 ((js-string->jsstring ?val)                                   */
;* 	  `(js-string->jsstring ,(e (evepairify val x) e)))            */
	 (else
	  (error "js-string->jsstring" "wrong syntax" x)))))

;*---------------------------------------------------------------------*/
;*    %js-jsstringliteral-begin! ...                                   */
;*---------------------------------------------------------------------*/
(define-expander %js-jsstringliteral-begin!
   (lambda (x e)
      (eval '(define js-strings '()))
;*       (eval '(define js-length 0))                                  */
;*       (eval '(define js-strings-vector (gensym 'JSSTRINGS)))        */
;*       (eval '(define (js-string-register! val)                      */
;* 	      (let ((c (assoc val js-strings)))                        */
;* 		 (if (pair? c)                                         */
;* 		     (cdr c)                                           */
;* 		     (let ((i js-length))                              */
;* 			(set! js-length (+fx 1 js-length))             */
;* 			(set! js-strings (cons (cons val i) js-strings)) */
;* 			i)))))                                         */
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
				    #;(weight ,(string-length s))
				    (left (string-ascii-sentinel-mark! ,s)))))
			(reverse! (eval 'js-strings)))))
	    e))))


   
