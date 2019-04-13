;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/stringliteral.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Sat Apr 13 07:35:50 2019 (serrano)                */
;*    Copyright   :  2014-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JsStringLiteral Helper macros.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   
   (static __js_strings::vector)
   
   (import __hopscript_stringliteral)
   
   (include "names.sch")

   (cond-expand
      ((config thread-local-storage #t)
       (pragma (__js_strings thread-local)))))

;*---------------------------------------------------------------------*/
;*    js-string->jsstring                                              */
;*---------------------------------------------------------------------*/
(define-expander js-string->jsstring
   (lambda (x e)
      (match-case x
	 ((js-string->jsstring (and ?val (? string?)))
	  (if (eq? (string-minimal-charset val) 'ascii)
	      (evepairify `(js-ascii->jsstring ,val) x)
	      (evepairify `(js-utf8->jsstring ,val) x)))
	 ((js-string->jsstring ?val)
	  `(js-string->jsstring
	      ,(e (evepairify val x) e)))
	 (else
	  (error "js-string->jsstring" "wrong syntax" x)))))

