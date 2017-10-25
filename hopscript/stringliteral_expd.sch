;*=====================================================================*/
;*    .../prgm/project/hop/3.2.x/hopscript/stringliteral_expd.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct 24 02:21:25 2017                          */
;*    Last change :  Tue Oct 24 02:23:30 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    string expanders                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-jsstring-append ...                                           */
;*---------------------------------------------------------------------*/
(define (js-jsstring-append-expander x e)
   
   (define (expand left right)
      `(if (or (isa? ,left JsStringLiteralUTF8) (isa? ,right JsStringLiteralUTF8))
	   (instantiate::JsStringLiteralUTF8
	      (weight (js-jsstring-length ,left))
	      (left ,left)
	      (right ,right))
	   (instantiate::JsStringLiteralASCII
	      (weight (js-jsstring-length ,left))
	      (left ,left)
	      (right ,right))))
   
   (match-case x
      ((js-jsstring-append (and (? symbol?) ?left) (and (? symbol?) ?right))
       (e (expand left right) e))
      ((js-jsstring-append (and (? symbol?) ?left) ?right)
       (let ((r (gensym 'right)))
	  (e `(let ((,r ,right)) ,(expand left r)) e)))
      ((js-jsstring-append ?left (and (? symbol?) ?right))
       (let ((l (gensym 'left)))
	  (e `(let ((,l ,left)) ,(expand l right)) e)))
      ((js-jsstring-append ?left ?right)
       (let ((l (gensym 'left))
	     (r (gensym 'right)))
	  (e `(let ((,l ,left) (,r ,right)) ,(expand l r)) e)))
      (else
       (e `((@ js-jsstring-append __hopscript_stringliteral) ,@(cdr x)) e))))

