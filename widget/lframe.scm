;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/widget/lframe.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 26 07:21:23 2009                          */
;*    Last change :  Fri Jun 26 10:37:31 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HOP's lframes.                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hopwidget-lframe

   (library hop)

   (export  (<LFRAME> . ::obj)
	    (<LFLABEL> . ::obj)))

;*---------------------------------------------------------------------*/
;*    <LFRAME> ...                                                     */
;*---------------------------------------------------------------------*/
(define-markup <LFRAME> ((attrs) body)
   (<DIV> :hssclass "hop-lframe"
      attrs
      (let loop ((body body)
		 (l #f)
		 (b '()))
	 (cond
	    ((null? body)
	     (<DIV> :hssclass "hop-lfborder"
		l
		(<DIV> :hssclass "hop-lfbody" (reverse! b))))
	    ((lflabel? (car body))
	     (loop (cdr body) (car body) b))
	    (else
	     (loop (cdr body) l (cons (car body) b)))))))

;*---------------------------------------------------------------------*/
;*    lflabel? ...                                                     */
;*---------------------------------------------------------------------*/
(define (lflabel? el)
   (when (xml-element? el)
      (let ((hc (memq :hssclass (xml-element-attributes el))))
	 (when hc (string=? (cadr hc) "hop-lflabel")))))

;*---------------------------------------------------------------------*/
;*    <LFLABEL> ...                                                    */
;*---------------------------------------------------------------------*/
(define-markup <LFLABEL> ((attrs) body)
   (<DIV> :hssclass "hop-lflabel"
      attrs
      (<SPAN> body)))
   
