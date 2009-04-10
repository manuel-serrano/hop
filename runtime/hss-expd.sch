;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hss-expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr  3 15:32:27 2009                          */
;*    Last change :  Fri Apr  3 15:54:42 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HSS expander                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    expand-define-hss-rule                                           */
;*---------------------------------------------------------------------*/
(define (expand-hss-rule x e)
   
   (define (expand-rewrite y prop)
      (match-case y
	 (((and (? string?) ?element) (and (? string?) ?prop))
	  y)
	 (((and (? string?) ?element))
	  (list element prop))
	 (else
	  (error 'define-hss-rule "Illegal rule" x))))
   
   (define (expand-rule y)
      (match-case y
	 (((and (? keyword?) ?prop) . ?rewrites)
	  (cons (keyword->string prop)
		(map (lambda (y) (expand-rewrite y prop))  rewrites)))
	 (((and (? string?) ?prop) . ?rewrites)
	  (cons prop
		(map (lambda (y) (expand-rewrite y prop))  rewrites)))
	 (else
	  (error 'define-hss-rule "Illegal rule" x))))
   
   (match-case x
      (((and (? symbol?) ?id) ?element . ?rules)
       (let ((cmp `(instantiate::hss-compiler
		      (element ,(format "~a.__HSS_~a" element id))
		      (properties `'(,(map expand-rule rules))))))
	  (e `(hss-register-compiler! (symbol->string id) ,cmp) e)))
      (else
       (error 'define-hss-rule "Illegal form" x))))
