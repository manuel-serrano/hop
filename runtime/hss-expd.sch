;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hss-expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr  3 15:32:27 2009                          */
;*    Last change :  Mon May 18 17:51:44 2009 (serrano)                */
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

;*---------------------------------------------------------------------*/
;*    expand-define-hss-property ...                                   */
;*---------------------------------------------------------------------*/
;* (define (expand-define-hss-property x e)                            */
;*                                                                     */
;*    (match-case x                                                    */
;*       ((define-hss-property ((and ?property ?symbol)                */
;* 			     (and ?expr ?symbol)                       */
;* 			     (and ?prio ?symbol)) . ?body)             */
;*        `(store-hss-property-compiler! hss-property-global-env ',id  */
;*           (lambda (,expr ,prio)				       */
;* 	     (list->css-declaration-list ',id ,@body))))               */
;*       (else                                                         */
;*        (error 'define-hss-property "Illegal property compiler" x)))) */
	  
