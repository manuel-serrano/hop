;*=====================================================================*/
;*    serrano/prgm/project/hop/2.0.x/runtime/hss-expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr  3 15:32:27 2009                          */
;*    Last change :  Sat Jun  6 17:49:40 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    HSS expander                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    expand-define-hss-rule                                           */
;*---------------------------------------------------------------------*/
#;(define (expand-hss-rule x e)
   
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
;*    hop-define-hss-type-expander ...                                 */
;*---------------------------------------------------------------------*/
(define (hop-define-hss-type-expander x e)
   (match-case x
      ((define-hss-type (and ?id (? symbol?)) (and ?selector (? string?)) . ?p*)
       (let* ((nx `(hss-bind-type-compiler! ',id ,selector
		     (list ,@(map (lambda (p)
				     (match-case p
					((define-hss-property
					    ((and ?id (? symbol?))
					     (and ?value (? symbol?)))
					    . ?body)
					 `(cons
					   ',(string->symbol
					      (string-downcase
					       (symbol->string id)))
					   (lambda (,value ,(gensym))
					      (hss-properties->ruleset-list
					       (begin ,@body)))))
					((define-hss-property
					    ((and ?id (? symbol?))
					     (and ?value (? symbol?))
					     (and ?prio (? symbol?)))
					    . ?body)
					 `(cons
					   ',(string->symbol
					      (string-downcase
					       (symbol->string id)))
					   (lambda (,value ,prio)
					      (hss-properties->ruleset-list
					       (begin ,@body)))))
					(else
					 (error 'define-hss-type
						"Illegal property compiler"
						p))))
				  p*)))))
	(e (evepairify nx x) e)))
      (else
       (error 'define-hss-type "Illegal type compiler" x))))
		   
;*---------------------------------------------------------------------*/
;*    hop-define-hss-property-expander ...                             */
;*---------------------------------------------------------------------*/
(define (hop-define-hss-property-expander x e)
   (match-case x
      ((define-hss-property ((and ?id (? symbol?)) (and ?value (? symbol?)))
	  . ?body)
       (let ((nx `(hss-bind-property-compiler!
		   ',(string->symbol (string-downcase (symbol->string id)))
		   (lambda (,value ,(gensym))
		      (hss-property->declaration-list (begin ,@body))))))
	  (e (evepairify nx x) e)))
      ((define-hss-property ((and ?id (? symbol?))
			     (and ?value (? symbol?))
			     (and ?prio (? symbol?)))
	  . ?body)
       (let ((nx `(hss-bind-property-compiler!
		   ',(string->symbol (string-downcase (symbol->string id)))
		   (lambda (,value ,prio)
		      (hss-property->declaration-list (begin ,@body))))))
	  (e (evepairify nx x) e)))
      (else
       (error 'define-hss-property "Illegal property compiler" x))))
	  
