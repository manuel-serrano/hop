;*=====================================================================*/
;*    serrano/prgm/project/hop/2.1.x/runtime/hss-expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr  3 15:32:27 2009                          */
;*    Last change :  Sat Jun 19 06:48:48 2010 (serrano)                */
;*    Copyright   :  2009-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HSS expander                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    hop-define-hss-type-expander ...                                 */
;*---------------------------------------------------------------------*/
(define (hop-define-hss-type-expander x e)

   (define (local-define-hss-property-expander p)
      (match-case p
	 ((define-hss-property
	     ((and ?id (? symbol?)) (and ?value (? symbol?))) . ?body)
	  `(cons
	    ',(string->symbol
	       (string-downcase
		(symbol->string id)))
	    (lambda (,value ,(gensym))
	       (hss-properties->ruleset-list
		(begin ,@body)))))
	 ((define-hss-property ((and ?id (? symbol?))
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
	  (error "define-hss-type"
		 "Illegal property compiler"
		 p))))
   
   (match-case x
      ((define-hss-type (and ?id (? symbol?))
	  (and ?selector (? string?))
	  :body (and ?body (? string?))
	  . ?p*)
       (let* ((nx `(hss-bind-type-compiler! ',id ,selector ,body
		     (list ,@(map local-define-hss-property-expander p*)))))
	(e (evepairify nx x) e)))
      ((define-hss-type (and ?id (? symbol?))
	  (and ?selector (? string?))
	  . ?p*)
       (let* ((nx `(hss-bind-type-compiler! ',id ,selector #f
		     (list ,@(map local-define-hss-property-expander p*)))))
	(e (evepairify nx x) e)))
      (else
       (error "define-hss-type" "Illegal type compiler" x))))
		   
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
       (error "define-hss-property" "Illegal property compiler" x))))

;*---------------------------------------------------------------------*/
;*    hop-define-hss-function-expander ...                             */
;*---------------------------------------------------------------------*/
(define (hop-define-hss-function-expander x e)
   (match-case x
      ((define-hss-function ((and ?id (? symbol?)) . ?args) . ?body)
       (let ((nx `(hss-bind-function-compiler!
		   ,(string-downcase (symbol->string id))
		   (lambda ,args ,@body))))
	  (e (evepairify nx x) e)))
      (else
       (error "define-hss-function" "Illegal function compiler" x))))
