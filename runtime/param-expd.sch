;*=====================================================================*/
;*    serrano/prgm/project/hop/runtime/param-expd.sch                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:17:19 2006                          */
;*    Last change :  Wed Dec  6 18:19:52 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Parameters expander                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    expand-define-parameter ...                                      */
;*---------------------------------------------------------------------*/
(define (expand-define-parameter id default setter)
   (let ((vid (symbol-append '* id '*))
	 (set (symbol-append id '-set!)))
      `(begin
	  (define ,vid ,default)
	  (define (,id) ,vid)
	  (define (,set v)
	     ,(if (pair? setter)
		  `(set! ,vid (,(car setter) v))
		  `(set! ,vid v))
	     v)
	  (,set (,id)))))

;*---------------------------------------------------------------------*/
;*    hop-define-parameter-expander ...                                */
;*---------------------------------------------------------------------*/
(define (hop-define-parameter-expander x e)
   (match-case x
      ((?- ?id ?default . ?setter)
       (e (evepairify (expand-define-parameter id default setter) x) e))
      (else
       (error 'define-expander "Illegal form" x))))
