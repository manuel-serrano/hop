;*=====================================================================*/
;*    serrano/prgm/project/hop/1.10.x/runtime/param-expd.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec  6 18:17:19 2006                          */
;*    Last change :  Mon Sep 29 09:56:21 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
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
;*    expand-define-lazy-parameter ...                                 */
;*---------------------------------------------------------------------*/
(define (expand-define-lazy-parameter id default setter)
   (let ((vid (symbol-append '* id '*))
	 (set (symbol-append id '-set!))
	 (key (symbol-append '* id '-key*)))
      `(begin
	  (define ,key (cons 1 2))
	  (define ,vid ,key)
	  (define (,id) (if (eq? ,vid ,key) ,default ,vid))
	  (define (,set v)
	     ,(if (pair? setter)
		  `(set! ,vid (,(car setter) v))
		  `(set! ,vid v))
	     v)
	  ,(when (pair? setter) `(,(car setter) ,default)))))

;*---------------------------------------------------------------------*/
;*    hop-define-parameter-expander ...                                */
;*---------------------------------------------------------------------*/
(define (hop-define-parameter-expander x e)
   (match-case x
      ((?- ?id ?default . ?setter)
       (e (evepairify (expand-define-parameter id default setter) x) e))
      (else
       (error 'define-expander "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    hop-define-lazy-parameter-expander ...                           */
;*---------------------------------------------------------------------*/
(define (hop-define-lazy-parameter-expander x e)
   (match-case x
      ((?- ?id ?default . ?setter)
       (e (evepairify (expand-define-lazy-parameter id default setter) x) e))
      (else
       (error 'define-expander "Illegal form" x))))
