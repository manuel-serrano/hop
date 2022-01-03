;*=====================================================================*/
;*    serrano/prgm/project/hop/hop/hopscript/tls_expd.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 22 06:35:05 2014                          */
;*    Last change :  Mon Dec 20 19:47:10 2021 (serrano)                */
;*    Copyright   :  2014-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    thread local variables macros.                                   */
;*    -------------------------------------------------------------    */
;*    See expanders.sch and tls.sch                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    js-declare-tls-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (js-declare-tls-expander x e)
   (match-case x
      ((declare-tls ?var)
       (let ((nx `(cond-expand
		     ((and enable-tls (not bigloo-eval))
		      (define ,var #unspecified))
		     (else
		      #unspecified))))
	  (if (epair? x)
	      (e (econs (car nx) (cdr nx) (cer x)) e)
	      (e nx e))))
      (else
       (error "declare-tls" "bad form" x))))

;*---------------------------------------------------------------------*/
;*    js-define-tls-expander ...                                       */
;*---------------------------------------------------------------------*/
(define (js-define-tls-expander x e)
   (match-case x
      ((define-tls ?var #unspecified)
       (let ((nx `(cond-expand
		     ((and enable-tls (not bigloo-eval)) #unspecified)
		     (else (define ,var #unspecified)))))
	  (if (epair? x)
	      (e (econs (car nx) (cdr nx) (cer x)) e)
	      (e nx e))))
      ((define-tls ?var ?val)
       (let ((nx `(cond-expand
		     ((and enable-tls (not bigloo-eval))
		      (set! ,var (js-tls-gc-mark! ,val)))
		     (else
		      (define ,var ,val)))))
	  (if (epair? x)
	      (e (econs (car nx) (cdr nx) (cer x)) e)
	      (e nx e))))
      (else
       (error "define-tls" "bad form" x))))
