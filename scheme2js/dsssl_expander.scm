;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/dsssl_expander.scm      */
;*    Author      :  Florian Loitsch                                   */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  2007-13                                           */
;*    Last change :  Fri Aug  2 16:23:26 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    DSSSL macro expansion (re-use Bigloo DSSSL utilities).           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module dsssl-expander
   (import config
	   error
	   verbose)
   (export (dsssl-expand! prog)))

;*---------------------------------------------------------------------*/
;*    dsssl-expand! ...                                                */
;*---------------------------------------------------------------------*/
(define (dsssl-expand! prog)
   (verbose "replacing dsssl-prototypes")
   (walk! prog))

;*---------------------------------------------------------------------*/
;*    walk! ...                                                        */
;*---------------------------------------------------------------------*/
(define (walk! exp)
   (match-case exp
      (((kwote quote) . ?-)
       exp)
      ((lambda ?proto . ?body)
       (let* ((dsssl-error (lambda (p m o) (scheme2js-error p m o exp)))
	      (formals (dsssl-formals->scheme-typed-formals proto dsssl-error #t))
	      (nbody (make-dsssl-function-prelude
			exp proto (walk! `(let () ,@body)) dsssl-error)))
	  (set-car! (cdr exp) formals)
	  (set-car! (cddr exp) nbody)
	  (set-cdr! (cddr exp) '())
	  exp))
      ((?- . ?-)
       (map! walk! exp))
      (else
       exp)))
