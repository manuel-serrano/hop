;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-09 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module dsssl-expander
   (import config
	   error
	   verbose)
   (export (dsssl-expand! prog)))

;; inlines/expands for-each, map, filter, ....

(define (dsssl-expand! prog)
   (verbose "replacing dsssl-prototypes")
   (walk! prog))

(define (walk! exp)
   (match-case exp
      (((kwote quote) . ?-)
       exp)
      ((lambda ?proto . ?body)
       (let* ((dsssl-error (lambda (p m o) (scheme2js-error p m o exp)))
	      (formals (dsssl-formals->scheme-formals proto dsssl-error))
	      (nbody (make-dsssl-function-prelude
		      exp proto (walk! `(begin ,@body)) dsssl-error)))
	  (set-car! (cdr exp) formals)
	  (set-car! (cddr exp) nbody)
	  (set-cdr! (cddr exp) '())
	  exp))
      ((?- . ?-)
       (map! walk! exp))
      (else
       exp)))
