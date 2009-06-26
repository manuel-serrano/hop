;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module srfi0
   (include "version.sch")
   (import error expand)
   (export
    (srfi0-expand expr)
    (srfi0-declare! sym::symbol)
    (srfi0-declared?::bool sym::symbol)))

(define *scheme2js-features*
   `(scheme2js srfi0 ,(string->symbol (format "scheme2js-~a" *version*))))

(define (srfi0-declare! sym)
   (cons sym *scheme2js-features*))

(define (srfi0-declared? sym)
   (memq sym *scheme2js-features*))

(define (srfi0-expand expr)
   (let ((e (lambda (x e) x)))
      (expand-cond-expand expr e *scheme2js-features*)))

(install-expander!
 'cond-expand
 (lambda (x e) (expand-cond-expand x e *scheme2js-features*)))
