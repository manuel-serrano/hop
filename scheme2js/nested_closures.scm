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

(module nested-closures
   (import nodes
	   export-desc
	   walk
	   captured-vars
	   verbose)
   (export (nested-closures tree::Module)))


(define (nested-closures tree)
   (verbose " nested-closures")
   (captured-vars tree)
   (nested tree #f #f))

(define-nmethod (Node.nested surrounding-fun)
   (default-walk this surrounding-fun))

(define-nmethod (Lambda.nested surrounding-fun)
   (with-access::Lambda this (closure?)
      (if (and surrounding-fun
	       closure?)
	  (Lambda-nested-closures?-set! surrounding-fun #t)))
   (default-walk this this))
