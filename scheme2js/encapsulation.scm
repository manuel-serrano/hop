;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-13 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module encapsulation
   (import symbol
	   nodes
	   export-desc
	   config
	   verbose)
   (export (encapsulation! tree::Module)))

;; if we are encapsulating the modules (ie putting it into an anonymous
;; function), then we need/want a 'return' in the anonymous function.
;; don't forget to transmit the 'this' either.
;; js-call is not inlined in later passes (otherwise we would need to flag this
;; node)
(define (encapsulation! tree)
   (when (or (config 'encapsulate-modules)
	     (config 'suspend/resume))
      (verbose " encapsulation")
      (with-access::Module tree (body this-var)
	 (let* ((encapsulated-body (instantiate::Return (val body)))
		(encapsulation-lambda (instantiate::Lambda
					 (scope-vars '())
					 (formals '())
					 (arity 0)
					 (vaarg? #f)
					 (body encapsulated-body)))
		(call (instantiate::Call
			 (operator (runtime-reference 'js-call))
			 (operands (list (var-reference this-var)
					 encapsulation-lambda)))))
	 (set! body call)))))
