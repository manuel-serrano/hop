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

(module compile-optimized-boolify
   (export (compile-boolified p
			      compile::procedure
			      node::Node))
   (import config
	   tools
	   nodes
	   export-desc
	   template-display
	   verbose))

(define (compile-optimized-if-boolify p compile n)
   (with-access::If n (test then else)
      (if (and (Const? else)
	       (with-access::Const else (value)
		  (not value)))
	  (template-display p
	     "(~e&&~e)"
	     (compile test p #f)
	     (compile-optimized-boolify p compile then))
	  (compile-unoptimized-boolify p compile n))))

(define (compile-optimized-boolify p compile n)
   (cond
      ((Call? n)
       (with-access::Call n (operator operands)
	  (if (Ref? operator)
	      (with-access::Ref operator (var)
		 (with-access::Var var (kind constant? export-desc)
		    (if (and (or (eq? kind 'exported)
				 (eq? kind 'imported))
			     constant?
			     (eq? (Export-Desc-return-type export-desc)
				  'bool))
			(compile n p #f)
			(compile-unoptimized-boolify p compile n))))
	      (compile-unoptimized-boolify p compile n))))
      ((If? n)
       (compile-optimized-if-boolify p compile n))
      ((Const? n)
       (with-access::Const n (value)
	  (template-display p
	     "~a" (if value "true" "false"))))
      (else
       (compile-unoptimized-boolify p compile n))))

(define (compile-unoptimized-boolify p compile node)
   (template-display p
      "(~e !== false)"
      (compile node p #f)))
   
(define (compile-boolified p compile node)
   ;; TODO: get rid of '(config ... )
   (if (config 'optimize-boolify)
       (compile-optimized-boolify p compile node)
       (compile-unoptimized-boolify p compile node)))

