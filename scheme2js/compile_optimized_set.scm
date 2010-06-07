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

(module compile-optimized-set
   (export
    (compile-unoptimized-set! p compile::procedure n::Node)
    (compile-set! p compile::procedure node::Node))
   (import config
	   tools
	   template-display
	   nodes
	   allocate-names
	   export-desc))

(define *set!-operators*
   '(("sc_plus" "+")
     ("sc_multi" "*")
     ("sc_minus" "-")
     ("sc_div" "/")
     ;; TODO: modulo can't be converted directly to '%'
     ("sc_modulo" "%")
     ("sc_bitAnd" "&")
     ("sc_bitOr" "|")
     ("sc_bitXor" "^")))

(define (compile-optimized-set! p compile n)
   (with-access::Set! n (lvalue val)
      (with-access::Ref lvalue (var)
	 (if (Call? val)
	     (with-access::Call val (operator operands)
		(if (and (not (null? operands))
			 (not (null? (cdr operands)))
			 ;; next test not strictly necessary, but
			 ;; simplifies cases like "(set! x (- x 1 2 3))"
			 (null? (cddr operands))
			 (Ref? (car operands))
			 (eq? (Ref-var (car operands)) var)
			 (Ref? operator)
			 (let ((op-var (Ref-var operator)))
			    (with-access::Var op-var (constant? kind)
			       (and (eq? kind 'imported)
				    constant?))))
		    (let* ((op-var (Ref-var operator))
			   (desc (Var-export-desc op-var))
			   (js-id (Export-Desc-js-id desc))
			   (entry (assoc js-id *set!-operators*)))
		       (if entry
			   ;; get ++ and --
			   (if (and (or (string=? (cadr entry) "+")
					(string=? (cadr entry) "-"))
				    (Const? (cadr operands))
				    (eq? 1 (Const-value (cadr operands))))
			       (template-display p
				  "(~a~a~a)"
				  (cadr entry) (cadr entry) ;; ++ or --
				  (Named-Var-js-id var))
			       (with-access::Named-Var var (js-id)
				  (template-display p
				     "($js-id ~a= ~e)"
				     (cadr entry)
				     (compile (cadr operands) p #f))))
			   (compile-unoptimized-set! p compile n)))
		    (compile-unoptimized-set! p compile n)))
	     (compile-unoptimized-set! p compile n)))))

(define (compile-unoptimized-set! p compile n)
   (with-access::Set! n (lvalue val)
      (template-display p
	 "~e = ~e"
	 (compile lvalue p #f)
	 (compile val p #f))))

(define (compile-set! p compile n)
   ;; TODO: get rid of '(config ...)
   (if (config 'optimize-set!)
       (compile-optimized-set! p compile n)
       (compile-unoptimized-set! p compile n)))
