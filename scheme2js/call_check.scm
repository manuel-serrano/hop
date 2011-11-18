;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-11 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module call-check
   (import config
	   error
	   nodes
	   export-desc
	   walk
	   side
	   verbose)
   (export (call-check tree::Module)))

(define (call-check tree)
   (when (config 'call-check)
      (verbose "call-check")
      (side-effect tree)
      (check tree #f)))

(define-nmethod (Node.check)
   (default-walk this))


(define (check-arity call-len target-len id loc-node)
   (cond
      ((and (>=fx target-len 0)
	    (=fx call-len target-len))
       'ok)
      ((and (<fx target-len 0)
	    (>=fx call-len (-fx -1 target-len)))
       'ok)
      (else
       (let ((ln (cond
		    ((not (isa? loc-node Node)) #f)
		    ((with-access::Node loc-node (location) location)
		     loc-node)
		    ((isa? loc-node Call)
		     (with-access::Call loc-node (operator operands)
			(any (lambda (n)
				(with-access::Node n (location) location))
			   (cons operator operands))))
		    (else #f))))
       (scheme2js-error
	'call-check
	(format "Wrong number of arguments: ~a expected, ~a provided"
		target-len call-len)
	id
	ln)))))

(define-nmethod (Call.check)
   (with-access::Call this (operator operands)
      (when (isa? operator Ref)
	 (with-access::Ref operator (var)
	    (with-access::Var var (value id constant? kind export-desc)
	       (when (and constant? value)
		  (when (isa? value Const)
		     ;; all others could potentially become functions.
		     (scheme2js-error 'call-check
				      "Call target not a function"
				      (with-access::Const value (value) value)
				      this))
		  (when (isa? value Lambda)
		     (with-access::Lambda value (formals vaarg?)
			(let ((call-len (length operands))
			      (target-len (if vaarg?
					      (negfx (length formals))
					      (length formals))))
			   (check-arity call-len target-len id this)))))
	       (when (and constant? (eq? kind 'imported))
		  (with-access::Export-Desc export-desc (arity)
		     (when arity
			(check-arity (length operands) arity id this))))))))
   (default-walk this))

	       

