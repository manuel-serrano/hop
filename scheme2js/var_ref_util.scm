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

(module var-ref-util
   (import config
	   tools
	   nodes
	   export-desc)
   (export (constant-var n)
	   (runtime-ref-var n)
	   (call-target operator::Node)
	   (runtime-ref-var-id n)
	   (runtime-ref?::bool n)
	   (higher-order-runtime-ref?::bool n)))

(define (constant-var n)
   (and n
	(isa? n Ref)
	(with-access::Ref n (var)
	   (with-access::Var var (constant?)
	      (and constant?
		   var)))))

;; if n is a runtime-ref then return the var. otherwise #f
(define (runtime-ref-var n)
   (let ((v (constant-var n)))
      (and v
	   (with-access::Var v (kind export-desc)
	      (eq? kind 'imported)
	      (with-access::Export-Desc export-desc (runtime?)
		 runtime?))
	   v)))

(define (runtime-ref? n)
   (and (runtime-ref-var n)
	#t))

(define (higher-order-runtime-ref? n)
   (let ((v (runtime-ref-var n)))
      (and v
	   (with-access::Var v (export-desc)
	      (with-access::Export-Desc export-desc (higher?)
		 higher?))
	   #t)))

(define (runtime-ref-var-id n)
   (let ((v (runtime-ref-var n)))
      (and v (with-access::Var v (id) id))))

(define (call-target operator)
   (cond
      ((isa? operator Lambda)
       operator)
      ((runtime-ref? operator)
       operator)
      ((and (isa? operator Ref)
	    (with-access::Ref operator (var)
	       (with-access::Var var (constant? value)
		  (and constant? value))))
       (with-access::Ref operator (var)
	  (with-access::Var var (value)
	     (call-target value))))
      (else
       #f)))
