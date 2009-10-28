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

;; if the inlining pass duplicated some pragmas then they have to be
;; moved to the top-level. They must only exist at one location.

(module pragma
   (import config
	   walk
	   export-desc
	   tools
	   nodes
	   verbose)
   (static (wide-class Duplicated-Pragma::Pragma
	      (needs-to-be-moved? (default #f))))
   (export (pragmas! tree::Module)))

(define (pragmas! tree)
   (verbose "pragmas")
   (mark tree #f)
   (move! tree #f #f))

(define-nmethod (Node.mark)
   (default-walk this))

(define-nmethod (Pragma.mark)
   (widen!::Duplicated-Pragma this))

(define-nmethod (Duplicated-Pragma.mark)
   (with-access::Duplicated-Pragma this (needs-to-be-moved?)
      (set! needs-to-be-moved? #t)))

(define (make-pragma-let ht body)
   (let ((bindings (hashtable-map ht
				  (lambda (pragmaa var)
				     (instantiate::Set!
					(lvalue (var-reference var))
					(val pragmaa))))))
      (if (pair? bindings)
	  (instantiate::Let
	     (scope-vars (hashtable->list ht))
	     (bindings bindings)
	     (body body)
	     (kind 'let))
	  body)))

(define-nmethod (Node.move! ht)
   (default-walk! this ht))

(define-nmethod (Module.move! ht)
   (with-access::Module this (body)
      (let ((ht (make-eq-hashtable)))
	 (default-walk! this ht)
	 (set! body (make-pragma-let ht body))
	 this)))

(define-nmethod (Duplicated-Pragma.move! ht)
   (with-access::Duplicated-Pragma this (needs-to-be-moved?)
      (cond
	 ((not needs-to-be-moved?)
	  this)
	 ((hashtable-get ht this)
	  =>
	  (lambda (var)
	     (var-reference var :location this)))
	 (else
	  (let ((new-pragma (Ref-of-new-Var 'pragma)))
	     (hashtable-put! ht this (Ref-var new-pragma))
	     (Node-location-set! new-pragma this)
	     new-pragma)))))
