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

(module constants
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   verbose)
   (export (constants! tree::Module)))

(define (constants! tree)
   (verbose "constants")
   (when (config 'optimize-consts)
      (const! tree #f #f)))

(define-nmethod (Node.const! constant-ht)
   (default-walk! this constant-ht))

(define (make-constants-let constants-ht body)
   (let ((bindings (hashtable-map
		    constants-ht
		    (lambda (const decl/loc)
		       (instantiate::Set!
			  (lvalue (car decl/loc))
			  (val (instantiate::Const
				  (location (cdr decl/loc))
				  (value const))))))))
      (if (pair? bindings)
	  (instantiate::Let
	     (scope-vars (map (lambda (binding)
				 (with-access::Set! binding (lvalue)
				    (with-access::Ref lvalue (var)
				       var)))
			      bindings))
	     (bindings bindings)
	     (body body)
	     (kind 'let))
	  body)))

(define (my-hash o)
   (cond
      ((pair? o)
       (bit-xor (my-hash (car o))
		(my-hash (cdr o))))
      (else
       (get-hashnumber o))))

(define-nmethod (Module.const! ht)
   (with-access::Module this (body)
      (if (config 'encapsulate-modules)
	  (default-walk! this #f)
	  (let ((ht (create-hashtable :hash my-hash)))
	     (default-walk! this ht)
	     (set! body (make-constants-let ht body))
	     this))))

(define-nmethod (Lambda.const! ht)
   (with-access::Lambda this (body)
      (if ht ;; either module or another lambda already created the ht)
	  (default-walk! this ht)
	  (let ((ht (create-hashtable :hash my-hash)))
	     (default-walk! this ht)
	     ;; Lambda-body must be Return.
	     (with-access::Return body (val)
		(set! val (make-constants-let ht val)))
	     this))))

(define-nmethod (Const.const! constant-ht)
   (define (long-enough-list? l)
      (let loop ((l l)
		 (nb 0))
	 (cond
	    ((> nb 5) #t)
	    ((and (pair? l)
		  (or (pair? (car l))
		      (vector? (car l))))
	     #t)
	    ((pair? l)
	     (loop (cdr l) (+ nb 1)))
	    ((vector? l)
	     #t)
	    (else
	     #f))))
   (define (long-enough-vector? v)
      (or (> (vector-length v) 5)
	  (let loop ((i 0))
	     (cond
		((>= i (vector-length v))
		 #f)
		((or (pair? (vector-ref v i))
		     (vector? (vector-ref v i)))
		 #t)
		(else (loop (+ i 1)))))))
   (define (long-enough-string? v)
      (>fx (string-length v) 15))

   (with-access::Const this (value)
      (if (or (and (pair? value)
		   (long-enough-list? value))
	      (and (vector? value)
		   (long-enough-vector? value))
	      (and (string? value)
		   (long-enough-string? value)))
	  (let ((cached (hashtable-get constant-ht value)))
	     (if cached
		 (begin
		    (set-cdr! cached #f)
		    (with-access::Ref (car cached) (var)
		       (var-reference var :location this)))
		 (let ((new-const (Ref-of-new-Var 'const)))
		    (hashtable-put! constant-ht value
				    (cons new-const
				       (with-access::Node this (location)
					  location)))
		    (with-access::Ref new-const (var)
		       (var-reference var :location this)))))
	  this)))
