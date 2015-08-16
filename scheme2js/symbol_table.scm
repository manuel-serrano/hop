;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-12 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module symbol-table
   (import tools)
   (static (class STScope
	      kind::symbol ;; might be 'small or 'big (small -> list. big-> ht)
	      ht
	      els::pair-nil
	      (nb-els::bint (default 0)))
	   (class Lazy-Scope::STScope
	      lazy::procedure))
   (export (make-scope #!optional size)
	   (make-lazy-scope lazy-fun::procedure)
	   (symbol-var-set! scope id var)
	   (symbol-var scope id)
	   (scope->list scope)))

;; the scope-hashtable (as well as the list, that is used for shorter scopes)
;; is using the following comparison-functions:
;;  - if a qualified name is given (a pair), then it matches only the
;;    corresponding qualified var. (pairs only match pairs).
;;  - if an unqualified name is given (a symbol), then it matches unqualified
;;     _and_ qualified vars of the same name. (if there are several qualified
;;     names then the matched var is unspecified).

(define (make-scope-hashtable size)
   (create-hashtable
    :size size
    :eqtest (lambda (a b)
	       (cond
		  ((and (pair? a) (pair? b))
		   (equal? a b))
		  ((pair? a)
		   (eq? (car a) b))
		  ((pair? b)
		   (eq? (car b) a))
		  (else
		   (eq? a b))))
    :hash (lambda (a)
	     (if (pair? a)
		 (get-hashnumber (car a))
		 (get-hashnumber a)))))

(define (make-scope #!optional size)
   (if (and size
	    (>fx size 50)) ;; TODO: hardcoded value
       (instantiate::STScope
	  (kind 'big)
	  (ht (make-scope-hashtable (* size 2))) ;; TODO: hardcoded value
	  (els '()))
       (instantiate::STScope
	  (kind 'small)
	  (ht #f)
	  (els '()))))

(define (make-lazy-scope lazy-fun)
   (instantiate::Lazy-Scope
      (kind 'small)
      (ht #f)
      (els '())
      (lazy lazy-fun)))

(define (symbol-var-set! scope id var)
   (with-access::STScope scope (kind ht els nb-els)
      (set! nb-els (+fx nb-els 1))
      (cond
	 ((eq? kind 'big)
	  (hashtable-put! ht id var))
	 ((< nb-els 50) ;; TODO: hardcoded value
	  (cons-set! els (cons id var)))
	 (else
	  (set! ht (make-scope-hashtable 100))
	  (set! kind 'big)
	  (for-each (lambda (el)
		       (hashtable-put! ht (car el) (cdr el)))
		    els)
	  (set! els '())
	  (hashtable-put! ht id var)))))

(define (symbol-var scope id)
   (with-access::STScope scope (kind ht els)
      (define (get-entry)
	 (if (eq? kind 'big)
	     (hashtable-get ht id)
	     (any (lambda (entry)
		     (cond
			((eq? (car entry) id)
			 (cdr entry))
			((and (pair? id) (pair? (car entry)))
			 (and (equal? id (car entry))
			      (cdr entry)))
			((pair? id) #f)
			((pair? (car entry))
			 (and (eq? (caar entry) id)
			      (cdr entry)))
			(else #f)))
		  els)))

      (let ((entry (get-entry)))
	 (cond
	    (entry entry)
	    ((isa? scope Lazy-Scope)
	     (with-access::Lazy-Scope scope (lazy)
		(lazy scope id)))
	    (else #f)))))

(define (scope->list scope)
   (with-access::STScope scope (kind ht els)
      (if (eq? kind 'big)
	  (hashtable->list ht)
	  (map cdr els))))
