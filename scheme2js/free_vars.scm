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

(module free-vars
   (import nodes
	   export-desc
	   tools
	   walk
	   verbose)
   (export (free-vars tree::Module)))

;; Every Lambda/Module receives a list .free-vars of free variables.
;; Modules will have imported variables marked as free.
;; variables that are escaping (i.e. are free in some fun) have their
;; .escapes? flag set to #t.
;; If a variable is mutated outside its local function (that is it must
;; escape), then .mutated-outside-local? is set to true.
(define (free-vars tree)
   (verbose " free vars")
   (find-free tree #f #f '()))

(define-nmethod (Node.find-free surrounding-fun visible-vars-list::pair-nil)
   (default-walk this surrounding-fun visible-vars-list))

(define-nmethod (Execution-Unit.find-free surrounding-fun visible-vars-list)
   (with-access::Execution-Unit this (scope-vars free-vars)
      (set! free-vars '())
      (default-walk this this (list scope-vars))
      (when surrounding-fun
	 (let ((this-free-vars free-vars))
	    (with-access::Execution-Unit surrounding-fun (free-vars)
	       ;; free vars could be free for surrounding fun too.
	       (for-each (lambda (var)
			    (unless (or (any? (lambda (s) (memq var s))
					      visible-vars-list)
					(memq var free-vars))
			       (cons-set! free-vars var)))
			 this-free-vars))))))

(define-nmethod (Scope.find-free surrounding-fun visible-vars-list)
   (with-access::Scope this (scope-vars)
      (default-walk this surrounding-fun (cons scope-vars visible-vars-list))))

(define-nmethod (Frame-alloc.find-free surrounding-fun visible-vars-list)
   (default-walk this surrounding-fun visible-vars-list)
   (with-access::Frame-alloc this (storage-var)
      (with-access::Var storage-var (escapes?)
	 (set! escapes? #t))))

(define-nmethod (Ref.find-free surrounding-fun visible-vars-list)
   (with-access::Ref this (var)
      (unless (or (eq? (with-access::Var var (kind) kind) 'this)
		  (any? (lambda (s) (memq var s))
			visible-vars-list))
	 (with-access::Execution-Unit surrounding-fun (free-vars)
	    (unless (memq var free-vars)
	       (cons-set! free-vars var)))
	 (with-access::Var var (escapes?)
	    (set! escapes? #t)))))

(define-nmethod (Set!.find-free surrounding-fun visible-vars-list)
   (default-walk this surrounding-fun visible-vars-list)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (with-access::Var var (escapes? mutated-outside-local?)
	    (when (and escapes?
		       (not mutated-outside-local?) ;; already marked
		       (not (any? (lambda (s) (memq var s))
				  visible-vars-list)))
	       (set! mutated-outside-local? #t))))))
