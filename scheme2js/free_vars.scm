;*=====================================================================*/
;*    serrano/prgm/project/hop/2.5.x/scheme2js/free_vars.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  2007-13                                           */
;*    Last change :  Tue Jul 30 07:14:10 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Free variables                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module free-vars
   (import nodes
	   dump-node
	   export-desc
	   tools
	   walk
	   verbose)
   (export (free-vars tree::Module)))

;*---------------------------------------------------------------------*/
;*    free-vars ...                                                    */
;*    -------------------------------------------------------------    */
;*    Every Lambda/Module receives a list .free-vars of free           */
;*    variables. Modules will have imported variables marked as free.  */
;*    variables that are escaping (i.e. are free in some fun) have     */
;*    their .escapes? flag set to #t.                                  */
;*    If a variable is mutated outside its local function (that is it  */
;*    must escape), then .mutated-outside-local? is set to true.       */
;*---------------------------------------------------------------------*/
(define (free-vars tree)
   (verbose " free vars")
   (find-free tree #f #f '()))

;*---------------------------------------------------------------------*/
;*    find-free ::Node ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Node.find-free surrounding-scope visible-vars-list::pair-nil)
   (default-walk this surrounding-scope visible-vars-list))

;*---------------------------------------------------------------------*/
;*    find-free ::Execution-Unit ...                                   */
;*---------------------------------------------------------------------*/
(define-nmethod (Execution-Unit.find-free surrounding-scope visible-vars-list)
   (with-access::Execution-Unit this (scope-vars free-vars)
      (set! free-vars '())
      (default-walk this this (list scope-vars))
      (when surrounding-scope
	 (let ((this-free-vars free-vars))
	    (with-access::Execution-Unit surrounding-scope (free-vars)
	       ;; free vars could be free for surrounding fun too.
	       (for-each (lambda (var)
			    (unless (or (any (lambda (s) (memq var s))
					   visible-vars-list)
					(memq var free-vars))
			       (cons-set! free-vars var)))
		  this-free-vars))))))

;*---------------------------------------------------------------------*/
;*    find-free ::Scope ...                                            */
;*---------------------------------------------------------------------*/
(define-nmethod (Scope.find-free surrounding-scope visible-vars-list)
   (with-access::Scope this (scope-vars)
      (default-walk this surrounding-scope (cons scope-vars visible-vars-list))))

;*---------------------------------------------------------------------*/
;*    find-free ::Frame-alloc ...                                      */
;*---------------------------------------------------------------------*/
(define-nmethod (Frame-alloc.find-free surrounding-scope visible-vars-list)
   (default-walk this surrounding-scope visible-vars-list)
   (with-access::Frame-alloc this (storage-var)
      (with-access::Var storage-var (escapes?)
	 (set! escapes? #t))))

;*---------------------------------------------------------------------*/
;*    find-free ::Ref ...                                              */
;*---------------------------------------------------------------------*/
(define-nmethod (Ref.find-free surrounding-scope visible-vars-list)
   (with-access::Ref this (var)
      (with-access::Var var (kind id)
	 (unless (or (memq kind '(this exported imported))
		     (any (lambda (s) (memq var s)) visible-vars-list))
	    (unless (or (memq kind '(this exported imported))
			(any (lambda (s) (memq var s)) visible-vars-list))
	       (with-access::Execution-Unit surrounding-scope (free-vars)
		  (unless (memq var free-vars)
		     (cons-set! free-vars var)))
	       (with-access::Var var (escapes?)
		  (set! escapes? #t)))))))

;*---------------------------------------------------------------------*/
;*    find-free ::Set! ...                                             */
;*---------------------------------------------------------------------*/
(define-nmethod (Set!.find-free surrounding-scope visible-vars-list)
   (default-walk this surrounding-scope visible-vars-list)
   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (with-access::Var var (escapes? mutated-outside-local?)
	    (when (and escapes?
		       (not mutated-outside-local?) ;; already marked
		       (not (any (lambda (s) (memq var s)) visible-vars-list)))
	       (set! mutated-outside-local? #t))))))
