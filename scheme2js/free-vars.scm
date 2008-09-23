(module free-vars
   (import nodes
	   tools
	   walk
	   verbose)
   (export (free-vars tree::Module)))

;; Every Lambda/Module receives a list .free-vars of free variables.
;; Modules will have imported variables marked as free.
;; variables that are escaping (i.e. are free in some fun) have their
;; .escapes? flag set to #t.
(define (free-vars tree)
   (verbose " free vars")
   (find-free tree #f #f '()))

(define-nmethod (Node.find-free surrounding-fun visible-vars-list::pair-nil)
   (default-walk this surrounding-fun visible-vars-list))

(define-nmethod (Execution-Unit.find-free surrounding-fun visible-vars-list)
   (with-access::Execution-Unit this (scope-vars free-vars)
      (set! free-vars '())
      (default-walk this this (list scope-vars))
      (for-each (lambda (v)
		   (with-access::Var v (escapes?)
		      (set! escapes? #t)))
		free-vars)))

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
      (unless (any? (lambda (s)
		       (memq var s))
		    visible-vars-list)
	 (with-access::Execution-Unit surrounding-fun (free-vars)
	    (unless (memq var free-vars)
	       (cons-set! free-vars var))))))
