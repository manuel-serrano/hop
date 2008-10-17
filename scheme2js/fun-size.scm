(module fun-size
   (import nodes
	   export-desc
	   walk
	   verbose)
   (export (fun-size tree::Module)))

;; rough estimation of function sizes.
;; nested functions are added to the size of the surrounding fun.

(define (fun-size tree)
   (verbose " fun-size")
   (size tree #f #f))

(define-nmethod (Node.size surrounding-fun)
   ;; TODO: optimize fun-size.
   (when surrounding-fun
      (with-access::Lambda surrounding-fun (size)
	 (set! size (+fx size 1))))
   (default-walk this surrounding-fun))

(define-nmethod (Lambda.size surrounding-fun)
   (with-access::Lambda this (size)
      (set! size 0))
   (default-walk this this)
   (when surrounding-fun
      (with-access::Lambda surrounding-fun (size)
	 (set! size (+fx size (Lambda-size this))))))
