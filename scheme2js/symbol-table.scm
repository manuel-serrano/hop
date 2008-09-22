(module symbol-table
   (import tools)
   (static (class Scope)
	   (final-class Big-Scope::Scope
	      ht)
	   (final-class Small-Scope::Scope ;; might become big (many els), but starts small...
	      els::pair-nil))
   (export (make-scope #!optional size)
	   
	   (symbol-var-set! scope symbol var)
	   (symbol-var scope symbol)
	   (scope->list scope)))

(define (make-scope #!optional size)
   (if (and size
	    (>fx size 50)) ;; TODO: hardcoded value
       (instantiate::Big-Scope
	  (ht (make-eq-hashtable (* size 2)))) ;; TODO: hardcoded value
       (instantiate::Small-Scope
	  (els '()))))

(define (symbol-var-set! scope symbol var)
   (if (Big-Scope? scope)
       (with-access::Big-Scope scope (ht)
	  (hashtable-put! ht symbol var))
       (with-access::Small-Scope scope (els)
	  (cons-set! els (cons symbol var)))))

(define (symbol-var scope symbol)
   (if (Big-Scope? scope)
       (with-access::Big-Scope scope (ht)
	  (hashtable-get ht symbol))
       (with-access::Small-Scope scope (els)
	  (let ((tmp (assq symbol els)))
	     (and tmp (cdr tmp))))))

(define (scope->list scope)
   (if (Big-Scope? scope)
       (with-access::Big-Scope scope (ht)
	  (hashtable->list ht))
       (with-access::Small-Scope scope (els)
	  (map cdr els))))
