(module symbol-table
   (import tools)
   (static (class Scope
	      kind::symbol ;; might be 'small or 'big (small -> list. big-> ht)
	      ht
	      els::pair-nil
	      (nb-els::bint (default 0)))
	   (class Lazy-Scope::Scope
	      lazy::procedure))
   (export (make-scope #!optional size)
	   (make-lazy-scope lazy-fun::procedure)
	   (symbol-var-set! scope id var)
	   (symbol-var scope id)
	   (scope->list scope)))

(define (make-scope #!optional size)
   (if (and size
	    (>fx size 50)) ;; TODO: hardcoded value
       (instantiate::Scope
	  (kind 'big)
	  (ht (make-hashtable (* size 2))) ;; TODO: hardcoded value
	  (els '()))
       (instantiate::Scope
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
   (with-access::Scope scope (kind ht els nb-els)
      (set! nb-els (+fx nb-els 1))
      (cond
	 ((eq? kind 'big)
	  (hashtable-put! ht id var))
	 ((< nb-els 50) ;; TODO: hardcoded value
	  (cons-set! els (cons id var)))
	 (else
	  (set! ht (make-hashtable 100))
	  (set! kind 'big)
	  (for-each (lambda (el)
		       (hashtable-put! ht (car el) (cdr el)))
		    els)
	  (set! els '())
	  (hashtable-put! ht id var)))))

(define (symbol-var scope id)
   (with-access::Scope scope (kind ht els)
      (define (get-entry)
	 (if (eq? kind 'big)
	     (hashtable-get ht id)
	     (let ((tmp (assq id els)))
		(and tmp (cdr tmp)))))

      (let ((entry (get-entry)))
	 (cond
	    (entry entry)
	    ((Lazy-Scope? scope)
	     (with-access::Lazy-Scope scope (lazy)
		(lazy scope id)))
	    (else #f)))))

(define (scope->list scope)
   (with-access::Scope scope (kind ht els)
      (if (eq? kind 'big)
	  (hashtable->list ht)
	  (map cdr els))))
