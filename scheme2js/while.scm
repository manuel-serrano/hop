(module while
   (import config
	   tools
	   nodes
	   walk
	   symbol
	   loop-updates
	   verbose)
   (static (wide-class Vars-Label::Label
	      vars)
	   (wide-class Tail-Label::Label
	      (used?::bool (default #f))
	      ;; if a Labeled surrounds a While, then the while-label is
	      ;; stored in the break-label too.
	      (while-label (default #f))))
   (export (tail-rec->while! tree::Module)
	   (optimize-while! tree::Module)))

;; transform Tail-recs into While-loops.
;;
;; this pass still introduces temporary variables (and scopes)
;; -> should be before scope-flattening
(define (tail-rec->while! tree)
   (verbose " tail-rec->while")
   (while! tree #f))

(define-nmethod (Node.while!)
   (default-walk! this))

(define-nmethod (Tail-rec.while!)
   (with-access::Tail-rec this (label scope-vars body inits)
      (widen!::Vars-Label label
	 (vars scope-vars))
      (default-walk! this)
      (shrink! label)
      (let* ((break-label (make-Label (gensym 'while-break)))
	     (while (instantiate::While
		       (scope-vars scope-vars)
		       (init (cond
				((null? inits)
				 (instantiate::Const (value #unspecified)))
				((null? (cdr inits))
				 (car inits))
				(else
				 (instantiate::Begin
				    (exprs inits)))))
		       (test (instantiate::Const (value #t)))
		       (body (instantiate::Break
				(val body)
				(label break-label)))
		       (label label)))
	     (labeled (instantiate::Labeled
			  (body while)
			  (label break-label))))
	 labeled)))

(define-nmethod (Tail-rec-Call.while!)
   (with-access::Tail-rec-Call this (label updates)
      (with-access::Vars-Label label (vars)
	 (default-walk! this)
	 (instantiate::Begin
	    (exprs (list (loop-updates-free-order vars updates)
			 (instantiate::Continue (label label))))))))
   
;; try to find loops, that can be transformed into optimized whiles.
;; In particular we want the test of the while to have a meaning (and not just
;; be "true").
;; Ex:
;; while (true) {
;;   break (
;;      if test
;;         foo
;;         blah_no_break
;;   )
;; }
;;   =>
;; while (not test) {
;;   blah_no_break [ remove continues to this while ]
;; }
;; foo
;;
(define (optimize-while! tree)
   (when (config 'while)
      (verbose " optimize-while")
      (search-patterns! tree)))

;; search for our pattern(s) and apply them/it if found.
(define (search-patterns! tree)
   (verbose " search-patterns")
   (patterns! tree #f))

(define-nmethod (Node.patterns!)
   (default-walk! this))

(define-nmethod (While.patterns!)
   (with-access::While this (test body label)
      
      (define (apply-pattern! continue-branch iff-test iff-then iff-else)
	 (case continue-branch
	    ((no-continue-in-else)
	     (set! test iff-test)
	     (set! body iff-then)
	     ;; note that the breaks are still there and go to the Labeled
	     ;; which is surrounding the While (and the new Begin).
	     (instantiate::Begin
		(exprs (list this iff-else))))
	    ((no-continue-in-then)
	     (set! test (instantiate::SCall
			   (operator (runtime-reference 'not))
			   (operands (list iff-test))))
	     (set! body iff-else)
	     ;; note that the breaks are still there and go to the Labeled
	     ;; which is surrounding the While (and the new Begin).
	     (instantiate::Begin
		(exprs (list this iff-then))))
	    (else
	     (error "While-patterns!" "should never happen" #f))))

      (if (and (Const? test)
	       (eq? (Const-value test) #t)
	       (If? body))
	  (with-access::If body (test then else)
	     (let ((new-this (cond
				((not (continue-in-branch? label else))
				 (apply-pattern! 'no-continue-in-else
						 test then else))
				((not (continue-in-branch? label then))
				 (apply-pattern! 'no-continue-in-then
						 test then else))
				(else
				 ;; we don't know how to optimize this case.
				 this))))
		(default-walk! new-this)))
	  (default-walk! this))))

;; #t if the given branch contains a continue to given label
(define (continue-in-branch? label branch)
   (bind-exit (found-fun)
      (search-shallow-continue branch #f label found-fun)
      #f))

(define-nmethod (Node.search-shallow-continue label found-fun)
   (default-walk this label found-fun))

(define-nmethod (Lambda.search-shallow-continue label found-fun)
   'do-not-go-into-lambdas)

(define-nmethod (While.search-shallow-continue label found-fun)
   'do-not-go-into-whiles)

(define-nmethod (Continue.search-shallow-continue search-label found-fun)
   (with-access::Continue this (label)
      (when (eq? search-label label)
	 (found-fun #t))))
