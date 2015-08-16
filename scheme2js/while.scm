;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-13 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module while
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   symbol
	   loop-updates
	   verbose)
   (static (wide-class Vars-Label::Label
	      vars)
	   )
   (static (final-class While-Env
	      call/cc?::bool))
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
      (let* ((break-label (instantiate::Label (id (gensym 'while-break))))
	     (location (with-access::Node this (location) location))
	     (while (instantiate::While
		       (location location)
		       (scope-vars scope-vars)
		       (init (cond
				((null? inits)
				 (instantiate::Const (value #unspecified)))
				((null? (cdr inits))
				 (car inits))
				(else
				 (instantiate::Begin
				    (exprs inits)))))
		       (test (instantiate::Const
				(location location)
				(value #t)))
		       (body (instantiate::Break
				(location location)
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
			 (instantiate::Continue
			    (location (with-access::Node this (location) location))
			    (label label))))))))
   
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
(define (optimize-while! tree)
   (when (config 'while)
      (verbose " optimize-while")
      ;; search for our pattern(s) and apply them/it if found.
      (patterns! tree (instantiate::While-Env (call/cc? (config 'call/cc))))))

(define-nmethod (Node.patterns!)
   (default-walk! this))

(define-nmethod (While.patterns!)
   (with-access::While this (call/cc? test body label)
      
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
	     (set! test (instantiate::Call
			   (operator (runtime-reference 'not))
			   (operands (list iff-test))))
	     (set! body iff-else)
	     ;; note that the breaks are still there and go to the Labeled
	     ;; which is surrounding the While (and the new Begin).
	     (instantiate::Begin
		(exprs (list this iff-then))))
	    (else
	     (error "While-patterns!" "Internal Error: should never happen" #f))))

      (if (and (or (not (with-access::While-Env env (call/cc?) call/cc?)) ;; call/cc (and not just suspend)
		   (not call/cc?)) ;; this could be set by suspend/resume too.
	       (isa? test Const)
	       (eq? (with-access::Const test (value) value) #t)
	       (isa? body If))
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
   (with-access::While this (body)
      (walk body label found-fun)))

(define-nmethod (Continue.search-shallow-continue search-label found-fun)
   (with-access::Continue this (label)
      (when (eq? search-label label)
	 (found-fun #t))))
