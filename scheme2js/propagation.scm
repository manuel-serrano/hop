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

(module propagation
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   var-ref-util
	   free-vars
	   side
	   verbose
	   mutable-strings)
   (static (class Prop-Env
	      call/cc?::bool
	      suspend/resume?::bool
	      bigloo-runtime-eval?::bool)
	   (wide-class Prop-Call/cc-Call::Call
	      mutated-vars ;; vars that are mutated after the call
	      visible-whiles)
	   (wide-class Prop-While::While
	      mutated-vars) ;; vars that are mutated inside the while.
	   (wide-class Prop-Label::Label
	      var/vals-list)
	   (wide-class Prop-Var::Var
	      (escaping-mutated?::bool (default #f))
	      (current (default #f)))
	   (class List-Box
	      v::pair-nil))
   (export (propagation! tree::Module)))

;; uses While -> must be after while-pass
;;
;; locally propagate variables/constants. -> Removes var-count.
;; 
;; first pass: determine which variables are modified inside loops.
;; call/cc-calls count as loops. suspend/resumes don't.
;; each of these loops have a list of all mutated vars.
;; Also determine if a variable is changed outside its local scope (that is, if
;; it escapes, is it modified in these functions). -> .escaping-mutated?
;;
;; second pass: linearly walk through the code. the variables 'current' holds
;; the current value. a set! obviously updates it. a loop kills all the
;; variables that are affected.
;; if we have a binding x = y, and this is the only use of y (see
;; var-elimination), then we replace x by y. Exception applies for call/cc.
(define (propagation! tree)
   (when (config 'propagation)
      (verbose "propagation")
      (side-effect tree)
      (free-vars tree)
      (pass1 tree)
      (pass2! tree)))

(define (pass1 tree)
   (verbose " propagation1")
   (changed tree (instantiate::Prop-Env
		    (call/cc? (config 'call/cc))
		    (suspend/resume? (config 'suspend/resume))
		    (bigloo-runtime-eval? (config 'bigloo-runtime-eval)))
	    #f '() #f))

(define (widen-vars! vars)
   (for-each (lambda (v)
		(widen!::Prop-Var v
		   (escaping-mutated? #f)
		   (current #f)))
	     vars))
   
(define (widen-scope-vars! s::Scope)
   (with-access::Scope s (scope-vars)
      (widen-vars! scope-vars)))
   
;; surrounding-fun might be the Module. Used to detect free-vars. (that's all)
;; surrounding-whiles are the active whiles.
;; call/ccs are all encountered call-locations (of the current function), that
;; might reach call/ccs. call/ccs is boxed. It is continually updated and
;; represents the entry-point for call/cc-loops which extend up to the end of
;; the function.
(define-nmethod (Node.changed surrounding-fun surrounding-whiles call/ccs)
   (default-walk this surrounding-fun surrounding-whiles call/ccs))

(define-nmethod (Module.changed surrounding-fun surrounding-whiles call/ccs)
   (with-access::Module this (scope-vars runtime-vars imported-vars this-var)
      (for-each (lambda (v)
		   (with-access::Var v (constant?)
		      (widen!::Prop-Var v
			 (escaping-mutated? (not constant?))
			 (current #f))))
		scope-vars)
      (widen-vars! runtime-vars)
      (widen-vars! imported-vars)
      (widen!::Prop-Var this-var))
   (default-walk this this '() (instantiate::List-Box (v '()))))

(define-nmethod (Lambda.changed surrounding-fun surrounding-whiles call/ccs)
   (widen-scope-vars! this)
   (widen!::Prop-Var (with-access::Lambda this (this-var) this-var))
   (default-walk this this '() (instantiate::List-Box (v '()))))

;; result will be merged into orig
;; relies on the fact that all boxes will only append elements in front (using
;; cons). Searches for the 'orig'-list in all boxes, and simply merges the
;; elements that are before the 'orig'.
(define (merge-call/ccs! orig::List-Box boxes::pair-nil)
   (with-access::List-Box orig (v)
      (let ((orig-list v))
	 (for-each (lambda (b)
		      (let loop ((other-v (with-access::List-Box b (v) v))
				 (last #f))
			 (cond
			    ((and (eq? orig-list other-v)
				  (not last))
			     ;; no call/cc has been added to this branch.
			     'do-nothing)
			    ((eq? orig-list other-v)
			     ;; replace tail with the current v
			     (set-cdr! last v)
			     (set! v (with-access::List-Box b (v) v)))
			    (else
			     (loop (cdr other-v) other-v)))))
		   boxes))))

(define-nmethod (If.changed surrounding-fun surrounding-whiles call/ccs)
   (if (with-access::Prop-Env env (call/cc?) call/cc?)
       ;; the default-method works fine, but is not optimized: if there's a
       ;; call/cc in the 'then'-branch, then any var-update in the else-branch
       ;; would be seen as an update inside the call/cc-loop.
       (with-access::If this (test then else)
	  (walk test surrounding-fun surrounding-whiles call/ccs)
	  (let ((then-call/ccs (duplicate::List-Box call/ccs))
		(else-call/ccs (duplicate::List-Box call/ccs)))
	     (walk then surrounding-fun surrounding-whiles then-call/ccs)
	     (walk else surrounding-fun surrounding-whiles else-call/ccs)
	     (merge-call/ccs! call/ccs (list then-call/ccs else-call/ccs))))
       (default-walk this surrounding-fun surrounding-whiles call/ccs)))

(define-nmethod (Case.changed surrounding-fun surrounding-whiles call/ccs)
   (if (with-access::Prop-Env env (call/cc?) call/cc?)
       ;; default-walk would work here too (as for the If), but optimized...
       (with-access::Case this (key clauses)
	  (walk key surrounding-fun surrounding-whiles call/ccs)
	  (let ((call/ccs-clauses (map (lambda (ign)
					  (duplicate::List-Box call/ccs))
				       clauses)))
	     (for-each (lambda (clause call/ccs-clause)
			  (walk clause surrounding-fun surrounding-whiles
				call/ccs-clause))
		       clauses
		       call/ccs-clauses)
	     (merge-call/ccs! call/ccs call/ccs-clauses)))
       (default-walk this surrounding-fun surrounding-whiles call/ccs)))
	 
(define-nmethod (Call.changed surrounding-fun surrounding-whiles
			      call/ccs)
   (default-walk this surrounding-fun surrounding-whiles call/ccs)
   (when (and (with-access::Call this (call/cc?) call/cc?)
	      (with-access::Prop-Env env (call/cc?) call/cc?))
      (with-access::List-Box call/ccs (v)
      (cons-set! v this)
      
      ;; the call/cc might come back into the while-loop -> the variables
      ;; inside the while are not "safe" anymore.
      ;; Ex:
      ;;   var x = 3;
      ;;   while(..) {
      ;;      print(x);
      ;;      call/cc(..);
      ;;   }
      ;;   var x = 5;       // <----
      ;;   invoke_continuation
      ;;
      ;;  x is changed outside the while, but the call/cc can bring the change
      ;;  back into the loop.

      (widen!::Prop-Call/cc-Call this
	 (mutated-vars '())
	 (visible-whiles surrounding-whiles)))))

(define-nmethod (Let.changed surrounding-fun surrounding-whiles call/ccs)
   (widen-scope-vars! this)
   (default-walk this surrounding-fun surrounding-whiles call/ccs))


(define-nmethod (While.changed surrounding-fun surrounding-whiles call/ccs)
   (widen-scope-vars! this)
   (with-access::While this (init test body)
      (walk init surrounding-fun surrounding-whiles call/ccs)
      (widen!::Prop-While this
	 (mutated-vars '()))
      (let ((new-surround-whiles (cons this surrounding-whiles)))
	 (walk test surrounding-fun new-surround-whiles call/ccs)
	 (walk body surrounding-fun new-surround-whiles call/ccs))))

(define-nmethod (Set!.changed surrounding-fun surrounding-whiles call/ccs)
   (define (update-while while)
      (with-access::Set! this (lvalue)
	 (with-access::Ref lvalue (var)
	    (with-access::Prop-While while (mutated-vars)
	       (unless (memq var mutated-vars)
		  (cons-set! mutated-vars var))))))

   (define (update-call/cc-call cc-call)
      (with-access::Set! this (lvalue)
	 (with-access::Ref lvalue (var)
	    (with-access::Prop-Call/cc-Call cc-call (mutated-vars
						     visible-whiles)
	       (unless (memq var mutated-vars)
		  (cons-set! mutated-vars var))
	       (for-each update-while visible-whiles)))))

   (default-walk this surrounding-fun surrounding-whiles call/ccs)
   (with-access::Set! this (lvalue)
      (with-access::Ref lvalue (var)
	 ;; if the lvar is modified outside its scope, mark it as such.
	 (with-access::Execution-Unit surrounding-fun (free-vars)
	    (with-access::Prop-Var var (escaping-mutated? escapes?)
	       (when (and escapes?
			  (not escaping-mutated?) ;; already marked
			  (memq var free-vars))
		  (set! escaping-mutated? #t))))

	 ;; store us inside the surrounding whiles
	 (for-each update-while surrounding-whiles)

	 ;; update the call/ccs (which in turn will update their whiles
	 (for-each update-call/cc-call (with-access::List-Box call/ccs (v) v)))))

(define (pass2! tree)
   (verbose " propagation2")
   (propagate! tree (instantiate::Prop-Env
		       (call/cc? (config 'call/cc))
		       (suspend/resume? (config 'suspend/resume))
		       (bigloo-runtime-eval? (config 'bigloo-runtime-eval)))
	       (instantiate::List-Box (v '()))))

;; b1 will be the result-box
(define (merge-vals! b1::List-Box boxes::pair-nil)
   (define *all-vars* '())

   (define (get-vars b::List-Box)
      (with-access::List-Box b (v)
	 (for-each (lambda (p)
		      (let ((var (car p)))
			 (when (not (memq var *all-vars*))
			    ;; add to *all-vars*
			    (cons-set! *all-vars* var))))
		   v)))

   (define (merge b::List-Box)
      (with-access::List-Box b (v)
	 (for-each (lambda (p)
		      (let ((var (car p))
			    (val (cdr p)))
			 (with-access::Prop-Var var (current)
			    (cond
			       ((eq? val 'unknown)
				(set! current 'unknown))
			       ((eq? val current)
				'do-nothing)
			       ((not current)
				(set! current val))
			       ((and (isa? val Const)
				     (isa? current Const))
				(unless (eqv? (with-access::Const val (value) value)
					      (with-access::Const current (value) value))
				   (set! current 'unknown)))
			       ((and (isa? val Ref) (isa? current Ref))
				(unless (eq? (with-access::Ref val (var) var)
					     (with-access::Ref current (var) var))
				   (set! current 'unknown)))
			       (else
				(set! current 'unknown))))))
		   v)))

   (get-vars b1)
   (for-each (lambda (b) (get-vars b))
	     boxes)
   (merge b1)
   (for-each (lambda (b) (merge b))
	     boxes)
   (with-access::List-Box b1 (v)
      (set! v (map (lambda (var)
		      (with-access::Prop-Var var (current)
			 (let ((tmp current))
			    (set! current #f)
			    (cons var tmp))))
		   *all-vars*))))

(define (assq-val x b::List-Box)
   (let ((tmp (assq x (with-access::List-Box b (v) v))))
      (and tmp (cdr tmp))))

(define (update-val b::List-Box x val)
   (with-access::List-Box b (v)
      (cons-set! v (cons x val))))

(define (kill-var b::List-Box var)
   (with-access::List-Box b (v)
      (set! v (map (lambda (p)
		      (let ((other-var (car p))
			    (other-val (cdr p)))
			 (if (and (isa? other-val Ref)
				  (eq? var (with-access::Ref other-val (var) var))
				  (not (eq? other-var var)))
			     (cons other-var 'unknown)
			     p)))
		   v))))

(define-nmethod (Node.propagate! var/vals::List-Box)
   (error "propagate"
	  "Internal Error: forgot node type"
	  this))

(define-nmethod (Const.propagate! var/vals)
   this)

(define-nmethod (Ref.propagate! var/vals)
   (with-access::Ref this (var)
      (with-access::Prop-Var var (escaping-mutated?)
	 (let* ((val (assq-val var var/vals)))
	    (cond
	       ((or (not val)
		    (eq? val 'unknown)
		    escaping-mutated?)
		this)
	       ;; do not propagate variable-references when in suspend/call/cc
	       ;; mode.
	       ;; i.e. avoid things like
	       ;;   (let ((x y)) (if x (set! y (not y))))
	       ;; ->
	       ;;   (if y (set! y (not y)))
	       ((and (not (with-access::Prop-Env env (suspend/resume?) suspend/resume?))
		     (isa? val Ref)
		     (not (with-access::Ref val (var)
			     (with-access::Prop-Var var (escaping-mutated?) escaping-mutated?))))
		(var-reference (with-access::Ref val (var) var) :location val))
	       ((and (isa? val Const)
		     (with-access::Const val (value)
			(or (number? value)
			    (symbol? value)
			    (char? value)
			    (boolean? value)
			    (and (not (use-mutable-strings?))
				 (string? value)
				 (not (>fx (string-length value) 15)))
			    (eqv? #unspecified value))))
		(instantiate::Const
		   (location (with-access::Node val (location) location))
		   (value (with-access::Const val (value) value))))
	       (else
		this))))))

(define-nmethod (Module.propagate! var/vals)
   (default-walk! this (instantiate::List-Box (v '()))))

(define-nmethod (Lambda.propagate! var/vals)
   (with-access::Lambda this (formals)
      (for-each (lambda (formal)
		   (with-access::Ref formal (var)
		      (with-access::Prop-Var var (current)
			 (set! current 'unknown))))
		formals)
      (let ((lb (instantiate::List-Box
		   (v (map (lambda (formal)
			      (with-access::Ref formal (var)
				 (cons var 'unknown)))
			 formals)))))
	 (default-walk! this lb))))

(define-nmethod (If.propagate! var/vals)
   (with-access::If this (test then else)
      (set! test (walk! test var/vals))
      (let ((tmp-var/vals (duplicate::List-Box var/vals)))
	 (set! then (walk! then var/vals))
	 (set! else (walk! else tmp-var/vals))
	 (merge-vals! var/vals (list tmp-var/vals))
	 this)))

(define-nmethod (Case.propagate! var/vals)
   (with-access::Case this (key clauses)
      (set! key (walk! key var/vals))
   
      (let ((var/vals-clauses (map (lambda (ign)
				      (duplicate::List-Box var/vals))
				   clauses)))
	 (set! clauses (map! (lambda (clause var/vals-clause)
				(walk! clause var/vals-clause))
			     clauses
			     var/vals-clauses))
	 (merge-vals! var/vals var/vals-clauses)
	 this)))

(define-nmethod (Clause.propagate! var/vals)
   (default-walk! this var/vals))

(define-nmethod (Set!.propagate! var/vals)
   (define (transitive-value val)
      (if (isa? val Begin)
	  (transitive-value (car (last-pair (with-access::Begin val (exprs) exprs))))
	  val))

   (with-access::Set! this (lvalue val)
      (with-access::Ref lvalue (var)
	 (set! val (walk! val var/vals))
	 (update-val var/vals var (transitive-value val))

	 ;; kill all vars that depend on lvalue.var
	 (kill-var var/vals var)
	 this)))

(define-nmethod (Let.propagate! var/vals)
   (default-walk! this var/vals))

(define-nmethod (Begin.propagate! var/vals)
   ;; walk ensures left to right order.
   (default-walk! this var/vals))

(define-nmethod (Call.propagate! var/vals)
   (define (constant-value? n)
      (or (isa? n Const)
	  (and (isa? n Ref)
	       (with-access::Ref n (var)
		  (with-access::Var var (constant? value id)
		     (and constant?
			  value
			  (isa? value Const)
			  (with-access::Const value (value)
			     (or (pair? value) (vector? value)))
			  #t))))))
   
   (default-walk! this var/vals)
   (with-access::Call this (operator operands)
      (if (and (with-access::Prop-Env env (bigloo-runtime-eval?) bigloo-runtime-eval?)
	       (isa? operator Ref)
	       (runtime-ref? operator)
	       (every? constant-value? operands))
	  ;; for most runtime-functions we should be able to compute the result
	  ;; right now. (obviously a "print" won't work now...)
	  ;;
	  ;; optimize-runtime-op is at bottom of file.
	  (with-access::Ref operator (var)
	     (with-access::Var var (id)
		(or (optimize-runtime-op id operands
		       (with-access::Node operator (location) location))
		    this)))
	  this)))

(define-nmethod (Frame-alloc.propagate! var/vals)
   (default-walk! this var/vals))

(define-nmethod (Frame-push.propagate! var/vals)
   (default-walk! this var/vals))

(define-nmethod (Return.propagate! var/vals)
   (default-walk! this var/vals))

(define-nmethod (Labeled.propagate! var/vals)
   (with-access::Labeled this (label body)
      (widen!::Prop-Label label (var/vals-list '()))
      (set! body (walk! body var/vals))
      (with-access::Prop-Label label (var/vals-list)
	 (merge-vals! var/vals var/vals-list)
	 (shrink! label)
	 this)))

(define-nmethod (Break.propagate! var/vals)
   (with-access::Break this (val label)
      (set! val (walk! val var/vals))
      (with-access::Prop-Label label (var/vals-list)
	 (cons-set! var/vals-list (duplicate::List-Box var/vals)))
      this))

(define-nmethod (Continue.propagate! var/vals)
   this)

(define-nmethod (Pragma.propagate! var/vals)
   (with-access::Pragma this (args)
      (for-each (lambda (a) (walk! a var/vals)) args))
   (default-walk! this var/vals))

;; Tail-rec and Tail-rec-Call must not exist anymore.

(define-nmethod (Prop-While.propagate! var/vals)
   (with-access::Prop-While this (init test body mutated-vars label)
      ;; inits are outside of the loop.
      (set! init (walk! init var/vals))

      ;; if the test is true (which is currently the case), then we can't exit
      ;; the loop only by a break. -> clear the current var/vals. Any
      ;; surrounding Labeled will therefore ignore the result of var/vals.
      (when (and (isa? test Const) (with-access::Const test (value) value))
	 (let ((tmp (duplicate::List-Box var/vals)))
	    (with-access::List-Box var/vals (v)
	       (set! v '()))
	    (set! var/vals tmp))) ;; <== we replace the var/vals given as param
      ;; ------ var/vals is not the var/vals given as parameter anymore !!!
      (for-each (lambda (var)
		   (update-val var/vals var 'unknown))
		mutated-vars)
      (widen!::Prop-Label label (var/vals-list '()))
      (set! test (walk! test var/vals))
      (set! body (walk! body var/vals))
      this))
   
(define-nmethod (Prop-Call/cc-Call.propagate! var/vals)
   (with-access::Prop-Call/cc-Call this (mutated-vars)
      (default-walk! this var/vals)
      (for-each (lambda (var)
		   (update-val var/vals var 'unknown))
		mutated-vars)
      this))

(define-nmethod (Call/cc-Resume.propagate! var/vals)
   (default-walk! this var/vals))



(define (optimize-runtime-op op operands location)
   (define (operand->val op)
      (let ((tmp (if (isa? op Const)
		     (with-access::Const op (value) value)
		     (with-access::Ref op (var)
			(with-access::Var var (value)
			   (with-access::Const value (value)
			      value))))))
	 (cond
	    ((or (number? tmp)
		 (char? tmp)
		 (string? tmp)
		 (symbol? tmp)
		 (boolean? tmp)
		 (keyword? tmp)
		 (eq? tmp #unspecified))
	     tmp)
	    (else
	     (list 'quote tmp)))))

   (case op
      ((eqv? eq?)
       ;; ignore cases where we need to know the pointer-location.
       ;; we could do better here, but just don't take the risk. not worth it.
       (if (null? operands)
	   #f
	   (let ((fst-operand (operand->val (car operands))))
	      (if (or (number? fst-operand)
		      (char? fst-operand)
		      (and (not (use-mutable-strings?))
			   (string? fst-operand))
		      (symbol? fst-operand)
		      (boolean? fst-operand)
		      (keyword? fst-operand)
		      (eq? fst-operand #unspecified))
		  (with-handler
		     (lambda (e)
			(exception-notify e)
			#f)
		     (instantiate::Const
			(location location)
			(value (apply equal? (map operand->val operands)))))
		  #f))))
      ((equal? number? = < > <= >= zero? zerofx? negative? odd? even? max min
	    + * - / remainder modulo gcd lcm floor ceiling truncate round exp
	    log sin cos tan asin acos atan sqrt expt not boolean? pair? null?
	    char-numeric? char-whitespace? char-upper-case? char-lower-case?
	    char->integer char-upcase char-downcase char<? char>? char<=?
	    char>=? char=? char-ci<? char-ci>? char-ci<=? char-ci>=? char-ci=?
	    string<? string>? string<=? string>=? string=? string-ci<?
	    string-ci>? string-ci<=? string-ci>=? string-ci=?
	    string->list vector? vector-ref vector->list list->vector
	    number->string string->number symbol? string? string-append
	    symbol-append string-length substring keyword->string
	    string->keyword string-ref string-copy
	    bit-not bit-and bit-or bit-xor bit-lsh bit-rsh bit-ursh length)
       (with-handler
	  (lambda (e)
	     (exception-notify e)
	     #f)
	  (let ((res (eval `(,op ,@(map operand->val operands)))))
	     (instantiate::Const
		(location location)
		(value res)))))
      ((car cdr cadr cddr caar cdar
	    cadar cddar caaar cdaar caddr cdddr caadr cdadr
	    member memq memv length assoc assq assv)
       (with-handler
	  (lambda (e)
	     (exception-notify e)
	     #f)
	  (let ((res (eval `(,op ,@(map operand->val operands)))))
	     ;; if the result is a pair, ... we ignore it. (in case it has been
	     ;; shared... ex: (cdr '(1 2)) would yield '(2). But when producing
	     ;; the JS-code the new '(2) would not be equal to the one
	     ;; of '(1 2). -> Just don't take any risk...
	     ;; member, assoc, ... therefore can only yield #f here.
	     (if (or (number? res)
		     (char? res)
		     (and (not (use-mutable-strings?))
			  (string? res))
		     (symbol? res)
		     (boolean? res)
		     (keyword? res)
		     (eq? res #unspecified))
		 (instantiate::Const
		    (location location)
		    (value res))
		 #f))))
      (else
       #f)))
