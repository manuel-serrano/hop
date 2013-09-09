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

(module deep-clone
   (import nodes
	   export-desc
	   tools
	   walk
	   verbose)
   (static (wide-class Cloned-Label::Label
	      replacement::Label)
	   (wide-class Cloned-Var::Var
	      replacement::Var)
	   (wide-class Cloned-Lambda::Lambda
	      replacement::Lambda))
   (export (deep-clone o::Node)))

;; recursively clones the given node and its children.
;; made for 'inlining'. If it is used otherwise the code has to be reviewed!
;;     (maybe it works, but I haven't payed attention).
;;
;; main-difficulty: when cloning a Let, Lambda, or Labeled the bodies have to
;; reference the new vars/labels. (that's what the Cloned-XXX above are for.
;;
;; most Nodes are redirected to 'do-clone' which simply recursively
;; clones. Others (vars, lambdas, ...) are directly treated in 'clone'. They
;; more or less represent special cases. Note, that 'clone' is generic on
;; 'object', but 'do-clone' is generic on 'Nodes' only.

(define (deep-clone o)
   (clone o))

(define-generic (clone this::object)
   (error "clone"
	  "Internal Error. forgot something..."
	  this))

(define-method (clone this::Node)
   (do-clone this))

(define-method (clone this::Label)
   this)
(define-method (clone this::Cloned-Label)
   (with-access::Cloned-Label this (replacement)
      replacement))

(define-method (clone this::Var)
   this)
(define-method (clone this::Cloned-Var)
   (with-access::Cloned-Var this (replacement)
      replacement))
(define (duplicate-Local var::Var)
   (with-access::Var var (value)
      (let ((new-value (cond
			  ((isa? value Cloned-Var)
			   (with-access::Cloned-Var var (replacement) replacement))
			  ((isa? value Cloned-Lambda)
			   (with-access::Cloned-Lambda value (replacement) replacement))
			  (else value))))
	 (duplicate::Var var
	    (value new-value)))))

(define-method (clone this::Lambda)
   (with-access::Lambda this (scope-vars this-var formals body)
      (let* ((new-scope-vars (map (lambda (var) (duplicate-Local var))
				  scope-vars))
	     (new-this-var (duplicate::Var this-var))
	     (new-lambda (duplicate::Lambda this
			    (scope-vars new-scope-vars)
			    (this-var new-this-var))))
	 ;; store clone-info
	 (map (lambda (old-var new-var)
		 (widen!::Cloned-Var old-var (replacement new-var)))
	      scope-vars new-scope-vars)
	 (widen!::Cloned-Var this-var (replacement new-this-var))
	 (widen!::Cloned-Lambda this (replacement new-lambda))

	 (with-access::Lambda new-lambda (formals body)
	    (set! formals (map clone formals))
	    (set! body (clone body)))
	 new-lambda)))

(define-method (clone this::Let)
   (with-access::Let this (scope-vars)
      (for-each (lambda (var)
		   (widen!::Cloned-Var var
		      (replacement (duplicate-Local var))))
		scope-vars)
      (do-clone this)))

(define-method (clone this::Labeled)
   (with-access::Labeled this (label)
      (with-access::Label label (id) 
	 (widen!::Cloned-Label label
	    (replacement (instantiate::Label (id (gensym id))))))
      (do-clone this)))

(define-method (clone this::Tail-rec)
   (with-access::Tail-rec this (label scope-vars)
      (for-each (lambda (var)
		   (widen!::Cloned-Var var
		      (replacement (duplicate-Local var))))
		scope-vars)
      (with-access::Label label (id)
	 (widen!::Cloned-Label label
	    (replacement (instantiate::Label (id (gensym id))))))
      (do-clone this)))

(define-method (clone this::Call/cc-Resume)
   (error "clone"
	  "Internal Error. clone on Call/cc-Resume should never happen."
	  ;; had the message 'Change method!'. no idea why... [flo]
	  #f))

(define-method (clone this::Pragma)
;*    ;; duplicated Pragmas will be (later) hoisted to the top-level so they are */
;*    ;; only used once.                                               */
   (with-access::Pragma this (args)
      (duplicate::Pragma this
	 (args (map clone args)))))

;; define-do-clone clones the given class, but calls 'clone' on the fields.
(define-macro (define-do-clone class . fields)
   `(define-method (do-clone ,(symbol-append 'this:: class))
       (,(symbol-append 'duplicate:: class)
	this
	,@(map (lambda (f)
		  (cond
		     ;; ex: (exprs) => (exprs (map clone (N-exprs this)))
		     ((pair? f)
		      (let ((name (car f)))
			 `(,name (map clone
				    (,(symbol-append 'with-access:: class)
				     this (,name) ,name)))))
		     (else
		      `(,f (clone (,(symbol-append 'with-access:: class)
				       this (,f) ,f))))))
	       fields))))

(define-generic (do-clone this::Node)
   (error "do-clone"
	  "Internal Error. forgot Node-type"
	  this))

(define-do-clone Const)
(define-do-clone Ref var)
;; Lambda can't appear here
(define-do-clone If test then else)
(define-do-clone Case key (clauses))
(define-do-clone Clause (consts) expr)
(define-do-clone Set! lvalue val)
(define-do-clone Let (scope-vars) (bindings) body)
(define-do-clone Begin (exprs))
(define-do-clone Call operator (operands))
(define-do-clone Return val)
(define-do-clone Labeled label body)
(define-do-clone Break val label)
(define-do-clone Continue label)
(define-do-clone Tail-rec (scope-vars) (inits) body label)
(define-do-clone Tail-rec-Call (updates) label)

;; should not be necessary, as deep-clone is only called from inside 'inline'.
(define-do-clone Frame-alloc storage-var (vars))
(define-do-clone Frame-push body frame-alloc)
(define-do-clone While test body label)
